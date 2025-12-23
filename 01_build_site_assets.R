# 01_build_site_assets.R
# Build static assets (maps + response-time outputs) for GitHub Pages (docs/)
# -------------------------------------------------------------------------
# KEY RULES (per your instructions)
# - KEEP coordinate approach: geo_x/geo_y in EPSG:2273 -> transform to EPSG:4326
# - Boundary: places(state="SC", cb=FALSE, year=2023), NAME=="Lexington"
# - OSM layers from bbox (streets/water/rivers)
# - DO NOT combine traffic & traffic_stop
# - Denominators include: burglary, domestic_problem, suspicious, violent_crime, traffic
# - Time: annual + all-time (NO in-between transitions)
# - Maps: no legends (left panel explanation is enough)
# - Response time: mean minutes from call received to first unit arrival
# - RT trend: one plot with systemwide + by-denom, colorblind palette
# -------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(sf)
  library(ggplot2)
  library(MASS)
  library(jsonlite)
  library(tigris)
  library(osmdata)
  library(stringr)
  library(readr)
  library(janitor)
  library(viridis)
})

options(tigris_use_cache = TRUE)

# -------------------------
# Paths
# -------------------------
rds_path  <- "data/cad_data.rds"
docs_dir  <- "docs"
maps_dir  <- file.path(docs_dir, "maps")
rt_dir    <- file.path(docs_dir, "rt")
tbl_dir   <- file.path(docs_dir, "tables")
manifest_path <- file.path(docs_dir, "manifest.json")

dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(maps_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(rt_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------
# Load data
# -------------------------
cad_data <- readRDS(rds_path) %>% janitor::clean_names()

# ------------------------------------------------------------------
# COORDINATES (KEEP EXACT APPROACH)
# ------------------------------------------------------------------
cad_sf <- cad_data %>%
  st_as_sf(coords = c("geo_x", "geo_y"), crs = 2273, remove = FALSE) %>%
  st_transform(4326)

coords <- st_coordinates(cad_sf)
cad_sf$lon <- coords[, 1]
cad_sf$lat <- coords[, 2]

# ------------------------------------------------------------------
# Lexington boundary (KEEP EXACT APPROACH)
# ------------------------------------------------------------------
lexington_boundary <- places(state = "SC", cb = FALSE, year = 2023) %>%
  filter(NAME == "Lexington") %>%
  st_transform(4326)

bbox <- st_bbox(lexington_boundary)
bbox_num <- c(xmin = bbox$xmin, ymin = bbox$ymin, xmax = bbox$xmax, ymax = bbox$ymax)
bbox_vec <- c(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)

# ------------------------------------------------------------------
# OSM layers (KEEP EXACT APPROACH)
# ------------------------------------------------------------------
osm_q <- opq(bbox = bbox_num)

streets <- osm_q %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

water <- osm_q %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

rivers <- osm_q %>%
  add_osm_feature(key = "waterway", value = c("river", "stream")) %>%
  osmdata_sf()

# ------------------------------------------------------------------
# CLEAN POINTS AND CLIP TO COUNTY (KEEP YOUR ORIGINAL FILTER)
# ------------------------------------------------------------------
cad_sf_clean <- cad_sf %>%
  filter(
    lon > -82, lon < -80,
    lat > 33.5, lat < 34.5
  )

cad_sf_clip <- cad_sf_clean[lexington_boundary, ]

# ------------------------------------------------------------------
# DENOMINATORS (traffic included; traffic_stop NOT combined)
# ------------------------------------------------------------------
denoms <- c("burglary", "domestic_problem", "suspicious", "violent_crime", "traffic")

# ------------------------------------------------------------------
# TIME PERIODS: ANNUAL + ALL-TIME (NO transitions)
# ------------------------------------------------------------------
time_col <- if ("call_time" %in% names(cad_sf_clip)) "call_time" else "call_datetime"

cad_sf_clip <- cad_sf_clip %>%
  mutate(year = lubridate::year(.data[[time_col]]))

years_vec <- cad_sf_clip %>%
  st_drop_geometry() %>%
  distinct(year) %>%
  filter(!is.na(year)) %>%
  arrange(year) %>%
  pull(year)

periods <- c("all", as.character(years_vec))

# ------------------------------------------------------------------
# PROACTIVE FLAG (KEEP YOUR EXISTING LOGIC)
# ------------------------------------------------------------------
cad_sf_clip <- cad_sf_clip %>%
  mutate(
    proactive = case_when(
      nature_recat %in% c("traffic_stop", "property_check") &
        abs(as.numeric(difftime(first_arrived, first_dispatched, units = "secs"))) <= 10 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ------------------------------------------------------------------
# RESPONSE TIMES IN MINUTES (MEAN), computed from timestamps
# ------------------------------------------------------------------
cad_sf_clip <- cad_sf_clip %>%
  mutate(
    rt_call_to_dispatch_min   = as.numeric(difftime(first_dispatched, .data[[time_col]], units = "mins")),
    rt_dispatch_to_arrive_min = as.numeric(difftime(first_arrived, first_dispatched, units = "mins")),
    rt_call_to_arrive_min     = as.numeric(difftime(first_arrived, .data[[time_col]], units = "mins")),
    rt_on_scene_min           = as.numeric(difftime(last_cleared, first_arrived, units = "mins"))
  ) %>%
  mutate(
    # sanity trims to protect the mean
    rt_call_to_dispatch_min   = if_else(rt_call_to_dispatch_min   < 0 | rt_call_to_dispatch_min   > 60,   NA_real_, rt_call_to_dispatch_min),
    rt_dispatch_to_arrive_min = if_else(rt_dispatch_to_arrive_min < 0 | rt_dispatch_to_arrive_min > 60,   NA_real_, rt_dispatch_to_arrive_min),
    rt_call_to_arrive_min     = if_else(rt_call_to_arrive_min     < 0 | rt_call_to_arrive_min     > 120,  NA_real_, rt_call_to_arrive_min),
    rt_on_scene_min           = if_else(rt_on_scene_min           < 0 | rt_on_scene_min           > 24*60, NA_real_, rt_on_scene_min)
  )

# ------------------------------------------------------------------
# KDE helpers
# ------------------------------------------------------------------
make_kde <- function(df, h = c(0.015, 0.015), n = 300) {
  MASS::kde2d(
    x = df$lon,
    y = df$lat,
    h = h,
    n = n,
    lims = bbox_vec
  )
}

normalize_kde <- function(kde) {
  z <- kde$z
  rng <- max(z, na.rm = TRUE) - min(z, na.rm = TRUE)
  if (is.na(rng) || rng == 0) {
    kde$z <- z * 0
  } else {
    kde$z <- (z - min(z, na.rm = TRUE)) / rng
  }
  kde
}

make_diff_df <- function(kde_pro, kde_den, pow = 0.8) {
  diff_raw <- kde_pro$z - kde_den$z
  diff_stretch <- sign(diff_raw) * abs(diff_raw)^pow
  expand.grid(lon = kde_pro$x, lat = kde_pro$y) %>%
    mutate(diff = as.vector(diff_stretch))
}

# ------------------------------------------------------------------
# Base map layers (consistent across all outputs)
# ------------------------------------------------------------------
plot_base_layers <- function() {
  ggplot() +
    { if (!is.null(water$osm_polygons))
      geom_sf(data = water$osm_polygons, fill = "#6baed6", color = NA, alpha = 0.85) } +
    { if (!is.null(rivers$osm_lines))
      geom_sf(data = rivers$osm_lines, color = "#2171b5", linewidth = 0.7, alpha = 0.9) } +
    geom_sf(
      data = streets$osm_lines %>% filter(highway %in% c("motorway","trunk","primary")),
      color = "black", linewidth = 1.0, alpha = 0.85
    ) +
    geom_sf(
      data = streets$osm_lines %>% filter(!highway %in% c("motorway","trunk","primary")),
      color = "gray50", linewidth = 0.3, alpha = 0.55
    ) +
    geom_sf(data = lexington_boundary, fill = NA, color = "black", linewidth = 0.8) +
    coord_sf(
      xlim = c(bbox$xmin, bbox$xmax),
      ylim = c(bbox$ymin, bbox$ymax),
      expand = FALSE
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

save_png <- function(p, path, w = 1400, h = 1000) {
  ggsave(path, p, width = w/150, height = h/150, dpi = 150)
}

# ------------------------------------------------------------------
# Precompute proactive KDE by period (annual + all)
# ------------------------------------------------------------------
pro_kde_by_period <- list()
for (per in periods) {
  df_per <- if (per == "all") {
    cad_sf_clip %>% st_drop_geometry()
  } else {
    cad_sf_clip %>% filter(as.character(year) == per) %>% st_drop_geometry()
  }
  
  df_pro <- df_per %>% filter(proactive == TRUE)
  pro_kde_by_period[[per]] <- if (nrow(df_pro) >= 25) normalize_kde(make_kde(df_pro)) else NULL
}

# Denom KDE by denom/period
den_kde_by_denom_period <- list()
for (d in denoms) {
  den_kde_by_denom_period[[d]] <- list()
  for (per in periods) {
    df_per <- if (per == "all") {
      cad_sf_clip %>% st_drop_geometry()
    } else {
      cad_sf_clip %>% filter(as.character(year) == per) %>% st_drop_geometry()
    }
    df_den <- df_per %>% filter(nature_recat == d)
    den_kde_by_denom_period[[d]][[per]] <- if (nrow(df_den) >= 25) normalize_kde(make_kde(df_den)) else NULL
  }
}

# ------------------------------------------------------------------
# Render annual + all-time PNGs (hotspot, proactive, diff)
# NO LEGENDS (left panel explains colors)
# ------------------------------------------------------------------
for (d in denoms) {
  message("Rendering maps for denom: ", d)
  
  for (per in periods) {
    kd_den <- den_kde_by_denom_period[[d]][[per]]
    kd_pro <- pro_kde_by_period[[per]]
    
    # HOTSPOT (denom) — polygons, consistent style
    if (!is.null(kd_den)) {
      cad_den <- cad_sf_clip[lexington_boundary, ] %>%
        filter(if (per == "all") TRUE else as.character(year) == per) %>%
        filter(nature_recat == d)
      
      p_hot <- plot_base_layers() +
        stat_density_2d(
          data = cad_den,
          aes(
            x = lon,
            y = lat,
            fill = after_stat(..level../max(..level..)),
            alpha = after_stat(..level../max(..level..))
          ),
          geom = "polygon",
          contour = TRUE,
          n = 400,
          h = c(0.005, 0.005)
        ) +
        scale_fill_viridis(option = "inferno", guide = "none") +
        scale_alpha(range = c(0.2, 0.7), guide = "none") +
        labs(
          title = paste("Hotspots —", d, "—", ifelse(per == "all", "All time", per)),
          subtitle = "Brighter = more calls in that area (relative within this map)"
        )
      
      save_png(p_hot, file.path(maps_dir, paste0("hotspot_", d, "_", per, ".png")))
    }
    
    # PROACTIVE — polygons; exported per denom for UI consistency
    if (!is.null(kd_pro)) {
      cad_pro <- cad_sf_clip[lexington_boundary, ] %>%
        filter(if (per == "all") TRUE else as.character(year) == per) %>%
        filter(proactive == TRUE)
      
      p_pro <- plot_base_layers() +
        stat_density_2d(
          data = cad_pro,
          aes(
            x = lon,
            y = lat,
            fill = after_stat(..level../max(..level..)),
            alpha = after_stat(..level../max(..level..))
          ),
          geom = "polygon",
          contour = TRUE,
          n = 400,
          h = c(0.005, 0.005)
        ) +
        scale_fill_viridis(option = "magma", guide = "none") +
        scale_alpha(range = c(0.15, 0.7), guide = "none") +
        labs(
          title = paste("Proactive Activity —", ifelse(per == "all", "All time", per)),
          subtitle = "Brighter = more proactive activity (relative within this map)"
        )
      
      save_png(p_pro, file.path(maps_dir, paste0("proactive_", d, "_", per, ".png")))
    }
    
    # DIFFERENCE (proactive - denom) — raster; NO legend
    if (!is.null(kd_pro) && !is.null(kd_den)) {
      df_diff <- make_diff_df(kd_pro, kd_den, pow = 0.8)
      
      p_diff <- ggplot() +
        geom_raster(data = df_diff, aes(lon, lat, fill = diff), alpha = 0.85) +
        scale_fill_continuous_divergingx(
          palette = "RdBu",
          mid = 0,
          rev = TRUE,
          guide = "none"
        ) +
        # reuse basemap layers manually so style matches (but keep legend off)
        { if (!is.null(water$osm_polygons))
          geom_sf(data = water$osm_polygons, fill = "#6baed6", color = NA, alpha = 0.55, inherit.aes = FALSE) } +
        { if (!is.null(rivers$osm_lines))
          geom_sf(data = rivers$osm_lines, color = "#2171b5", linewidth = 0.7, alpha = 0.9, inherit.aes = FALSE) } +
        geom_sf(
          data = streets$osm_lines %>% filter(highway %in% c("motorway","trunk","primary")),
          color = "black", linewidth = 1.0, alpha = 0.85, inherit.aes = FALSE
        ) +
        geom_sf(
          data = streets$osm_lines %>% filter(!highway %in% c("motorway","trunk","primary")),
          color = "gray50", linewidth = 0.3, alpha = 0.55, inherit.aes = FALSE
        ) +
        geom_sf(data = lexington_boundary, fill = NA, color = "black", linewidth = 0.8, inherit.aes = FALSE) +
        coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none"
        ) +
        labs(
          title = paste("Proactive vs Demand —", d, "—", ifelse(per == "all", "All time", per)),
          subtitle = "Red = relatively MORE proactive than demand • Blue = relatively LESS proactive than demand"
        )
      
      save_png(p_diff, file.path(maps_dir, paste0("diff_", d, "_", per, ".png")))
    }
    
  } # per
} # denom

# ------------------------------------------------------------------
# RESPONSE TIME OUTPUTS (NOT MAPS)
# - systemwide annual + all-time
# - by-denom annual + all-time
# - ONE trend PNG with systemwide + by-denom, colorblind palette
# ------------------------------------------------------------------
q90 <- function(x) suppressWarnings(quantile(x, 0.90, na.rm = TRUE, names = FALSE))
rt_base <- cad_sf_clip %>% st_drop_geometry()

rt_system_year <- rt_base %>%
  filter(!is.na(year)) %>%
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean_call_to_arrive_min = mean(rt_call_to_arrive_min, na.rm = TRUE),
    p90_call_to_arrive_min  = q90(rt_call_to_arrive_min),
    proactive_share         = mean(proactive, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(as.integer(year))

rt_system_all <- rt_base %>%
  summarise(
    year = "all",
    n = n(),
    mean_call_to_arrive_min = mean(rt_call_to_arrive_min, na.rm = TRUE),
    p90_call_to_arrive_min  = q90(rt_call_to_arrive_min),
    proactive_share         = mean(proactive, na.rm = TRUE)
  )

rt_system <- bind_rows(rt_system_all, rt_system_year)
write_csv(rt_system, file.path(tbl_dir, "rt_system_annual.csv"))

rt_byden_year <- rt_base %>%
  filter(!is.na(year), nature_recat %in% denoms) %>%
  mutate(year = as.character(year)) %>%
  group_by(nature_recat, year) %>%
  summarise(
    n = n(),
    mean_call_to_arrive_min = mean(rt_call_to_arrive_min, na.rm = TRUE),
    p90_call_to_arrive_min  = q90(rt_call_to_arrive_min),
    .groups = "drop"
  ) %>%
  arrange(nature_recat, as.integer(year))

rt_byden_all <- rt_base %>%
  filter(nature_recat %in% denoms) %>%
  group_by(nature_recat) %>%
  summarise(
    year = "all",
    n = n(),
    mean_call_to_arrive_min = mean(rt_call_to_arrive_min, na.rm = TRUE),
    p90_call_to_arrive_min  = q90(rt_call_to_arrive_min),
    .groups = "drop"
  )

rt_byden <- bind_rows(rt_byden_all, rt_byden_year)
write_csv(rt_byden, file.path(tbl_dir, "rt_by_denom_annual.csv"))

# ---- Colorblind palette (Okabe-Ito) + systemwide in black
okabe_ito <- c(
  black  = "#000000",
  orange = "#E69F00",
  sky    = "#56B4E9",
  green  = "#009E73",
  yellow = "#F0E442",
  blue   = "#0072B2",
  verm   = "#D55E00",
  purple = "#CC79A7"
)

# Assign HEX values (drop any carried names)
denom_cols <- c(
  burglary         = unname(okabe_ito["blue"]),
  domestic_problem = unname(okabe_ito["orange"]),
  suspicious       = unname(okabe_ito["green"]),
  violent_crime    = unname(okabe_ito["purple"]),
  traffic          = unname(okabe_ito["sky"])
)

# Combined series colors (must match series values exactly)
series_cols <- c(
  systemwide = unname(okabe_ito["black"]),
  denom_cols
)

# Combined trend plot: systemwide + by denom (legend kept; useful here)

rt_plot_byden <- rt_byden %>%
  filter(year != "all") %>%
  mutate(year_i = as.integer(year)) %>%
  dplyr::select(series = nature_recat, year_i, mean_call_to_arrive_min)

rt_plot_sys <- rt_system %>%
  filter(year != "all") %>%
  mutate(year_i = as.integer(year)) %>%
  dplyr::transmute(series = "systemwide", year_i, mean_call_to_arrive_min)

rt_plot_all <- dplyr::bind_rows(rt_plot_byden, rt_plot_sys) %>%
  mutate(series = factor(series, levels = c("systemwide", denoms)))

series_cols <- c(systemwide = okabe_ito["black"], denom_cols)

p_rt_combo <- ggplot(rt_plot_all, aes(x = year_i, y = mean_call_to_arrive_min, color = series, group = series)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.0) +
  scale_color_manual(values = series_cols) +
  labs(
    title = "Response Time Trend (Systemwide + Call Type)",
    subtitle = "Mean minutes from call received to first unit arrival (call→arrival)",
    x = "Year",
    y = "Mean call→arrival (minutes)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

ggsave(file.path(rt_dir, "rt_by_denom_trend.png"), p_rt_combo, width = 10, height = 6, dpi = 150)

# Keep systemwide-only file too (in case you want it later)
p_rt_system <- ggplot(rt_system %>% filter(year != "all") %>% mutate(year_i = as.integer(year)),
                      aes(x = year_i, y = mean_call_to_arrive_min)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.0) +
  labs(
    title = "Response Time Trend (Systemwide)",
    subtitle = "Mean minutes from call received to first unit arrival (call→arrival)",
    x = "Year",
    y = "Mean call→arrival (minutes)"
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(rt_dir, "rt_system_trend.png"), p_rt_system, width = 10, height = 6, dpi = 150)

# ------------------------------------------------------------------
# Manifest (no transitions)
# ------------------------------------------------------------------
manifest <- list(
  generated_at = as.character(Sys.time()),
  denominators = denoms,
  periods = periods,                 # ["all","2022","2023",...]
  years = as.character(years_vec),
  alpha_steps = NULL,                # transitions removed
  files = list(
    hotspot = "docs/maps/hotspot_{denom}_{period}.png",
    proactive = "docs/maps/proactive_{denom}_{period}.png",
    diff = "docs/maps/diff_{denom}_{period}.png",
    rt_system_csv = "docs/tables/rt_system_annual.csv",
    rt_byden_csv = "docs/tables/rt_by_denom_annual.csv",
    rt_system_trend_png = "docs/rt/rt_system_trend.png",
    rt_byden_trend_png = "docs/rt/rt_by_denom_trend.png"
  )
)

writeLines(toJSON(manifest, pretty = TRUE, auto_unbox = TRUE), manifest_path)
writeLines(toJSON(manifest, pretty = TRUE, auto_unbox = TRUE), "manifest.json")

message("DONE. Assets written to: ", docs_dir)
