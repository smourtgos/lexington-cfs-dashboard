# 01_build_site_assets.R
# Static GitHub Pages assets: quarterly KDE maps + proactive overlays + difference + response-time
# Includes all fixes for geom_sf "object 'x' not found" + traffic/traffic_stop unification.

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
  library(colorspace)
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
# Output format: prefer WebP if ragg installed
# -------------------------
use_webp <- requireNamespace("ragg", quietly = TRUE)
img_ext <- if (use_webp) "webp" else "png"

save_plot <- function(p, filepath, w = 10, h = 8, dpi = 220) {
  if (use_webp) {
    ragg::agg_webp(filename = filepath, width = w, height = h, units = "in", res = dpi)
    print(p)
    dev.off()
  } else {
    ggsave(filename = filepath, plot = p, width = w, height = h, dpi = dpi)
  }
}

# -------------------------
# Tunables
# -------------------------
min_points_map <- 30         # skip maps if too few points in quarter/category
kde_n <- 300
kde_h <- c(0.015, 0.015)     # kde2d bandwidth for diff surface
density_n <- 400             # stat_density_2d resolution
density_h <- c(0.005, 0.005) # stat_density_2d bandwidth
diff_power <- 0.8

# -------------------------
# Load data
# -------------------------
if (!file.exists(rds_path)) {
  stop("Missing data/cad_data.rds. Create it with: saveRDS(cad_data, 'data/cad_data.rds')")
}

cad_data <- readRDS(rds_path)

stopifnot(all(c(
  "incident_id","call_time","first_dispatched","first_arrived","last_cleared",
  "nature","nature_code","nature_recat","station_beat","geo_x","geo_y"
) %in% names(cad_data)))

# Ensure POSIXct
cad_data <- cad_data %>%
  mutate(
    call_time        = as.POSIXct(call_time, tz = "UTC"),
    first_dispatched = as.POSIXct(first_dispatched, tz = "UTC"),
    first_arrived    = as.POSIXct(first_arrived, tz = "UTC"),
    last_cleared     = as.POSIXct(last_cleared, tz = "UTC")
  )

# -------------------------
# Standardize denom_group + unify traffic categories
# -------------------------
cad_data <- cad_data %>%
  mutate(
    denom_group = nature_recat,
    denom_group = case_when(
      denom_group %in% c("traffic", "traffic_stop") ~ "traffic_stop",
      TRUE ~ denom_group
    )
  )

# -------------------------
# Response-time measures (seconds)
# -------------------------
cad_data <- cad_data %>%
  mutate(
    rt_call_to_dispatch   = as.numeric(difftime(first_dispatched, call_time, units = "secs")),
    rt_dispatch_to_arrive = as.numeric(difftime(first_arrived, first_dispatched, units = "secs")),
    rt_call_to_arrive     = as.numeric(difftime(first_arrived, call_time, units = "secs")),
    rt_on_scene           = as.numeric(difftime(last_cleared, first_arrived, units = "secs"))
  ) %>%
  mutate(across(starts_with("rt_"), ~ ifelse(is.finite(.x) & .x >= 0, .x, NA_real_)))

# -------------------------
# Quarter variable
# -------------------------
cad_data <- cad_data %>%
  mutate(
    year = year(call_time),
    qtr  = quarter(call_time),
    period = sprintf("%dQ%d", year, qtr)
  )

periods <- cad_data %>%
  distinct(period, year, qtr) %>%
  arrange(year, qtr) %>%
  pull(period)

# -------------------------
# Denominators shown on the site (curated + present-only)
# -------------------------
denoms <- c(
  "all_calls",
  "traffic_stop",
  "property_check",
  "suspicious",
  "domestic_problem",
  "alarm",
  "civil_conflict",
  "welfare_mh",
  "violent_crime",
  "burglary"
)

present_denoms <- sort(unique(cad_data$denom_group))
denoms <- c("all_calls", intersect(denoms[denoms != "all_calls"], present_denoms))

# -------------------------
# Proactive proxy (traffic_stop + property_check AND dispatch→arrive <= 10 sec)
# -------------------------
cad_data <- cad_data %>%
  mutate(
    proactive_calltype = denom_group %in% c("traffic_stop", "property_check"),
    proactive = proactive_calltype & !is.na(rt_dispatch_to_arrive) & (abs(rt_dispatch_to_arrive) <= 10)
  )

# Validation table for proactive proxy
proactive_validation <- cad_data %>%
  group_by(denom_group) %>%
  summarise(
    n = n(),
    proactive_n = sum(proactive, na.rm = TRUE),
    proactive_share = mean(proactive, na.rm = TRUE),
    med_dispatch_to_arrive = median(rt_dispatch_to_arrive, na.rm = TRUE),
    p90_dispatch_to_arrive = quantile(rt_dispatch_to_arrive, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

write_csv(proactive_validation, file.path(tbl_dir, "proactive_validation_by_group.csv"))

# -------------------------
# SF conversion (EPSG:2273 -> 4326)
# -------------------------
cad_sf <- cad_data %>%
  filter(!is.na(geo_x), !is.na(geo_y)) %>%
  st_as_sf(coords = c("geo_x", "geo_y"), crs = 2273, remove = FALSE) %>%
  st_transform(4326)

xy <- st_coordinates(cad_sf)
cad_sf$lon <- xy[, 1]
cad_sf$lat <- xy[, 2]

# -------------------------
# Boundary (your original "worked" approach)
# -------------------------
lexington_boundary <- tigris::places(state = "SC", cb = FALSE, year = 2023) %>%
  filter(NAME == "Lexington") %>%
  st_transform(4326)

if (nrow(lexington_boundary) == 0) stop("Could not find Lexington boundary via tigris::places(state='SC', year=2023, cb=FALSE) NAME=='Lexington'.")

bbox <- st_bbox(lexington_boundary)
bbox_num <- c(xmin = bbox$xmin, ymin = bbox$ymin, xmax = bbox$xmax, ymax = bbox$ymax)
bbox_vec <- c(bbox$xmin, bbox$xmax, bbox$ymin, bbox$ymax)

# -------------------------
# OSM basemap layers (streets/water/rivers) — pulled once
# -------------------------
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

# -------------------------
# FIX: drop x/y cols and force inherit.aes=FALSE (prevents geom_sf 'x' not found)
# -------------------------
drop_xy_cols <- function(x) {
  if (!inherits(x, "sf")) return(x)
  bad <- intersect(names(x), c("x", "y"))
  if (length(bad) > 0) x <- dplyr::select(x, -dplyr::all_of(bad))
  x
}

if (!is.null(streets$osm_lines)) streets$osm_lines <- drop_xy_cols(streets$osm_lines)
if (!is.null(water$osm_polygons)) water$osm_polygons <- drop_xy_cols(water$osm_polygons)
if (!is.null(rivers$osm_lines)) rivers$osm_lines <- drop_xy_cols(rivers$osm_lines)

coord_limits <- coord_sf(
  xlim = c(bbox$xmin, bbox$xmax),
  ylim = c(bbox$ymin, bbox$ymax),
  expand = FALSE
)

theme_map <- theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

add_basemap <- function() {
  list(
    # Water polygons
    { if (!is.null(water$osm_polygons))
      geom_sf(
        data = water$osm_polygons,
        fill = "#6baed6", color = NA, alpha = 0.9,
        inherit.aes = FALSE
      ) },
    
    # Rivers/streams
    { if (!is.null(rivers$osm_lines))
      geom_sf(
        data = rivers$osm_lines,
        color = "#2171b5", linewidth = 0.7, alpha = 0.9,
        inherit.aes = FALSE
      ) },
    
    # Major roads
    { if (!is.null(streets$osm_lines))
      geom_sf(
        data = streets$osm_lines %>%
          filter(highway %in% c("motorway", "trunk", "primary")),
        color = "black", linewidth = 1.2,
        inherit.aes = FALSE
      ) },
    
    # Minor roads
    { if (!is.null(streets$osm_lines))
      geom_sf(
        data = streets$osm_lines %>%
          filter(!highway %in% c("motorway", "trunk", "primary")),
        color = "gray50", linewidth = 0.3, alpha = 0.6,
        inherit.aes = FALSE
      ) },
    
    # Boundary
    geom_sf(
      data = lexington_boundary,
      fill = NA, color = "black", linewidth = 0.8,
      inherit.aes = FALSE
    )
  )
}

clip_to_boundary <- function(sf_points) {
  sf_points[lexington_boundary, ]
}

# -------------------------
# KDE difference helpers
# -------------------------
make_kde <- function(df, h = kde_h, n = kde_n) {
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
  zmin <- min(z, na.rm = TRUE)
  zmax <- max(z, na.rm = TRUE)
  if (!is.finite(zmin) || !is.finite(zmax) || zmin == zmax) {
    kde$z <- z * 0
    return(kde)
  }
  kde$z <- (z - zmin) / (zmax - zmin)
  kde
}

make_diff_df <- function(kde_pro, kde_den) {
  diff_raw <- kde_pro$z - kde_den$z
  diff_stretch <- sign(diff_raw) * (abs(diff_raw)^diff_power)
  expand.grid(lon = kde_pro$x, lat = kde_pro$y) %>%
    mutate(diff = as.vector(diff_stretch))
}

# -------------------------
# Plot functions (ALL inherit.aes fixes included)
# -------------------------
make_hotspot_map <- function(category_to_map, period_i) {
  
  cad_period <- cad_sf %>% filter(period == period_i) %>% clip_to_boundary()
  
  cad_cat <- if (category_to_map == "all_calls") {
    cad_period
  } else {
    cad_period %>% filter(denom_group == category_to_map)
  }
  
  if (nrow(cad_cat) < min_points_map) return(NULL)
  
  ggplot() +
    add_basemap() +
    stat_density_2d(
      data = cad_cat,
      aes(
        x = lon,
        y = lat,
        fill  = after_stat(..level../max(..level..)),
        alpha = after_stat(..level../max(..level..))
      ),
      geom = "polygon",
      contour = TRUE,
      n = density_n,
      h = density_h,
      inherit.aes = FALSE
    ) +
    scale_alpha(range = c(0.2, 0.7), guide = "none") +
    coord_limits +
    labs(
      title = paste0("Hotspots — ", category_to_map, " — ", period_i),
      subtitle = "Street-level basemap + KDE hotspots",
      x = "Longitude", y = "Latitude"
    ) +
    theme_map
}

make_proactive_map <- function(category_to_map, period_i) {
  
  cad_period <- cad_sf %>% filter(period == period_i) %>% clip_to_boundary()
  
  cad_cat <- if (category_to_map == "all_calls") {
    cad_period
  } else {
    cad_period %>% filter(denom_group == category_to_map)
  }
  
  cad_pro <- cad_period %>% filter(proactive == TRUE)
  
  if (nrow(cad_cat) < min_points_map || nrow(cad_pro) < min_points_map) return(NULL)
  
  ggplot() +
    add_basemap() +
    stat_density_2d(
      data = cad_cat,
      aes(
        x = lon,
        y = lat,
        fill  = after_stat(..level../max(..level..)),
        alpha = after_stat(..level../max(..level..))
      ),
      geom = "polygon",
      contour = TRUE,
      n = density_n,
      h = density_h,
      inherit.aes = FALSE
    ) +
    scale_alpha(range = c(0.15, 0.7), guide = "none") +
    stat_density_2d(
      data = cad_pro,
      aes(
        x = lon,
        y = lat,
        fill  = after_stat(..level../max(..level..)),
        alpha = after_stat(..level../max(..level..))
      ),
      geom = "polygon",
      contour = TRUE,
      n = density_n,
      h = density_h,
      inherit.aes = FALSE
    ) +
    coord_limits +
    labs(
      title = paste0("Proactive vs Demand — ", category_to_map, " — ", period_i),
      subtitle = "Layer 1 = demand KDE; Layer 2 = proactive KDE",
      x = "Longitude", y = "Latitude"
    ) +
    theme_map
}

make_diff_map <- function(category_to_map, period_i) {
  
  cad_period <- cad_sf %>% filter(period == period_i) %>% clip_to_boundary()
  
  cad_den <- if (category_to_map == "all_calls") {
    cad_period
  } else {
    cad_period %>% filter(denom_group == category_to_map)
  }
  
  cad_pro <- cad_period %>% filter(proactive == TRUE)
  
  if (nrow(cad_den) < min_points_map || nrow(cad_pro) < min_points_map) return(NULL)
  
  kde_pro <- normalize_kde(make_kde(cad_pro))
  kde_den <- normalize_kde(make_kde(cad_den))
  
  df_diff <- make_diff_df(kde_pro, kde_den)
  lims <- range(df_diff$diff, na.rm = TRUE)
  
  ggplot() +
    geom_raster(data = df_diff, aes(lon, lat, fill = diff), alpha = 0.85, inherit.aes = FALSE) +
    colorspace::scale_fill_continuous_divergingx(
      palette = "RdBu",
      mid = 0,
      rev = TRUE,
      limits = lims
    ) +
    { if (!is.null(streets$osm_lines))
      geom_sf(data = streets$osm_lines, color="black", linewidth=0.3, alpha=0.7, inherit.aes = FALSE) } +
    { if (!is.null(rivers$osm_lines))
      geom_sf(data = rivers$osm_lines, color="#2171b5", linewidth=0.6, alpha=0.9, inherit.aes = FALSE) } +
    geom_sf(data = lexington_boundary, fill=NA, color="black", linewidth=0.8, inherit.aes = FALSE) +
    coord_limits +
    labs(
      title = paste0("Difference (Proactive – Demand) — ", category_to_map, " — ", period_i),
      fill = "Proactive – Demand\nBlue = under\nRed = over"
    ) +
    theme_map
}

make_rt_beat_map <- function(period_i) {
  
  cad_period <- cad_sf %>% filter(period == period_i) %>% clip_to_boundary()
  
  beat_df <- cad_period %>%
    st_drop_geometry() %>%
    filter(!is.na(station_beat), !is.na(rt_call_to_arrive)) %>%
    group_by(station_beat) %>%
    summarise(
      n_calls = n(),
      med_call_to_arrive = median(rt_call_to_arrive, na.rm = TRUE),
      p90_call_to_arrive = quantile(rt_call_to_arrive, 0.90, na.rm = TRUE),
      lon = median(lon, na.rm = TRUE),
      lat = median(lat, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(lon), is.finite(lat), is.finite(med_call_to_arrive), n_calls >= 15)
  
  if (nrow(beat_df) == 0) return(NULL)
  
  ggplot() +
    add_basemap() +
    geom_point(
      data = beat_df,
      aes(x = lon, y = lat, size = n_calls, fill = med_call_to_arrive),
      shape = 21, alpha = 0.85,
      inherit.aes = FALSE
    ) +
    scale_size_continuous(range = c(2.5, 10)) +
    coord_limits +
    labs(
      title = paste0("Response Time (Median Call→Arrive) — ", period_i),
      fill = "Median secs",
      size = "Calls"
    ) +
    theme_map
}

# -------------------------
# Manifest skeleton
# -------------------------
manifest <- list(
  generated_utc = format(Sys.time(), tz = "UTC"),
  img_ext = img_ext,
  denoms = denoms,
  periods = periods,
  views = c("hotspot","overlay","diff","rt_map","rt_trend"),
  files = list()
)

# -------------------------
# Systemwide quarterly RT table + trend plot
# -------------------------
rt_quarter_tbl <- cad_sf %>%
  st_drop_geometry() %>%
  group_by(period) %>%
  summarise(
    n = n(),
    med_call_to_arrive = median(rt_call_to_arrive, na.rm = TRUE),
    p90_call_to_arrive = quantile(rt_call_to_arrive, 0.90, na.rm = TRUE),
    med_call_to_dispatch = median(rt_call_to_dispatch, na.rm = TRUE),
    med_dispatch_to_arrive = median(rt_dispatch_to_arrive, na.rm = TRUE),
    med_on_scene = median(rt_on_scene, na.rm = TRUE),
    proactive_share = mean(proactive, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period)

write_csv(rt_quarter_tbl, file.path(tbl_dir, "rt_quarter_systemwide.csv"))

rt_trend_plot <- ggplot(rt_quarter_tbl, aes(x = period, y = med_call_to_arrive, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Median Call→Arrive Time by Quarter (Systemwide)",
    x = NULL,
    y = "Seconds"
  )

rt_trend_file <- file.path(rt_dir, paste0("rt_trend_systemwide.", img_ext))
save_plot(rt_trend_plot, rt_trend_file, w = 11, h = 6, dpi = 220)
manifest$files$rt_trend_systemwide <- sub("^docs/", "", rt_trend_file)

# -------------------------
# Build quarterly assets
# -------------------------
for (period_i in periods) {
  
  # Response-time beat map
  p_rt <- make_rt_beat_map(period_i)
  if (!is.null(p_rt)) {
    f_rt <- file.path(rt_dir, paste0("rt_map__", period_i, ".", img_ext))
    save_plot(p_rt, f_rt, w = 10, h = 8, dpi = 220)
    manifest$files[[paste0("rt_map__", period_i)]] <- sub("^docs/", "", f_rt)
  }
  
  # Beat RT table for this quarter
  beat_tbl <- cad_sf %>%
    filter(period == period_i) %>%
    st_drop_geometry() %>%
    filter(!is.na(station_beat), !is.na(rt_call_to_arrive)) %>%
    group_by(station_beat) %>%
    summarise(
      n = n(),
      med_call_to_arrive = median(rt_call_to_arrive, na.rm = TRUE),
      p90_call_to_arrive = quantile(rt_call_to_arrive, 0.90, na.rm = TRUE),
      med_dispatch_to_arrive = median(rt_dispatch_to_arrive, na.rm = TRUE),
      proactive_share = mean(proactive, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  write_csv(beat_tbl, file.path(tbl_dir, paste0("rt_by_beat__", period_i, ".csv")))
  
  for (den in denoms) {
    
    p_hot <- make_hotspot_map(den, period_i)
    if (!is.null(p_hot)) {
      f_hot <- file.path(maps_dir, paste0("hotspot__", den, "__", period_i, ".", img_ext))
      save_plot(p_hot, f_hot, w = 10, h = 8, dpi = 220)
      manifest$files[[paste0("hotspot__", den, "__", period_i)]] <- sub("^docs/", "", f_hot)
    }
    
    p_ov <- make_proactive_map(den, period_i)
    if (!is.null(p_ov)) {
      f_ov <- file.path(maps_dir, paste0("overlay__", den, "__", period_i, ".", img_ext))
      save_plot(p_ov, f_ov, w = 10, h = 8, dpi = 220)
      manifest$files[[paste0("overlay__", den, "__", period_i)]] <- sub("^docs/", "", f_ov)
    }
    
    p_df <- make_diff_map(den, period_i)
    if (!is.null(p_df)) {
      f_df <- file.path(maps_dir, paste0("diff__", den, "__", period_i, ".", img_ext))
      save_plot(p_df, f_df, w = 10, h = 8, dpi = 220)
      manifest$files[[paste0("diff__", den, "__", period_i)]] <- sub("^docs/", "", f_df)
    }
  }
}

write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)

message("DONE. Assets written to /docs (maps/, rt/, tables/, manifest.json).")


