options(HTTPUserAgent = "nononoexe@ymail.ne.jp (RCRS MapConverter Source Maps)")

Sys.setenv(OSM_USE_CUSTOM_INDEXING = "NO")

sf::sf_use_s2(FALSE)

summarize_osm <- function(file_path) {

  file_size_mb <- file.info(file_path)$size / (1024^2)

  na_row <- data.frame(
    file           = file_path,
    file_size_mb   = file_size_mb,
    center_lon     = NA_real_,
    center_lat     = NA_real_,
    bbox_width_km  = NA_real_,
    bbox_height_km = NA_integer_
  )

  layers <- tryCatch(sf::st_layers(file_path)$name, error = function(e) NULL)
  if (is.null(layers)) return(na_row)

  layer_data <- lapply(layers, function(lyr) {
    obj <- tryCatch(
      suppressWarnings(sf::st_read(file_path, layer = lyr, quiet = TRUE)),
      error = function(e) NULL
    )
    if (is.null(obj) || nrow(obj) == 0) return(NULL)

    centroids <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(obj))))

    list(coords = centroids, n = nrow(obj))
  })
  layer_data <- Filter(Negate(is.null), layer_data)

  if (length(layer_data) == 0) return(na_row)

  all_coords <- do.call(rbind, lapply(layer_data, function(x) x$coords))
  all_coords <- all_coords[stats::complete.cases(all_coords), , drop = FALSE]

  if (nrow(all_coords) == 0) return(na_row)

  center_lon <- stats::median(all_coords[, "X"])
  center_lat <- stats::median(all_coords[, "Y"])

  trim <- 0.01
  xr <- stats::quantile(all_coords[, "X"], c(trim, 1 - trim), names = FALSE)
  yr <- stats::quantile(all_coords[, "Y"], c(trim, 1 - trim), names = FALSE)

  height_km <- (ymax - ymin) * 111.32
  width_km  <- (xmax - xmin) * 111.32 * cos(center_lat * pi / 180)

  n_features <- sum(sapply(layer_data, function(x) x$n))

  data.frame(
    file           = file_path,
    file_size_mb   = file_size_mb,
    center_lon     = center_lon,
    center_lat     = center_lat,
    bbox_width_km  = width_km,
    bbox_height_km = height_km,
    n_features     = n_features,
    stringsAsFactors = FALSE
  )
}

na_if_null <- function(x) if (is.null(x)) NA_character_ else x

perform_reverse_geocoding <- function(longitude, latitude, language = "en") {
  if (is.na(longitude) || is.na(latitude)) {
    return(data.frame(
      city = NA_character_, county = NA_character_,
      state = NA_character_, country = NA_character_,
      stringAsFactors = FALSE
    ))
  }

  url <- sprintf(
    "https://nominatim.openstreetmap.org/reverse?lon=%f&lat=%f&format=json&addressdetails=1&accept-language=%s",
    longitude, latitude, language
  )

  response <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  address <- response$address

  data.frame(
    city    = address_field(address$city),
    county  = address_field(address$country),
    state   = address_field(address$state),
    country = address_field(address$country),
    stringAsFactors = FALSE
  )
}

geocode_address <- function(longitude, latitude) {

  address_en <- perform_reverse_geocoding(longitude, latitude, "en")
  Sys.sleep(1)

  address_ja <- perform_reverse_geocoding(longitude, latitude, "ja")
  Sys.sleep(1)

  data.frame(
    city_en    = address_en$city,
    county_en  = address_en$county,
    state_en   = address_en$state,
    country_en = address_en$country,
    city_ja    = address_ja$city,
    county_ja  = address_ja$county,
    state_ja   = address_ja$state,
    country_ja = address_ja$country,
    stringsAsFactors = FALSE
  )
}

osm_files <- list.files(
  "OSM_FILES_ROOT_DIR",
  pattern    = "\\.osm$",
  recursive  = TRUE,
  full.names = FALSE
)

osm_summary <- do.call(rbind, lapply(osm_files, summarize_osm))

n_files <- nrow(osm_summary)
address_rows <- vector("list", n_files)
pb <- utils::txtProgressBar(min = 0, max = n_files, style = 3)

for (i in seq_len(n_files)) {
  address_rows[[i]] <- geocode_address(
    osm_summary$center_lon[i], osm_summary$center_lat[i]
  )
  utils::setTxtProgressBar(pb, i)
}
close(pb)

address_summary <- do.call(rbind, address_rows)

map_catalog <- cbind(osm_summary, address_summary)
row.names(map_catalog) <- NULL

jsonlite::write_json(
  map_catalog,
  path       = "docs/data/map-address.json",
  auto_unbox = TRUE,
  pretty     = TRUE,
  na         = "null"
)
