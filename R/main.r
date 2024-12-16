# 1. PACKAGES
#------------

install.packages("pacman")
pacman::p_load(
    sf, terra, maptiles,
    tidyverse, elevatr,
    RCurl, mapdeck, earthdatalogin
)

# 2. BUFFER
#------------

lat <- 34.3555761
lon <- -119.0878552

get_buffer <- function() {
    cents <- sf::st_as_sf(
        x = data.frame(
            lat = lat,
            lon = lon
        ), coords = c(
            "lon", "lat"
        ),
        crs = 4326
    )

    circle <- sf::st_buffer(
        cents,
        dist = units::set_units(
            15, "km"
        )
    ) |>
        sf::st_set_crs(
            4326
        )
    
    return(circle)
}

area_sf <- get_buffer()

bbox <- sf::st_bbox(
    area_sf
)

# 3. BACKGROUND LAYER
#---------------------

bg_map <- maptiles::get_tiles(
    x = bbox,
    provider = "Esri.WorldTopoMap",
    zoom = 14,
    crop = TRUE,
    project = TRUE
)

terra::writeRaster(
    bg_map,
    "bg_map.png",
    NAflag = 0,
    overwrite = TRUE
)

# 4. DIGITAL ELEVATION MODEL
#---------------------------

dem <- elevatr::get_elev_raster(
    locations = area_sf,
    z = 14, clip = "bbox"
) |>
    terra::rast()

elev <- terra::resample(
    x = dem,
    y = bg_map,
    method = "bilinear"
)

# 5. RASTER to RGB
#-----------------

dem_values <- terra::values(elev)

r <- floor(
    (100000 + dem_values * 10) / 65536 # 100000
)
g <- floor(
    (100000 + dem_values * 10) / 256 # 100000
) - r * 256
b <- floor(
    (100000 + dem_values * 10) # 100000
) - r * 65536 - g * 256

layers <- lapply(
    list(
        r, g, b
    ),
    function(channel) { # function(channel)
        terra::setValues(
            elev, channel
        )
    }
)

names(layers) <- c(
    "r_layer", "g_layer", "b_layer"
)

layers_rast <- terra::rast(
    layers
)

terra::plotRGB(
    layers_rast
)


terra::writeRaster(
    layers_rast,
    "elevation_map.png",
    datatype = "INT1U",
    gdal = c(
        "COMPRESS=LZW"
    ),
    overwrite = TRUE
)

# 6. PATH TO IMAGES
#------------------

elevation <- normalizePath(paste0(
    getwd(), "/elevation_map.png"
))

texture <- normalizePath(paste0(
    getwd(), "/bg_map.png"
))

txt <- RCurl::base64Encode(
    readBin(
        elevation, "raw",
        file.info(
            elevation
        )[1, "size"]
    ), "txt"
)

elevation_data <- sprintf(
    "data:image/png;base64,%s", #"data:image/png;base64,%s"
    txt
)

txt <- RCurl::base64Encode(
    readBin(
        texture, "raw",
        file.info(
            texture
        )[1, "size"]
    ), "txt"
)

texture_data <- sprintf(
    "data:image/png;base64,%s", #image
    txt
)

# 7. NASA FIRE DATA
#------------------

main_url <- "https://nrt3.modaps.eosdis.nasa.gov/archive/FIRMS/noaa-21-viirs-c2/USA_contiguous_and_Hawaii/J2_VIIRS_C2_USA_contiguous_and_Hawaii_VJ214IMGTDL_NRT_2024"
id <- c(309:314)

urls <- paste0(
    main_url, id, ".txt"
)

earthdatalogin::edl_netrc(
    username = "***********", # PLEASE TYPE YOUR EARTHDATA USERNAME
    password = "***********"  # PLEASE TYPE YOUR EARTHDATA PASSWORD
)

lapply(
    url,
    earthdatalogin::edl_download
)

read_data_from_drive <- function(file){
    df <- readr::read_delim(
        file,
        delim = ",",
        col_types = readr::cols(
            .default = "c"
        )
    )

    return(df)
}

filenames <- basename(urls)

combined_data <- do.call(
    rbind,
    Map(
        read_data_from_drive, filenames
    )
)

head(combined_data)

fire_points_sf <- sf::st_as_sf(
    combined_data,
    coords = c(
        "longitude",
        "latitude"
    )
) |>
    sf::st_set_crs(4326) |> # should be sf::st_set_crs
    sf::st_intersection(
        area_sf
    ) |>
    sf::st_as_sf()

altitude <- terra::extract(
    x = elev,
    y = fire_points_sf,
    fun = max
)

z <- altitude[, 2]

fire_df <- fire_points_sf |>
    dplyr::mutate(
        bright_ti5 = as.numeric(bright_ti5) - 273.15,
        z = z + 100
    ) |>
    dplyr::select(
        z, bright_ti5, acq_date
    ) |>
    na.omit()

# 8. DYNAMIC MAP
#---------------

bounds <- c(
    bbox[["xmin"]],
    bbox[["ymin"]],
    bbox[["xmax"]],
    bbox[["ymax"]]
)

terrain_3d <- mapdeck::mapdeck(
    location = c(lon, lat),
    max_zoom = 15,
    min_zoom = 12,
    bearing = 0,
    min_pitch = 60,
    max_pitch = 120,
    height = "1000px"
) |>
    mapdeck::add_terrain(
        elevation_data = elevation_data,
        texture = texture_data,
        elevation_decoder = c(
            6553.6,
            25.6,
            0.1,
            -10000
        ),
        bounds = bounds,
        max_error = 5
    ) |>
    mapdeck::add_pointcloud(
        data = fire_df,
        radius = 5,
        elevation = "z",
        tooltip = "acq_date",
        fill_colour = "bright_ti5",
        palette = "heat"
    )

htmlwidgets::saveWidget(
    terrain_3d,
    file = "index.html",
    selfcontained = FALSE
)

colourvalues::color_palettes()
