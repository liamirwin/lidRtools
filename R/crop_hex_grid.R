#' Tile a Raster into a Grid of Polygons
#'
#' This function creates a grid of square or hexagonal polygons over the extent
#' of a source raster. It then iterates through each grid cell, cropping and
#' masking the source raster to create individual raster tiles.
#'
#' @param raster_layer A `SpatRaster` object from the `terra` package to be tiled.
#' @param grid_area Numeric. The target area for each grid cell in the square
#'   meters of the raster's CRS.
#' @param grid_shape Character. The shape of the grid cells. Must be either
#'   `"square"` or `"hexagon"`.
#' @param out_dir Character. Optional path to a directory where the output raster
#'   tiles and grid geopackage will be saved. If `NULL`, no files are written.
#' @param plot_grid Logical. If `TRUE`, displays a plot of the generated grid
#'   overlaid on the raster for review before processing.
#' @param prompt_user Logical. If `TRUE` and `plot_grid` is `TRUE`, the function
#'   will pause and ask for user confirmation before proceeding to tile the raster.
#' @param acceptable_coverage Numeric. A value between 0 and 1 representing the
#'   minimum proportional overlap a grid cell must have with the raster's extent
#'   to be processed. Defaults to `0.5` (50%).
#' @param write_grid Logical. If `TRUE` and `out_dir` is specified, the grid
#'   polygons will be saved as a geopackage in the output directory.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{raster_tiles}{A named list of `SpatRaster` objects, where each element is a
#'     tiled portion of the input raster. The names of the list elements
#'     correspond to the unique `grid_id` of each cell.}
#'   \item{grid}{An `sf` object containing the grid polygons used for tiling.}
#' }
#'
#' @importFrom terra ext vect crs crop mask writeRaster
#' @importFrom sf st_as_sf st_make_grid st_centroid st_coordinates st_bbox st_intersection st_area st_write
#' @importFrom dplyr mutate if_else select `%>%`
#' @importFrom cli cli_h1 cli_process_start cli_process_done cli_alert_info cli_alert_success cli_progress_bar cli_progress_update cli_progress_done cli_bullets
#' @importFrom glue glue
#' @importFrom ggplot2 theme_classic
#' @importFrom tidyterra autoplot geom_spatvector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("lidR", quietly = TRUE)) {
#'
#'   # Create and tile a 1m CHM from the Megaplot.laz
#'  output <- system.file("extdata", "Megaplot.laz", package = "lidR") %>%
#'    lidR::readLAS() %>%
#'    lidR::rasterize_canopy(res = 1, algorithm = lidR::p2r(subcircle = 0.25)) %>%
#'    tile_raster(
#'      grid_area   = 900,         # 30x30m tiles
#'      grid_shape  = "square",
#'      acceptable_coverage = 0.95,
#'      out_dir     = NULL,        # Don't write files to disk
#'      plot_grid   = FALSE,       # Don't show a plot
#'      write_grid  = FALSE,
#'      prompt_user = FALSE
#'    )
#'
#'   # Check the raster output (a list of SpatRasters)
#'   print(output$raster_tiles[[1]])
#'
#'   # Check the grid polygon output
#'   print(head(output$grid))
#' }
#' }
tile_raster <- function(raster_layer,
                        grid_area,
                        grid_shape,
                        out_dir = NULL,
                        plot_grid = FALSE,
                        prompt_user = TRUE,
                        acceptable_coverage = 0.5,
                        write_grid = TRUE) {

  cli::cli_h1("Tiling Raster")

  # Create grid polygons
  cli::cli_process_start("Creating {grid_shape} grid with {grid_area}m^2 cells")
  sf_ext <- terra::ext(raster_layer) %>%
    terra::vect(crs = terra::crs(raster_layer)) %>%
    sf::st_as_sf()

  grid_polygons <- .create_grid(sf_ext, grid_area, grid_shape)
  cli::cli_process_done()
  cli::cli_alert_info("Generated {nrow(grid_polygons)} total grid cells.")

  # Optional prompt_user plotting for review
  if (plot_grid) {
    cli::cli_alert_info("Displaying grid for review. Close the plot window to continue.")
    p <- tidyterra::autoplot(raster_layer) +
      tidyterra::geom_spatvector(
        data  = terra::vect(grid_polygons),
        fill  = "transparent",
        color = "red"
      ) +
      ggplot2::theme_classic()
    print(p)

    if (prompt_user) {
      cli::cli_bullets(c("*" = "Review the grid plot."))
      response <- readline(" > Press [Enter] to continue or type 'n' to abort: ")
      if (tolower(response) == "n") {
        cli::cli_alert_info("Processing aborted by user.")
        return(invisible(NULL))
      }
    }
  }

  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      cli::cli_alert_success("Created output directory: {.path {out_dir}}")
    }
  }

  # Crop and mask raster for each valid polygon
  raster_list <- list()
  # Create a vector of grid_ids that will actually be processed
  processed_ids <- c()

  cli::cli_progress_bar("Processing grid cells", total = nrow(grid_polygons))

  for (i in seq_len(nrow(grid_polygons))) {
    poly <- grid_polygons[i, ]
    grid_id <- poly$grid_id

    # Check for sufficient overlap between the polygon and raster extent
    overlap_geom <- suppressWarnings(sf::st_intersection(poly, sf_ext))
    intersect_area <- sum(sf::st_area(overlap_geom), na.rm = TRUE) %>% as.numeric()

    if (is.na(intersect_area) || intersect_area < grid_area * acceptable_coverage) {
      cli::cli_progress_update()
      next # Skip to the next polygon
    }

    # If the polygon is valid, add its ID to the list
    processed_ids <- c(processed_ids, grid_id)

    # Crop and mask the raster
    poly_vect <- terra::vect(poly)
    cropped <- terra::crop(raster_layer, poly_vect)
    masked <- terra::mask(cropped, poly_vect)

    # Store the result in the list
    raster_list[[as.character(grid_id)]] <- masked

    # Write the tile to a file if an output directory is provided
    if (!is.null(out_dir)) {
      out_tif <- file.path(
        out_dir,
        paste0(basename(names(raster_layer)[1]),"_", grid_id, ".tif")
      )
      terra::writeRaster(masked, out_tif, overwrite = TRUE)
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # Filter the grid polygons to only include those that were processed
  final_grid_polygons <- grid_polygons[grid_polygons$grid_id %in% processed_ids, ]

  # Optionally write grid polygons to a gpkg
  if (write_grid && !is.null(out_dir)) {
    out_gpkg <- glue::glue("{out_dir}/grid_polygons_{grid_area}m_{grid_shape}.gpkg")
    sf::st_write(final_grid_polygons, out_gpkg, append = FALSE, quiet = TRUE)
    cli::cli_alert_success("Grid gpkg written to {.path {out_gpkg}}")
  }

  cli::cli_alert_success("Processing complete. Returning {length(raster_list)} raster tiles and the corresponding grid.")

  # Return a list containing both the raster tiles and the grid polygons
  return(list(raster_tiles = raster_list, grid = final_grid_polygons))
}
