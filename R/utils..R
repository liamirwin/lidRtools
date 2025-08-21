#' Create a Square or Hexagonal Grid
#' @param sf_bbox An 'sf' polygon object representing the desired grid extent.
#' @param grid_area Numeric. The target area of each grid cell.
#' @param grid_shape Character. Either "square" or "hexagon".
#' @return An 'sf' object containing the grid polygons with a unique 'grid_id'.
#' @noRd
.create_grid <- function(sf_bbox, grid_area, grid_shape) {

  # Calculate cell size based on shape and area
  cell_size <- switch(
    grid_shape,
    square = sqrt(grid_area),
    hexagon = sqrt((2 * grid_area) / (3 * sqrt(3))) * sqrt(3),
    stop("Invalid grid_shape: must be 'square' or 'hexagon'")
  )

  # Generate the grid geometry
  if (grid_shape == "square") {
    grid <- sf::st_make_grid(
      sf_bbox,
      cellsize = c(cell_size, cell_size),
      square = TRUE
    )
  } else {
    coords <- sf::st_coordinates(sf_bbox)
    xbnds <- range(coords[, 1])
    hex_cols <- ceiling(diff(xbnds) / cell_size)
    grid <- sf::st_make_grid(
      sf_bbox,
      cellsize = c(cell_size, cell_size),
      square = FALSE,
      n = c(hex_cols, NA)
    )
  }

  grid_sf <- sf::st_as_sf(grid)

  # Calculate centroid to help determine row/column indices
  centroids <- sf::st_centroid(grid_sf)
  grid_sf <- grid_sf %>%
    dplyr::mutate(
      cx = sf::st_coordinates(centroids)[, 1],
      cy = sf::st_coordinates(centroids)[, 2]
    )

  bbox_vals <- sf::st_bbox(sf_bbox)
  minx <- bbox_vals[["xmin"]]
  miny <- bbox_vals[["ymin"]]

  # Calculate column and row indices for unique IDs
  if (grid_shape == "square") {
    grid_sf <- grid_sf %>%
      dplyr::mutate(
        col_idx = floor((cx - minx) / cell_size) + 1,
        row_idx = floor((cy - miny) / cell_size) + 1
      )
  } else {
    # Hex grid indexing using an "odd-r" offset coordinate system
    # See: https://www.redblobgames.com/grids/hexagons/#coordinates-offset
    dx <- 3 / 4 * cell_size      # Horizontal distance between column centers
    dy <- (sqrt(3) / 2) * cell_size # Vertical distance between row centers

    grid_sf <- grid_sf %>%
      dplyr::mutate(
        rough_row = floor((cy - miny) / dy),
        # Adjust x-coordinate based on whether the row is odd or even
        effective_x = dplyr::if_else(
          rough_row %% 2 == 1,
          cx - (cell_size / 2),
          cx
        ),
        col_idx = floor((effective_x - minx) / dx),
        row_idx = rough_row
      )
  }

  # Create a final grid_id and clean up intermediate columns
  grid_sf <- grid_sf %>%
    dplyr::mutate(grid_id = paste0("C", col_idx, "_R", row_idx)) %>%
    dplyr::select(grid_id, dplyr::everything(), -cx, -cy, -tidyselect::any_of(c("effective_x", "rough_row", "col_idx", "row_idx")))

  return(grid_sf)
}
