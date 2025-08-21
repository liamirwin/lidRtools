#' Interactive Square ROI Clipping with lidR
#'
#' Interactively clips a square ROI from a LAS dataset.
#' It first builds a canopy height model and displays it. Then the user
#' clicks the centre of the square. A square polygon is constructed and used
#' to clip the LAS dataset.
#'
#' @param las A character string (path to a LAS/LAZ) or a LAS object.
#' @param width Numeric. Side length of the square ROI (same units as CRS). Default: 100.
#' @param res Numeric. Resolution (in same units) for rasterizing the CHM. Default: 1.
#' @param subcircle Numeric. \code{p2r} subcircle for canopy rasterization. Default: 0.
#' @param return_type Character. What to return: "las" (default), "both", or "polygon".
#'   - "las": return the clipped LAS object (original behavior)
#'   - "polygon": return only the square ROI sf polygon (sfc POLYGON)
#'   - "both": return a named list with \code{list(las = <LAS>, roi = <sfc POLYGON>)}
#'
#' @return Depending on \code{return_type}:
#'   - "las": a LAS object containing only the points within the square ROI
#'   - "polygon": an \code{sf} \code{sfc} POLYGON of the selected ROI
#'   - "both": a named list with elements \code{las} and \code{roi}
#'
#' @examples
#' \dontrun{
#'   # Example using the Megaplot dataset
#'   lasFile <- system.file("extdata", "Megaplot.laz", package = "lidR")
#'
#'   las_clip <- clip_square_interactive(
#'     las       = lasFile,
#'     width     = 15,
#'     res       = 1,
#'     subcircle = 0.1,
#'     return_type = "las"
#'   )
#' }
#'
#' @importFrom lidR readLAS rasterize_canopy clip_roi p2r projection npoints
#' @importFrom sf st_polygon st_sfc st_crs
#' @importFrom cli cli_alert_danger cli_alert_info cli_progress_message cli_progress_step
#' @importFrom glue glue
#' @importFrom grDevices dev.list dev.off
#' @importFrom graphics locator
#' @importFrom terra plot crs
#' @export
clip_square_interactive <- function(las,
                                    width       = 100,
                                    res         = 1,
                                    subcircle   = 0,
                                    return_type = c("las", "both", "polygon")) {

  return_type <- match.arg(return_type)

  # ---- load LAS ----
  if (is.character(las)) {
    las <- tryCatch(
      lidR::readLAS(las),
      error = function(e) {
        cli::cli_alert_danger(glue::glue("Error reading LAS file: {e$message}"))
        stop("Cannot read LAS file.")
      }
    )
    if (is.null(las)) {
      cli::cli_alert_danger("LAS file is empty or invalid.")
      stop("LAS file is empty or invalid.")
    }
    cli::cli_progress_step("LAS loaded")
  } else if (!inherits(las, "LAS")) {
    cli::cli_alert_danger("`las` must be a file path or LAS object.")
    stop("Invalid `las` argument.")
  }

  # ---- build CHM ----
  cli::cli_progress_message(glue::glue("Rasterizing CHM at {res} m..."))
  chm <- tryCatch(
    lidR::rasterize_canopy(las, res = res, lidR::p2r(subcircle = subcircle)),
    error = function(e) {
      cli::cli_alert_danger(glue::glue("CHM generation failed: {e$message}"))
      stop(e)
    }
  )
  cli::cli_progress_step("CHM generated")

  # ---- plot and get centre click ----
  while (length(grDevices::dev.list()) > 1) grDevices::dev.off()
  main_title <- glue::glue("Click centre of {width}x{width} m square")
  if (!inherits(chm, "SpatRaster")) {
    stop("CHM must be a terra::SpatRaster.")
  }
  terra::plot(chm, main = main_title)
  cli::cli_alert_info("Please click the centre point...")
  click <- graphics::locator(1)
  if (is.null(click$x) || is.null(click$y)) {
    cli::cli_alert_danger("No point clicked; aborting.")
    stop("You must click one point.")
  }
  cli::cli_progress_step("User clicked point")

  # ---- compute square corners ----
  x <- click$x; y <- click$y
  half <- width / 2
  coords <- matrix(c(
    x - half, y - half,
    x + half, y - half,
    x + half, y + half,
    x - half, y + half,
    x - half, y - half
  ), ncol = 2, byrow = TRUE)

  # ---- build sf polygon ----
  sq_poly <- tryCatch({
    poly <- sf::st_polygon(list(coords))
    crs_info <- tryCatch({
      sf::st_crs(terra::crs(chm, proj = TRUE))
    }, error = function(e) {
      sf::st_crs(lidR::projection(las))
    })
    sf::st_sfc(poly, crs = crs_info)
  }, error = function(e) {
    cli::cli_alert_danger(glue::glue("Failed to build polygon: {e$message}"))
    stop(e)
  })
  cli::cli_progress_step("Square polygon created")

  # ---- overlay and (optionally) clip ----
  plot(sq_poly, add = TRUE, border = "blue", lwd = 2)

  if (return_type == "polygon") {
    return(sq_poly)
  }

  cli::cli_progress_message("Clipping LAS to square ROI...")
  las_clip <- tryCatch(
    lidR::clip_roi(las, sq_poly),
    error = function(e) {
      cli::cli_alert_danger(glue::glue("Clipping failed: {e$message}"))
      stop(e)
    }
  )
  cli::cli_progress_step("LAS clipped")

  # Handle empty results explicitly
  n_pts <- tryCatch(lidR::npoints(las_clip), error = function(e) NA_integer_)
  if (is.na(n_pts)) {
    # Fallback if npoints() is unavailable
    n_pts <- tryCatch(nrow(las_clip@data), error = function(e) 0L)
  }

  message(glue::glue(
    "Square ROI ({width}x{width} m) clipped: {n_pts} points returned."
  ))

  if (return_type == "las") {
    return(las_clip)
  } else {
    return(list(las = las_clip, roi = sq_poly))
  }
}
