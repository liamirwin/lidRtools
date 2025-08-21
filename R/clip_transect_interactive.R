#' Interactive Transect Clipping with lidR
#'
#' This function interactively clips a transect from a LAS dataset. It first
#' generates a surface model (either a Canopy Height Model or a Digital Terrain Model)
#' and displays it. The user is then prompted to click two points (start and end
#' of the transect) on the plot. A transect polygon is constructed from these
#' points and used to clip the LAS dataset.
#'
#' @param las A character string specifying the path to a LAS/LAZ file or a LAS object.
#' @param width Numeric. The width of the transect. Default is 5.
#' @param res Numeric. The resolution for rasterizing the surface model. Default is 1.
#' @param subcircle Numeric. Parameter for the \code{p2r} function used in canopy
#'        rasterization (CHM only). Default is 0.
#' @param plot_dtm Logical. If TRUE, generates and plots a Digital Terrain Model (DTM)
#'        using \code{rasterize_terrain} for transect selection. If FALSE (default),
#'        a Canopy Height Model (CHM) is used. Note: DTM generation requires
#'        classified ground points for best results.
#'
#' @return A LAS object corresponding to the clipped transect.
#'
#' @importFrom lidR readLAS rasterize_canopy rasterize_terrain clip_roi p2r
#' @importFrom sf st_polygon st_sfc st_crs
#' @importFrom cli cli_alert_danger cli_alert_warning cli_progress_message cli_progress_step
#' @importFrom glue glue
#' @importFrom grDevices dev.list dev.off
#' @importFrom graphics locator
#'
#' @examples
#' \dontrun{
#'   # use the Megaplot example dataset from lidR:
#'   lasFile <- system.file("extdata", "Megaplot.laz", package = "lidR")
#'
#'   # Example 1: Interactive clipping using the default CHM
#'   las_clipped_chm <- clip_transect_interactive(
#'       las       = lasFile,
#'       width     = 5,
#'       res       = 1,
#'       subcircle = 0.25
#'     )
#'
#'   # Example 2: Interactive clipping using a DTM
#'   # First, we need to read the LAS file and classify ground points
#'   las <- readLAS(lasFile)
#'   las <- classify_ground(las, algorithm = csf())
#'
#'   las_clipped_dtm <- clip_transect_interactive(
#'       las       = las,
#'       width     = 10,
#'       res       = 1,
#'       plot_dtm  = TRUE
#'     )
#' }
#' @export
clip_transect_interactive <- function(las,
                                      width = 5,
                                      res = 1,
                                      subcircle = 0,
                                      plot_dtm = FALSE) {

  # Check if 'las' is a file path or a LAS object, and load the file if needed
  if (is.character(las)) {
    las <- tryCatch(
      lidR::readLAS(las),
      error = function(e) {
        # Show the original readLAS error, but unify the final error message.
        cli::cli_alert_danger(glue::glue("Error reading LAS file: {e$message}"))
        cli::cli_alert_danger("LAS file could not be read or is empty.")
        stop("LAS file could not be read or is empty.")
      }
    )

    if (is.null(las)) {
      cli::cli_alert_danger("LAS file could not be read or is empty.")
      stop("LAS file could not be read or is empty.")
    }
    cli::cli_progress_step("Load LAS file")
  } else if (!inherits(las, "LAS")) {
    cli::cli_alert_danger("Input is neither a LAS object nor a valid file path.")
    stop("Input is neither a LAS object nor a valid file path.")
  }

  # Generate the surface model (CHM or DTM) at the specified resolution
  if (plot_dtm) {
    # Warn user if no ground points are classified, as DTM will be poor
    if (!any(las$Classification == 2L)) {
      cli::cli_alert_warning("No ground points found (Classification = 2). DTM generation may fail or be inaccurate.")
    }

    cli::cli_progress_message(glue::glue("Rasterizing digital terrain model at {res}m spatial resolution..."))
    surface_model <- tryCatch(
      lidR::rasterize_terrain(las, res = res, algorithm = lidR::tin()),
      error = function(e) {
        cli::cli_alert_danger(glue::glue("Error during DTM generation: {e$message}"))
        stop(e)
      }
    )
    plot_title <- "DTM - Click on two points to define the transect"

  } else {
    cli::cli_progress_message(glue::glue("Rasterizing canopy height model at {res}m spatial resolution..."))
    surface_model <- tryCatch(
      lidR::rasterize_canopy(las, res = res, lidR::p2r(subcircle = subcircle)),
      error = function(e) {
        cli::cli_alert_danger(glue::glue("Error during CHM generation: {e$message}"))
        stop(e)
      }
    )
    plot_title <- "CHM - Click on two points to define the transect"
  }
  cli::cli_progress_step("Generate surface model")


  # Close extra graphics devices if any are open
  while (length(dev.list()) > 1) {
    dev.off()
  }

  # Plot the surface model and prompt user for two clicks
  plot(surface_model, main = plot_title)
  cli::cli_progress_message("Please click on two points in the plot (start and end of transect)...")
  clicks <- locator(2)
  cli::cli_progress_step("User selected points")

  if (length(clicks$x) < 2 || length(clicks$y) < 2) {
    cli::cli_alert_danger("You must click on two points to define the transect.")
    stop("You must click on two points to define the transect.")
  }

  # Compute the transect endpoints and check that they are distinct
  p1 <- c(clicks$x[1], clicks$y[1])
  p2 <- c(clicks$x[2], clicks$y[2])
  dx <- p2[1] - p1[1]
  dy <- p2[2] - p1[2]
  line_length <- sqrt(dx^2 + dy^2)

  if (line_length == 0) {
    cli::cli_alert_danger("The two clicked points are identical. Please select two distinct points.")
    stop("The two clicked points are identical. Please select two distinct points.")
  }

  cli::cli_alert_info(glue::glue(
    "Transect defined by P1=({round(p1[1], 2)}, {round(p1[2], 2)}) and P2=({round(p2[1], 2)}, {round(p2[2], 2)})"
  ))

  # Compute perpendicular vectors to define the transect's width
  half_width <- width / 2
  ux <- -dy / line_length
  uy <- dx / line_length

  p1_left  <- c(p1[1] + half_width * ux, p1[2] + half_width * uy)
  p2_left  <- c(p2[1] + half_width * ux, p2[2] + half_width * uy)
  p2_right <- c(p2[1] - half_width * ux, p2[2] - half_width * uy)
  p1_right <- c(p1[1] - half_width * ux, p1[2] - half_width * uy)

  polygon_coords <- rbind(p1_left, p2_left, p2_right, p1_right, p1_left)

  # Create an sf polygon from the clicked points
  transect_poly <- tryCatch(
    {
      poly <- sf::st_polygon(list(polygon_coords))
      # Retrieve CRS from the surface model or fallback to LAS
      crs_info <- tryCatch(
        sf::st_crs(surface_model),
        error = function(e) sf::st_crs(las)
      )
      sf::st_sfc(poly, crs = crs_info)
    },
    error = function(e) {
      cli::cli_alert_danger(glue::glue("Error creating transect polygon: {e$message}"))
      stop(e)
    }
  )


  # Overlay the transect polygon on the plot
  plot(transect_poly, add = TRUE, border = "red", lwd = 2)
  cli::cli_progress_step("Polygon created")

  # Clip the LAS data using the transect polygon
  las_clipped <- tryCatch(
    lidR::clip_roi(las, transect_poly),
    error = function(e) {
      cli::cli_alert_danger(glue::glue("Error clipping the LAS object: {e$message}"))
      stop(e)
    }
  )
  cli::cli_progress_step("LAS object clipped")

  message(glue::glue(
    "Finished clipping {width}m transect from the LAS object (",
    "{nrow(las_clipped@data)} points)."
  ))

  return(las_clipped)
}
