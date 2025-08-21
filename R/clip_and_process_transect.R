#' Process a LiDAR Point Cloud with Optional Transect Extraction
#'
#' This function provides a complete workflow for processing a LAS/LAZ file. It
#' can perform decimation, interactive or automated transect extraction, height
#' normalization, and tree segmentation. The key feature is its ability to define a
#' transect area once and then clip it from both the original (unnormalized) and
#' the fully processed point cloud.
#'
#' @param las_file A character string specifying the path to the LAS/LAZ file.
#' @param normalize Logical. If TRUE, the point cloud is height-normalized using a
#'        TIN-based DTM. Ground classification should be present for best results. Default is TRUE.
#' @param perform_treeID Logical. If TRUE, performs tree segmentation using
#'        \code{lidR::silva2016} and merges the `treeID` into the point cloud. Default is FALSE.
#' @param decimation Logical. If TRUE, decimates the point cloud to a resolution
#'        of one point per 0.5m voxel. Default is FALSE.
#' @param extract_transect Logical. If TRUE, the user will be prompted to interactively
#'        draw the transect on a DTM plot. If FALSE, a transect is automatically
#'        created through the center of the point cloud. Default is TRUE.
#' @param cs_width Numeric. The width of the transect in the same units as the LAS
#'        data. Default is 1.
#' @param offset_x Numeric. If `extract_transect = FALSE`, this value specifies the
#'        start point's X-offset from the minimum X coordinate. Default is 15.
#' @param odir Character string. The path to the output directory where processed
#'        files will be saved. If NULL, no files are written. Default is NULL.
#' @param oname Character string. A base name for the output files. If NULL,
#'        "output" is used. Default is NULL.
#'
#' @return A list containing three LAS objects:
#' \itemize{
#'   \item \code{processed_cloud}: The full point cloud after all processing steps.
#'   \item \code{processed_transect}: The transect clipped from the fully processed cloud.
#'   \item \code{unnormalized_transect}: The transect clipped from the cloud *before* normalization,
#'         retaining original Z values but including any initial classifications (hence the `_class` suffix when saved).
#' }
#'
#' @importFrom lidR readLAS decimate_points random_per_voxel clip_roi normalize_height tin rasterize_terrain merge_spatial writeLAS
#' @importFrom sf st_polygon st_sfc st_crs
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_process_start cli_process_done
#' @importFrom graphics locator plot
#'
#' @examples
#' \dontrun{
#' # Get the path to the example LAS file from lidR
#' las_file_in <- system.file("extdata", "Megaplot.laz", package = "lidR")
#'
#' results <- process_point_cloud(
#'   las_file           = las_file_in,
#'   extract_transect   = FALSE, # Run automatically
#'   perform_treeID     = TRUE  # Skip tree identification
#' )
#'
#' # The results are a list of LAS objects. We can plot them.
#' # Plot the final, processed transect (heights are normalized)
#' plot(results$processed_transect)
#'
#' # To run interactively instead, simply use the default:
#' # results_interactive <- process_point_cloud(las_file = las_file_in)
#' }
#' @export
process_point_cloud <- function(las_file,
                                normalize = TRUE,
                                perform_treeID = FALSE,
                                decimation = FALSE,
                                extract_transect = TRUE,
                                cs_width = 1,
                                offset_x = 15,
                                odir = NULL,
                                oname = NULL) {

  cli::cli_h1("Starting Point Cloud Processing")

  cli::cli_process_start("Loading LAS file: {basename(las_file)}")
  las_full <- lidR::readLAS(las_file)
  cli::cli_process_done()
  cli::cli_alert_info("Loaded {nrow(las_full@data)} points.")

  if (decimation) {
    cli::cli_process_start("Decimating point cloud")
    las_proc <- lidR::decimate_points(las_full, lidR::random_per_voxel(res = 0.50, n = 1))
    cli::cli_process_done()
    cli::cli_alert_info("Decimated to {nrow(las_proc@data)} points.")
  } else {
    las_proc <- las_full
  }

  cli::cli_process_start("Defining transect Region of Interest (ROI)")
  if (extract_transect) {
    cli::cli_alert_info("Interactive mode: Please define transect on the plot.")
    dtm <- lidR::rasterize_terrain(las_proc, res = 1, algorithm = lidR::tin())
    plot(dtm, main = "Click on two points to define the transect")
    clicks <- graphics::locator(2)
    p1 <- c(clicks$x[1], clicks$y[1])
    p2 <- c(clicks$x[2], clicks$y[2])
  } else {
    cli::cli_alert_info("Automated mode: Creating central transect.")
    p1 <- c(min(las_proc@data$X) + offset_x, mean(las_proc@data$Y))
    p2 <- c(max(las_proc@data$X), mean(las_proc@data$Y))
  }

  dx <- p2[1] - p1[1]
  dy <- p2[2] - p1[2]
  line_length <- sqrt(dx^2 + dy^2)
  ux <- -dy / line_length
  uy <- dx / line_length
  half_width <- cs_width / 2

  p1_left  <- c(p1[1] + half_width * ux, p1[2] + half_width * uy)
  p2_left  <- c(p2[1] + half_width * ux, p2[2] + half_width * uy)
  p2_right <- c(p2[1] - half_width * ux, p2[2] - half_width * uy)
  p1_right <- c(p1[1] - half_width * ux, p1[2] - half_width * uy)
  polygon_coords <- rbind(p1_left, p2_left, p2_right, p1_right, p1_left)

  transect_poly <- sf::st_sfc(sf::st_polygon(list(polygon_coords)), crs = sf::st_crs(las_proc))
  cli::cli_process_done()

  cli::cli_process_start("Clipping unnormalized transect ('_class' version)")
  las_transect_class <- lidR::clip_roi(las_proc, transect_poly)
  cli::cli_process_done()

  if (normalize) {
    cli::cli_process_start("Normalizing full point cloud")
    las_proc <- lidR::normalize_height(las_proc, lidR::tin())
    cli::cli_process_done()
  }

  if (perform_treeID) {
    cli::cli_process_start("Performing tree segmentation")
    chm <- lidR::rasterize_canopy(las_proc, res = 1, algorithm = lidR::p2r())
    ttops <- lidR::locate_trees(chm, lidR::lmf(2))
    las_proc <- lidR::segment_trees(las_proc, lidR::silva2016(chm = chm, treetops = ttops))
    cli::cli_process_done()
  }

  cli::cli_process_start("Clipping processed transect")
  las_transect_proc <- lidR::clip_roi(las_proc, transect_poly)
  cli::cli_process_done()

  if (!is.null(odir)) {
    cli::cli_process_start("Writing output files to '{.path {odir}}'")
    if (!dir.exists(odir)) {
      dir.create(odir, recursive = TRUE)
    }
    base_name <- if (is.null(oname)) "output" else oname

    lidR::writeLAS(las_proc, file.path(odir, paste0(base_name, "_processed.laz")))
    lidR::writeLAS(las_transect_proc, file.path(odir, paste0(base_name, "_transect.laz")))
    lidR::writeLAS(las_transect_class, file.path(odir, paste0(base_name, "_transect_class.laz")))
    cli::cli_process_done()
  }

  cli::cli_alert_success("All processing complete.")

  return(list(
    processed_cloud = las_proc,
    processed_transect = las_transect_proc,
    unnormalized_transect = las_transect_class
  ))
}
