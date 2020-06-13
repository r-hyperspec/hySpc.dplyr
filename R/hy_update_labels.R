#' Ensure that hyperSpec and non hyperSpec objects have the correct labels
#'
#' @param .data hyperSpec object
#' @param res resulting data frame
#' @return Object with the correct labels
#' @md
#' @export
hy_update_labels <- function(.data, res){

  # Update labels
  labels <- labels(.data)[c(".wavelength", colnames(res))]
  if (is.null(res$spc)) {

    # use attribute to have correct labels when piping into `as.hyperSpec`
    attr(res, "labels") <- labels
    res
  } else {
    .data@data <- res
    labels(.data) <- labels
    .data
  }
}
