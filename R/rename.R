#' rename for hyperSpec objects
#'
#' rename extra data columns of hyperSpec objects.
#' Special column `$spc` contains the spectra matrix.
#' Unlike dplyr::select(), dplyr::rename() keeps all the variables of the data frame intact.
#' However, if `$spc` is renamed, the result is an error instead of a hyperSpec object.
#'
#'
#'
#'
#'
#' @inheritParams dplyr::rename
#' @seealso [dplyr::rename()]
#' @return hyperSpec object. If `$spc` is renamed, the result is an error.
#' @include unittest.R
#' @importFrom dplyr rename
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @export
#'
#' @examples
#'
#' chondro %>%
#'   rename (region = clusters) %>%
#'   head # results in a hyperSpec object
rename.hyperSpec <- function(.data, ...){
  # Check if user passed in a hyperSpec object
  chk.hy(.data)
  # Use dplyr::rename() to rename hyperSpec object data slot
  res <- rename(.data@data, ...)
  # Check if $spc was renamed
  if(!'spc' %in% colnames(res)){
    # Throw an error
    stop("$spc cannot be renamed")
  }else{
    # Get new and current column names
    labels.to.update <- setdiff(colnames(res), colnames(.data))
    labels.to.remove <- setdiff(colnames(.data), colnames(res))
    # Update the data slot with newly renamed data frame
    .data@data <- res
    # Update labels of hyperSpec object
    new.labels <- lapply(labels(.data, labels.to.update), as.expression)
    labels(.data)[c(labels.to.remove)] <- NULL
    labels(.data)[c(labels.to.update)] <- new.labels
    .data
  }
}

# Begin unit testing (UT)
.test(rename.hyperSpec) <- function(){
  context("rename.hyperSpec")

  # UT1
  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(rename.hyperSpec(df))
  })

  # UT2
  test_that("renaming $spc throws an error", {
    expect_error(rename.hyperSpec(chondro, spc_newname = spc))
  })

  # UT3
  test_that("labels attribute when returning hyperSpec object", {
    tmp <- rename.hyperSpec(chondro, region = clusters)
    tmp <- rename.hyperSpec(tmp, clusters = region)
    expect_equal(labels(tmp), labels(chondro))
  })

}
