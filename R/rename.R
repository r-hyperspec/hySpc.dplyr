#' rename for hyperSpec objects
#'
#' rename extra data columns of hyperSpec objects.
#' Special column `$spc` contains the spectra matrix.
#' Unlike dplyr::select() dplyr::rename() keeps all the variables of the data frame intact.
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
#' @include setLabels.R
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
  if (!'spc' %in% colnames(res)) {
    # Throw an error
    stop("$spc cannot be renamed")
  } else {
      setLabels.rename(.data, res)
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
