#' select for hyperSpec objects
#'
#' Select extra data columns from the hyperSpec object.
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is dropped, the result is a data.frame instead of a hyperSpec object.
#' Convert the data.frame again into a hyperSpec object if needed by using `as.hyperSpec ()`.
#'
#' Wavelength selection is not yet provided. #is done separately by `select_wl()`.
#'
#'
#'
#' @inheritParams dplyr::select
#' @seealso [dplyr::select()]
#' @return hyperSpec object or data.frame with the selected columns. If the `$spc` is not included in the selection, the result will be a data.frame.
#' @include unittest.R
#' @importFrom dplyr select
#' @importFrom hyperSpec labels<-
#' @export
#'
#' @examples
#'
#' chondro %>% select (x, y, clusters) %>% head # spc not selected => data.frame
#' chondro %>% select (x, clusters, spc) # => hyperSpec object
#'
#' chondro %>% select (-spc) %>% head # all columns but $spc => data.frame same as chondro$..
#' chondro %>% select (-spc) %>% as.hyperSpec() # hyperSpec object with 0 wavelengths


select.hyperSpec <- function(.data, ...) {

  res <- select (.data@data, ...)

  if (is.null (res$spc)){
    attr (res, "labels") <- labels (.data) # allows to have correct labels when piping into `as.hyperSpec`
    res
  }else{
    .data@data <- res
    labels (.data) <- labels (.data) [c ( ".wavelength", colnames (.data))]
    .data
  }
}

# Begin unit testing (UT)
.test(select.hyperSpec) <- function(){
  context("select")

  # UT1
  test_that("selecting data columns correctly", {
    df <- data.frame(a = NA, b = NA)
    expect_identical(
      select(df, a),
      data.frame(a = NA)
    )
  })

  # UT2
  test_that("using select to rename data columns", {
    df <- data.frame(a = NA, b = NA)
    expect_identical(
      select(df, a_newcolname = a),
      data.frame(a_newcolname = NA)
    )
  })

  # UT3
  test_that("dropping spectra column", {
    tmp <- chondro@data
    tmp$spc <- NULL
    expect_equivalent(
      select(chondro, x, y),
      data.frame(x = chondro@data$x, y = chondro@data$y)
    )
    expect_equivalent(
      select(chondro, filename, clusters),
      data.frame(filename = chondro@data$filename, y = chondro@data$clusters, stringsAsFactors = FALSE)
    )
    expect_equivalent(
      select(chondro, -spc),
      tmp
    )
  })

  # UTTODO: Create unit test for hyperSpec object nuances.


}
