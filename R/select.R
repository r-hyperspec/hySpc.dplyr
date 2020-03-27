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
  test_that("dropping spectra column", {

    # UT1.1
    expect_equivalent(
      select(chondro, x, y),
      data.frame(x = chondro@data$x, y = chondro@data$y)
    )

    # UT1.2
    tmp <- chondro@data
    tmp$spc <- NULL
    expect_equivalent(
      select(chondro, -spc),
      tmp
    )

    # UT1.3 -- not sure if this is necessary
    expect_equivalent(
      is.data.frame(select(chondro, -spc)),
      is.data.frame(tmp)
    )

    # UT1.4
    tmp <- data.frame(filename = chondro@data$filename,
                      clusters = chondro@data$clusters,
                      stringsAsFactors = FALSE)
    attr(tmp, "labels") <- labels(chondro)
    expect_equal(
      select(chondro, filename, clusters),
      tmp,
    )

    # UT1.5
    expect_error(chk.hy(select(chondro, filename, clusters)))
  })

  # UT2
  test_that("retaining spectra column", {

    # UT2.1
    expect_equivalent(
      chk.hy(select(chondro, x, clusters, spc)),
      chk.hy(chondro)
    )

    # UT2.2
    expect_equal(
      attr(select(chondro, x, clusters, spc), "labels"),
      attr(chondro, "labels")
    )

    # UT2.3 -- Why does this keep failing?
    tmp <- new("hyperSpec", spc = chondro@data$spc)
    attr(tmp, "labels") <- labels(chondro)
    expect_equal(select(chondro, spc), tmp)
  })

  # UTTODO: Create unit test for hyperSpec object nuances.

}
