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
#' @seealso [dplyr::select()], [keep.hy()]
#' @param keep.hyperSpec should the result always be a hyperSpec object (`keep.hyperSpec = TRUE`),
#' or  be converted into a data.frame if the spectra matrix is not included in the selection
#' (`keep.hyperSpec = FALSE`)?
#'
#' @return hyperSpec object or data.frame with the selected columns. If the `$spc` is not included in the selection, the result will be a data.frame.
#' @include unittest.R
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

# @rdname select
select_.hyperSpec <- select.hyperSpec

.test (select.hyperSpec) <- function() {
  context ("select")

  test_that ("unselecting $spc produces data.frame", {
    expect_s3_class(select (flu, -spc), "data.frame")

    tmp <- chondro %>%
      select (-spc) %>%
      as.hyperSpec

    expect_s4_class (tmp, "hyperSpec")
    expect_equivalent(dim (tmp), c (nrow (chondro), ncol (chondro), 0L))
    expect_equal (tmp$.., chondro$..)
    expect_equal (labels (tmp), labels (chondro [,,FALSE]))
  })

  test_that("selecting with $spc => hyperSpec object", {

    tmp <- select (chondro, x, spc)
    expect_equal(tmp$.., chondro [,c ("x", "spc")]$..)


    tmp <- select (chondro, spc, x)
    expect_equal(tmp$.., chondro [,c ("spc", "x")]$..)
  })
}
