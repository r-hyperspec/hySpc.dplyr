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
#' @include setLabels.R
#' @importFrom dplyr select
#' @importFrom hyperSpec labels labels<-
#' @export
#'
#' @examples
#'
#' chondro %>%
#'   select(x, y, clusters) %>%
#'   head() # spc not selected => data.frame
#' chondro %>% select(x, clusters, spc) # => hyperSpec object
#'
#' chondro %>%
#'   select(-spc) %>%
#'   head() # all columns but $spc => data.frame same as chondro$..
#' chondro %>%
#'   select(-spc) %>%
#'   as.hyperSpec() # hyperSpec object with 0 wavelengths
select.hyperSpec <- function(.data, ...) {
  res <- select(.data@data, ...)
  # Update labels
  setLabels.select(.data, res)
}

.test(select.hyperSpec) <- function() {
  context("select.hyperSpec")

  test_that("labels attribute when returning data.frame", {
    ref_labels <- labels(chondro [, c("x", "y")])

    # label $spc is added automatically by initialize -
    # it is not supposed to be returned by select.hyperSpec
    ref_labels <- ref_labels [!grepl("spc", ref_labels)]
    ref_labels <- ref_labels [order(names(ref_labels))]

    test_labels <- attr(select(chondro, x, y), "labels")
    test_labels <- test_labels [order(names(test_labels))]

    expect_equal(test_labels, ref_labels)
  })

  test_that("labels attribute when returning data.frame", {
    expect_equal(
      labels(as.hyperSpec(select(chondro, x, y))),
      labels(chondro [, c("x", "y")])
    )
  })
}
