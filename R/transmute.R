#' Transmute for hyperSpec Object
#'
#' `transmute` adds new variables and drops all pre-existing variables.
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a
#' hyperSpec object.
#'
#' @inheritParams dplyr::transmute
#'
#' @return hyperSpec object.
#' @include unittest.R
#' @include setLabels.R
#' @seealso [dplyr::transmute()]
#' @importFrom hyperSpec assert_hyperSpec
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr transmute
#' @export
#'
#' @examples
#' data(laser, package = "hyperSpec")
#'
#' laser %>%
#'   transmute(t, filename) %>%
#'   head() # => results in a data frame
#'
#' laser %>%
#'   transmute(-spc) # => results in a hyperSpec object
#'
#' laser %>%
#'   transmute(spc2 = spc * 2) %>%
#'   transmute(spc2) %>%
#'   transmute(spc2 * 2) # => results in a hyperSpec object
transmute.hyperSpec <- function(.data, ...) {

  # Check if user passed in a hyperSpec object
  assert_hyperSpec(.data)

  # Pass transmute arguments to dplyr::transmute
  res <- transmute(.data@data, ...)

  # Update labels
  setLabels_select(.data, res)
}

hySpc.testthat::test(transmute.hyperSpec) <- function() {
  context("transmute.hyperSpec")

  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(transmute.hyperSpec(df))
  })

  test_that("arguments are correctly passed onto transmute", {
    # skip("@eoduniyi FIX SOMEHOW...")
    hy_tmp <- .testdata
    hy_tmp@data <- dplyr::transmute(hy_tmp@data, spc * 2, spc * 3, spc * 0)
    expect_equivalent(transmute.hyperSpec(.testdata, spc * 2, spc * 3, spc * 0), hy_tmp)
  })

  test_that("$spc can be used for mutation", {
    # skip("@eoduniyi FIX SOMEHOW...")
    hy_tmp <- .testdata
    hy_tmp@data$spc2 <- hy_tmp@data$spc * 2
    expect_identical(transmute.hyperSpec(.testdata, spc2 = spc * 2), hy_tmp)
  })
}
