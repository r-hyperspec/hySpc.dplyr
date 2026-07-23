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

  test_that("data.frame is returned when `$spc` are not transmuted", {
    expect_equivalent(
      transmute(.testdata, c = c*2),
      .testdata[[, c("c")]]*2
    )
  })

  test_that("columns are returned in the correct order", {
    expect_equivalent(
      transmute(.testdata, c = c*0, c1 = c*1, c2 = c*2),
      cbind(c = .testdata[[, c("c")]]*0, c1 = 0, c2 = 0)
      )
  })

  test_that("hyperSpec object is returned when `$spc` is transmuted", {
    expect_equivalent(
      assert_hyperSpec(transmute(.testdata, spc = spc*2)),
      assert_hyperSpec(.testdata)
      )
  })
}
