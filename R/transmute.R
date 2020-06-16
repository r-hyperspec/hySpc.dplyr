#' transmute for hyperSpec object
#'
#' `transmute` adds new variables and drops all pre-existing variables.
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a hyperSpec object.
#'
#' @inheritParams dplyr::transmute
#'
#' @return hyperSpec object.
#' @include unittest.R
#' @include setLabels.R
#' @seealso [dplyr::transmute()]
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr transmute
#' @export
#'
#' @examples
#' laser %>%
#'   transmute (t, filename)
#'   head # => results in a data frame
#' laser %>%
#'    transmute (-spc) # => results in a hyperSpec object
#' laser %>%
#'    transmute (spc2 = spc*2) %>%
#'    transmute (spc2) %>%
#'    transmute (spc2*2) # => results in a hyperSpec object
transmute.hyperSpec <- function(.data, ...) {

  # Check if user passed in a hyperSpec object
  chk.hy(.data)

  # Pass transmute arguments to dplyr::transmute
  res <- transmute(.data@data, ...)

  # Update labels
  setLabels.select(.data, res)
}

.test(transmute.hyperSpec) <- function() {
  context("transmute.hyperSpec")

  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(transmute.hyperSpec(df))
  })

  test_that("arguments are correctly passed onto mutate/transmute", {
    df <- flu
    df <- dplyr::transmute(df@data, c, c = 0, c = 1)
    expect_equivalent(transmute.hyperSpec(flu, c, c = 0, c = 1), df)
  })

  test_that("$spc can be used for mutation", {
    hy_tmp <- .testdata
    hy_tmp@data$spc2 <- hy_tmp@data$spc*2
    expect_identical(mutate.hyperSpec(.testdata, spc2 = spc*2), hy_tmp)
  })
}
