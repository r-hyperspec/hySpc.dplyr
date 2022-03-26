#' mutate for hyperSpec object
#'
#' `mutate` adds new variables and preserves all the existing variables.
#' Special column `$spc` contains the spectra matrix.
#'
#' @inheritParams dplyr::mutate
#'
#' @return hyperSpec object.
#' @include unittest.R
#' @include setLabels.R
#' @seealso [dplyr::mutate()]
#' @importFrom hyperSpec assert_hyperSpec
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' laser %>%
#'   mutate (t, filename)
#'   head # => results in a hyperSpec object
#' laser %>%
#'    mutate (-spc) # => results in a hyperSpec object
#' laser %>%
#'    mutate (spc2 = spc*2) %>%
#'    mutate (spc2) %>%
#'    mutate (spc2*2) # => results in a hyperSpec object
mutate.hyperSpec <- function(.data, ...) {

    # Check if user passed in a hyperSpec object
    assert_hyperSpec(.data)

    # Pass mutate arguments to dplyr::mutate
    res <- mutate(.data@data, ...)
    .data@data <- res
    .data
}

hySpc.testthat::test(mutate.hyperSpec) <- function() {
  context("mutate.hyperSpec")

  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(mutate.hyperSpec(df))
  })

  test_that("arguments are correctly passed onto mutate/transmute", {
    hy_tmp <- .testdata
    hy_tmp@data <- dplyr::mutate(hy_tmp@data, spc*2, spc*3, spc*0)
    expect_equivalent(mutate.hyperSpec(.testdata, spc*2, spc*3, spc*0), hy_tmp)
  })

  test_that("$spc can be used for mutation", {
    hy_tmp <- .testdata
    hy_tmp@data$spc2 <- hy_tmp@data$spc*2
    expect_identical(mutate.hyperSpec(.testdata, spc2 = spc*2), hy_tmp)
  })
}
