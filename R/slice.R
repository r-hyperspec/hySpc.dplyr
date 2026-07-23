#' @inheritParams dplyr::slice
#' @rdname filter.hyperSpec
#' @importFrom dplyr slice
#' @export
#'
#' @examples
#' chondro %>% slice(1:3)
#' chondro %>% slice(800:n())
#' chondro %>% slice(-10:-n())
slice.hyperSpec <- function(.data, ..., .preserve = FALSE) {
  .data@data <- slice(.data@data, ..., .preserve = .preserve)
  .data <- .spc_fix_colnames(.data)

  assert_hyperSpec(.data)

  .data
}

hySpc.testthat::test(slice.hyperSpec) <- function() {
  context("slice")

  test_that("simple slicing", {
    expect_equal(slice(flu, 1:3), flu[1:3])
    expect_equal(slice(flu, 0), flu[0])

    # slice drops row names, so only equivalent, not equal:
    expect_equivalent(slice(flu, 3:n()), flu[3:nrow(flu)])

    expect_equal(slice(flu, -3:-n()), flu[-3:-nrow(flu)])
  })

  test_that("multiple index parameters", {
    expect_equivalent(
      slice(chondro, 1:3, n() - (0:3)),
      chondro[c(1:3, nrow(chondro) - (0:3))]
    )
  })

  # Maybe we should table this for a larger discussion/issue
  test_that("grouping and slice", {
    skip("grouping not yet implemented")
  })
}
