#' filter or slice hyperSpec object
#'
#' `filter`ing based on extra data columns works smoothly,
#' but filtering on the spectra matrix needs some attention:
#' the filtering expression must return a logical vector with one value per spectrum,
#' see examples.
#'
#' @inheritParams dplyr::filter
#'
#' @return hyperSpec object with rows (spectra) matching the conditions (logical predicates) or the indices specified in `...`
#' @include unittest.R
#' @seealso [dplyr::filter()]
#' @importFrom dplyr filter
#' @importFrom hyperSpec assert_hyperSpec
#' @importFrom hyperSpec .spc_fix_colnames
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_equivalent
#' @importFrom testthat expect_error
#' @importFrom testthat skip
#' @import magrittr
#' @export
#'
#' @examples
#' filter(laser, t < 250)
#' laser %>% filter(t < 250)
#'
#' ## filtering based on the spectra matrix:
#' # remove all spectra with NAs
#' tmp <- laser[1:6]
#' tmp[[3:4, , 404.7 ~ 404.9]] <- NA
#' tmp[[6]] <- NA
#'
#' tmp %>% filter(!any_wl(is.na(spc)))
#' tmp %>% filter(all_wl(!is.na(spc))) # the same
#'
#' # remove spectra that contain /only/ NAs:
#' tmp %>% filter(!all_wl(is.na(spc)))
#' tmp %>% filter(any_wl(!is.na(spc))) # the same
#'
#' # keep only spectra with minimum average intensity
#' laser %>%
#'   plot(spc.nmax = Inf)
#'
#' laser %>%
#'   filter(rowMeans(spc) > 9000) %>%
#'   plot(col = "red", add = TRUE)
filter.hyperSpec <- function(.data, ..., .preserve = FALSE) {
  .data@data <- filter(.data@data, ..., .preserve = .preserve)
  .data <- .spc_fix_colnames(.data)

  assert_hyperSpec(.data)

  .data
}

hySpc.testthat::test(filter.hyperSpec) <- function() {
  context("filter")


  test_that("filtering extra data columns: numeric", {
    # 0 row object
    expect_equal(filter(.testdata, c > 0.3), .testdata[.testdata$c > 0.3])

    # filter drops row names, so only equivalent, not equal:
    expect_equivalent(filter(.testdata, c > 0.2), .testdata[.testdata$c > 0.2])

    expect_equivalent(
      filter(.testdata, region == "a"),
      .testdata[.testdata$region == "a" & !is.na(.testdata$region)]
    )
  })

  test_that("filtering extra data columns: factor", {
    expect_equivalent(
      filter(.testdata, region == "a"),
      .testdata[.testdata$region == "a" & !is.na(.testdata$region)]
    )
  })

  test_that("filtering the spectra matrix", {
    ## comparison on spectra matrix yields nrow * nwl results, but filter needs
    ## nrow results

    expect_equivalent(
      filter(.testdata, all_wl(spc > 100)),
      .testdata[all_wl(.testdata > 100) & !is.na(all_wl(.testdata > 100))]
    )

    expect_equivalent(
      filter(.testdata, all_wl(!is.na(spc))),
      .testdata[all_wl(!is.na(.testdata))]
    )

    expect_equivalent(
      filter(.testdata, !any_wl(is.na(spc))),
      .testdata[all_wl(!is.na(.testdata))]
    )
  })

  # Maybe we should table this for a larger discussion/issue
  test_that("grouping and filter", {
    skip("grouping not yet implemented")
  })
}
