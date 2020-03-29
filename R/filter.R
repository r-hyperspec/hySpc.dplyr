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
#' @importFrom hyperSpec chk.hy
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_equivalent
#' @importFrom testthat expect_error
#' @importFrom testthat skip
#' @importFrom hyperSpec chondro
#' @import magrittr
#' @export
#'
#' @examples
#' filter (chondro, clusters == "matrix")
#' chondro %>% filter (clusters == "matrix")
#'
#' ## filtering based on the spectra matrix:
#' # remove all spectra with NAs
#' tmp <- chondro [1:6]
#' tmp [[3:4,, 1300 ~ 1450]] <- NA
#' tmp [[6]] <- NA
#'
#' tmp %>% filter (!any_wl (is.na (spc)))
#' tmp %>% filter (all_wl (!is.na (spc))) # the same
#'
#' # remove spectra that contain only NAs:
#' tmp %>% filter (!all_wl (is.na (spc)))
#' tmp %>% filter (any_wl (!is.na (spc))) # the same
#'
#' # keep only spectra with minimum average intensity
#' chondro %>%
#'    filter (rowMeans (spc) > 500) %>%
#'    plotmap
filter.hyperSpec <- function(.data, ..., .preserve = FALSE) {

  .data@data <- filter(.data@data, ..., .preserve = .preserve)
  .data <- .fix_spc_colnames(.data)

  chk.hy(.data)

  .data
}

.test(filter.hyperSpec) <- function (){
  context("filter")


  test_that ("filtering extra data columns: numeric", {
    skip ("until flu is exported from hyperSpec")
    expect_equal(filter (flu, c > 0.3), flu [flu$c > 0.3]) # 0 row object

    # filter drops row names, so only equivalent, not equal:
    expect_equivalent(filter (flu, c > 0.2), flu [flu$c > 0.2])

    expect_equivalent(filter (chondro, clusters == "lacuna"),
                      chondro [chondro$clusters == "lacuna" & !is.na (chondro$clusters)]

    )
  })

    test_that ("filtering extra data columns: factor", {
    expect_equivalent(filter (chondro, clusters == "lacuna"),
                 chondro [chondro$clusters == "lacuna" & !is.na (chondro$clusters)]

    )
  })

  test_that ("filtering the spectra matrix", {
    expect_error (filter (chondro, spc > 250))

    expect_equivalent(
      filter (chondro, all_wl (spc > 250)),
      chondro [all_wl (chondro > 250)]
    )

    tmp <- chondro [1:6]
    tmp [[3:4,, 1300 ~ 1450]] <- NA
    tmp [[6]] <- NA

    expect_equivalent(
      filter (tmp, all_wl (! is.na (spc))),
      tmp [all_wl (! is.na (tmp))]
    )

    expect_equivalent(
      filter (tmp, !any_wl (is.na (spc))),
      tmp [all_wl (! is.na (tmp))]
    )

  })

  test_that("grouping and filter", {
    skip ("grouping not yet implemented")
  })
}

