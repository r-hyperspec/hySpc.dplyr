#' @description dplyr::transmute() adds and only keeps new variables.
#' @rdname mutate.hyperSpec
#' @aliases mutate.hyperSpec
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a hyperSpec object.
#'
#' @examples
#'
#' chondro %>%
#'   transmute (x, y) %>%
#'   head # => results in a data frame
#' chondro %>%
#'    transmute (x = y, y = x) # => results in a data frame
#' chondro %>%
#'    transmute (x2 = y, y2 = x) # => results in a data frame
#' chondro %>%
#'    transmute (y, x, spc2 = spc*2) # => results in a hyperSpec object
#' chondro %>%
#'    transmute (spc2 = spc*2) %>%
#'    transmute (spc2) %>%
#'    transmute (spc2*2) # => results in a hyperSpec object
#' chondro %>%
#'   transmute (x, y, spc) %>%
#'   head # => results in a hyperSpec object
#' @export
transmute.hyperSpec <- function(.data, ...) {

  # Check if user passed in a hyperSpec object
  chk.hy(.data)

  # Collect transmute arguments
  transmute_args <- pre_mutation(.data, ...)

  # Pass transmute arguments to dplyr::transmute
  res <- eval(parse(text = paste("transmute(transmute_args$tmp_data,", transmute_args$args, ")")))

  # Update labels
  setLabels.select(.data, res)
}

.test(transmute.hyperSpec) <- function() {
  context("transmute.hyperSpec")

  # UT1
  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(transmute.hyperSpec(df))
  })

  # UT2
  test_that("arguments are correctly passed onto mutate/transmute", {
    df <- flu
    df <- dplyr::transmute(df@data, c, c = 0, c = 1)
    expect_equivalent(transmute.hyperSpec(flu, c, c = 0, c = 1), df)
  })

  # UT3
  test_that("$spc cannot be mutated", {
    expect_error(mutate.hyperSpec(chondro, x, spc*2))
    expect_error(transmute.hyperSpec(chondro, x, spc = c))
    expect_error(transmute.hyperSpec(chondro, x, spc = spc))
    expect_error(transmute.hyperSpec(chondro, x, spc = spc*2))
  })

  # UT4
  test_that("$spc can be used for mutation", {
    hy_tmp <- chondro
    hy_tmp@data$spc2 <- hy_tmp@data$spc*2
    expect_equivalent(mutate.hyperSpec(chondro, spc2 = spc*2), hy_tmp)
  })
}
