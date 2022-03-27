#' Ensure that hyperSpec and non hyperSpec objects have the correct labels

#' @param .data hyperSpec object
#' @param ... resulting data frame
#' @return Object with the correct labels
#' @md
#'
#' @importFrom hyperSpec assert_hyperSpec
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#'
#' @examples
#' flu %>%
#'   setLabels(.wavelength = "f / THz", c = "c / ml")
#'
#' @export
setLabels <- function(.data, ...) {

  # Check if user passed in a hyperSpec object
  assert_hyperSpec(.data)
  args <- enquos(...)
  args_names <- names(args)
  labels2update <- args_names[args_names %in% names(labels(.data))]
  if (length(args) == 0L) {
    return(NULL)
  }
  for (i in seq_along(args)) {
    setlabels2 <- quo_name(quo_get_expr(args[[i]]))
    if (grepl("expr", setlabels2)) {
      setlabels2 <- as.expression(setlabels2)
    }
    labels(.data, labels2update[i]) <- setlabels2
  }
  .data
}

#' @rdname setLabels
#' @aliases setLabels
#' @param .data hyperSpec object
#' @param ... list of columns to update on
#' @export
setLabels_transmute <- function(.data, ...) {
  setLabels(.data, ...)
}

#' @rdname setLabels
#' @aliases setLabels
#' @param .data hyperSpec object
#' @param res resulting data frame
#' @export
setLabels_select <- function(.data, res) {

  # Update labels
  labels <- labels(.data)[c(".wavelength", colnames(res))]
  if (is.null(res$spc)) {

    # use attribute to have correct labels when piping into `as.hyperSpec`
    attr(res, "labels") <- labels
    res
  } else {
    .data@data <- res
    labels(.data) <- labels
    .data
  }
}

#' @rdname setLabels
#' @aliases setLabels
#' @param .data hyperSpec object
#' @param res resulting data frame
#' @export
setLabels_rename <- function(.data, res) {
  labels.to.update <- setdiff(colnames(res), colnames(.data))
  labels.to.remove <- setdiff(colnames(.data), colnames(res))

  # Update the data slot with newly renamed data frame
  .data@data <- res

  # Update labels of hyperSpec object
  new.labels <- lapply(labels(.data, labels.to.update), as.expression)
  labels(.data)[c(labels.to.remove)] <- NULL
  labels(.data)[c(labels.to.update)] <- new.labels
  .data
}

hySpc.testthat::test(setLabels) <- function() {
  context("setLabels")

  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(setLabels(df))
  })

  test_that("labels are correctly set", {
    tmp <- laser
    labels(tmp, ".wavelength") <- "f / THz"
    expect_equivalent(setLabels(laser, .wavelength = "f /THz"), tmp)
  })
}
