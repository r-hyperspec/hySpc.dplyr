#' mutate for hyperSpec objects
#'
#' dplyr::mutate() adds mew variables and preserves all the existing variables.
#' dplyr::transmute() adds and only keeps new variables.
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a hyperSpec object.
#'
#' @inheritParams dplyr::mutate
#' @inheritParams dplyr::transmute
#' @seealso [dplyr::mutate()]
#' @return hyperSpec object.
#' @include unittest.R
#' @include setLabels.R
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr mutate
#' @importFrom dplyr transmute
#'
#' @examples

#' # Mutate -----------------------------------------------
#' chondro %>%
#'   mutate (y, x, filename, spc) %>%
#'   head # => results in a hyperSpec object
#' chondro %>%
#'   mutate (y, x)
#'   head # => results in a hyperSpec object
#' chondro %>%
#'   mutate (y, x, filename, spc = spc*2)
#'   head # => results in a hyperSpec object
#' flu %>%
#'   mutate (spc, filename, c)
#'   head # => results in a hyperSpec object
#' flu %>%
#'   mutate (filename, c)
#'   head # => results in a hyperSpec object
#' flu %>%
#'   mutate (c=c*2, c=c*0)
#'   # => results in a hyperSpec object
#'
#' # Transmute -----------------------------------------------
#' chondro %>%
#'   transmute (y, x, filename, spc) %>%
#'   head # => results in a hyperSpec object
#' chondro %>%
#'   transmute (y, x)
#'   head # => results in a data frame
#' chondro %>%
#'   transmute (y, x, filename, spc = spc*2)
#'   head # => results in a hyperSpec object
#' flu %>%
#'   transmute (spc, filename, c)
#'   head # => results in a hyperSpec object
#' flu %>%
#'   transmute (filename, c)
#'   head # => results in a data frame
#' flu %>%
#'   transmute (c=c*2, c=c*0)
#'   # => results in a data frame
#' @export
mutate.hyperSpec <- function(.data, ...) {
    # Check if user passed in a hyperSpec object
    chk.hy(.data)
    # Collect mutate arguments
    ls_hy <- get_args(.data, ...)
    # Pass mutate arguments to dplyr::mutate
    .data@data <- eval(parse(text = paste("mutate(ls_hy$tmp_data,", ls_hy$args, ")")))
    .data
}

#' @export
transmute.hyperSpec <- function(.data, ...) {
    # Check if user passed in a hyperSpec object
    chk.hy(.data)
    # Collect transmute arguments
    ls_hy <- get_args(.data, ...)
    # Pass transmute arguments to dplyr::transmute
    res <- eval(parse(text = paste("transmute(ls_hy$tmp_data,", ls_hy$args, ")")))
    # Update labels
    setLabels.select(.data, res)
}
# Helper(s) -----------------------------------------------

get_args <- function(.data, ...) {
    # Collect function arguments
    args <- enquos(...)
    args_names <- names(args)
    if (length(args) == 0L) {
      return(NULL)
    }
    # Prepare a copy of the original hyperSpec object
    tmp_hy <- .data
    cols2get <- vector() # create a list to save the column names to
    tmp_spc <- tmp_hy@data[c('spc')] # store original $spc column
    # Prepare function arguments for transmute()
    # assumption: the variable name and expr
    # share the same index (i.e., args[i] is the expr for the variable names(args[i]))
    for (i in seq_along(args)) {
      expr <- quo_name(quo_get_expr(args[[i]]))
      # Process arguments with no names (assignments)
      if ('' %in% args_names[i]) {
        cols2get <- c(cols2get, expr)
      # Process `spc` argument assignments
      # Manipulate `spc` column before passing it on to transmute()
      } else if ('spc' %in% args_names[i]) {
        if (grepl('spc', expr)) {
          tmp_hy@data[c('spc')] <- tmp_spc
          eval(parse(text = paste("tmp_hy@data[c('spc')]<-", "tmp_hy@data$", expr)))
          if (!'spc' %in% cols2get) {
            cols2get <- c(cols2get, 'spc') # ensures there is, and only one `spc` column
          }
        } else {
          # Throw an error
          stop("$spc must be mutated from a $spc column")
        }
      # Process non `spc` argument assignments
      } else {
        assign <- paste(args_names[i],'=', expr, sep='')
        cols2get <- c(cols2get, assign)
      }
    }
    # Hand off columns (i.e., prepared arguments) to mutate()/transmute()
    ls_hy <- list(tmp_data = tmp_hy@data, args = paste(cols2get, collapse=", "))
}

.test(mutate.hyperSpec) <- function() {
  context("mutate.hyperSpec")

  # UT1
  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(mutate.hyperSpec(df))
    expect_error(transmute.hyperSpec(df))
  })

  # UT2
  test_that("arguments are correctly passed onto mutate/transmute", {
    df <- flu
    df <- dplyr::transmute(df@data, c, c = 0, c = 1)
    expect_equivalent(transmute.hyperSpec(flu, c, c = 0, c = 1), df)
  })

  # UT3
  test_that("$spc can only be mutated from a $spc column", {
    expect_error(transmute.hyperSpec(flu, c, spc = c))
    expect_error(transmute.hyperSpec(flu, c, spc = filename*0, spc = spc))
    df <- flu
    df@data$spc <- df@data$spc*2
    expect_equal(transmute.hyperSpec(flu, spc = spc*2), select(df, spc))
  })
}
