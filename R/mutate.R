#' mutate for hyperSpec objects
#'
#' dplyr::mutate() adds new variables and preserves all the existing variables.
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
    mutate_args <- get_args(.data, ...)

    # Pass mutate arguments to dplyr::mutate
    .data@data <- eval(parse(text = paste("mutate(mutate_args$tmp_data,", mutate_args$args, ")")))
    .data
}

#' @export
transmute.hyperSpec <- function(.data, ...) {

    # Check if user passed in a hyperSpec object
    chk.hy(.data)

    # Collect transmute arguments
    transmute_args <- get_args(.data, ...)

    # Pass transmute arguments to dplyr::transmute
    res <- eval(parse(text = paste("transmute(transmute_args$tmp_data,", transmute_args$args, ")")))

    # Update labels
    setLabels.select(.data, res)
}
# Helper(s) -----------------------------------------------

get_args <- function(.data, ...) {

  # Collect function arguments
  args <- enquos(...)
  args_names <- names(args)

  # Give nothing, return nothing
  if (length(args) == 0L) {
    return(NULL)
  }

  # Prepare a copy of the original hyperSpec object
  tmp_hy <- .data
  cols2get <- vector() # creates a list to save the column names to

  # Prepare function arguments for mutate/transmute
  # assumption: the variable name and expr
  # share the same index (i.e., args_name[i] is the expr for the variable names(args[i]))
  for (i in seq_along(args)) {
    expr <- quo_name(quo_get_expr(args[[i]]))
    col_tmp <- charmatch(colnames(.data), deparse(substitute(expr)))
    if (col_tmp[!is.na(col_tmp)] > 0) {
      var_expr <- eval(parse(text = paste("tmp_hy@data$", expr, sep="")))
    }
    print(args_names[i])
    print(expr)
    # Process arguments with no names (assignments)
    if ("" %in% args_names[i]) {
      cols2get <- c(cols2get, expr)
      # Process matrix argument assignments
      # Manipulate `matrix column before passing it on to mutate/transmute
    } else if (args_names[i] %in% colnames(.data) && is.matrix(var_expr)) {

      # Handle misuse of spc column
      if (grepl("spc", deparse(substitute(expr))) && !"spc" %in% args_names[i]) {

        # Throw an error
        stop("$spc can only be mutated from itself")
      }
      tmp_hy@data[[args_names[i]]] <- .data@data[[args_names[i]]] # ensures operation on original column
      tmp_hy@data[[args_names[i]]] <- var_expr
      cols2get <- c(cols2get, args_names[i])
      # Process non matrix argument assignments
    } else {
      print("Here")
      assign <- paste(args_names[i],"=", expr, sep="")
      cols2get <- c(cols2get, assign)
    }
  }

  # Hand off columns (i.e., prepared arguments) to mutate()/transmute()
  cols2get <- unique(cols2get) # transmute/mutate already take care of this...
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
  test_that("$spc can only be mutated from itself", {
    expect_error(transmute.hyperSpec(flu, c, spc = c))
    expect_error(transmute.hyperSpec(flu, c, spc = filename*0, spc = spc))
    df <- flu
    df@data$spc <- df@data$spc*2
    expect_equal(transmute.hyperSpec(flu, spc = spc*2), select(df, spc))
  })
}
