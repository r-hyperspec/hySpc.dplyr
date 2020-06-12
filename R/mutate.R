#' mutate and transmute for hyperSpec objects
#'
#' @description dplyr::mutate() adds new variables and preserves all the existing variables.
#' Special column `$spc` contains the spectra matrix.
#'
#' @inheritParams dplyr::mutate
#' @inheritParams dplyr::transmute
#' @seealso [dplyr::mutate()]
#' @return hyperSpec object.
#' @include unittest.R
#' @include setLabels.R
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr mutate
#' @importFrom dplyr transmute
#'
#' @examples
#'
#' chondro %>%
#'   mutate (x, y)
#'   head # => results in a hyperSpec object
#' chondro %>%
#'    mutate (x = y, y = x) # => results in a hyperSpec object
#' chondro %>%
#'    mutate (x2 = y, y2 = x) # => results in a hyperSpec object
#' chondro %>%
#'    mutate (y, x, spc2 = spc*2) # => results in a hyperSpec object
#' chondro %>%
#'    mutate (spc2 = spc*2) %>%
#'    mutate (spc2) %>%
#'    mutate (spc2*2) # => results in a hyperSpec object
#' chondro %>%
#'   mutate (y, x, spc) %>%
#'   head # => results in a hyperSpec object
#'
#' @export
mutate.hyperSpec <- function(.data, ...) {

    # Check if user passed in a hyperSpec object
    chk.hy(.data)

    # Collect mutate arguments
    mutate_args <- pre_mutation(.data, ...)

    # Pass mutate arguments to dplyr::mutate
    .data@data <- eval(parse(text = paste("mutate(mutate_args$tmp_data,", mutate_args$args, ")")))
    .data
}

.test(mutate.hyperSpec) <- function() {
  context("mutate.hyperSpec")

  # UT1
  test_that("non hyperSpec objects are rejected", {
    df <- data.frame(a = NA, b = NA)
    expect_error(mutate.hyperSpec(df))
  })

  # UT2
  test_that("arguments are correctly passed onto mutate/transmute", {
    df <- chondro
    df <- dplyr::mutate(df@data, x, x = 0, x = 1)
    expect_identical(mutate.hyperSpec(chondro, x, x = 0, x = 1)@data, df)
  })

  # UT3
  test_that("$spc cannot be mutated", {
    expect_error(mutate.hyperSpec(chondro, x, spc*2))
    expect_error(mutate.hyperSpec(chondro, x, spc = c))
    expect_error(mutate.hyperSpec(chondro, x, spc = spc))
    expect_error(mutate.hyperSpec(chondro, x, spc = spc*2))
  })

  # UT4
  test_that("$spc can be used for mutation", {
    hy_tmp <- chondro
    hy_tmp@data$spc2 <- hy_tmp@data$spc*2
    expect_equivalent(mutate.hyperSpec(chondro, spc2 = spc*2), hy_tmp)
  })
}


# Helper(s) -----------------------------------------------
pre_mutation <- function(.data, ...) {

  # Collect function arguments
  args <- enquos(...)
  args_names <- names(args)

  # Give nothing, return nothing
  if (length(args) == 0L) {
    return(NULL)
  }

  # Make a copy of the original hyperSpec object
  tmp_hy <- .data
  cols2get <- vector()

  # Prepare function arguments for mutate/transmute
  # assumption: the variable name and expr
  # share the same index (i.e., args_name[i] is the expr for the variable names(args[i]))
  for (i in seq_along(args)) {
    expr <- quo_name(quo_get_expr(args[[i]]))
    col_name <- trimws(gsub("[[:punct:]].*","", expr), "right") # "base" expr ~should~ be in colnames(.data)
    # print(col_name)
    # print(args_names[i])

    # Capture expression value
    if (!grepl("\\D", expr)) {
      expr_val <- eval(parse(text = expr))
    } else {
      expr_val <- eval(parse(text = paste0("tmp_hy@data$", expr)))
    }

    # Argument has no name (only an expression)
    if ("" %in% args_names[i]) {

      # Expression is a column with row matrices
      if (is.matrix(expr_val)) {

        # Mutation is being performed on `spc``
        if ("spc" %in% col_name && grepl('[^[:alnum:]]', expr)) {

          # Throw error
          stop("$spc column cannot be mutated")

          # Collect `spc` column
        } else if ("spc" %in% expr) {

          # Store expr in column
          cols2get <- c(cols2get, 'spc')

          # Update temporary hyperSpec object before passing it to mutate/transmute
        } else {

          # Update tmp_hy@data
          tmp_hy@data[[col_name]] <- expr_val

          # Store expr in column (# just store `mat` not `mat`+anything_else)
          cols2get <- c(cols2get, col_name)
        }

        # Column already exist in the hyperSpec object
      } else {

        # Store "base" expr in column
        cols2get <- c(cols2get, expr)
      }

      # Expression's name (args_name[i]) is not empty
    } else {

      # Mutation is being performed on `spc`
      if ("spc" %in% args_names[i]) {

        # Throw error
        stop("$spc column cannot be a named argument")

        # Expression is a column with row matrices
      } else if (is.matrix(expr_val)) {

        # Update tmp_hy@data
        tmp_hy@data[[args_names[i]]] <- expr_val

        # Store "base" expr in column
        cols2get <- c(cols2get, args_names[i])

        # "vanilla" assignment
      } else {

        # Create an assignment using paste
        assign <- paste0(args_names[i], "=", expr)

        # Store expr in column
        cols2get <- c(cols2get, assign)
      }
    }
  }
    # Hand off columns (i.e., prepared arguments) to mutate()/transmute()
    cols2get <- unique(cols2get) # transmute/mutate might already take care of this...
    return(list(tmp_data = tmp_hy@data, args = paste(cols2get, collapse=", ")))
}
