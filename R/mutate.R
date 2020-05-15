#' mutate for hyperSpec objects
#'
#' dplyr::mutate() preserves all the existing variables.
#' dplyr::transmute() adds new variables and drops existing ones.
#' Special column `$spc` contains the spectra matrix.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a hyperSpec object.
#'
#'
#'
#'
#' @inheritParams dplyr::mutate
#' @inheritParams dplyr::transmute
#' @seealso [dplyr::mutate()]
#' @return hyperSpec object.
#' @include unittest.R
#' @include hy_update_labels.R
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr mutate
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @export
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
      if('' %in% args_names[i]) {
        cols2get <- c(cols2get, expr)
      # Process `spc` argument assignments
      # Manipulate `spc` column before passing it on to transmute()
      } else if('spc' %in% args_names[i]) {
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
    # Hand off columns (i.e., prepared arguments) to mutate()
    transmute_args <- paste(cols2get, collapse=", ")
    res <- eval(parse(text = paste("mutate(tmp_hy@data,", transmute_args, ")")))
    # Update labels
    hy_update_labels(.data, res)
}

#' @export
transmute.hyperSpec <- function(.data, ...) {
    mutate.hyperSpec(.data, ..., .keep = "none")
}
