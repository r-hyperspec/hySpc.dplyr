
#' transmute for hyperSpec objects
#'
#' adds new variables and drops existing ones.
#' Special column `$spc` contains the spectra matrix.
#' Unlike dplyr::transmute() dplyr::mutate() preserves all the existing variables.
#' If `$spc` is not being transmuted, the result is a data.frame instead of a hyperSpec object.
#'
#'
#'
#'
#'
#' @inheritParams dplyr::transmute
#' @seealso [dplyr::transmute()]
#' @return hyperSpec object. If `$spc` is renamed, the result is an error.
#' @include unittest.R
#' @importFrom rlang quo_name
#' @importFrom rlang quo_get_expr
#' @importFrom dplyr transmute
#' @importFrom hyperSpec chk.hy
#' @importFrom hyperSpec labels labels<-
#' @export
#'
#' @examples
#'
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
transmute.hyperSpec <- function(.data, ...){
  # Check if user passed in a hyperSpec object
  chk.hy(.data)
  # Collect function arguments
  args <- enquos(...)
  args_names <- names(args)
  if(length(args) == 0L){
    return(NULL)
  }
  # Prepare a copy of the original hyperSpec object
  tmp_hy <- .data
  cols2get <- vector() # create a list to save the column names to
  tmp_spc <- tmp_hy@data[c('spc')] # store original $spc column
  # Prepare function arguments for transmute()
  # assumption: the variable name and expr
  # share the same index (i.e., args[i] is the expr for the variable names(args[i]))
  for(i in seq_along(args)){
    expr <- quo_name(quo_get_expr(args[[i]]))
    # Process arguments with no names (assignments)
    if('' %in% args_names[i]){
      cols2get <- c(cols2get, expr)
    # Process `spc` argument assignments
    # Manipulate `spc` column before passing it on to transmute()
    }else if('spc' %in% args_names[i]){
      if(grepl('spc', expr)){
        tmp_hy@data[c('spc')] <- tmp_spc
        eval(parse(text = paste("tmp_hy@data[c('spc')]<-", "tmp_hy@data$", expr)))
        if(!'spc' %in% cols2get){
          cols2get <- c(cols2get, 'spc') # ensures there is, and only one `spc` column
        }
      }else{
        # Throw an error
        stop("$spc must be mutated from a $spc column")
      }
    # Process non `spc` argument assignments
    }else{
      assign <- paste(args_names[i],'=', expr, sep='')
      cols2get <- c(cols2get, assign)
    }
  }
  # Hand off columns (i.e., prepared arguments) to transmute()
  transmute_args <- paste(cols2get, collapse=", ")
  res <- eval(parse(text = paste("transmute(tmp_hy@data,", transmute_args, ")")))
  # Update labels
  labels <- labels(.data)[c(".wavelength", colnames(res))]
  if(is.null(res$spc)){
    # use attribute to have correct labels when piping into `as.hyperSpec`
    attr(res, "labels") <- labels
    res
  }else{
    .data@data <- res
    labels(.data) <- labels
    .data
  }
}

# Begin unit testing (UT)
.test(transmute.hyperSpec) <- function(){
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
  test_that("$spc can only be mutated from a $spc column", {
    expect_error(transmute.hyperSpec(flu, c, spc = c))
    expect_error(transmute.hyperSpec(flu, c, spc = filename*0, spc = spc))
    df <- flu
    df@data$spc <- df@data$spc*2
    expect_identical(transmute.hyperSpec(flu, spc = spc*2), select(df, spc))
  })

  # UT4
  test_that("labels attribute when returning data.frame", {
    ref_labels <- labels(chondro [, c("x", "y")])

    # label $spc is added automatically by initialize -
    # it is not supposed to be returned by transmute.hyperSpec???
    ref_labels <- ref_labels [!grepl("spc", ref_labels)]
    ref_labels <- ref_labels [order(names(ref_labels))]

    test_labels <- attr(transmute.hyperSpec(chondro, x, y), "labels")
    test_labels <- test_labels [order(names(test_labels))]

    expect_equal(test_labels, ref_labels)
  })
  test_that("labels attribute when returning data.frame", {
    expect_equal(
      labels(as.hyperSpec(transmute.hyperSpec(chondro, x, y))),
      labels(chondro [, c("x", "y")])
    )
  })
}
