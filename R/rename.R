#' rename for hyperSpec objects
#'
#' rename extra data columns from the hyperSpec object.
#' Special column `$spc` contains the spectra matrix.
#' Unlike the dplyr select(), dplyr rename() keeps all the variables of the data frame intact.
#' However, if `$spc` is renamed, the result is technically a data.frame instead of a hyperSpec object.
#' Convert the data.frame again into a hyperSpec object if needed by using `as.hyperSpec ()`.
#'
#'
#'
#'
#'
#' @inheritParams dplyr::rename
#' @seealso [dplyr::rename()]
#' @return hyperSpec object or data.frame with the rename columns. If the `$spc` is renamed, the result will be a data.frame.
#' @include unittest.R
#' @importFrom dplyr rename
#' @importFrom hyperSpec labels<-
#' @export
#'
#' @examples
#'
#' chondro %>% rename (region = clusters) %>% head # spc not renamed => hyperSpec object
#' chondro %>% rename (spc2 = spc) # => data.frame
rename.hyperSpec <- function(.data, ...){
  res <- dplyr::rename(.data@data, ...)
  chk.hy(res)
  res
}

# Begin unit testing (UT)
.test(rename.hyperSpec) <- function(){
  context("rename")

  # UT1
  test_that("renaming data columns correctly", {
    df <- data.frame(a = NA, b = NA)
    expect_identical(
      rename(df, a = a, b = b),
      data.frame(a = NA, b = NA)
    )
    expect_identical(
      rename(df, a_newcolname = a, b_newcolname = b),
      data.frame(a_newcolname = NA, b_newcolname = NA)
    )
  })

  # UT2
  test_that("renaming perserves order", {
    df <- data.frame(a = NA, b = NA)
    expect_identical(
      rename(df, b_newcolname = b, a_newcolname = a),
      data.frame(a_newcolname = NA, b_newcolname = NA)
    )
  })

  # UT3
  test_that("renaming preserves grouping", {
    grouped_df <- group_by(data.frame(a = NA, b = NA), a)
    rename_grouped_df <- rename(grouped_df, a_newcolname = a)
    expect_equal(group_vars(rename_grouped_df), "a_newcolname")
  })

  # UT4
  test_that("renaming spectrum column throws error", {
    expect_error(chk.hy(rename(chondro, spc_newcolname = spc)))
  })

  # UTTODO: Create unit test for hyperSpec object nuances.
}
