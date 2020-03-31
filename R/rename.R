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
#' chondro %>% rename (region = clusters) %>% head => hyperSpec object
#' chondro %>% rename (spc2 = spc) => renaming spc should result in an error
rename.hyperSpec <- function(.data, ...){
  res <- rename(.data@data, ...)

  # Check if the spc column was renamed
  if(!'spc' %in% colnames(res)){
    # Throw an error if spc column is missing
    stop("Error: `spc` column is missing")
  }else{
    .data@data <- res
    chk.hy(.data)
    .data
  }
}

# Begin unit testing (UT)
.test(rename.hyperSpec) <- function(){
  context("rename")

  # UT1
  test_that("renaming `spc` throws an error", {
    expect_error(rename(chondro, spc_newname = spc))
  })

}
