#' between for matrices and hyperSpec objects
#'
#' Between is a shortcut function for `x >= left & x <= right`.
#'
#' @seealso [dplyr::between()]
#' @inheritParams dplyr::between
#' @export
#' @include unittest.R
#' @family between
#'
setGeneric ("between", useAsDefault = dplyr::between)

.between.matrix <- function (x, left, right){
  res <- dplyr::between (x, left, right)
  dim (res) <- dim (x)
  dimnames (res) <- dimnames(x)

  res
}
#' @rdname between
#' @param x matrix
#' @return logical *matrix* indicating which elements of the matrix are between `left` and `right`.
#' @family between
#' @export
#'
#' @examples
#' between (flu [[]], 100, 400)
setMethod("between", signature = "matrix", .between.matrix)

.test (.between.matrix) <- function (){
  context ("between")

  test_that("correct matrix behaviour",{
    expect_equal (between (flu[[]], 100, 400),
                  flu[[]] >= 100 & flu[[]] <= 400)
  })

}


.between.hyperSpec <- function (x, left, right){
  between (x$spc, left, right)
}

#' @param x hyperSpec object
#' @return logical *matrix* indicating which elements of the hyperSpec object's spectra matrix are between `left` and `right`.
#' @family between
#' @rdname between
#' @export
#'
#' @examples
#' between (flu [[]], 100, 400)
setMethod("between", signature = "hyperSpec", .between.hyperSpec)


.test (.between.hyperSpec) <- function (){
  context ("between")

  test_that("correct hyperSpec behaviour",{
    expect_equal (between (flu, 100, 400),
                  flu >= 100 & flu <= 400)
  })
}
