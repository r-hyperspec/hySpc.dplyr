#' Run Unit Tests
#'
#' If [testthat::testthat()] is available, run the unit tests and
#' display the results.
#'
#' @rdname unittests
#' @return Invisibly returns a data.frame with the test results
#'
#' @author Claudia Beleites
#'
#' @keywords programming utilities
#' @importFrom  testthat SummaryReporter
#' @importFrom  testthat ListReporter
#' @importFrom  testthat MultiReporter
#' @importFrom  testthat with_reporter
#' @importFrom  testthat get_reporter
#' @import hySpc.testthat
#' @export
# FIXME: why is this example needed at all?
#' @examples
#' # hySpc.dplyr.unittest()
#'
hySpc.dplyr.unittest <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    warning("Package testthat required to run the unit tests.")
    return(NA)
  }
  if (!"package:testthat" %in% search()) {
    attachNamespace("testthat")
  }

  tests <-
    eapply(env = getNamespace("hySpc.dplyr"), FUN = get.test, all.names = TRUE)
  tests <- tests[!sapply(tests, is.null)]

  reporter <- SummaryReporter$new()
  lister <- ListReporter$new()
  reporter <- MultiReporter$new(reporters = list(reporter, lister))

  with_reporter(reporter = reporter, start_end_reporter = TRUE, {
    for (t in seq_along(tests)) {
      lister$start_file(names(tests[t]))
      tests[[t]]()
    }
    get_reporter()$.end_context()
  })

  invisible(lister$get_results())
}

##' @noRd
{
  `.test<-` <- function(f, value) {
    attr(f, "test") <- value
    f
  }
}

##' get test that is attached to object as "test" attribute
##' @noRd
get.test <- function(object) {
  attr(object, "test")
}

# internal test data set
{
  .testdata <- flu[, , min ~ 410]
  .testdata[[6, ]] <- NA
  .testdata[[3:4, , 406 ~ 407]] <- NA
  .testdata$region <- factor(c("a", "a", "b", "b", "a", "a"))
}
