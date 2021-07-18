
#' Helper Function: Total Sample Size
#' @export
total_n <- function(x) {
  list("TRD"=sum(unlist(sapply(x$TRD, function(y) nrow(y$data)))),
       "PRD"=sum(unlist(sapply(x$PRD, function(y) nrow(y$data)))))
}

