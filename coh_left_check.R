
#' Helper Function: Check which cohorts are left
#' @export
coh_left_check <- function(x) {
  cohorts_left_TRD <- sapply(x$TRD, function(y) {y$decision[1] %in% c("none", "PROMISING", "CONTINUE") & y$decision[2] == "none"})
  cohorts_left_PRD <- sapply(x$PRD, function(y) {y$decision[1] %in% c("none", "PROMISING", "CONTINUE") & y$decision[2] == "none"})

  return(cbind(as.data.frame(cohorts_left_TRD), as.data.frame(cohorts_left_PRD)))
}

#coh_left_check(x=res_list)

