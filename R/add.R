#' @export
add_indicator <- function(...){
  dots <- substitute(list(...))[-1]
  .indicators_fun <<- modifyList(.indicators_fun,
                             structure(list(...), names = sapply(dots, deparse)))
}

#' @export
add_asset <- function(...){
  .assets <<- modifyList(.assets,
                         structure(list(...), names = sapply(list(...), names)))
}


