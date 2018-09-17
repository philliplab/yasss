#' Cache a function call with an associated seed value
#'
#' Uses simpleCache::simpleCache to implement a form of memoisation. It caches a function's result based on the arguments provided and will reload that value instead of recomputing the function if the arguments match. Additionally allows the user to specify a seed value so that different results based on the same arguments.
#'
#' @param fun The name of the function to execute as a character string
#' @param args A list of the arguments used by do.call to run fun.
#' @param cacheNamePrefix Optional prefix that gets added to the cacheName. Frequent usage is a diagnostic marker for OCD.
#' @param seed A positive integer that allows extra control over whether or not to compute or load the value from a cache
#' @export

memoiseCache <- function(fun, args, cacheNamePrefix = NULL, seed = NULL, ...){
  do.call(fun, args)
}

