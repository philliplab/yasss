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
  if (length(fun) != 1){
    stop("fun must be character or a function of length 1")
  }
  if (!(class(fun) %in% c('character', 'function'))){
    stop("fun must be character or a function of length 1")
  }
  if ((class(args) != 'list')){
    stop("args must be a list")
  }
  if (class(fun) == 'function'){
    fun <- as.character(substitute(fun))
  }
  if (!is.null(cacheNamePrefix)){
    if ((length(cacheNamePrefix) != 1)){
      stop("cacheNamePrefix must be a character of length 1 or NULL")
    }
    if ((class(cacheNamePrefix) != 'character')){
      stop("cacheNamePrefix must be a character of length 1 or NULL")
    }
  }
  if (!is.null(seed)){
    if ((length(seed) != 1)){
      stop("seed must be a positive integer or NULL")
    }
    if (!(class(seed) %in% c("numeric", "integer"))){
      stop("seed must be a positive integer or NULL")
    }
    if (ceiling(seed) != floor(seed)){
      stop("seed must be a positive integer or NULL")
    }
    if (seed <= 0){
      stop("seed must be a positive integer or NULL")
    }
  }

  cacheName <- paste(
    cacheNamePrefix,
    fun,
    digest(args, 'sha512'),
    seed,
    sep = '_')

  x <- simpleCache(cacheName, do.call(fun, args), reload = TRUE)

  return(x)
}
