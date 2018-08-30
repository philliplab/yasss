#' Simulate the next generation
#'
#' Simulates the next generation by calling a mutator on each parent
#'
#' @return Currently, a list with a single element which is a character vector with each element being the sequence of a single offspring. In future, this should be expanded to include much more.
#'
#' @param parents A character vector with each element being a parent sequence
#' @param gen_size The size of each generation. Currently only allowed to be a
#' single integer. Default value is 2.
#' @param mutator A list with two elements fun and args specifying the function that mutates parents into their offspring and the list of arguments said function requires. 
#' @export

sim_next_gen <- function(parents, gen_size, mutator){
  offspring <- character(length(parents)*gen_size)
  total_offspring <- 0
  result <- list()
  for (i in 1:length(parents)){
    for (j in 1:gen_size){
      total_offspring <- total_offspring + 1
      child <- parents[i]
      offspring[total_offspring] <- child
    }
  }
  result$offspring <- offspring
  return(result)
}

