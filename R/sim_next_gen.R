#' Simulate the next generation
#'
#' Simulates the next generation by calling a mutator on each parent
#'
#' @param parents A character vector with each element being a parent sequence
#' @param gen_size The size of each generation. Currently only allowed to be a single integer. Default value is 2.
#' @export

sim_next_gen <- function(parents, gen_size){
  offspring <- character(length(parents)*gen_size)
  total_offspring <- 0
  for (i in 1:length(parents)){
    for (j in 1:gen_size){
      total_offspring <- total_offspring + 1
      child <- parents[i]
      offspring[total_offspring] <- child
    }
  }
  return(offspring)
}
