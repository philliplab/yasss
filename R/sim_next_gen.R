#' Simulate the next generation
#'
#' Simulates the next generation by calling a mutator on each parent
#'
#' @param parents A character vector with each element being a parent sequence
#' @param gen_size The size of each generation. Currently only allowed to be a single integer. Default value is 2.
#' @export

sim_next_gen <- function(parents, gen_size){
  return (rep(parents, gen_size))
}
