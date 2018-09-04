fitness_evaluator_uniform <- function(the_seq){
  return(list(fitness = runif(1)))
}

fitness_evaluator <- list(fun = "fitness_evaluator_uniform",
                          args = NULL)
