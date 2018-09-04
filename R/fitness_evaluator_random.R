fitness_evaluator_random <- function(the_seq){
  return(list(fitness = runif(1)))
}

fitness_evaluator <- list(fun = "fitness_evaluator_random",
                          args = NULL)
