assign_fitness <- function(genealogy, fitness_evaluator){
  for (i in 1:nrow(genealogy)){
    args <- fitness_evaluator$args
    args$the_seq <- genealogy[i,'the_seq']
    fit_fun <- get(fitness_evaluator$fun)
    x <- do.call(fit_fun, args)
    genealogy[i,'fitness'] <- x$fitness
  }
  return(genealogy)
}

