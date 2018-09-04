assign_fitness <- function(genealogy, darwin){
  for (i in 1:nrow(genealogy)){
    args <- darwin$args
    args$the_seq <- genealogy[i,'the_seq']
    x <- do.call(darwin$fun, args)
    genealogy[i,'fitness'] <- x$fitness
  }
  return(genealogy)
}

darwin_random <- function(the_seq){
  return(runif(1))
}

darwin <- list(fun = "darwin_random",
               args = NULL)
