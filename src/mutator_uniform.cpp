#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]                                        

// [[Rcpp::export]]
List cpp_mutator_uniform_fun(StringVector parent, double mu){

  List mutation_stats;
  mutation_stats = List::create(Named("n_mut") = 0);

  return List::create(Named("parent") = parent,
                      Named("child") = parent,
                      Named("mutation_stats") = mutation_stats);
}



//#' The uniform mutator
//#'
//#' Mutates a sequence giving each base an equal chance to experience a random mutation
//#'
//#' @param patent A character string containing the DNA sequence of the parent
//#' @param mu A single number between 0 and 1 specifying the per base mutation rate
//#' @export
//
//mutator_uniform_fun <- function(parent, mu){
//
//  if (length(parent) != 1){
//    stop("The length of the parent argument to a mutator must be 1")
//  }
//
//  parent <- strsplit(as.character(parent), '')[[1]]
//
//  mut_spots <- which(runif(length(parent)) < mu)
//  child <- parent
//
//  if (length(mut_spots > 0)){
//    for (mut_spot in mut_spots){
//      child[mut_spot] <- sample(setdiff(c('A', 'C', 'G', 'T'), child[mut_spot]), 1)
//    }
//  }
//
//  parent <- paste(parent, sep = '', collapse = '')
//  child <- paste(child, sep = '', collapse = '')
//
//  result <- list(parent = parent,
//                 child = child,
//                 mutation_stats = list(n_mut = length(mut_spots)),
//                 mu = mu)
//  return(result)
//}
