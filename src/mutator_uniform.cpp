#include <iostream>
#include <random>
#include <string>
#include <iterator>
#include <experimental/algorithm>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::plugins(cpp14)]]                                        

// [[Rcpp::export]]
List cpp_mutator_uniform_fun(StringVector parent, double mu){

  std::string child;

  child = Rcpp::as<std::string>(parent[0]);

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<> dis(0.0, 1.0);

  std::vector<int> mut_pos;
  for (int i = 0; i < parent[0].size(); ++i) {
    if (dis(gen) < mu){
      mut_pos.push_back(i);
    }
  }
  std::vector<std::string> lets = {"A", "C", "G", "T"};

  std::vector<int> mut_draws;
  std::uniform_int_distribution<> dis_unif(0, 3);

  for (int i = 0; i < mut_pos.size(); i++){
    mut_draws.push_back(dis_unif(gen));
    child.replace(mut_pos[i], 1, lets[mut_draws[i]]);
  }

  List mutation_stats;
  mutation_stats = List::create(Named("n_mut") = mut_pos,
                                Named("mut_draws") = mut_draws);

  return List::create(Named("parent") = parent[0],
                      Named("child") = child,
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
