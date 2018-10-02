#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp14)]]                                        

// [[Rcpp::export]]

NumericVector cpp_fibo(double s1, double s2, int n) {

  std::vector<double> x = {s1, s2};

  for (int i = 2; i < n; i++){
    x.push_back(pow(x[i-2], 0.25) + x[i-1]);
  }
  return wrap(x);
}



