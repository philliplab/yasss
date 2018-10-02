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

std::vector<double> cpp_fibo_no_wrap(double s1, double s2, int n) {

  std::vector<double> x = {s1, s2};

  for (int i = 2; i < n; i++){
    x.push_back(pow(x[i-2], 0.25) + x[i-1]);
  }
  return x;
}


// [[Rcpp::export]]
List cpp_fibo_df(int n, int n_columns) {
  std::vector<std::vector<double>> x;
//  std::vector<double> y;


  double s1 = 1;
  double s2 = 1;
  for (int i = 0; i < n_columns; i++){
//    y = cpp_fibo_no_wrap(s1, s2, n);
//    y.push_back(1);
//    y.push_back(1);
//    y.push_back(1);
    x.push_back(cpp_fibo_no_wrap(s1, s2, n));
  }
  return wrap(x);
}


