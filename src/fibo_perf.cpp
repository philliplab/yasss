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
List cpp_fibo_df(int n, int n_columns, bool rify = false) {
  std::vector<std::vector<double>> x;
  std::vector<double> z;
  NumericMatrix y(n, n_columns);

  double s1 = 1;
  double s2 = 1;
  for (int i = 0; i < n; i++){
    z = cpp_fibo_no_wrap(s1, s2, n_columns);
    x.push_back(z);
  }
  if (rify){
    for (int i = 0; i < n; i++){
      for (int j = 0; j < n_columns; j++){
        y(i,j) = x[i][j];
      }
    }
  }
  return wrap(x);
}


