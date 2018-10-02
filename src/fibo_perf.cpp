#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]                                        

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
NumericMatrix cpp_fibo_df(int n, int n_columns) {
  std::vector<double> z;
  NumericMatrix y(n, n_columns);

  double s1 = 1;
  double s2 = 1;
  for (int i = 0; i < n; i++){
    z = cpp_fibo_no_wrap(s1, s2, n_columns);

    for (int j = 0; j < n_columns; j++){
      y(i,j) = z[j];
    }
  }
  return y;
}

// [[Rcpp::export]]

NumericMatrix cpp_fibo_df_pass(int n, int n_columns, Function foo) {
  NumericVector z;
  NumericMatrix y(n, n_columns);

  double s1 = 1;
  double s2 = 1;
  for (int i = 0; i < n; i++){
    z = foo(s1, s2, n_columns);

    for (int j = 0; j < n_columns; j++){
      y(i,j) = z[j];
    }
  }
  return y;
}

