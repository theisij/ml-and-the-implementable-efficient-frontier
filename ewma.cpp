#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ewma_c(NumericVector x, double lambda, int start) {
  int n = x.size();
  int na = 0;
  double initial_value = 0;
  NumericVector var_vec(n, NA_REAL);
  
  if(n <= start) {
    return var_vec;
  }
  
  // Calculate simple variance to get started (assuming mean = 0)
  for(int i = 0; i < start; ++i) {
    if(NumericVector::is_na(x(i))) {
      na += 1;  
    } else {
      initial_value += pow(x(i), 2);
    }
  }
  initial_value = initial_value / (start - 1 - na); 
  
  // Iteratively Updating EWMA Vol 
  var_vec(start) = initial_value;  
  for(int j = start + 1; j < n; ++j) {
    if(NumericVector::is_na(x(j - 1))) {
      var_vec(j) = var_vec(j - 1);  
    } else {
      var_vec(j) = lambda * var_vec(j - 1) + (1 - lambda) * pow(x(j - 1), 2);  
    }
  }
  
  return sqrt(var_vec);
}


