#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' Calculate square root of a matrix from Armadillo
 //' @export
 // [[Rcpp::export]]
 arma::cx_mat sqrtm_cpp (arma::mat x) {
   arma::cx_mat x_sqrt;
   x_sqrt = arma::sqrtmat(x);
   return(x_sqrt);
}