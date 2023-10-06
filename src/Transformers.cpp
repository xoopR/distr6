#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_vec_PdfCdf(NumericVector x) {
  NumericVector out(x.size());
  out = clone(x);
  for (int i = 1; i < out.size(); i++) {
    out[i] = out[i] + out[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
NumericVector C_vec_CdfPdf(NumericVector x) {
  NumericVector out(x.size());
  out = clone(x);
  for (int i = x.size(); i > 0; i--) {
    out[i] = out[i] - out[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix C_mat_PdfCdf(NumericMatrix x) {

  int nc = x.ncol();
  int nr = x.nrow();

  NumericMatrix out(nr, nc);
  out = clone(x);

  for (int i = 0; i < nr; i++) {
    for (int j = 1; j < nc; j++) {
      out(i, j) = out(i, j) + out(i, j - 1);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix C_mat_CdfPdf(NumericMatrix x) {

  int nr = x.nrow();
  int nc = x.ncol();

  NumericMatrix out(nr, nc);
  out = clone(x);

  for (int i = 0; i < nr; i++) {
    for (int j = nc; j > 0; j--) {
      out(i, j) = out(i, j) - out(i, j - 1);
    }
  }
  return out;
}
