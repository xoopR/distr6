#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_vec_PdfCdf(NumericVector x) {
  NumericVector out(x.size());
  out[0] = x[0];
  for (int i = 1; i < out.size(); i++) {
    out[i] = x[i] + out[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
NumericVector C_vec_CdfPdf(NumericVector x) {
  NumericVector out(x.size());
  out[0] = x[0];
  for (int i = x.size(); i > 0; i--) {
    out[i] = x[i] - x[i - 1];
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix C_mat_PdfCdf(NumericMatrix x) {

  int nc = x.ncol();
  int nr = x.nrow();

  NumericMatrix out(nr, nc);

  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      if (j == 0) {
        out(i, j) = x(i, j);
      } else {
        out(i, j) = x(i, j) + out(i, j - 1);
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix C_mat_CdfPdf(NumericMatrix x) {

  int nr = x.nrow();
  int nc = x.ncol();

  NumericMatrix out(nr, nc);

  for (int i = 0; i < nr; i++) {
    for (int j = (nc - 1); j >= 0; j--) {
      if (j == 0) {
        out(i, j) = x(i, j);
      } else {
        out(i, j) = x(i, j) - x(i, j - 1);
      }
    }
  }
  return out;
}
