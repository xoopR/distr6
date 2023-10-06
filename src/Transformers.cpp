#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_vec_PdfCdf(NumericVector pdf) {
  NumericVector cdf(pdf.size());
  cdf = clone(pdf);
  for (int i = 0; i < cdf.size(); i++) {
    cdf[i] = cdf[i] + cdf[i - 1];
  }
  return cdf;
}

// [[Rcpp::export]]
NumericVector C_vec_CdfPdf(NumericVector cdf) {
  NumericVector pdf(cdf.size());
  pdf = clone(cdf);
  for (int i = cdf.size(); i > 0; i--) {
    pdf[i] = pdf[i] - pdf[i - 1];
  }
  return pdf;
}

// [[Rcpp::export]]
NumericMatrix C_mat_PdfCdf(NumericMatrix pdf) {

  int nc = pdf.ncol();
  int nr = pdf.nrow();

  NumericMatrix cdf(nr, nc);
  cdf = clone(pdf);

  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      cdf(i, j) = cdf(i, j) + cdf(i, j - 1);
    }
  }
  return cdf;
}

// [[Rcpp::export]]
NumericMatrix C_mat_CdfPdf(NumericMatrix cdf) {

  int nr = cdf.nrow();
  int nc = cdf.ncol();

  NumericMatrix pdf(nr, nc);
  pdf = clone(cdf);

  for (int i = 0; i < nr; i++) {
    for (int j = nc; j > 0; j--) {
      pdf(i, j) = pdf(i, j) - pdf(i, j - 1);
    }
  }
  return pdf;
}
