#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_WeightedDiscretePdf(NumericVector x, NumericVector data, NumericVector pdf,
                                    bool logp) {

  int nr = data.length();
  int n = x.length();

  NumericVector mat(n);

  for (int k = 0; k < n; k++) {
    for (int j = 0; j < nr; j++) {
      if (data[j] == x[k]) {
        if (logp) {
          mat[k] = log(pdf[j]);
        } else {
          mat[k] = pdf[j];
        }
        break;
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_Vec_WeightedDiscretePdf(NumericVector x, NumericMatrix data, NumericMatrix pdf,
                                        bool logp) {

  int nc = data.ncol();
  int nr = data.nrow();
  int n = x.length();

  NumericMatrix mat(n, nc);

  // i - distribution
  // j - data samples
  // k - evaluates

  for (int i = 0; i < nc; i++) {
    for (int k = 0; k < n; k++) {
      for (int j = 0; j < nr; j++) {
        if (data(j, i) == x[k]) {
          if (logp) {
            mat(k, i) = log(pdf(j, i));
          } else {
            mat(k, i) = pdf(j, i);
          }
          break;
        }
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericVector C_WeightedDiscreteCdf(NumericVector x, NumericVector data, NumericVector cdf,
                                    bool lower, bool logp) {

  int nr = data.length();
  int n = x.length();

  NumericVector mat(n);

  for (int k = 0; k < n; k++) {
    for (int j = 0; j < nr; j++) {
      if (data[j] >= x[k]) {
        mat[k] = cdf[j];
        if (!lower) {
          mat[k] = 1 - mat[k];
        }
        if (logp) {
          mat[k] = log(mat[k]);
        }
        break;
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_Vec_WeightedDiscreteCdf(NumericVector x, NumericMatrix data, NumericMatrix cdf,
                                        bool lower, bool logp) {

  int nc = data.ncol();
  int nr = data.nrow();
  int n = x.length();

  NumericMatrix mat(n, nc);

  for (int i = 0; i < nc; i++) {
    for (int k = 0; k < n; k++) {
      for (int j = 0; j < nr; j++) {
        if (data(j, i) >= x[k]) {
          mat(k, i) = cdf(j, i);
          if (!lower) {
            mat(k, i) = 1 - mat[k];
          }
          if (logp) {
            mat(k, i) = log(mat[k]);
          }
          break;
        }
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericVector C_WeightedDiscreteQuantile(NumericVector x, NumericVector data, NumericVector cdf,
                                         bool lower, bool logp) {

  int nr = data.length();
  int n = x.length();

  NumericVector mat(n);

  for (int k = 0; k < n; k++) {
    for (int j = 0; j < nr; j++) {

      if (logp) {
        x[k] = exp(x[k]);
      }

      if (!lower) {
        x[k] = 1 - x[k];
      }

      if (cdf[j] >= x[k]) {
        mat[k] = data[j];
        break;
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_Vec_WeightedDiscreteQuantile(NumericVector x, NumericMatrix data, NumericMatrix cdf,
                                             bool lower, bool logp) {

  int nc = data.ncol();
  int nr = data.nrow();
  int n = x.length();

  NumericMatrix mat(n, nc);

  for (int i = 0; i < nc; i++) {
    for (int k = 0; k < n; k++) {
      for (int j = 0; j < nr; j++) {
        if (logp) {
          x[k] = exp(x[k]);
        }
        if (!lower) {
          x[k] = 1 - x[k];
        }

        if (x[k] <= cdf(j, i)) {
          mat(k, i) = data(j, i);
          break;
        }
      }
    }
  }

  return mat;
}
