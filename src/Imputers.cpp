#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_NumericCdf_Discrete(NumericVector q, NumericVector x, NumericVector pdf,
                                    bool lower, bool logp) {

  NumericVector ret(q.size());
  int j;

  for (int i = 0; i < q.size(); i++) {

    if (q[i] < x[0]) {
      ret[i] = 0;
    } else if (q[i] >= x[x.size() - 1]) {
      ret[i] = 1;
    } else if (q[i] == x[0]) {
      ret[i] = pdf[0];
    } else {
      j = 0;
      while (x[j] <= q[i]) {
        ret[i] += pdf[j];
        j++;
      }
    }

    if (!lower) {
      ret[i] = 1 - ret[i];
    }

    if (logp) {
      ret[i] = log(ret[i]);
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericVector C_NumericQuantile(NumericVector p, NumericVector x, NumericVector cdf,
                              bool lower, bool logp) {

  NumericVector ret(p.length());

  for (int i = 0; i < p.length(); i++) {

    if (logp) {
      p[i] = exp(p[i]);
    }

    if (!lower) {
      p[i] = 1 - p[i];
    }

    if (p[i] < 0 || p[i] > 1) {
      stop("All values of 'p' should be in [0, 1].");
    } else if (p[i] == 0) {
      ret[i] = x[0];
    } else if (p[i] == 1) {
      ret[i] = x[x.length() - 1];
    } else {
      for (int j = 0; j < cdf.length(); j++) {
        if (p[i] <= cdf[j]) {
          ret[i] = x[j];
          break;
        }
      }
    }
  }

  return ret;
}
