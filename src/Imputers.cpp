#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector NumericCdf_Discrete(NumericVector q, NumericVector x, NumericVector pdf,
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
NumericVector NumericCdf_Continuous(NumericVector q, NumericVector x, NumericVector pdf,
                         bool lower, bool logp) {

  NumericVector ret(q.size());

  for (int i = 0; i < q.size(); i++) {

    if (q[i] < x[0]) {
      ret[i] = 0;
    } else if (q[i] >= x[x.size() - 1]) {
      ret[i] = 1;
    } else if (q[i] == x[0]) {
      ret[i] = pdf[0];
    } else {
      for (int j = 0; j < x.size(); j++) {
        if (x[j] <= q[i]) {
          if (j == 0 || x[j + 1] > q[i]) {
            ret[i] += pdf[j];
          } else if (j % 2 == 0) {
            ret[i] += 2 * pdf[j];
          } else {
            ret[i] += 4 * pdf[j];
          }
        } else {
          ret[i] *= ((x[j - 1] - x[0])/(j-1)) / 3;
          break;
        }
      }

      // while (x[j] <= q[i]) {
      //   if (strategy.compare("simpson") == 0) {
      //     ret[i] += (pdf[j] + 4.0*pdf[j - 1] + pdf[j - 2]) * (x[j] - x[0])/(x.size() * 3);
      //     j++;
      //     j++;
      //   } else {
      //     ret[i] += (pdf[j] + pdf[j - 1]) * (x[j] - x[0])/(x.size() * 2);
      //     j++;
      //   }
        // if (j == 0) {
        //   ret[i] += pdf[j];
        // } else {

        // }


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
NumericVector NumericQuantile(NumericVector p, NumericVector x, NumericVector cdf,
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
