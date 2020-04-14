#include <Rcpp.h>
using namespace Rcpp;

// It's quicker to redefine our own choose function than to import the Internal one.
// [[Rcpp::export]]
int C_Choose(int x, int y) {
  if (y == 0 | y == x) {
    return 1;
  } else if (y < 0 | y > x) {
    return 0;
  } else {
    int res = x;
    for(int i = 2; i <= y; i++){
      res *= (x-i+1);
      res /= i;
    }
    return res;
  }
}

// [[Rcpp::export]]
NumericMatrix C_NegativeBinomialPdf(NumericVector x, NumericVector size, NumericVector prob, const char* form) {
  int ParamLength = std::max({
    size.length(),
    prob.length()
  });
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (strcmp (form, "fbs") == 0) {
        // Return 0 if x not in Naturals
        if (floor (x[j]) != x[j]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] + size[i] - 1, size[i] - 1) * pow(prob[i], size[i]) * pow(1-prob[i], x[j]);
        }
      } else if (strcmp (form, "sbf") == 0) {
        // Return 0 if x not in Naturals
        if (floor (x[j]) != x[j]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] + size[i] - 1, x[j]) * pow(prob[i], x[j]) * pow(1-prob[i], size[i]);
        }
      } else if (strcmp (form, "tbf") == 0) {
        // Return 0 if x not in Naturals or < size
        if (floor (x[j]) != x[j] | x[j] < size[i]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] - 1, size[i] - 1) * pow(prob[i], x[j] - size[i]) * pow(1-prob[i], size[i]);
        }
      } else {
        // Return 0 if x not in Naturals or < size
        if (floor (x[j]) != x[j] | x[j] < size[i]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] - 1, size[i] - 1) * pow(prob[i], size[i]) * pow(1 - prob[i], x[j] - size[i]);
        }
      }
    }
  }
  return mat;
}




