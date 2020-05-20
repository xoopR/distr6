#include <Rcpp.h>
using namespace Rcpp;
#define _USE_MATH_DEFINES
#include <cmath>

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
NumericMatrix C_ArcsinePdf(NumericVector x, NumericVector min, NumericVector max, bool logp) {
  int ll = min.length();
  int ul = max.length();
  int ParamLength = std::max({ll, ul});

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      if (x[j] < min[i % ll] || x[j] > max[i % ul]) {
        if (logp) {
          mat(j, i) = R_NegInf;
        } else {
          mat(j, i) = 0;
        }
      } else if (x[j] == min[i % ll] || x[j] == max[i % ul]) {
        mat(j, i) = R_PosInf;
      } else {
        if (!logp) {
          mat(j, i) = 1/(M_PI * sqrt((x[j] - min[i % ll]) * (max[i % ul] - x[j])));
        } else {
          mat(j, i) = -log(M_PI) - (log(x[j] - min[i % ll]) / 2) - (log(max[i % ul] - x[j]) / 2);
        }
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_ArcsineCdf(NumericVector x, NumericVector min, NumericVector max,
                           bool lower, bool logp) {
  int ll = min.length();
  int ul = max.length();
  int ParamLength = std::max({ll, ul});

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] < min[i % ll]) {
        mat(j, i) = 0;
      } else if (x[j] >= max[i % ul]) {
        mat(j, i) = 1;
      } else {
        mat(j, i) = (2 / M_PI) * (asin(sqrt((x[j] - min[i % ll]) / (max[i % ul] - min[i % ll]))));
      }

      if (!lower) {
        mat(j, i) = 1 - mat(j, i);
      }

      if (logp) {
        mat(j, i) = log(mat(j, i));
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_ArcsineQuantile(NumericVector x, NumericVector min, NumericVector max,
                           bool lower, bool logp) {
  int ll = min.length();
  int ul = max.length();
  int ParamLength = std::max({ll, ul});

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      if (logp) {
        x[j] = exp(x[j]);
      }

      if (!lower) {
        x[j] = 1 - x[j];
      }

      if (x[j] < 0 || x[j] > 1) {
        mat(j, i) = R_NaN;
      } else if (x[j] == 0) {
        mat(j, i) = min[i % ll];
      } else if (x[j] == 1) {
        mat(j, i) = max[i % ul];
      } else {
        mat(j, i) = ((max[i % ul] - min[i % ll]) * pow(sin(x[j] * M_PI * 0.5), 2)) + min[i % ll];
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_DegeneratePdf(NumericVector x, NumericVector mean, bool logp) {
  int ParamLength = mean.length();

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (logp) {
        mat(j, i) = log(x[j] == mean[i]);
      } else {
        mat(j, i) = x[j] == mean[i];
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_DegenerateCdf(NumericVector x, NumericVector mean, bool lower, bool logp) {
  int ParamLength = mean.length();

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      mat(j, i) = x[j] >= mean[i];

      if (!lower) {
        mat(j, i) = 1 - mat(j, i);
      }

      if (logp) {
        mat(j, i) = log(mat(j, i));
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_DegenerateQuantile(NumericVector x, NumericVector mean, bool lower, bool logp) {
  int ParamLength = mean.length();

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      if (logp) {
        x[j] = exp(x[j]);
      }

      if (!lower) {
        x[j] = 1 - x[j];
      }

      if (x[j] < 0 || x[j] > 1) {
        mat(j, i) = R_NaN;
      } else if (x[j] == 0) {
        mat(j, i) = R_NegInf;
      } else {
        mat(j, i) = mean[i];
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_NegativeBinomialPdf(NumericVector x, NumericVector size, NumericVector prob,
                                    StringVector form) {
  int sl = size.length();
  int pl = prob.length();
  int fl = form.length();

  int ParamLength = std::max({sl, pl, fl});
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (strcmp (form[i % fl], "fbs") == 0) {
        // Return 0 if x not in Naturals
        if (floor (x[j]) != x[j]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] + size[i % sl] - 1, size[i % sl] - 1) * pow(prob[i % pl], size[i % sl]) * pow(1-prob[i % pl], x[j]);
        }
      } else if (strcmp (form[i % fl], "sbf") == 0) {
        // Return 0 if x not in Naturals
        if (floor (x[j]) != x[j]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] + size[i % sl] - 1, x[j]) * pow(prob[i % pl], x[j]) * pow(1-prob[i % pl], size[i % sl]);
        }
      } else if (strcmp (form[i % fl], "tbf") == 0) {
        // Return 0 if x not in Naturals or < size
        if (floor (x[j]) != x[j] | x[j] < size[i % sl]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] - 1, size[i % sl] - 1) * pow(prob[i % pl], x[j] - size[i % sl]) * pow(1-prob[i % pl], size[i % sl]);
        }
      } else {
        // Return 0 if x not in Naturals or < size
        if (floor (x[j]) != x[j] | x[j] < size[i % sl]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = C_Choose(x[j] - 1, size[i % sl] - 1) * pow(prob[i % pl], size[i % sl]) * pow(1 - prob[i % pl], x[j] - size[i % sl]);
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_ShiftedLoglogisticPdf(NumericVector x, NumericVector location,
                                      NumericVector shape, NumericVector scale, bool logp) {
  int locn = location.length();
  int shan = shape.length();
  int scan = scale.length();
  int ParamLength = std::max({locn, shan, scan});

  float z;

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if ((shape[i % shan] > 0 && (x[j] < location[i & locn] - scale[i & scan]/shape[i % shan])) ||
          (shape[i % shan] < 0 && (x[j] > location[i & locn] - scale[i & scan]/shape[i % shan]))) {

        if (logp) {
          mat(j, i) = R_NegInf;
        } else {
          mat(j, i) = 0;
        }

      } else {
        z = (x[j] - location[i & locn])/scale[i & scan];

        if (logp) {
          mat(j, i) = (-(1/shape[i % shan] + 1)*log(1 + shape[i % shan]*z)) - log(scale[i & scan]) -
            2*log(1 + pow(1 + shape[i % shan]*z, -1/shape[i % shan]));
        } else {
          mat(j, i) = pow(1 + shape[i % shan]*z, -(1/shape[i % shan] + 1)) * pow(scale[i & scan], -1) *
            pow(1 + pow(1 + shape[i % shan]*z, -1/shape[i % shan]), -2);
        }
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_ShiftedLoglogisticCdf(NumericVector x, NumericVector location,
                                      NumericVector shape, NumericVector scale,
                                      bool lower, bool logp) {
  int locn = location.length();
  int shan = shape.length();
  int scan = scale.length();
  int ParamLength = std::max({locn, shan, scan});

  float z;

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if ((shape[i % shan] > 0 && (x[j] < location[i & locn] - scale[i & scan]/shape[i % shan])) ||
          (shape[i % shan] < 0 && (x[j] > location[i & locn] - scale[i & scan]/shape[i % shan]))) {

        if (logp) {
          if (!lower) {
            mat(j, i) = R_PosInf;
          } else {
            mat(j, i) = R_NegInf;
          }
        } else {
          if (!lower) {
            mat(j, i) = 0;
          } else {
            mat(j, i) = 1;
          }
        }

      } else {
        z = (x[j] - location[i & locn])/scale[i & scan];
        mat(j, i) = pow(1 + pow(1 + shape[i % shan]*z, -1/shape[i % shan]), -1);

        if (!lower) {
          mat(j, i) = 1 - mat(j, i);
        }

        if (logp) {
          mat(j, i) = log(mat(j, i));
        }
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_ShiftedLoglogisticQuantile(NumericVector x, NumericVector location,
                                      NumericVector shape, NumericVector scale,
                                      bool lower, bool logp) {
  int locn = location.length();
  int shan = shape.length();
  int scan = scale.length();
  int ParamLength = std::max({locn, shan, scan});

  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      if (logp) {
        x[j] = exp(x[j]);
      }

      if (!lower) {
        x[j] = 1 - x[j];
      }

      if (x[j] < 0 || x[j] > 1) {
        mat(j, i) = R_NaN;
      } else {
        mat(j, i) = ((pow(x[j]/(1-x[j]), shape[i % shan]) - 1) * scale[i & scan]/shape[i % shan]) +
          location[i & locn];
      }
    }
  }

  return mat;
}

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
