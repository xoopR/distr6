#include <Rcpp.h>
using namespace Rcpp;
#define _USE_MATH_DEFINES
#include <cmath>

// It's quicker to redefine our own choose function than to import the Internal one.
// [[Rcpp::export]]
long double C_Choose(int x, int y) {
  if (y == 0 || y == x) {
    return 1;
  } else if (y < 0 || y > x) {
    return 0;
  } else {
    long double res = x;
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

  double y;

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      y = x[j];

      if (logp) {
        y = exp(y);
      }

      if (!lower) {
        y = 1 - y;
      }

      if (y < 0 || y > 1) {
        mat(j, i) = R_NaN;
      } else if (y == 0) {
        mat(j, i) = min[i % ll];
      } else if (y == 1) {
        mat(j, i) = max[i % ul];
      } else {
        mat(j, i) = ((max[i % ul] - min[i % ll]) * pow(sin(y * M_PI * 0.5), 2)) + min[i % ll];
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
        if (x[j] == mean[i]) {
          mat(j, i) = 0;
        } else {
          mat(j, i) = R_NegInf;
        }
      } else {
        if (x[j] == mean[i]) {
          mat(j, i) = 1;
        } else {
          mat(j, i) = 0;
        }
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

  double y;

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {

      y = x[j];

      if (logp) {
        y = exp(y);
      }

      if (!lower) {
        y = 1 - y;
      }

      if (y < 0 || y > 1) {
        mat(j, i) = R_NaN;
      } else if (y == 0) {
        mat(j, i) = R_NegInf;
      } else {
        mat(j, i) = mean[i];
      }
    }
  }

  return mat;
}

// [[Rcpp::export]]
NumericVector C_EmpiricalMVPdf(NumericMatrix x, NumericMatrix data) {

  float n = data.nrow();
  float vbj;
  int vk;
  NumericVector ret(x.nrow());

  // loop over evaluates
  for (int i = 0; i < x.nrow(); i++) {
    // loop over all observations
    for (int k = 0; k < n; k++) {
      vbj = 0;
      vk = 1;

      // loop over variates
      for (int j = 0; j < x.ncol(); j++) {
        vk = vk && (data(k, j) ==  x(i, j));
      }

      vbj += vk;

      ret[i] += (vbj/n);
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericVector C_EmpiricalMVCdf(NumericMatrix x, NumericMatrix data) {

  float n = data.nrow();
  float vbj;
  int vk;
  NumericVector ret(x.nrow());

  for (int i = 0; i < x.nrow(); i++) {
    for (int k = 0; k < n; k++) {
      vbj = 0;
      vk = 1;

      for (int j = 0; j < x.ncol(); j++) {
        vk = vk && (data(k, j) <=  x(i, j));
      }

      vbj += vk;

      ret[i] += (vbj/n);
    }
  }

  return ret;
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
      if ((shape[i % shan] > 0 && (x[j] < (location[i & locn] - scale[i & scan]/shape[i % shan]))) ||
          (shape[i % shan] < 0 && (x[j] > (location[i & locn] - scale[i & scan]/shape[i % shan])))) {

        if (logp) {
          mat(j, i) = R_NegInf;
        }

      } else {
        z = (x[j] - location[i & locn])/scale[i & scan];

        if (logp) {
          mat(j, i) = (-(1/shape[i % shan] + 1)*log(1 + shape[i % shan]*z)) - log(scale[i & scan]) -
            2*log(1 + pow(1 + shape[i % shan]*z, -1.0/shape[i % shan]));
        } else {
          mat(j, i) = pow(1 + shape[i % shan]*z, -(1.0/shape[i % shan] + 1)) * pow(scale[i & scan], -1) *
            pow(1 + pow(1 + shape[i % shan]*z, -1.0/shape[i % shan]), -2);
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
            mat(j, i) = 0;
          } else {
            mat(j, i) = R_NegInf;
          }
        } else {
          if (!lower) {
            mat(j, i) = 1;
          } else {
            mat(j, i) = 0;
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

  double y;

  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      y = x[j];

      if (logp) {
        y = exp(y);
      }

      if (!lower) {
        y = 1 - y;
      }

      if (y < 0 || y > 1) {
        mat(j, i) = R_NaN;
      } else {
        mat(j, i) = ((pow(y/(1 - y), shape[i % shan]) - 1) *
          scale[i & scan]/shape[i % shan]) + location[i & locn];
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
        if (data[j] > x[k] && j == 0) {
          mat[k] = 0;
        } else {
          mat[k] = cdf[j];
        }

        if (!lower) {
          mat[k] = 1 - mat[k];
        }
        if (logp) {
          mat[k] = log(mat[k]);
        }
        break;
      } else if (j == nr - 1) {
        mat[k] = 1;
        if (!lower) {
          mat[k] = 1 - mat[k];
        }
        if (logp) {
          mat[k] = log(mat[k]);
        }
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
            mat(k, i) = 1 - mat(k, i);
          }
          if (logp) {
            mat(k, i) = log(mat(k, i));
          }
          break;
        } else if (j == nr - 1) {
          mat(k, i) = 1;
          if (!lower) {
            mat(k, i) = 1 - mat(k, i);
          }
          if (logp) {
            mat(k, i) = log(mat(k, i));
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
  double y;

  NumericVector mat(n);

  for (int k = 0; k < n; k++) {
    for (int j = 0; j < nr; j++) {
      y = x[k];

      if (logp) {
        y = exp(y);
      }

      if (!lower) {
        y = 1 - y;
      }

      if (cdf[j] >= y) {
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

  double y;

  for (int i = 0; i < nc; i++) {
    for (int k = 0; k < n; k++) {
      for (int j = 0; j < nr; j++) {

        y = x[k];

        if (logp) {
          y = exp(y);
        }

        if (!lower) {
          y = 1 - y;
        }

        if (y <= cdf(j, i)) {
          mat(k, i) = data(j, i);
          break;
        }
      }
    }
  }

  return mat;
}
