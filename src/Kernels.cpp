#include <Rcpp.h>
using namespace Rcpp;
#define _USE_MATH_DEFINES
#include <cmath>

// [[Rcpp::export]]
NumericMatrix C_CosineKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i) = log(M_PI) - log(4.0) - log(bw[i]) + log(cos(M_PI / 2.0 * x[j] / bw[i]));
        } else {
          mat(j, i) = 1 / bw[i] * M_PI / 4.0 * cos(M_PI / 2.0 * x[j] / bw[i]);
          }
        } else {
        mat(j, i) = 0;
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_CosineKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i)  = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else {
        mat(j, i)  = 0.5 * (sin((M_PI * x[j] / bw[i]) / 2.0) + 1);
      }

      if (!lower) {
        mat(j, i)  = 1 - mat(j, i);
      }
      if (logp) {
        mat(j, i)  = log(mat(j, i));
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericVector C_CosineKernelQuantile(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  double y;

  for (int i = 0; i < x.size(); i++) {

      y = x[i];

      if (logp) {
        y = exp(y);
      }

      if (!lower) {
        y = 1 - y;
      }

      if (y < 0 || y > 1) {
        ret[i] = R_NaN;
      } else if (y == 0) {
        ret[i] = -1;
      } else if (y == 1) {
        ret[i] = 1;
      } else {
        ret[i] = (2 * asin(2 * y - 1)) / M_PI;
      }
  }

  return ret;
}

// [[Rcpp::export]]
NumericMatrix C_EpanechnikovKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i) = log(3.0) - log(4.0) - log(bw[i]) + log(1 - pow(x[j] / bw[i], 2));
        } else {
          mat(j, i) = 0.75 * 1 / bw[i] * (1 - pow(x[j] / bw[i], 2));
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_EpanechnikovKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else {
        mat(j, i) = 3.0 / 4.0 * x[j] / bw[i] - 1.0 / 4.0 * pow(x[j] / bw[i], 3) + 1.0 / 2.0;
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
NumericMatrix C_LogisticKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (logp) {
        mat(j, i) = -log(exp(x[j] / bw[i]) + 2 + exp(- x[j] / bw[i])) - log(bw[i]);
      } else {
        mat(j, i) = 1/(exp(x[j] / bw[i]) + 2 + exp(-(x[j] / bw[i]))) * 1 / bw[i];
      }
    }
  }
    return mat;
}

// [[Rcpp::export]]
NumericMatrix C_LogisticKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      mat(j, i) = exp(x[j] / bw[i]) / (exp(x[j] / bw[i]) + 1);

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
NumericVector C_LogisticKernelQuantile(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  double y;

  for (int i = 0; i < x.size(); i++) {

    y = x[i];

    if (logp) {
      y = exp(y);
    }

    if (!lower) {
      y = 1 - y;
    }

    if (y < 0 || y > 1) {
      ret[i] = R_NaN;
    } else if (y == 0) {
      ret[i] = R_NegInf;
    } else if (y == 1) {
      ret[i] = R_PosInf;
    } else {
      ret[i] = log(y / (1 - y));
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericMatrix C_NormalKernelPdf(NumericVector x, NumericVector bw, bool logp) {

  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (logp) {
        mat(j, i) = -(log(2*M_PI) + log(bw[i]) + (pow(x[j] / bw[i], 2))) / 2.0;
      } else {
        mat(j, i) = 1.0 / (sqrt(2 * M_PI) * bw[i]) * exp(-0.5 * pow((x[j] / bw[i]), 2));
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_QuarticKernelPdf(NumericVector x, NumericVector bw, bool logp) {

  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i) = log(15.0) - log(16.0)  - log(bw[i]) + 2*log(1 - pow(x[j] / bw[i], 2));
        } else {
          mat(j, i) = 15.0 / 16.0 * pow((1 - pow(x[j] / bw[i], 2)), 2) * 1 / bw[i];
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_QuarticKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i]<= -1) {
        mat(j, i) = 0;
      } else {
        mat(j, i) = 15.0 / 16.0 * (x[j] / bw[i] - 2.0 / 3.0 * pow(x[j] / bw[i], 3) + 1.0 / 5.0 * pow(x[j] / bw[i], 5) + 8.0 / 15.0);
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
NumericMatrix C_SigmoidKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (logp) {
        mat(j, i) = log(2/M_PI) - log(bw[i]) - log(exp(x[j] / bw[i]) + exp(- x[j] / bw[i]));
      } else {
        mat(j, i) = (2 / M_PI) / (exp(x[j] / bw[i]) + exp(- x[j] / bw[i])) * 1 / bw[i];
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_SilvermanKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (logp) {
        mat(j, i) = -log(2.0) - fabs(x[j] / bw[i])/sqrt(2.0) + log(sin(fabs(x[j] / bw[i]) / sqrt(2.0) + M_PI / 4)) - log(bw[i]);
      } else {
        mat(j, i) = 0.5 * exp(-fabs(x[j] / bw[i]) / sqrt(2.0)) * sin(fabs(x[j] / bw[i]) / sqrt(2.0) + M_PI / 4) * 1 / bw[i];
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_SilvermanKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[i] <= 0) {
        mat(j, i) = 0.5 * exp((x[j] / bw[i]) /sqrt(2.0)) * cos((x[j] / bw[i]) / sqrt(2.0));
      } else {
        mat(j, i) = 1 - (0.5 * exp(-(x[j] / bw[i])/ sqrt(2.0)) * cos((x[j] / bw[i]) / sqrt(2.0)));
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
NumericMatrix C_TriangularKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i)= log(1 - fabs(x[j] / bw[i])) - log(bw[i]);
        } else {
          mat(j, i) = (1 - fabs(x[j] / bw[i])) / bw[i];
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_TriangularKernelCdf(NumericVector x, NumericVector bw,  bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else if (x[j] / bw[i] < 0) {
        mat(j, i) = x[j] / bw[i] + 0.5 * pow(x[j] / bw[i], 2) + 1.0 / 2.0;
      } else if (x[j] / bw[i] == 0) {
        mat(j, i) = 0.5;
      } else if (x[j] / bw[i] > 0) {
        mat(j, i) = x[j] / bw[i] - 0.5 * pow(x[j] / bw[i], 2) + 1.0 / 2.0;
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
NumericVector C_TriangularKernelQuantile(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  double y;

  for (int i = 0; i < x.size(); i++) {

    y = x[i];

    if (logp) {
      y = exp(y);
    }

    if (!lower) {
      y = 1 - y;
    }

    if (y < 0 || y > 1) {
      ret[i] = R_NaN;
    } else if (y == 0) {
      ret[i] = R_NegInf;
    } else if (y == 1) {
      ret[i] = R_PosInf;
    } else if (y < 0.5) {
      ret[i] = -1 + sqrt(2 * y);
    } else if (y == 0.5) {
      ret[i] = 0;
    } else {
      ret[i] = 1 - sqrt(2 - 2 * y);
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericMatrix C_TricubeKernelPdf(NumericVector x, NumericVector bw,  bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i) = log(70.0) - log(81.0) - log(bw[i]) + 3*log(1 - pow(fabs(x[j] / bw[i]), 3));
        } else {
          mat(j, i) = 70.0 / 81.0 * pow((1 - pow(fabs(x[j] / bw[i]), 3)), 3) * 1 / bw[i];
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_TricubeKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else if ((x[j] / bw[i] > -1) & (x[j] / bw[i] <= 0)) {
        mat(j, i) = (81 + 140 * x[j] / bw[i] + 150 * pow(x[j] / bw[i], 4) + 60 * pow(x[j] / bw[i], 7) + 14 * pow(x[j] / bw[i], 10)) /
          162;
      } else if ((x[j] / bw[i] > 0) & (x[j] / bw[i] <= 1)) {
        mat(j, i) = (81 + 140 * x[j] / bw[i] - 105 * pow(x[j] / bw[i], 4) + 60 * pow(x[j] / bw[i], 7) - 14 * pow(x[j] / bw[i], 10)) /
          162;
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
NumericMatrix C_TriweightKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i]<= 1) {
        if (logp) {
          mat(j, i) = log(35.0) - log(32.0) - log(bw[i] )+ 3*log(1 - pow(x[j] / bw[i], 2));
        } else {
          mat(j, i) = 35.0 / 32.0 * pow((1 - pow(x[j] / bw[i], 2)), 3) * 1 / bw[i];
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_TriweightKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else {
        mat(j, i) = 35.0 / 32.0 * (x[j] / bw[i] - pow(x[j] / bw[i], 3) + 3.0 / 5.0 * pow(x[j] / bw[i], 5) -
          1 / 7.0 * pow(x[j] / bw[i], 7) + 16.0 / 35.0);
      }

      if (!lower) {
        mat(j, i) = 1 - mat(j, i) ;
      }
      if (logp) {
        mat(j, i)  = log(mat(j, i) );
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_UniformKernelPdf(NumericVector x, NumericVector bw, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= -1 && x[j] / bw[i] <= 1) {
        if (logp) {
          mat(j, i)  = -log(2.0) - log(bw[i]);
        } else {
          mat(j, i)  = 0.5 * 1 / bw[i];
        }
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericMatrix C_UniformKernelCdf(NumericVector x, NumericVector bw, bool lower, bool logp) {
  int ParamLength = bw.length();
  int XLength = x.size();
  NumericMatrix mat(XLength, ParamLength);
  for (int i = 0; i < ParamLength; i++) {
    for (int j = 0; j < XLength; j++) {
      if (x[j] / bw[i] >= 1) {
        mat(j, i) = 1;
      } else if (x[j] / bw[i] <= -1) {
        mat(j, i) = 0;
      } else {
        mat(j, i) = (0.5 * x[j] / bw[i]) + 0.5;
      }

      if (!lower) {
        mat(j, i)  = 1 - mat(j, i) ;
      }
      if (logp) {
        mat(j, i)  = log(mat(j, i) );
      }
    }
  }
  return mat;
}

// [[Rcpp::export]]
NumericVector C_UniformKernelQuantile(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  double y;

  for (int i = 0; i < x.size(); i++) {

    y = x[i];

    if (logp) {
      y = exp(y);
    }

    if (!lower) {
      y = 1 - y;
    }

    if (y < 0 || y > 1) {
      ret[i] = R_NaN;
    } else if (y == 0) {
      ret[i] = R_NegInf;
    } else if (y == 1) {
      ret[i] = R_PosInf;
    } else {
      ret[i] = 2 * (y - 0.5);
    }
  }

  return ret;
}
