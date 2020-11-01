#include <Rcpp.h>
using namespace Rcpp;
#define _USE_MATH_DEFINES
#include <cmath>

// [[Rcpp::export]]
NumericVector C_CosineKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(M_PI) - log(4.0) + log(cos(M_PI / 2.0 * x[i]));
      } else {
        ret[i] = M_PI / 4.0 * cos(M_PI / 2.0 * x[i]);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_CosineKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else {
      ret[i] = 0.5 * (sin((M_PI * x[i]) / 2.0) + 1);
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
NumericVector C_EpanechnikovKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(3.0) - log(4.0) + log(1 - pow(x[i], 2));
      } else {
        ret[i] = 0.75 * (1 - pow(x[i], 2));
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_EpanechnikovKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else {
      ret[i] = 3.0 / 4.0 * x[i] - 1.0 / 4.0 * pow(x[i], 3) + 1.0 / 2.0;
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
NumericVector C_LogisticKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (logp) {
      ret[i] = -log(exp(x[i]) + 2 + exp(-x[i]));
    } else {
      ret[i] = 1/(exp(x[i]) + 2 + exp(-x[i]));
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_LogisticKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    ret[i] = exp(x[i]) / (exp(x[i]) + 1);

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
NumericVector C_NormalKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (logp) {
      ret[i] = -(log(2*M_PI) + (pow(x[i], 2))) / 2.0;
    } else {
      ret[i] = 1.0 / sqrt(2 * M_PI) * exp(-0.5 * pow(x[i], 2));
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_QuarticKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(15.0) - log(16.0) + 2*log(1 - pow(x[i], 2));
      } else {
        ret[i] = 15.0 / 16.0 * pow((1 - pow(x[i], 2)), 2);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_QuarticKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else {
      ret[i] = 15.0 / 16.0 * (x[i] - 2.0 / 3.0 * pow(x[i], 3) + 1.0 / 5.0 * pow(x[i], 5) + 8.0 / 15.0);
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
NumericVector C_SigmoidKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (logp) {
      ret[i] = log(2/M_PI) - log(exp(x[i]) + exp(-x[i]));
    } else {
      ret[i] = (2 / M_PI) / (exp(x[i]) + exp(-x[i]));
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_SilvermanKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (logp) {
      ret[i] = -log(2.0) - fabs(x[i])/sqrt(2.0) + log(sin(fabs(x[i]) / sqrt(2.0) + M_PI / 4));
    } else {
      ret[i] = 0.5 * exp(-fabs(x[i]) / sqrt(2.0)) * sin(fabs(x[i]) / sqrt(2.0) + M_PI / 4);
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_SilvermanKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());

  for (int i = 0; i < x.size(); i++){
    if (x[i] <= 0) {
      ret[i] = 0.5 * exp(x[i]/sqrt(2.0)) * cos(x[i]/sqrt(2.0));
    } else {
      ret[i] = 1 - (0.5 * exp(-x[i]/sqrt(2.0)) * cos(x[i]/sqrt(2.0)));
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
NumericVector C_TriangularKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(1 - fabs(x[i]));
      } else {
        ret[i] = 1 - fabs(x[i]);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_TriangularKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else if (x[i] < 0) {
      ret[i] = x[i] + 0.5 * pow(x[i], 2) + 1.0 / 2.0;
    } else if (x[i] == 0) {
      ret[i] = 0.5;
    } else if (x[i] > 0) {
      ret[i] = x[i] - 0.5 * pow(x[i], 2) + 1.0 / 2.0;
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
NumericVector C_TricubeKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(70.0) - log(81.0) + 3*log(1 - pow(fabs(x[i]), 3));
      } else {
        ret[i] = 70.0 / 81.0 * pow((1 - pow(fabs(x[i]), 3)), 3);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_TricubeKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else if ((x[i] > -1) & (x[i] <= 0)) {
      ret[i] = (81 + 140 * x[i] + 150 * pow(x[i], 4) + 60 * pow(x[i], 7) + 14 * pow(x[i], 10)) /
        162;
    } else if ((x[i] > 0) & (x[i] <= 1)) {
      ret[i] = (81 + 140 * x[i] - 105 * pow(x[i], 4) + 60 * pow(x[i], 7) - 14 * pow(x[i], 10)) /
        162;
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
NumericVector C_TriweightKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = log(35.0) - log(32.0) + 3*log(1 - pow(x[i], 2));
      } else {
        ret[i] = 35.0 / 32.0 * pow((1 - pow(x[i], 2)), 3);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_TriweightKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else {
      ret[i] = 35.0 / 32.0 * (x[i] - pow(x[i], 3) + 3.0 / 5.0 * pow(x[i], 5) - 1 / 7.0 * pow(x[i], 7) + 16.0 / 35.0);
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
NumericVector C_UniformKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 && x[i] <= 1) {
      if (logp) {
        ret[i] = -log(2.0);
      } else {
        ret[i] = 0.5;
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_UniformKernelCdf(NumericVector x, bool lower, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= 1) {
      ret[i] = 1;
    } else if (x[i] <= -1) {
      ret[i] = 0;
    } else {
      ret[i] = (0.5 * x[i]) + 0.5;
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
