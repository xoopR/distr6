#include <Rcpp.h>
using namespace Rcpp;
#define _USE_MATH_DEFINES
#include <cmath>

// [[Rcpp::export]]
NumericVector C_CosineKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(M_PI) - log(4) + log(cos(M_PI / 2 * x[i]));
      } else {
        ret[i] = M_PI / 4 * cos(M_PI / 2 * x[i]);
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
      ret[i] = 0.5 * (sin((M_PI * x[i]) / 2) + 1);
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

  for (int i = 0; i < x.size(); i++) {

      if (logp) {
        x[i] = exp(x[i]);
      }

      if (!lower) {
        x[i] = 1 - x[i];
      }

      if (x[i] < 0 || x[i] > 1) {
        ret[i] = R_NaN;
      } else if (x[i] == 0) {
        ret[i] = -1;
      } else if (x[i] == 1) {
        ret[i] = 1;
      } else {
        ret[i] = (2 * asin(2 * x[i] - 1)) / M_PI;
      }
  }

  return ret;
}

// [[Rcpp::export]]
NumericVector C_EpanechnikovKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(3) - log(4) + log(1 - pow(x[i], 2));
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
      ret[i] = 3 / 4 * x[i] - 1 / 4 * pow(x[i], 3) + 1 / 2;
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

  for (int i = 0; i < x.size(); i++) {

    if (logp) {
      x[i] = exp(x[i]);
    }

    if (!lower) {
      x[i] = 1 - x[i];
    }

    if (x[i] < 0 || x[i] > 1) {
      ret[i] = R_NaN;
    } else if (x[i] == 0) {
      ret[i] = R_NegInf;
    } else if (x[i] == 1) {
      ret[i] = R_PosInf;
    } else {
      ret[i] = log(x[i] / (1 - x[i]));
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericVector C_NormalKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (logp) {
      ret[i] = -(log(2*M_PI) + (pow(x[i], 2)))/2;
    } else {
      ret[i] = 1 / sqrt(2 * M_PI) * exp(-0.5 * pow(x[i], 2));
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_QuarticKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(15) - log(16) + 2*log(1 - pow(x[i], 2));
      } else {
        ret[i] = 15 / 16 * pow((1 - pow(x[i], 2)), 2);
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
      ret[i] = 15 / 16 * (x[i] - 2 / 3 * pow(x[i], 3) + 1 / 5 * pow(x[i], 5) + 8 / 15);
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
      ret[i] = -log(2) - abs(x[i])/sqrt(2) + log(sin(abs(x[i]) / sqrt(2) + M_PI / 4));
    } else {
      ret[i] = 0.5 * exp(-abs(x[i]) / sqrt(2)) * sin(abs(x[i]) / sqrt(2) + M_PI / 4);
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_TriangularKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(1 - abs(x[i]));
      } else {
        ret[i] = 1 - abs(x[i]);
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
      ret[i] = x[i] + 0.5 * pow(x[i], 2) + 1 / 2;
    } else if (x[i] == 0) {
      ret[i] = 0.5;
    } else if (x[i] > 0) {
      ret[i] = x[i] - 0.5 * pow(x[i], 2) + 1 / 2;
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

  for (int i = 0; i < x.size(); i++) {

    if (logp) {
      x[i] = exp(x[i]);
    }

    if (!lower) {
      x[i] = 1 - x[i];
    }

    if (x[i] < 0 || x[i] > 1) {
      ret[i] = R_NaN;
    } else if (x[i] == 0) {
      ret[i] = R_NegInf;
    } else if (x[i] == 1) {
      ret[i] = R_PosInf;
    } else if (x[i] < 0.5) {
      ret[i] = -1 + sqrt(2 * x[i]);
    } else if (x[i] == 0.5) {
      ret[i] = 0;
    } else {
      ret[i] = 1 - sqrt(2 - 2 * x[i]);
    }
  }

  return ret;
}

// [[Rcpp::export]]
NumericVector C_TricubeKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(70) - log(81) + 3*log(1 - pow(abs(x[i]), 3));
      } else {
        ret[i] = 70 / 81 * pow((1 - pow(abs(x[i]), 3)), 3);
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
NumericVector C_TriweightKernelPdf(NumericVector x, bool logp) {
  NumericVector ret(x.size());
  for (int i = 0; i < x.size(); i++){
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = log(35) - log(32) + 3*log(1 - pow(x[i], 2));
      } else {
        ret[i] = 35 / 32 * pow((1 - pow(x[i], 2)), 3);
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
      ret[i] = 35 / 32 * (x[i] - pow(x[i], 3) + 3 / 5 * pow(x[i], 5) - 1 / 7 * pow(x[i], 7) + 16 / 35);
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
    if (x[i] >= -1 & x[i] <= 1) {
      if (logp) {
        ret[i] = -log(2);
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

  for (int i = 0; i < x.size(); i++) {

    if (logp) {
      x[i] = exp(x[i]);
    }

    if (!lower) {
      x[i] = 1 - x[i];
    }

    if (x[i] < 0 || x[i] > 1) {
      ret[i] = R_NaN;
    } else if (x[i] == 0) {
      ret[i] = R_NegInf;
    } else if (x[i] == 1) {
      ret[i] = R_PosInf;
    } else {
      ret[i] = 2 * (x[i] - 0.5);
    }
  }

  return ret;
}
