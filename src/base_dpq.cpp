#include <Rcpp.h>
using namespace Rcpp;

NumericVector C_std_d1(std::string fun, NumericVector x, double a1, int log) {
  NumericVector ret(x.size());

  if (fun == "dchisq") {
    ret = dchisq(x, a1, log);
  } else if (fun == "dexp") {
    ret = dexp(x, a1, log);
  } else if (fun == "dgeom") {
    ret = dgeom(x, a1, log);
  } else if (fun == "dpois") {
    ret = dpois(x, a1, log);
  } else if (fun == "dt") {
    ret = dt(x, a1, log);
  }

  return ret;
}
NumericVector C_std_p1(std::string fun, NumericVector x, double a1, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "pchisq") {
    ret = pchisq(x, a1, lower, log);
  } else if (fun == "pexp") {
    ret = pexp(x, a1, lower, log);
  } else if (fun == "pgeom") {
    ret = pgeom(x, a1, lower, log);
  } else if (fun == "ppois") {
    ret = ppois(x, a1, lower, log);
  } else if (fun == "pt") {
    ret = pt(x, a1, lower, log);
  }

  return ret;
}
NumericVector C_std_q1(std::string fun, NumericVector x, double a1, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "qchisq") {
    ret = qchisq(x, a1, lower, log);
  } else if (fun == "qexp") {
    ret = qexp(x, a1, lower, log);
  } else if (fun == "qgeom") {
    ret = qgeom(x, a1, lower, log);
  } else if (fun == "qpois") {
    ret = qpois(x, a1, lower, log);
  } else if (fun == "qt") {
    ret = qt(x, a1, lower, log);
  }

  return ret;
}
NumericVector C_std_r1(std::string fun, int x, double a1) {
  NumericVector ret(x);

  if (fun == "rchisq") {
    ret = rchisq(x, a1);
  } else if (fun == "rexp") {
    ret = rexp(x, a1);
  } else if (fun == "rgeom") {
    ret = rgeom(x, a1);
  } else if (fun == "rpois") {
    ret = rpois(x, a1);
  } else if (fun == "rt") {
    ret = rt(x, a1);
  }

  return ret;
}

NumericMatrix C_vec_d1(std::string fun, NumericVector x, NumericVector a1, int log) {
  int n = a1.size();
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_d1(fun, x, a1[i], log);
  }

  return ret;
}
NumericMatrix C_vec_pq1(std::string fun, NumericVector x, NumericVector a1, int lower, int log) {
  int n = a1.size();
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    if (fun.substr(0, 1).compare("p") == 0) {
      ret(_,i) = C_std_p1(fun, x, a1[i], lower, log);
    } else if (fun.substr(0, 1).compare("q") == 0) {
      ret(_,i) = C_std_q1(fun, x, a1[i], lower, log);
    }
  }

  return ret;
}
NumericMatrix C_vec_r1(std::string fun, int x, NumericVector a1) {
  int n = a1.size();
  NumericMatrix ret(x, n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_r1(fun, x, a1[i]);
  }

  return ret;
}

NumericVector C_std_d2(std::string fun, NumericVector x, double a1, double a2, int log) {
  NumericVector ret(x.size());

  if (fun == "dbinom") {
    ret = dbinom(x, a1, a2, log);
  } else if (fun == "dbeta") {
    ret = dbeta(x, a1, a2, log);
  } else if (fun == "dcauchy") {
    ret = dcauchy(x, a1, a2, log);
  } else if (fun == "dchisq") {
    ret = dnchisq(x, a1, a2, log);
  } else if (fun == "df") {
    ret = df(x, a1, a2, log);
  } else if (fun == "dgamma") {
    ret = dgamma(x, a1, a2, log);
  } else if (fun == "dlogis") {
    ret = dlogis(x, a1, a2, log);
  } else if (fun == "dlnorm") {
    ret = dlnorm(x, a1, a2, log);
  } else if (fun == "dnbinom") {
    ret = dnbinom(x, a1, a2, log);
  } else if (fun == "dnorm") {
    ret = dnorm(x, a1, a2, log);
  } else if (fun == "dt") {
    ret = dnt(x, a1, a2, log);
  } else if (fun == "dunif") {
    ret = dunif(x, a1, a2, log);
  } else if (fun == "dweibull") {
    ret = dweibull(x, a1, a2, log);
  }

  return ret;
}
NumericVector C_std_p2(std::string fun, NumericVector x, double a1, double a2, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "pbinom") {
    ret = pbinom(x, a1, a2, lower, log);
  } else if (fun == "pbeta") {
    ret = pbeta(x, a1, a2, lower, log);
  } else if (fun == "pcauchy") {
    ret = pcauchy(x, a1, a2, lower, log);
  } else if (fun == "pchisq") {
    ret = pnchisq(x, a1, a2, lower, log);
  } else if (fun == "pf") {
    ret = pf(x, a1, a2, lower, log);
  } else if (fun == "pgamma") {
    ret = pgamma(x, a1, a2, lower, log);
  } else if (fun == "plogis") {
    ret = plogis(x, a1, a2, lower, log);
  } else if (fun == "plnorm") {
    ret = plnorm(x, a1, a2, lower, log);
  } else if (fun == "pnbinom") {
    ret = pnbinom(x, a1, a2, lower, log);
  } else if (fun == "pnorm") {
    ret = pnorm(x, a1, a2, lower, log);
  } else if (fun == "pt") {
    ret = pnt(x, a1, a2, lower, log);
  } else if (fun == "punif") {
    ret = punif(x, a1, a2, lower, log);
  } else if (fun == "pweibull") {
    ret = pweibull(x, a1, a2, lower, log);
  }

  return ret;
}
NumericVector C_std_q2(std::string fun, NumericVector x, double a1, double a2, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "qbinom") {
    ret = qbinom(x, a1, a2, lower, log);
  } else if (fun == "qbeta") {
    ret = qbeta(x, a1, a2, lower, log);
  } else if (fun == "qcauchy") {
    ret = qcauchy(x, a1, a2, lower, log);
  } else if (fun == "qchisq") {
    ret = qnchisq(x, a1, a2, lower, log);
  } else if (fun == "qf") {
    ret = qf(x, a1, a2, lower, log);
  } else if (fun == "qgamma") {
    ret = qgamma(x, a1, a2, lower, log);
  } else if (fun == "qlogis") {
    ret = qlogis(x, a1, a2, lower, log);
  } else if (fun == "qlnorm") {
    ret = qlnorm(x, a1, a2, lower, log);
  } else if (fun == "qnbinom") {
    ret = qnbinom(x, a1, a2, lower, log);
  } else if (fun == "qnorm") {
    ret = qnorm(x, a1, a2, lower, log);
  } else if (fun == "qt") {
    ret = qnt(x, a1, a2, lower, log);
  } else if (fun == "qunif") {
    ret = qunif(x, a1, a2, lower, log);
  } else if (fun == "qweibull") {
    ret = qweibull(x, a1, a2, lower, log);
  }

  return ret;
}
NumericVector C_std_r2(std::string fun, int x, double a1, double a2) {
  NumericVector ret(x);

  if (fun == "rbinom") {
    ret = rbinom(x, a1, a2);
  } else if (fun == "rbeta") {
    ret = rbeta(x, a1, a2);
  } else if (fun == "rcauchy") {
    ret = rcauchy(x, a1, a2);
  } else if (fun == "rchisq") {
    ret = rnchisq(x, a1, a2);
  } else if (fun == "rf") {
    ret = rf(x, a1, a2);
  } else if (fun == "rgamma") {
    ret = rgamma(x, a1, a2);
  } else if (fun == "rlogis") {
    ret = rlogis(x, a1, a2);
  } else if (fun == "rlnorm") {
    ret = rlnorm(x, a1, a2);
  } else if (fun == "rnbinom") {
    ret = rnbinom(x, a1, a2);
  } else if (fun == "rnorm") {
    ret = rnorm(x, a1, a2);
  } else if (fun == "rt") {
    ret = rnorm(x, a2)/sqrt(rchisq(x, a1)/a1);
  } else if (fun == "runif") {
    ret = runif(x, a1, a2);
  } else if (fun == "rweibull") {
    ret = rweibull(x, a1, a2);
  }

  return ret;
}

NumericMatrix C_vec_d2(std::string fun, NumericVector x, NumericVector a1, NumericVector a2, int log) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n = std::max(n1, n2);
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_d2(fun, x, a1[i % n1], a2[i % n2], log);
  }

  return ret;
}
NumericMatrix C_vec_pq2(std::string fun, NumericVector x, NumericVector a1, NumericVector a2, int lower, int log) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n = std::max(n1, n2);
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    if (fun.substr(0, 1).compare("p") == 0) {
      ret(_,i) = C_std_p2(fun, x, a1[i % n1], a2[i % n2], lower, log);
    } else if (fun.substr(0, 1).compare("q") == 0) {
      ret(_,i) = C_std_q2(fun, x, a1[i % n1], a2[i % n2], lower, log);
    }
  }

  return ret;
}
NumericMatrix C_vec_r2(std::string fun, int x, NumericVector a1, NumericVector a2) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n = std::max(n1, n2);
  NumericMatrix ret(x, n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_r2(fun, x, a1[i % n1], a2[i % n2]);
  }

  return ret;
}

NumericVector C_std_d3(std::string fun, NumericVector x, double a1, double a2, double a3, int log) {
  NumericVector ret(x.size());

  if (fun == "dbeta") {
    ret = dnbeta(x, a1, a2, a3, log);
  } else if (fun == "df") {
    ret = dnf(x, a1, a2, a3, log);
  } else if (fun == "dhyper") {
    ret = dhyper(x, a1, a2, a3, log);
  }

  return ret;
}
NumericVector C_std_p3(std::string fun, NumericVector x, double a1, double a2, double a3, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "pbeta") {
    ret = pnbeta(x, a1, a2, a3, lower, log);
  } else if (fun == "pf") {
    ret = pnf(x, a1, a2, a3, lower, log);
  } else if (fun == "phyper") {
    ret = phyper(x, a1, a2, a3, lower, log);
  }

  return ret;
}
NumericVector C_std_q3(std::string fun, NumericVector x, double a1, double a2, double a3, int lower, int log) {
  NumericVector ret(x.size());

  if (fun == "qbeta") {
    ret = qnbeta(x, a1, a2, a3, lower, log);
  } else if (fun == "qf") {
    ret = qnf(x, a1, a2, a3, lower, log);
  } else if (fun == "qhyper") {
    ret = qhyper(x, a1, a2, a3, lower, log);
  }

  return ret;
}
NumericVector C_std_r3(std::string fun, int x, double a1, double a2, double a3) {
  NumericVector ret(x);

  if (fun == "rbeta") {
    ret = rnchisq(x, 2 * a1, a3);
    ret = ret / (ret + rchisq(x, 2 * a2));
  } else if (fun == "rf") {
    ret = (rnchisq(x, a1, a3)/a1)/(rchisq(x, a2)/a2);
  } else if (fun == "rhyper") {
    ret = rhyper(x, a1, a2, a3);
  }

  return ret;
}

NumericMatrix C_vec_d3(std::string fun, NumericVector x, NumericVector a1, NumericVector a2, NumericVector a3, int log) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n3 = a3.size();
  int n = std::max({n1, n2, n3});
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_d3(fun, x, a1[i % n1], a2[i % n2], a3[i % n3], log);
  }

  return ret;
}
NumericMatrix C_vec_pq3(std::string fun, NumericVector x, NumericVector a1, NumericVector a2, NumericVector a3, int lower, int log) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n3 = a3.size();
  int n = std::max({n1, n2, n3});
  NumericMatrix ret(x.size(), n);

  for(int i = 0; i < n; i++){
    if (fun.substr(0, 1).compare("p") == 0) {
      ret(_,i) = C_std_p3(fun, x, a1[i % n1], a2[i % n2], a3[i % n3], lower, log);
    } else if (fun.substr(0, 1).compare("q") == 0) {
      ret(_,i) = C_std_q3(fun, x, a1[i % n1], a2[i % n2], a3[i % n3], lower, log);
    }
  }

  return ret;
}
NumericMatrix C_vec_r3(std::string fun, int x, NumericVector a1, NumericVector a2, NumericVector a3) {
  int n1 = a1.size();
  int n2 = a2.size();
  int n3 = a3.size();
  int n = std::max({n1, n2, n3});
  NumericMatrix ret(x, n);

  for(int i = 0; i < n; i++){
    ret(_,i) = C_std_r3(fun, x, a1[i % n1], a2[i % n2], a3[i % n3]);
  }

  return ret;
}

// [[Rcpp::export]]
NumericMatrix C_dpq(std::string fun, NumericVector x, std::list<NumericVector> args, int lower = true, int log = false) {
  int n = args.size();
  NumericVector arg1;
  NumericVector arg2;
  NumericVector arg3;
  std::string funtype = fun.substr(0, 1);
  int funcode;
  if (funtype.compare("d") == 0) {
    funcode = 0;
  } else if (funtype.compare("p") == 0 || funtype.compare("q") == 0) {
    funcode = 1;
  } else {
    stop("Function must start with `d`, `p` or `q`");
  }

  if (n == 1) {
    arg1 = args.front();
    switch(funcode) {
    case 0:
      return C_vec_d1(fun, x, arg1, log);
    case 1:
      return C_vec_pq1(fun, x, arg1, lower, log);
    }
  } else if (n == 2) {
    arg1 = args.front();
    args.pop_front();
    arg2 = args.front();
    switch(funcode) {
    case 0:
      return C_vec_d2(fun, x, arg1, arg2, log);
    case 1:
      return C_vec_pq2(fun, x, arg1, arg2, lower, log);
    }
  } else {
    arg1 = args.front();
    args.pop_front();
    arg2 = args.front();
    args.pop_front();
    arg3 = args.front();
    switch(funcode) {
    case 0:
      return C_vec_d3(fun, x, arg1, arg2, arg3, log);
    case 1:
      return C_vec_pq3(fun, x, arg1, arg2, arg3, lower, log);
    }
  }

  return NumericMatrix(0,0); // never reached # nocov
}
// [[Rcpp::export]]
NumericMatrix C_r(std::string fun, int x, std::list<NumericVector> args) {
  int n = args.size();
  NumericVector arg1;
  NumericVector arg2;
  NumericVector arg3;

  if (n == 1) {
    arg1 = args.front();
    return C_vec_r1(fun, x, arg1);
  } else if (n == 2) {
    arg1 = args.front();
    args.pop_front();
    arg2 = args.front();
    return C_vec_r2(fun, x, arg1, arg2);
  } else {
    arg1 = args.front();
    args.pop_front();
    arg2 = args.front();
    args.pop_front();
    arg3 = args.front();
    return C_vec_r3(fun, x, arg1, arg2, arg3);
  }

  return NumericMatrix(0, 0); // never reached
}
