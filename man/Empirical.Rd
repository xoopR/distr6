% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDistribution_Empirical.R
\name{Empirical}
\alias{Empirical}
\title{Empirical Distribution Class}
\value{
Returns an R6 object inheriting from class \link{SDistribution}.
}
\description{
Mathematical and statistical functions for the Empirical distribution, which
is commonly used in sampling such as MCMC.
}
\details{
The Empirical distribution   is defined by the pmf, \deqn{p(x) = \sum I(x = x_i) / k} for \eqn{x_i \epsilon R, i = 1,...,k}.

Sampling from this distribution is performed with the \link{sample} function with the elements given
as the support set and uniform probabilities. Sampling is performed with replacement, which is
consistent with other distributions but non-standard for Empirical distributions. Use
\link{simulateEmpiricalDistribution} to sample without replacement.

The cdf and quantile assumes that the
elements are supplied in an indexed order (otherwise the results are meaningless).
}
\section{Distribution support}{

The distribution is supported on \eqn{x_1,...,x_k}.
}

\section{Default Parameterisation}{

Emp(samples = 1)
}

\section{Omitted Methods}{


N/A
}

\section{Also known as}{


N/A
}

\examples{

## ------------------------------------------------
## Method `Empirical$new`
## ------------------------------------------------

Empirical$new(runif(1000))
}
\references{
McLaughlin, M. P. (2001). A compendium of common probability distributions (pp. 2014-01).
Michael P. McLaughlin.
}
\seealso{
Other discrete distributions: 
\code{\link{Arrdist}},
\code{\link{Bernoulli}},
\code{\link{Binomial}},
\code{\link{Categorical}},
\code{\link{Degenerate}},
\code{\link{DiscreteUniform}},
\code{\link{EmpiricalMV}},
\code{\link{Geometric}},
\code{\link{Hypergeometric}},
\code{\link{Logarithmic}},
\code{\link{Matdist}},
\code{\link{Multinomial}},
\code{\link{NegativeBinomial}},
\code{\link{WeightedDiscrete}}

Other univariate distributions: 
\code{\link{Arcsine}},
\code{\link{Arrdist}},
\code{\link{Bernoulli}},
\code{\link{BetaNoncentral}},
\code{\link{Beta}},
\code{\link{Binomial}},
\code{\link{Categorical}},
\code{\link{Cauchy}},
\code{\link{ChiSquaredNoncentral}},
\code{\link{ChiSquared}},
\code{\link{Degenerate}},
\code{\link{DiscreteUniform}},
\code{\link{Erlang}},
\code{\link{Exponential}},
\code{\link{FDistributionNoncentral}},
\code{\link{FDistribution}},
\code{\link{Frechet}},
\code{\link{Gamma}},
\code{\link{Geometric}},
\code{\link{Gompertz}},
\code{\link{Gumbel}},
\code{\link{Hypergeometric}},
\code{\link{InverseGamma}},
\code{\link{Laplace}},
\code{\link{Logarithmic}},
\code{\link{Logistic}},
\code{\link{Loglogistic}},
\code{\link{Lognormal}},
\code{\link{Matdist}},
\code{\link{NegativeBinomial}},
\code{\link{Normal}},
\code{\link{Pareto}},
\code{\link{Poisson}},
\code{\link{Rayleigh}},
\code{\link{ShiftedLoglogistic}},
\code{\link{StudentTNoncentral}},
\code{\link{StudentT}},
\code{\link{Triangular}},
\code{\link{Uniform}},
\code{\link{Wald}},
\code{\link{Weibull}},
\code{\link{WeightedDiscrete}}
}
\concept{discrete distributions}
\concept{univariate distributions}
\section{Super classes}{
\code{\link[distr6:Distribution]{distr6::Distribution}} -> \code{\link[distr6:SDistribution]{distr6::SDistribution}} -> \code{Empirical}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{Full name of distribution.}

\item{\code{short_name}}{Short name of distribution for printing.}

\item{\code{description}}{Brief description of the distribution.}

\item{\code{alias}}{Alias of the distribution.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Empirical-new}{\code{Empirical$new()}}
\item \href{#method-Empirical-mean}{\code{Empirical$mean()}}
\item \href{#method-Empirical-mode}{\code{Empirical$mode()}}
\item \href{#method-Empirical-variance}{\code{Empirical$variance()}}
\item \href{#method-Empirical-skewness}{\code{Empirical$skewness()}}
\item \href{#method-Empirical-kurtosis}{\code{Empirical$kurtosis()}}
\item \href{#method-Empirical-entropy}{\code{Empirical$entropy()}}
\item \href{#method-Empirical-mgf}{\code{Empirical$mgf()}}
\item \href{#method-Empirical-cf}{\code{Empirical$cf()}}
\item \href{#method-Empirical-pgf}{\code{Empirical$pgf()}}
\item \href{#method-Empirical-setParameterValue}{\code{Empirical$setParameterValue()}}
\item \href{#method-Empirical-clone}{\code{Empirical$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="cdf"><a href='../../distr6/html/Distribution.html#method-Distribution-cdf'><code>distr6::Distribution$cdf()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="confidence"><a href='../../distr6/html/Distribution.html#method-Distribution-confidence'><code>distr6::Distribution$confidence()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="correlation"><a href='../../distr6/html/Distribution.html#method-Distribution-correlation'><code>distr6::Distribution$correlation()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="getParameterValue"><a href='../../distr6/html/Distribution.html#method-Distribution-getParameterValue'><code>distr6::Distribution$getParameterValue()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="iqr"><a href='../../distr6/html/Distribution.html#method-Distribution-iqr'><code>distr6::Distribution$iqr()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="liesInSupport"><a href='../../distr6/html/Distribution.html#method-Distribution-liesInSupport'><code>distr6::Distribution$liesInSupport()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="liesInType"><a href='../../distr6/html/Distribution.html#method-Distribution-liesInType'><code>distr6::Distribution$liesInType()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="median"><a href='../../distr6/html/Distribution.html#method-Distribution-median'><code>distr6::Distribution$median()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="parameters"><a href='../../distr6/html/Distribution.html#method-Distribution-parameters'><code>distr6::Distribution$parameters()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="pdf"><a href='../../distr6/html/Distribution.html#method-Distribution-pdf'><code>distr6::Distribution$pdf()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="prec"><a href='../../distr6/html/Distribution.html#method-Distribution-prec'><code>distr6::Distribution$prec()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="print"><a href='../../distr6/html/Distribution.html#method-Distribution-print'><code>distr6::Distribution$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="quantile"><a href='../../distr6/html/Distribution.html#method-Distribution-quantile'><code>distr6::Distribution$quantile()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="rand"><a href='../../distr6/html/Distribution.html#method-Distribution-rand'><code>distr6::Distribution$rand()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="stdev"><a href='../../distr6/html/Distribution.html#method-Distribution-stdev'><code>distr6::Distribution$stdev()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="strprint"><a href='../../distr6/html/Distribution.html#method-Distribution-strprint'><code>distr6::Distribution$strprint()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="summary"><a href='../../distr6/html/Distribution.html#method-Distribution-summary'><code>distr6::Distribution$summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="workingSupport"><a href='../../distr6/html/Distribution.html#method-Distribution-workingSupport'><code>distr6::Distribution$workingSupport()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-new"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$new(samples = NULL, decorators = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{samples}}{\code{(numeric())} \cr
Vector of observed samples, see examples.}

\item{\code{decorators}}{\code{(character())} \cr
Decorators to add to the distribution during construction.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{Empirical$new(runif(1000))
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-mean"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-mean}{}}}
\subsection{Method \code{mean()}}{
The arithmetic mean of a (discrete) probability distribution X is the expectation
\deqn{E_X(X) = \sum p_X(x)*x}
with an integration analogue for continuous distributions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$mean(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-mode"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-mode}{}}}
\subsection{Method \code{mode()}}{
The mode of a probability distribution is the point at which the pdf is
a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
maxima).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$mode(which = "all")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{which}}{\verb{(character(1) | numeric(1)}\cr
Ignored if distribution is unimodal. Otherwise \code{"all"} returns all modes, otherwise specifies
which mode to return.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-variance"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-variance}{}}}
\subsection{Method \code{variance()}}{
The variance of a distribution is defined by the formula
\deqn{var_X = E[X^2] - E[X]^2}
where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
covariance matrix is returned.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$variance(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-skewness"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-skewness}{}}}
\subsection{Method \code{skewness()}}{
The skewness of a distribution is defined by the third standardised moment,
\deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
distribution and \eqn{\sigma} is the standard deviation of the distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$skewness(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-kurtosis"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-kurtosis}{}}}
\subsection{Method \code{kurtosis()}}{
The kurtosis of a distribution is defined by the fourth standardised moment,
\deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
distribution and \eqn{\sigma} is the standard deviation of the distribution.
Excess Kurtosis is Kurtosis - 3.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$kurtosis(excess = TRUE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{excess}}{\code{(logical(1))} \cr
If \code{TRUE} (default) excess kurtosis returned.}

\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-entropy"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-entropy}{}}}
\subsection{Method \code{entropy()}}{
The entropy of a (discrete) distribution is defined by
\deqn{- \sum (f_X)log(f_X)}
where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
continuous distributions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$entropy(base = 2, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{base}}{\code{(integer(1))} \cr
Base of the entropy logarithm, default = 2 (Shannon entropy)}

\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-mgf"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-mgf}{}}}
\subsection{Method \code{mgf()}}{
The moment generating function is defined by
\deqn{mgf_X(t) = E_X[exp(xt)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$mgf(t, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{\code{(integer(1))} \cr
t integer to evaluate function at.}

\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-cf"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-cf}{}}}
\subsection{Method \code{cf()}}{
The characteristic function is defined by
\deqn{cf_X(t) = E_X[exp(xti)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$cf(t, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{\code{(integer(1))} \cr
t integer to evaluate function at.}

\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-pgf"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-pgf}{}}}
\subsection{Method \code{pgf()}}{
The probability generating function is defined by
\deqn{pgf_X(z) = E_X[exp(z^x)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$pgf(z, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{z}}{\code{(integer(1))} \cr
z integer to evaluate probability generating function at.}

\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-setParameterValue"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-setParameterValue}{}}}
\subsection{Method \code{setParameterValue()}}{
Sets the value(s) of the given parameter(s).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$setParameterValue(
  ...,
  lst = NULL,
  error = "warn",
  resolveConflicts = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{\code{ANY} \cr
Named arguments of parameters to set values for. See examples.}

\item{\code{lst}}{\code{(list(1))} \cr
Alternative argument for passing parameters. List names should be parameter names and list values
are the new values to set.}

\item{\code{error}}{\code{(character(1))} \cr
If \code{"warn"} then returns a warning on error, otherwise breaks if \code{"stop"}.}

\item{\code{resolveConflicts}}{\code{(logical(1))}\cr
If \code{FALSE} (default) throws error if conflicting parameterisations are provided, otherwise
automatically resolves them by removing all conflicting parameters.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Empirical-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Empirical-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Empirical$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
