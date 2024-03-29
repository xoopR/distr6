% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDistribution_Categorical.R
\name{Categorical}
\alias{Categorical}
\title{Categorical Distribution Class}
\value{
Returns an R6 object inheriting from class \link{SDistribution}.
}
\description{
Mathematical and statistical functions for the Categorical distribution, which
is commonly used in classification supervised learning.
}
\details{
The Categorical distribution parameterised with  a given support set, \eqn{x_1,...,x_k}, and respective probabilities, \eqn{p_1,...,p_k}, is defined by the pmf, \deqn{f(x_i) = p_i} for \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}.

Sampling from this distribution is performed with the \link{sample} function with the elements given
as the support set and the probabilities from the \code{probs} parameter. The cdf and quantile assumes
that the elements are supplied in an indexed order (otherwise the results are meaningless).

The number of points in the distribution cannot be changed after construction.
}
\section{Distribution support}{

The distribution is supported on \eqn{x_1,...,x_k}.
}

\section{Default Parameterisation}{

Cat(elements = 1, probs = 1)
}

\section{Omitted Methods}{


N/A
}

\section{Also known as}{


N/A
}

\examples{

## ------------------------------------------------
## Method `Categorical$new`
## ------------------------------------------------

# Note probabilities are automatically normalised (if not vectorised)
x <- Categorical$new(elements = list("Bapple", "Banana", 2), probs = c(0.2, 0.4, 1))

# Length of elements and probabilities cannot be changed after construction
x$setParameterValue(probs = c(0.1, 0.2, 0.7))

# d/p/q/r
x$pdf(c("Bapple", "Carrot", 1, 2))
x$cdf("Banana") # Assumes ordered in construction
x$quantile(0.42) # Assumes ordered in construction
x$rand(10)

# Statistics
x$mode()

summary(x)
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
\code{\link{Degenerate}},
\code{\link{DiscreteUniform}},
\code{\link{EmpiricalMV}},
\code{\link{Empirical}},
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
\code{\link{Cauchy}},
\code{\link{ChiSquaredNoncentral}},
\code{\link{ChiSquared}},
\code{\link{Degenerate}},
\code{\link{DiscreteUniform}},
\code{\link{Empirical}},
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
\code{\link[distr6:Distribution]{distr6::Distribution}} -> \code{\link[distr6:SDistribution]{distr6::SDistribution}} -> \code{Categorical}
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
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{properties}}{Returns distribution properties, including skewness type and symmetry.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Categorical-new}{\code{Categorical$new()}}
\item \href{#method-Categorical-mean}{\code{Categorical$mean()}}
\item \href{#method-Categorical-mode}{\code{Categorical$mode()}}
\item \href{#method-Categorical-variance}{\code{Categorical$variance()}}
\item \href{#method-Categorical-skewness}{\code{Categorical$skewness()}}
\item \href{#method-Categorical-kurtosis}{\code{Categorical$kurtosis()}}
\item \href{#method-Categorical-entropy}{\code{Categorical$entropy()}}
\item \href{#method-Categorical-mgf}{\code{Categorical$mgf()}}
\item \href{#method-Categorical-cf}{\code{Categorical$cf()}}
\item \href{#method-Categorical-pgf}{\code{Categorical$pgf()}}
\item \href{#method-Categorical-clone}{\code{Categorical$clone()}}
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
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="setParameterValue"><a href='../../distr6/html/Distribution.html#method-Distribution-setParameterValue'><code>distr6::Distribution$setParameterValue()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="stdev"><a href='../../distr6/html/Distribution.html#method-Distribution-stdev'><code>distr6::Distribution$stdev()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="strprint"><a href='../../distr6/html/Distribution.html#method-Distribution-strprint'><code>distr6::Distribution$strprint()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="summary"><a href='../../distr6/html/Distribution.html#method-Distribution-summary'><code>distr6::Distribution$summary()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="Distribution" data-id="workingSupport"><a href='../../distr6/html/Distribution.html#method-Distribution-workingSupport'><code>distr6::Distribution$workingSupport()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Categorical-new"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$new(elements = NULL, probs = NULL, decorators = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{elements}}{\code{list()}\cr
Categories in the distribution, see examples.}

\item{\code{probs}}{\code{numeric()}\cr
Probabilities of respective categories occurring.}

\item{\code{decorators}}{\code{(character())} \cr
Decorators to add to the distribution during construction.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Note probabilities are automatically normalised (if not vectorised)
x <- Categorical$new(elements = list("Bapple", "Banana", 2), probs = c(0.2, 0.4, 1))

# Length of elements and probabilities cannot be changed after construction
x$setParameterValue(probs = c(0.1, 0.2, 0.7))

# d/p/q/r
x$pdf(c("Bapple", "Carrot", 1, 2))
x$cdf("Banana") # Assumes ordered in construction
x$quantile(0.42) # Assumes ordered in construction
x$rand(10)

# Statistics
x$mode()

summary(x)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Categorical-mean"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-mean}{}}}
\subsection{Method \code{mean()}}{
The arithmetic mean of a (discrete) probability distribution X is the expectation
\deqn{E_X(X) = \sum p_X(x)*x}
with an integration analogue for continuous distributions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$mean(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-mode"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-mode}{}}}
\subsection{Method \code{mode()}}{
The mode of a probability distribution is the point at which the pdf is
a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
maxima).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$mode(which = "all")}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-variance"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-variance}{}}}
\subsection{Method \code{variance()}}{
The variance of a distribution is defined by the formula
\deqn{var_X = E[X^2] - E[X]^2}
where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
covariance matrix is returned.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$variance(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-skewness"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-skewness}{}}}
\subsection{Method \code{skewness()}}{
The skewness of a distribution is defined by the third standardised moment,
\deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
distribution and \eqn{\sigma} is the standard deviation of the distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$skewness(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-kurtosis"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-kurtosis}{}}}
\subsection{Method \code{kurtosis()}}{
The kurtosis of a distribution is defined by the fourth standardised moment,
\deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
distribution and \eqn{\sigma} is the standard deviation of the distribution.
Excess Kurtosis is Kurtosis - 3.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$kurtosis(excess = TRUE, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-entropy"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-entropy}{}}}
\subsection{Method \code{entropy()}}{
The entropy of a (discrete) distribution is defined by
\deqn{- \sum (f_X)log(f_X)}
where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
continuous distributions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$entropy(base = 2, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-mgf"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-mgf}{}}}
\subsection{Method \code{mgf()}}{
The moment generating function is defined by
\deqn{mgf_X(t) = E_X[exp(xt)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$mgf(t, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-cf"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-cf}{}}}
\subsection{Method \code{cf()}}{
The characteristic function is defined by
\deqn{cf_X(t) = E_X[exp(xti)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$cf(t, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-pgf"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-pgf}{}}}
\subsection{Method \code{pgf()}}{
The probability generating function is defined by
\deqn{pgf_X(z) = E_X[exp(z^x)]}
where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$pgf(z, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-Categorical-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Categorical-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Categorical$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
