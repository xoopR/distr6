% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wrapper_TruncatedDistribution.R
\name{TruncatedDistribution}
\alias{TruncatedDistribution}
\title{Distribution Truncation Wrapper}
\description{
A wrapper for truncating any probability distribution at given limits.
}
\details{
The pdf and cdf of the distribution are required for this wrapper, if unavailable decorate with
\link{FunctionImputation} first.

Truncates a distribution at lower and upper limits on a left-open interval, using the formulae
\deqn{f_T(x) = f_X(x) / (F_X(upper) - F_X(lower))}
\deqn{F_T(x) = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
where \eqn{f_T}/\eqn{F_T} is the pdf/cdf of the truncated distribution
T = Truncate(X, lower, upper) and \eqn{f_X}, \eqn{F_X} is the pdf/cdf of the
original distribution. T is supported on (].
}
\examples{

## ------------------------------------------------
## Method `TruncatedDistribution$new`
## ------------------------------------------------

TruncatedDistribution$new(
  Binomial$new(prob = 0.5, size = 10),
  lower = 2, upper = 4
)

# alternate constructor
truncate(Binomial$new(), lower = 2, upper = 4)
}
\seealso{
Other wrappers: 
\code{\link{Convolution}},
\code{\link{DistributionWrapper}},
\code{\link{HuberizedDistribution}},
\code{\link{MixtureDistribution}},
\code{\link{ProductDistribution}},
\code{\link{VectorDistribution}}
}
\concept{wrappers}
\section{Super classes}{
\code{\link[distr6:Distribution]{distr6::Distribution}} -> \code{\link[distr6:DistributionWrapper]{distr6::DistributionWrapper}} -> \code{TruncatedDistribution}
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
\item \href{#method-TruncatedDistribution-new}{\code{TruncatedDistribution$new()}}
\item \href{#method-TruncatedDistribution-clone}{\code{TruncatedDistribution$clone()}}
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
<li><span class="pkg-link" data-pkg="distr6" data-topic="DistributionWrapper" data-id="wrappedModels"><a href='../../distr6/html/DistributionWrapper.html#method-DistributionWrapper-wrappedModels'><code>distr6::DistributionWrapper$wrappedModels()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TruncatedDistribution-new"></a>}}
\if{latex}{\out{\hypertarget{method-TruncatedDistribution-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TruncatedDistribution$new(distribution, lower = NULL, upper = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{distribution}}{\verb{([Distribution])} \cr
\link{Distribution} to wrap.}

\item{\code{lower}}{\code{(numeric(1))} \cr
Lower limit to huberize the distribution at. If \code{NULL} then the lower bound of
the \link{Distribution} is used.}

\item{\code{upper}}{\code{(numeric(1))} \cr
Upper limit to huberize the distribution at. If \code{NULL} then the upper bound of
the \link{Distribution} is used.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{TruncatedDistribution$new(
  Binomial$new(prob = 0.5, size = 10),
  lower = 2, upper = 4
)

# alternate constructor
truncate(Binomial$new(), lower = 2, upper = 4)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TruncatedDistribution-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TruncatedDistribution-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TruncatedDistribution$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
