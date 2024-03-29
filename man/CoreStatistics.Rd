% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistributionDecorator_CoreStatistics.R
\name{CoreStatistics}
\alias{CoreStatistics}
\title{Core Statistical Methods Decorator}
\description{
This decorator adds numeric methods for missing analytic expressions in
\link{Distribution}s as well as adding generalised expectation and moments functions.
}
\details{
Decorator objects add functionality to the given \link{Distribution} object by copying methods
in the decorator environment to the chosen \link{Distribution} environment.

All methods implemented in decorators try to exploit analytical results where possible, otherwise
numerical results are used with a message.
}
\examples{
decorate(Exponential$new(), "CoreStatistics")
Exponential$new(decorators = "CoreStatistics")
CoreStatistics$new()$decorate(Exponential$new())
}
\seealso{
Other decorators: 
\code{\link{ExoticStatistics}},
\code{\link{FunctionImputation}}
}
\concept{decorators}
\section{Super class}{
\code{\link[distr6:DistributionDecorator]{distr6::DistributionDecorator}} -> \code{CoreStatistics}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CoreStatistics-mgf}{\code{CoreStatistics$mgf()}}
\item \href{#method-CoreStatistics-cf}{\code{CoreStatistics$cf()}}
\item \href{#method-CoreStatistics-pgf}{\code{CoreStatistics$pgf()}}
\item \href{#method-CoreStatistics-entropy}{\code{CoreStatistics$entropy()}}
\item \href{#method-CoreStatistics-skewness}{\code{CoreStatistics$skewness()}}
\item \href{#method-CoreStatistics-kurtosis}{\code{CoreStatistics$kurtosis()}}
\item \href{#method-CoreStatistics-variance}{\code{CoreStatistics$variance()}}
\item \href{#method-CoreStatistics-kthmoment}{\code{CoreStatistics$kthmoment()}}
\item \href{#method-CoreStatistics-genExp}{\code{CoreStatistics$genExp()}}
\item \href{#method-CoreStatistics-mode}{\code{CoreStatistics$mode()}}
\item \href{#method-CoreStatistics-mean}{\code{CoreStatistics$mean()}}
\item \href{#method-CoreStatistics-clone}{\code{CoreStatistics$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="distr6" data-topic="DistributionDecorator" data-id="decorate"><a href='../../distr6/html/DistributionDecorator.html#method-DistributionDecorator-decorate'><code>distr6::DistributionDecorator$decorate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="distr6" data-topic="DistributionDecorator" data-id="initialize"><a href='../../distr6/html/DistributionDecorator.html#method-DistributionDecorator-initialize'><code>distr6::DistributionDecorator$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-mgf"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-mgf}{}}}
\subsection{Method \code{mgf()}}{
Numerically estimates the moment-generating function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$mgf(t, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{\code{(integer(1))} \cr
t integer to evaluate function at.}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-cf"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-cf}{}}}
\subsection{Method \code{cf()}}{
Numerically estimates the characteristic function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$cf(t, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{\code{(integer(1))} \cr
t integer to evaluate function at.}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-pgf"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-pgf}{}}}
\subsection{Method \code{pgf()}}{
Numerically estimates the probability-generating function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$pgf(z, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{z}}{\code{(integer(1))} \cr
z integer to evaluate probability generating function at.}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-entropy"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-entropy}{}}}
\subsection{Method \code{entropy()}}{
Numerically estimates the entropy function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$entropy(base = 2, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{base}}{\code{(integer(1))} \cr
Base of the entropy logarithm, default = 2 (Shannon entropy)}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-skewness"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-skewness}{}}}
\subsection{Method \code{skewness()}}{
Numerically estimates the distribution skewness.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$skewness(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-kurtosis"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-kurtosis}{}}}
\subsection{Method \code{kurtosis()}}{
Numerically estimates the distribution kurtosis.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$kurtosis(excess = TRUE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{excess}}{\code{(logical(1))} \cr
If \code{TRUE} (default) excess kurtosis returned.}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-variance"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-variance}{}}}
\subsection{Method \code{variance()}}{
Numerically estimates the distribution variance.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$variance(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-kthmoment"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-kthmoment}{}}}
\subsection{Method \code{kthmoment()}}{
The kth central moment of a distribution is defined by
\deqn{CM(k)_X = E_X[(x - \mu)^k]}
the kth standardised moment of a distribution is defined by
\deqn{SM(k)_X = \frac{CM(k)}{\sigma^k}}{SM(k)_X = CM(k)/\sigma^k}
the kth raw moment of a distribution is defined by
\deqn{RM(k)_X = E_X[x^k]}
where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
distribution and \eqn{\sigma} is the standard deviation of the distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$kthmoment(k, type = c("central", "standard", "raw"), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{k}}{\code{integer(1)} \cr
The \code{k}-th moment to evaluate the distribution at.}

\item{\code{type}}{\code{character(1)} \cr
Type of moment to evaluate.}

\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-genExp"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-genExp}{}}}
\subsection{Method \code{genExp()}}{
Numerically estimates \eqn{E[f(X)]} for some function \eqn{f}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$genExp(trafo = NULL, cubature = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{trafo}}{\verb{function()} \cr
Transformation function to define the expectation, default is distribution mean.}

\item{\code{cubature}}{\code{logical(1)} \cr
If \code{TRUE} uses \link[cubature:cubintegrate]{cubature::cubintegrate} for approximation, otherwise \link{integrate}.}

\item{\code{...}}{\code{ANY} \cr
Passed to \link[cubature:cubintegrate]{cubature::cubintegrate}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-mode"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-mode}{}}}
\subsection{Method \code{mode()}}{
Numerically estimates the distribution mode.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$mode(which = "all")}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-CoreStatistics-mean"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-mean}{}}}
\subsection{Method \code{mean()}}{
Numerically estimates the distribution mean.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$mean(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{\code{ANY} \cr
Passed to \verb{$genExp}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoreStatistics-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CoreStatistics-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoreStatistics$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
