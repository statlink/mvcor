\name{Squared multivariate correlation between two sets of variables}
\alias{sq.correl}
\title{
Squared multivariate correlation between two sets of variables
}

\description{
Squared multivariate correlation between two sets of variables.
}

\usage{
sq.correl(y, x)
}

\arguments{
\item{y}{
A numerical matrix.
}
\item{x}{
A numerical matrix.
}
}

\details{
Mardia, Kent and Bibby (1979, pg. 171) defined two squared multiple correlation coefficient between the dependent variable \eqn{\bf Y} and the independent variable \eqn{\bf X}. They mention that these are a similar measure of the coefficient determination in the univariate regression. Assume that the multivariate regression model is written as \eqn{{\bf Y}={\bf XB}+{\bf U}}, where \eqn{\bf U} is the matrix of residuals. Then, they write \eqn{{\bf D}=\left({\bf Y}^T{\bf Y}\right)^{-1}\hat{\bf U}^T\hat{\bf U}}, with \eqn{\hat{\bf U}^T\hat{\bf U}={\bf Y}^T{\bf PY}} and \eqn{\bf P} is \eqn{{\bf P}={\bf I}_n-{\bf X}\left({\bf X}^T{\bf X}\right)^{-1}{\bf X}^T}. The matrix \eqn{\bf D} is a generalization of \eqn{1-R^2} in the univariate case. Mardia, Kent and Bibby (1979, pg. 171) mentioned that the dependent variable \eqn{\bf Y} has to be centred.

The squared multivariate correlation should lie between 0 and 1 and this property is satisfied by the trace correlation \eqn{r_T} and the determinant correlation \eqn{r_D}, defined as
\eqn{r^2_T=d^{-1}\text{tr}\left({\bf I}-{\bf D}\right)} and \eqn{r^2_D=\text{det}\left({\bf I}-{\bf D}\right)}
respectively, where \eqn{d} denotes the dimensionality of \eqn{\bf Y}. So, high values indicate high proportion of variance of the dependent variables explained. Alternatively, one can calculate the trace and the determinant of the matrix \eqn{{\bf E}=\left({\bf Y}^T{\bf Y}\right)^{-1}\hat{\bf Y}^T\hat{\bf Y}}. Try something else also, use the function "sq.correl()" in a univariate regression example and then calculate the \eqn{R^2} for the same dataset. Try this example again but without centering the dependent variable. In addition, take two variables and calculate their squared correlation coefficient and then square it and using "sq.correl()".
}

\value{
A vector with two values, the trace and determinant \eqn{R^2}.
}

%\references{
%
%}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{rv}, \link{dcor}
}
}

\examples{
sq.correl( as.matrix(iris[, 1:2]), as.matrix(iris[, 3:4]) )
}


