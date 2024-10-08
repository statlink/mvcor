\name{RV correlation between two sets of variables}
\alias{rv}
\title{
RV correlation between two sets of variables
}

\description{
RV correlation between two sets of variables.
}

\usage{
rv(y, x)
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
The RV correlation coefficient
}

\value{
The value of the RV coefficient.
}

\references{
Robert P. and Escoufier Y. (1976). A Unifying Tool for Linear Multivariate Statistical Methods: The RV-Coefficient. Applied Statistics, 25(3): 257--265.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{mrv}, \link{drv}, \link{sq.correl}, \link{bcdcor}
}
}

\examples{
rv( as.matrix(iris[, 1:2]), as.matrix(iris[, 3:4]) )
}


