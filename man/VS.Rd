\name{VS}
\alias{VS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Variable selection based selector on variable selection deviation (VSD)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The package selects a subset of variables to produce a model that achieves the smallest VSD value over all possible sets of features.
}
\usage{
VS(x, y, minvalue = 0.5, threshold = 0.95, weight.method, n.sim = 100, n.train=ceiling(dim(x)[1]/2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
  A matrix of predictors.
}
  \item{y}{
%%     ~~Describe \code{y} here~~
  A vector of sample labels.
}
  \item{cutoff}{
%%     ~~Describe \code{minvalue} here~~
  The cutoff value that determines whether a certain variable should be included in the subset of variables. The subset of variables that results in the smallest VSD value is when the cutoff value equals 0.5.
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
  The threshold is used for ranking variable.
}
  \item{weight.method}{
%%     ~~Describe \code{weight.method} here~~
Methods for computing the weights for VSD measure. Users select from AIC, BIC, and ARM.
}
  \item{n.sim}{
%%     ~~Describe \code{n.sim} here~~
The number of times to calculate the weights. The default is 100.
}
  \item{n.train}{
%%     ~~Describe \code{n.train} here~~
The size of the training set when used for ARM weighting.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{VSD}{
VSD value of selected variable subset.
}
\item{VSD_plus}{
VSD plus value of selected variable subset.
}
\item{VSD_minus}{
VSD minus value of selected variable subset.
}
\item{best}{
The selected variable subset which have smallest VSD value.
}
\item{model}{
The selected variable subset which is obtained by ranking variable algorithm based on variable selection deviation.
}
}
\references{
%% ~put references to the literature/web site here ~

}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  library(Matrix)
  library(MASS)
  library(glmvsd)
  beta <- c(2,2,1,1,0.5,0,0,0,0,0)
  beta <- matrix(beta,ncol=1)
  sigma <- diag(rep(1,10))
  traindata <- mvrnorm(n=30,Sigma=sigma,rep(0,10))
  trainlabel <- traindata \%*\% beta + matrix(rnorm(30,mean=0,sd=1),ncol=1)
  object <- VS(traindata,trainlabel,weight.method='BIC',n.train=10)
  print(object$best)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
