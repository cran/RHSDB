\name{rh.sd.sidak}
\alias{rh.sd.sidak}
\title{Ryan-Holm Step-Down Sidak Procedure}
\description{
This procedure provides the adjusting p-values and adjusting CIs.
}
\usage{
rh.sd.sidak(p,effect,effect.se,df,type,sig,side,digits)
}
\arguments{
  \item{p}{the raw p values}
  \item{effect}{the effect size from the multiple comparisons, e.g. the mean difference from t test or paried t test }
  \item{effect.se}{the standard error of effect size from the multiple comparisons, e.g. the standard error of mean difference from t test or paried t test)}
  \item{df}{the degree of freedom of hypothesis test, e.g. n1+n2-2 for t test, n-1 for paried t test}
  \item{type}{the type of the effect size, default is "mean"}
  \item{sig}{the significance level, default is 0.05}
  \item{side}{"one" or "two" sided hypothesis test}
  \item{digits}{the number of decimal digits}
}
\value{
  \item{p.adj}{the adjusted p value}
  \item{ci.adj.l}{the lower limit of adjusted confidence interval}
  \item{ci.adj.u}{the upper limit of adjusted confidence interval}
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Reference:

1. John Ludbrook (2000). MULTIPLE INFERENCES USING CONFIDENCE INTERVALS. Clinical and Experimental Pharmacology and Physiology. 27: 212-215.

Update:

Version 0.1.0: The first version.

}
\seealso{
\code{\link{rh.sd.bonferroni}}
}
\examples{
p=c(0.217,0.00028,0,0.001,0.024,0.719,0.00033)
effect=c(16,74,-85,-38,29,5,91)
effect.se=c(12,16,14,9,12,16,20)
df=16
rh.sd.sidak(p,effect,effect.se,df)
}
