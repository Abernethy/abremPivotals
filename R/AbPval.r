AbPval <- function(fail,r2,dist="weibull")  {
## validation of dist argument entry else "lnorm" would return weibull result
	if(tolower(dist)!="weibull" || tolower(dist)!="lognormal")  {
	stop("distribution not recognized - use 'weibull' or 'lognormal'")
    m <- 1
    if(tolower(dist)=="lognormal") {m <- 0}
    ret <- .Call( "CallgetPvalue", F=fail, r2, m, PACKAGE = "abremPivotals" )
    # TODO: don't we need 'try' wrappers when calling CPP code?
    names(ret) <- c("AbPval","CCC2")
    # TODO: the above is needed?
    ret
}