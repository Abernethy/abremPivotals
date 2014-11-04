AbPval <- function(fail,r2,dist="weibull")  {
## validation of dist argument entry else "lnorm" would return weibull result
	if(tolower(dist)!="weibull" && tolower(dist)!="lognormal")  {
	stop("distribution not recognized - use 'weibull' or 'lognormal'")
	}
	m <- 1
    if(tolower(dist)=="lognormal") {m <- 0}
	if(fail<3) stop("insufficient failures")
	if(fail>1500) stop("failures beyond 1500 not correlated")
	if(r2>0 && r2<=1)  {
    ret <- .Call( "CallgetPvalue", F=fail, r2, m, PACKAGE = "abremPivotals" )
	}else{
	stop("r2 value invalid")
	}
    # TODO: don't we need 'try' wrappers when calling CPP code?
	## Okay, argument validation needs to prevent R environment from lockup
    names(ret) <- c("AbPval","CCC2")
    # TODO: the above is needed?
	## I find it nice to have labels for the output, else on casual use must reference man page.
    ret
}