AbPval <- function(fail,R2,dist="weibull")  {
    m <- 1
    if(tolower(dist)=="lognormal") {m <- 0}
    ret <- .Call( "CallgetPvalue", F=fail, R2, m, PACKAGE = "abremPivotals" )
    # TODO: don't we need 'try' wrappers when calling CPP code?
    names(ret) <- c("AbPval","CCC2")
    # TODO: the above is needed?
    ret
}