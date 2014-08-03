AbPval <- function(fail,R2,dist="weibull")  {
    m <- 1
    if(tolower(dist)=="lnorm") {m <- 0}    
    ret <- .Call( "CallgetPvalue", F=fail, R2, m, PACKAGE = "abremPivotals" )
    # TODO: don't we need 'try' wrappers when calling CPP code?
    names(ret) <- c("AbPval","ccc2")
        # TODO: Note use of lowercase ccc2 while using mixed case for AbPval
    ret
}