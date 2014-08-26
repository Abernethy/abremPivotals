getCCC2<-function(fail, dist="weibull"){
    m <- 1
    if(dist=="lognormal") {m <- 0}
	.Call( "CallgetCCC2", fail, m, PACKAGE = "abremPivotals" )
    # TODO: no 'try()' wrapping needed here?
}