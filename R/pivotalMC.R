pivotalMC <- function(x, dist="weibull", reg_method="xony", r2, CL, unrel, P1=1.0, P2=1.0, S=10^4, seed=1234, ProgRpt=FALSE)  {	
    # NOTES (Jurgen):
    #   adaptation of original pivotalMC2 to be pin compatible with abrem's x$fit[[i]]$data 
    #   dataframe in the abrem object:
    #   the dataframe 'x' always holds 'time', 'event' and 'ppp' columns
    
    #   I removed event argument because this might conflict with
    #   the event column in x (when x is a data frame)
    #   the proper way to allow passing events to the function is by using the 
    #   ellipsis function so that the user must explicitly specify event vectors
    #   *both* by name AND value
    #   in the current iteration, event vectors are only supported as a column of
    #   x. 
    
    #   see Abrem() for code example on how to accomplish this, but you will notice that
    #   we are about to duplicate efforts ...

    #   P1 or p1, P2 or p2? 

    if(is.data.frame(x)){
        if(!is.null(x$time) && !is.null(x$event)){
            if(!any(where <- (tolower(names(x)) %in% 
                c("ppp","ppp.benard","ppp.beta","ppp.mean",
                "ppp.km","ppp.kaplan-meier","ppp.hazen","ppp.blom")))){
                stop(': Argument \"x\" is missing ppp columns(s)...')
                # Todo (Jurgen): make this code more elegant 
            }else{
#                if(length(x[,which(where)[1]]) < 3)  {
                if(length(x[,which(where)[1]]) < 2)  {
                    stop("insufficient failure points")
                }
            }
        }else{
            stop(': Argument \"x\" is missing $time and/or ",
                "$event columns...')
        }
    }

	## validate the event vector
	zeros <- length(x$event[sapply(x$event, function(x) x==0)])
#	if(length(x)!=(length(x$event)-zeros)) {
#		stop("event vector has wrong length")
#	}
# TODO: the above is not needed here because the event vectors in x are always correct
	    if (r2 < 0|| r2 > 1) stop("Invalid R-squared value")
	    if (CL < 0|| CL > 1) stop("Invalid Confidence Level")	
		if(min(unrel)<=0||max(unrel)>=1) stop("Invalid unreliability vector")
		
		if(dist!="weibull" && P1==1.0) message("lognormal or gumbel sampled with P1=1.0")
		
	S = as.integer(S/10)*10	
#	if (S < 10^3) {
### return the full vector or matrix output for special small sampled cases
#	    if(r2>0) r2=1.0
#		if(CL>0) CL=1.0
#	   }	
    # (Jurgen) Why? this makes debugging the code much harder since I have to scroll thousands of output lines.
    # Perhaps a warning is sufficient
    
	if(S > 4*10^9)   {
		stop("Samples beyond MAX_INT")
        # TODO (Jurgen, 11/10/2014: replace the above with .Machine$integer.max, which
        # on my machine is half of 4*10^9
	}
				
	casenum <- 0			
	if(tolower(reg_method)=="yonx") casenum <- casenum+1						
	if(dist=="lognormal") casenum <- casenum+2
	if(dist=="gumbel") casenum <- casenum+4			
				
				
	result <- .Call("pivotalMC", na.omit(x[,which(where)[1]]), x$event, c(r2,CL,P1,P2), S, seed, unrel, ProgRpt, casenum , package="abremPivotals")
    # TODO: x[,which(where)[1]]: use the first match of a supported ranking method for calculating pivotals


return(result)				
}				
