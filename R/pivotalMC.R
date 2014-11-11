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
## Please just send a dataframe with time and whatever desired ppp values you have chosen, without an event column and no NA's
    if(is.data.frame(x)){
        if(!is.null(x$time) && !is.null(x$event)){
            if(!any(where <- (tolower(names(x)) %in% 
                c("ppp","ppp.benard","ppp.beta","ppp.mean",
                "ppp.km","ppp.kaplan-meier","ppp.hazen","ppp.blom")))){
                stop(': Argument \"x\" is missing ppp columns(s)...')
                # Todo (Jurgen): make this code more elegant 
            }else{
                if(length(x[,which(where)[1]]) < 3)  {
## shall not run pivotal with 2 points				
#                if(length(x[,which(where)[1]]) < 2)  {
                    stop("insufficient failure points for pivotal analysis")
                }
            }
        }else{
            stop(': Argument \"x\" must be a dataframe with $time and/or ",
                "$event columns...')
        }
    }

	## validate the event vector
##	zeros <- length(x$event[sapply(x$event, function(x) x==0)])
#	if(length(x)!=(length(x$event)-zeros)) {
#		stop("event vector has wrong length")
#	}
# TODO: the above is not needed here because the event vectors in x are always correct
## (always??) A complete review of input arguments and use for this function is required.
## Must satisfy both abrem calling and external (non-abrem) use.

		
	S = as.integer(S/10)*10	
#	if (S < 10^3) {
### return the full vector or matrix output for special small sampled cases
#	    if(r2>0) r2=1.0
#		if(CL>0) CL=1.0
#	   }	
    # (Jurgen) Why? this makes debugging the code much harder since I have to scroll thousands of output lines.
    # Perhaps a warning is sufficient
## This entire block was to permit some special small sampled cases you wanted to look at
## There is a danger of integer conversion problem calculating prr with reduced S values.
## So, we were just delivering the entire matrix in those cases.
## The plan was to apply this in special case in an undocumented fashion.
## We need to stop prr calculation at some value of S.  10^3 is a safe number. 10^2 can generate problems.
## The original desire was to permit S=10 as I recall.
    
	if(S > 4*10^9)   {
		stop("Samples beyond MAX_INT")
        # TODO (Jurgen, 11/10/2014: replace the above with .Machine$integer.max, which
        # on my machine is half of 4*10^9
	}
				
	casenum <- 0			
	if(tolower(reg_method)=="yonx") casenum <- casenum+1						
	if(dist=="lognormal") casenum <- casenum+2
	if(dist=="gumbel") casenum <- casenum+4	
	
##	How do I know that object na.omit(x[,which(where)[1]]) is indeed the ppp vector?	
##  Why can't this simply be delivered in 'x' argument without event column and NA's?
##  I probably should validate that the ppp values are >0<1, and sorted.
##  As abremPivotals administrator I have no assurance where these are coming from. Just my man pages to tell how to use.  
##  I intended this function to be called like: pivotalMC(getPPP(failures, suspensions),r2=.9435,CL=.95) using defaults in this example
##  Making allowance for pin compatibility with some other object in abrem should not be done here.
##  below is a minimal fix for nullifying the sample_mask vector, but return to original code form is ultimately required.
	ppp<-na.omit(x[,which(where)[1]])				
	result <- .Call("pivotalMC", ppp, rep(1,length(ppp)), c(r2,CL,P1,P2), S, seed, unrel, ProgRpt, casenum , package="abremPivotals")
    # TODO: x[,which(where)[1]]: use the first match of a supported ranking method for calculating pivotals


return(result)				
}				
