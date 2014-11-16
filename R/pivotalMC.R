pivotalMC <- function(x, sample_mask=NULL,dist="weibull", reg_method="xony", r2, CL, unrel, P1=1.0, P2=1.0, S=10^4, seed=1234, ProgRpt=FALSE)  {	
    #   P1 or p1, P2 or p2? 
	
## Effort to match input needs for pivotalMC need only to be made in abrem package
## pivotalMC is then free to strictly validate argument values due to potential independent use
## with no assumptions about source of arguments

 	if(is.vector(x))  {				
		stop("use MRR functions for casual fitting, or pre-process with getPPP")			
	}else{
		if(is.data.frame(x)){
			if(names(x)[1]=="time"&&names(x)[2]=="ppp")  {			
			## will handle the output from getPPP			
			## this format is also generated in abrem::calculateSingleConf			
			}else{			
				stop("dataframe argument format not recognized")		
			}	
		}else{
            stop(': Argument \"x\" must be a dataframe with $time  ",
                "$ppp columns...')
		}
	}
	
	## any additional validations, such as positive numeric checking					
	## removal of potential na's, etc. could take place here					
		if(anyNA(x))  {				
		stop("NA not handled in pivotalMC argument data")				
		}				
		if(any(c(x$time,x$ppp)<=0)) {				
		stop("negative values or zero in pivotalMC argument data")				
		}				
		if(any(c(x$ppp)>=1)) {				
		stop("invalid percentile value in ppp")				
		}    

                if(dim(x)[1] < 3)  {
	## shall not run pivotal with 2 points				
#                if(length(x[,which(where)[1]]) < 2)  {
                    stop("insufficient failure points for pivotal analysis")
                }

	## populate or validate the sample_mask vector
	if(is.null(sample_mask))  {
		sample_mask=rep(1,dim(x)[1])
	}else{	
		if(length(x)!=(length(sample_mask)-zeros)) {
			stop("sample_mask has wrong number of failure indicators")
		}
	}
		
	S = as.integer(S/10)*10	
#	if (S < 10^3) {
### return the full vector or matrix output for special small sampled cases
#	    if(r2>0) r2=1.0
#		if(CL>0) CL=1.0
#	   }	
## small sampling to be resolved later

    
	if(S > .Machine$integer.max)   {
		stop("Samples beyond MAX_INT")
        # TODO (Jurgen, 11/10/2014: replace the above with .Machine$integer.max, which
        # on my machine is half of 4*10^9
	}
				
	casenum <- 0			
	if(tolower(reg_method)=="yonx") casenum <- casenum+1						
	if(dist=="lognormal") casenum <- casenum+2
	if(dist=="gumbel") casenum <- casenum+4	
	
				
	result <- .Call("pivotalMC", x$ppp, sample_mask, c(r2,CL,P1,P2), S, seed, unrel, ProgRpt, casenum , package="abremPivotals")

return(result)				
}				
