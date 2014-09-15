lslr <- function(x, dist="weibull", npar=2, reg_method="xony")  {				
	## a convergence limit is fixed here for 3rd parameter  convergence			
	## no longer an argument for the R function, but still an argument to C++ functions			
	limit<-1e-5			
    if(is.data.frame(x)){
        if(!is.null(x$time)){
            if(!any(where <- (tolower(names(x)) %in% 
                c("ppp","ppp.benard","ppp.beta","ppp.mean",
                "ppp.hazen","ppp.km","ppp.kaplan-meier","ppp.blom")))){
                stop(': Argument \"x\" is missing ppp columns(s)...')
                # Todo: consider NOT supporting all ranking methods;
            }else{
#                if(length(x[,which(where)[1]]) < 3){
                if(length(x[,which(where)[1]]) < 2){
                    # no reason to not allow a perfect fit between two ppp's
                    stop("Insufficient failure times")
                }
            }
        }else{
            stop(': Argument \"x\" is missing $time column...')
        }
    }

	casenum<-0			
	if(tolower(reg_method)=="yonx") casenum=casenum+1
	if(npar==3) casenum=casenum+2			
	if(dist=="lognormal") casenum=casenum+4
	if(dist=="gumbel") casenum=casenum+8			

    na.omit				
				
	resultVec<-.Call("LSLR", na.omit(x)$time, na.omit(x)[,which(where)[1]], limit, casenum , package="abremPivotals")	
        # TODO: check proper usage of na.omit. Not too strict? any row with a NA value in the $time OR in the $event column will be omitted
        # (jurgen) there was a case where lslr() failed with a mention of "object 'where' not found -> look into this
				
	if(casenum < 4) {			
		if(length(resultVec)==3)  {	
			prr<-AbPval(dim(x)[1], resultVec[3])
			outVec<-c(Eta=resultVec[1],Beta=resultVec[2],Rsqr=resultVec[3], AbPval=prr[[1]])
            # double prr[[1]] brackets are needed here of the naming of the number gets messed up in outVec
		}else{		
			outVec<-c(Eta=resultVec[1],Beta=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])
			if(resultVec[5]==1)  {			
				warn="3p optimization did not converge"
				attr(outvec,"warning")<-warn
			}			
		}		
	}else{			
		if(casenum < 8) {		
			if(length(resultVec)==3)  {	
				prr<-AbPval(length(x[,1]), resultVec[3],"lognormal")
				outVec<-c(Mulog=resultVec[1],Sigmalog=resultVec[2],Rsqr=resultVec[3], AbPval=prr[[1]])	
			}else{	
				outVec<-c(Mulog=resultVec[1],Sigmalog=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])
				if(resultVec[5]==1)  {
					warn="3p optimization did not converge"
					attr(outVec,"warning")<-warn
				}
			}	
		}else{		
			if(length(resultVec)==3)  {	
				outVec<-c(Etalog=resultVec[1],Betalog=resultVec[2],Rsqr=resultVec[3])
			}else{	
				outVec<-c(Etalog=resultVec[1],Betalog=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])
				if(resultVec[5]==1)  {
					warn="3p optimization did not converge"
					attr(outVec,"warning")<-warn
				}				
			}	
		}		
	}			
				
return(outVec)				
}				
