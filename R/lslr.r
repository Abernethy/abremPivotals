lslr <- function(x, dist="weibull", npar=2, reg_method="xony")  {				
	## a convergence limit is fixed here for 3rd parameter  convergence			
	## no longer an argument for the R function, but still an argument to C++ functions			
	limit<-1e-5			
    if(is.data.frame(x)){
        if(!is.null(x$time)){
            if(!any(where <- (tolower(names(x)) %in% 
                c("rank","rank.benard","rank.beta","rank.mean",
                "rank.hazen","rank.km","rank.kaplan-meier","rank.blom")))){
                stop(': Argument \"x\" is missing rank columns(s)...')
                # Todo: consider NOT supporting all ranking methods;
            }else{
                if(length(x[,which(where)[1]]) < 3){
                    stop("insufficient failure points")
                }
            }
        }else{
            stop(': Argument \"x\" is missing $time column...')
        }
    }

	casenum<-0			
	if(tolower(reg_method)=="yonx") casenum=casenum+1
	if(npar==3) casenum=casenum+2			
	if(dist=="lnorm") casenum=casenum+4			
	if(dist=="gumbel") casenum=casenum+8			
				
				
	resultVec<-.Call("LSLR", x$time, x[,which(where)[1]], limit, casenum , package="abremPivotals")			
				
	if(casenum < 4) {			
		if(length(resultVec)==3)  {	
			prr<-AbPval(dim(x)[1], resultVec[3])
			outVec<-c(Eta=resultVec[1],Beta=resultVec[2],Rsqr=resultVec[3], AbPval=prr[[1]])
            # double prr[[1]] brackets are needed here of the naming of the number gets messed up in outVec
		}else{		
			outVec<-c(Eta=resultVec[1],Beta=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])	
		}		
	}else{			
		if(casenum < 8) {		
			if(length(resultVec)==3)  {	
				prr<-AbPval(length(x[,1]), resultVec[3],"lnorm")
				outVec<-c(Mulog=resultVec[1],Sigmalog=resultVec[2],Rsqr=resultVec[3], AbPval=prr[[1]])	
			}else{	
				outVec<-c(Mulog=resultVec[1],Sigmalog=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])
			}	
		}else{		
			if(length(resultVec)==3)  {	
				outVec<-c(Etalog=resultVec[1],Betalog=resultVec[2],Rsqr=resultVec[3])
			}else{	
				outVec<-c(Etalog=resultVec[1],Betalog=resultVec[2], t0=resultVec[3],Rsqr=resultVec[4])
			}	
		}		
	}			
				
return(outVec)				
}				
