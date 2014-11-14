lslr <- function(x, dist="weibull", npar=2, reg_method="xony")  {				
	## a convergence limit is fixed here for 3rd parameter  convergence			
	## no longer an argument for the R function, but still an argument to C++ functions			
	limit<-1e-5	
	
	if(is.vector(x))  {				
		stop("use MRR functions for casual fitting, or pre-process with getPPP")			
	}else{				
		if(names(x)[1]=="time"&&names(x)[2]=="ppp")  {			
		## will handle the output from getPPP			
		## this format is also generated in abrem::calculateSingleFit			
		}else{			
			stop("dataframe argument format not recognized")		
		}			
	}				
## any additional validations, such as positive numeric checking					
## removal of potential na's, etc. could take place here					
	if(anyNA(x))  {				
	stop("NA not handled in lslr argument data")				
	}				
	if(any(c(x$time,x$ppp)<=0)) {				
	stop("negative values or zero in lslr argument data")				
	}				
	if(any(c(x$ppp)>=1)) {				
	stop("invalid percentile value in ppp")				
	}				
					
## A rather extensive validation is performed on dist and npar arguments here					
## The npar tests may be removed if we dispense with npar as an argument					
## in the mean time it is permissible to enter dist suffixes with 2p or 3p					
					
	if(!any(tolower(dist) %in% 					
		c("weibull","weibull2p","weibull3p","lognormal","lognormal2p",					
		"lognormal3p","gumbel","gumbel2p","gumbel3p"))){					
		stop("dist argument not recognized ")					
	}
	
	if(tolower(dist)=="weibull3p" )  {					
		dist="weibull"				
		if(npar!=3)  {				
			npar=3			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="weibull2p" )  {					
		dist="weibull"				
		if(npar!=2)  {				
			npar=2			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="weibull" )  {
		dist="weibull"		
		if(npar!=2 && npar!=3)  {				
			stop("npar not valid for distribution")			
		}				
	}					
						
	if(tolower(dist)=="lognormal3p" )  {					
		dist="lognormal"				
		if(npar!=3)  {				
			npar=3			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="lognormal2p" )  {					
		dist="lognormal"				
		if(npar!=2)  {				
			npar=2			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="lognormal" )  {
		dist="lognormal"	
		if(npar!=2 && npar!=3)  {				
			stop("npar not valid for distribution")			
		}				
	}					
						
	if(tolower(dist)=="gumbel3p" )  {					
		dist="gumbel"				
		if(npar!=3)  {				
			npar=3			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="gumbel2p" )  {					
		dist="gumbel"				
		if(npar!=2)  {				
			npar=2			
			warning("npar argument overridden by dist name")			
		}				
	}					
						
	if(tolower(dist)=="gumbel" )  {	
		dist="gumbel"
		if(npar!=2 && npar!=3)  {				
			stop("npar not valid for distribution")			
		}				
	}					
					
## it is okay not to qualify casenum=0 for dist=="weibull" now					
## no need to continue tolower testing either					
	casenum<-0			
	if(tolower(reg_method)=="yonx") casenum=casenum+1
	if(npar==3) casenum=casenum+2			
	if(dist=="lognormal") casenum=casenum+4
	if(dist=="gumbel") casenum=casenum+8
	
	## return the .Call to original form			
	resultVec<-.Call("LSLR", x$time, x$ppp, limit, casenum , package="abremPivotals")			

## all modifications for 0.2.10 accepted below				
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
