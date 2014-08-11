## getProbabilityPlottingPositions.r file
 ##
 ## Author: Jacob T. Ormerod
 ##   (c)2014 OpenReliability.org
##

getProbabilityPlottingPositions <- function(x, s=NULL, interval=NULL, ppos="Benard", aranks="Johnson", ties=NULL)  {							
	fail  <-  length(x)
	n  <-  fail+length(s)
	##  create the event vector						
	if(!missing(s)) {						
	## if(length(s)>0)  {						
		## suspension data has been provided					
		    data<-c(x,s)					
		    event<-c(rep(1,fail),rep(0,n-fail))					
		    prep_df<-data.frame(data=data,event=event)					
		## now sort the dataframe on data values					
		    NDX<-order(prep_df[,1])					
		    prep_df<-prep_df[NDX,]					
	  }else{						
		## this is simply a complete failure set					
		    data<-sort(x)					
		    event<-rep(1,fail)					
		    prep_df<-data.frame(data=data,event=event)					
	  }						
							
	if(tolower(aranks)=="johnson")  {						
		## adjust ranks using Drew Auth's simplification of Leonard Johnson's method					
		## start with extra element to reference zero as previous rank to first					
		adj_rank<-0					
		for(k in 1:n)  {					
			rr <- n-k+1				
			if(prep_df$event[k]>0)  {				
			this_rank<-(rr*adj_rank[k]+n+1)/(rr+1)				
			adj_rank<-c(adj_rank, this_rank)				
			}else{				
			adj_rank<-c(adj_rank,adj_rank[k])				
			}				
		}					
		prep_df<-cbind(prep_df,adj_rank=adj_rank[-1])					
		## now eliminate the suspension data					
		prep_df<-prep_df[sapply(prep_df$event, function(x) x>0),c(1,3)]					
	}else{						
		if(tolower(aranks)=="kmestimator")  {					
		## adjust ranks using David Silkworth's adaptation of the modified					
		## Kaplan-Meier estimator used by Minitab as "nonparametric"					
			## start with extra element to reference zero as previous rank to first				
			adj_rank<-0				
			for(k in 1:n)  {				
				if(prep_df$event[k]>0)  {			
					this_rank<-1-((1-adj_rank[k])*(n-k)/(n-k+1))		
				adj_rank<-c(adj_rank, this_rank)			
				}else{			
				adj_rank<-c(adj_rank,adj_rank[k])			
				}			
			}				
			prep_df<-cbind(prep_df,adj_rank=adj_rank[-1])				
			## now eliminate the suspension data				
			prep_df<-prep_df[sapply(prep_df$event, function(x) x>0),c(1,3)]				
			## Now provide a modification for the final element if it was a failure
			## This adjustment is used by Minitab
			if(prep_df$adj_rank[fail]==1)  {				
				prep_df$adj_rank[fail]=1-((1-prep_df$adj_rank[fail-1])*1/10)			
			}				
			## Finally reverse the Kaplan-Meier plotting position to reveal useable
			## adj_rank	for any plotting position method.			
			prep_df$adj_rank<-prep_df$adj_rank*n
		}else{	
			stop("aranks argument not recognized")		
		}					
	}						
							
	## now to handle ties, if called for						
	if(!missing(ties))  {						
	## if(length(ties)>0) {						
		test_hi<-prep_df$data-c(prep_df$data[-1],0)					
		test_lo<-prep_df$data-c(1,prep_df$data[1:(fail-1)])					
		highest<-prep_df[sapply(test_hi, function(y) y!=0),]					
		lowest<-prep_df[sapply(test_lo, function(y) y!=0),]					
		if(tolower(ties)=="highest")  {					
			prep_df<-highest				
		}else{					
			if(tolower(ties)=="lowest")  {				
				prep_df<-lowest			
			}else{				
				if(tolower(ties)=="mean")  {			
					prep_df<-data.frame(data=highest$data,adj_rank=(highest$adj_rank+lowest$adj_rank)/2)		
				}else{			
					if(tolower(ties)=="sequential")  {		
						seq_adj<-cumsum(highest$adj_rank-lowest$adj_rank)	
						prep_df<-data.frame(data=highest$data, adj_rank=highest$adj_rank-seq_adj)	
					}else{		
						stop("ties argument not recognized")	
					}		
				}			
			}				
		}					
	}						
							
	## special note:  the number of data entries n remains as originally identified						
	## if CANNOT be changed due to removal of suspensions or ties						
							
	## prep_df now contains the data to be plotted with their final adjusted ranks						
	## finally we get the probability plotting positions						
    if(any(tolower(ppos) %in% c("benard" , "beta", "mean"))){							
		if(tolower(ppos)=="benard")  {
			ppp<-(prep_df$adj_rank-0.3)/(n+0.4)				
		}else{					
			## the incomplete beta function is what				
			## Benard was approximating				
			## we can simply use this directly				
			if(tolower(ppos)=="beta")  {
				ppp<-qbeta(0.5, prep_df$adj_rank,n-prep_df$adj_rank+1)			
			}else{				
				if(tolower(ppos)=="mean")  {
					## mean ranks (aka Herd-Johnson) give even spacing 		
					## mean ranks were originally proposed by Weibull, but later		
					## he agreed that the Benard approximation gave		
					## more pleasing results when used with fatigue failure data		
					ppp<-prep_df$adj_rank/(n+1)		
				}			
			}				
		}					
	}else{						
		if(any(tolower(ppos) %in% c("hazen", "kaplan-meier", "km", "blom")))  {
			if(tolower(ppos)=="hazen")  {
				## aka modified Kaplan-Meier			
				ppp <- (prep_df$adj_rank-0.5)/n			
			}else{				
				if(any(c("kaplan-meier","km") %in% tolower(ppos)))  {
				## This is method used by SuperSMITH with Johnson aranks			
					ppp <- (prep_df$adj_rank)/n		
					K <- length(prep_df[,1])		
					if(prep_df$adj_rank[K]==n)  {		
						ppp[K] <- K/(n+0.001)	
					}		
				}else{			
					if(tolower(ppos)=="blom")  {
						ppp <- (prep_df$adj_rank-0.375)/(n+0.25)	
					}		
				}			
			}				
		}else{
			stop("ppos argument not recognized")
		}		
	}						
							
	outDF<-cbind(prep_df$data,data.frame(ppp),prep_df$adj_rank)						
	colnames(outDF)<-c("time","ppp","adj_rank")	
return(outDF)							
}							
## assign the alias							
getPPP<-getProbabilityPlottingPositions							
