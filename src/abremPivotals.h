#ifndef _abremPivotals_H
#define _abremPivotals_H

struct AbPval{
	double Pval,CCC2;
};

#ifdef __cplusplus

#include <RcppArmadillo.h>

RcppExport SEXP LSLRw2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRw2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRln2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRln2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRg2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRg2pYonX (SEXP arg1, SEXP arg2);

RcppExport SEXP LSLR(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4);
RcppExport SEXP pivotalMC(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6,SEXP arg7,SEXP arg8);	
RcppExport SEXP CallgetCCC2(SEXP arg1, SEXP model);
RcppExport SEXP CallgetPvalue(SEXP arg1, SEXP arg2, SEXP arg3);

extern "C" double getCCC2( int F, int model);
extern "C" struct AbPval getPvalue(int F, double R2, int model);


#endif
#endif
