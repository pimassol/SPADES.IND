#include <Rcpp.h>
#include <R_ext/Rdynload.h>
using namespace Rcpp;
List spades_sa_cpp(IntegerMatrix X, List specs, IntegerVector group, NumericVector gw,
 NumericVector correct, double refacc, IntegerVector initial, int L, int pilot,
 int log_every, std::string checkpoint, Function savefun, bool regression, double utility_scale, bool verbose);
RcppExport SEXP _SPADES_IND_spades_sa_cpp(SEXP XSEXP, SEXP specsSEXP, SEXP groupSEXP, SEXP gwSEXP, SEXP correctSEXP, SEXP refaccSEXP, SEXP initialSEXP, SEXP LSEXP, SEXP pilotSEXP, SEXP log_everySEXP, SEXP checkpointSEXP, SEXP savefunSEXP, SEXP regressionSEXP, SEXP utility_scaleSEXP, SEXP verboseSEXP) {
 BEGIN_RCPP
 Rcpp::RNGScope scope;
 Rcpp::traits::input_parameter< IntegerMatrix >::type X(XSEXP);
 Rcpp::traits::input_parameter< List >::type specs(specsSEXP);
 Rcpp::traits::input_parameter< IntegerVector >::type group(groupSEXP);
 Rcpp::traits::input_parameter< NumericVector >::type gw(gwSEXP);
 Rcpp::traits::input_parameter< NumericVector >::type correct(correctSEXP);
 Rcpp::traits::input_parameter< double >::type refacc(refaccSEXP);
 Rcpp::traits::input_parameter< IntegerVector >::type initial(initialSEXP);
 Rcpp::traits::input_parameter< int >::type L(LSEXP);
 Rcpp::traits::input_parameter< int >::type pilot(pilotSEXP);
 Rcpp::traits::input_parameter< int >::type log_every(log_everySEXP);
 Rcpp::traits::input_parameter< std::string >::type checkpoint(checkpointSEXP);
 Rcpp::traits::input_parameter< Function >::type savefun(savefunSEXP);
 Rcpp::traits::input_parameter< bool >::type regression(regressionSEXP);
 Rcpp::traits::input_parameter< double >::type utility_scale(utility_scaleSEXP);
 Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
 return Rcpp::wrap(spades_sa_cpp(X, specs, group, gw, correct, refacc, initial, L, pilot, log_every, checkpoint, savefun, regression, utility_scale, verbose));
 END_RCPP
}
RcppExport SEXP _SPADES_IND_audit_dist(SEXP,SEXP,SEXP,SEXP,SEXP);
RcppExport SEXP _SPADES_IND_sa_response(SEXP);
static const R_CallMethodDef CallEntries[] = {
 {"_SPADES_IND_sa_response", (DL_FUNC) &_SPADES_IND_sa_response, 1},
 {"_SPADES_IND_audit_dist", (DL_FUNC) &_SPADES_IND_audit_dist, 5},
 {"_SPADES_IND_spades_sa_cpp", (DL_FUNC) &_SPADES_IND_spades_sa_cpp, 15},
 {NULL,NULL,0}
};
RcppExport void R_init_SPADES_IND(DllInfo *dll) { R_registerRoutines(dll,NULL,CallEntries,NULL,NULL);R_useDynamicSymbols(dll,FALSE); }
