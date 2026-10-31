#include <Rcpp.h>
#include <cmath>
#include <algorithm>
using namespace Rcpp;
RcppExport SEXP _SPADES_IND_audit_dist(SEXP q_, SEXP s_, SEXP qn_, SEXP sn_, SEXP numeric_) {
 BEGIN_RCPP
 IntegerMatrix q(q_),s(s_);NumericMatrix qn(qn_),sn(sn_);LogicalVector num(numeric_);
 int n=q.nrow(),m=s.nrow(),k=q.ncol();
 if(m<5||k<1||s.ncol()!=k||qn.nrow()!=n||sn.nrow()!=m||qn.ncol()!=k||sn.ncol()!=k||num.size()!=k) stop("Invalid distance dimensions");
 NumericMatrix out(n,8);
 for(int i=0;i<n;i++) {
  if(i%25==0) checkUserInterrupt();
  double bh[5]={2,2,2,2,2},bg[5]={2,2,2,2,2},exact=0,near=0,kh=0,kg=0;
  for(int j=0;j<m;j++) {
   double h=0,g=0;
   for(int c=0;c<k;c++){double neq=q(i,c)!=s(j,c);h+=neq;g+=num[c]?std::min(1.0,std::abs(qn(i,c)-sn(j,c))):neq;}
   h/=k;g/=k;exact+=(h==0);near+=(h<=1.0/k);kh+=std::exp(-h/.125);kg+=std::exp(-g/.125);
   for(int t=0;t<5;t++)if(h<bh[t]){for(int u=4;u>t;u--)bh[u]=bh[u-1];bh[t]=h;break;}
   for(int t=0;t<5;t++)if(g<bg[t]){for(int u=4;u>t;u--)bg[u]=bg[u-1];bg[t]=g;break;}
  }
  out(i,0)=bh[0];out(i,1)=bg[0];double mh=0,mg=0;for(int t=0;t<5;t++){mh+=bh[t];mg+=bg[t];}
  out(i,2)=mh/5;out(i,3)=mg/5;out(i,4)=exact/m;out(i,5)=near/m;out(i,6)=kh/m;out(i,7)=kg/m;
 }
 return out;
 END_RCPP
}
