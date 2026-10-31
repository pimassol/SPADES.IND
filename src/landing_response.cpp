#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;
struct ResponseTerm { std::vector<int> id, count; std::vector<double> target, ge; int levels; bool conditional; double value; };
RcppExport SEXP _SPADES_IND_sa_response(SEXP argsSEXP) {
 BEGIN_RCPP
 RNGScope scope;
 List args(argsSEXP);
 IntegerMatrix X=args["X"]; List specs=args["specs"];
 IntegerVector group=args["group"], initial=args["initial"];
 NumericVector gw=args["gw"], predictions=args["predictions"], lo=args["lo"], hi=args["hi"];
 NumericVector ys=clone(as<NumericVector>(args["initial_y"]));
 NumericVector initialY=clone(ys);
 bool integer_y=as<bool>(args["integer_y"]), rmse=as<bool>(args["rmse"]), verbose=as<bool>(args["verbose"]);
 int L=as<int>(args["L"]), pilot=as<int>(args["pilot"]), log_every=as<int>(args["log_every"]);
 double refacc=as<double>(args["reference"]), utility_scale=as<double>(args["scale"]);
 auto loss=[&](double y,int row)->double{double e=y-predictions[row];return rmse?e*e:std::abs(e);};
 auto draw=[&](int row)->double{
   if(integer_y)return lo[row]+std::min(hi[row]-lo[row],std::floor(R::unif_rand()*(hi[row]-lo[row]+1)));
   double y=lo[row]+R::unif_rand()*(hi[row]-lo[row]);
   // Avoid a lower-bound roundoff crossing in right-closed quantile bins.
   if(y<=lo[row])y=std::nextafter(lo[row],hi[row]);
   return y;
 };
 int M=X.nrow(), n=initial.size(), G=gw.size(), J=specs.size();
 std::vector<int> idx(n), outside, ng(G,0); std::vector<bool> selected(M,false);
 for(int i=0;i<n;++i){idx[i]=initial[i]-1;selected[idx[i]]=true;ng[group[idx[i]]]++;}
 for(int i=0;i<M;++i)if(!selected[i])outside.push_back(i);
 std::vector<ResponseTerm> terms(J);
 auto gerror=[&](ResponseTerm &t,int g)->double {
   if(ng[g]==0)return 1.0;
   double z=0;for(int v=0;v<t.levels;++v){int c=g*t.levels+v;z+=std::abs(double(t.count[c])/ng[g]-t.target[c]);}
   return .5*z;
 };
 for(int j=0;j<J;++j){ List s=specs[j]; ResponseTerm &t=terms[j];
   t.id=as<std::vector<int>>(s["id"]);t.target=as<std::vector<double>>(s["target"]);
   t.conditional=as<bool>(s["conditional"]);t.levels=as<int>(s["levels"]);
   t.count.assign(t.target.size(),0);for(int a:idx)t.count[t.id[a]]++;
   t.value=0;
   if(t.conditional){t.ge.resize(G);for(int g=0;g<G;++g){t.ge[g]=gerror(t,g);t.value+=gw[g]*t.ge[g];}}
   else for(size_t c=0;c<t.count.size();++c)t.value+=.5*std::abs(double(t.count[c])/n-t.target[c]);
 }
 double sumacc=0;for(int pos=0;pos<n;++pos)sumacc+=loss(ys[pos],idx[pos]);
 auto fmean=[&]()->double{double f=0;for(ResponseTerm &t:terms)f+=t.value;return f/J;};
 auto utility=[&](double a)->double{return std::abs((rmse?std::sqrt(std::max(0.0,a)):a)-refacc)/utility_scale;};
 auto total=[&](double f,double a)->double{return f+utility(a);};
 double F=fmean(), Finitial=F, Ainitial=sumacc/n;
 std::vector<double> initialTerms(J);for(int j=0;j<J;++j)initialTerms[j]=terms[j].value;
 std::vector<double> oldval(J),newval(J),newg0(J),newg1(J),deltas;
 double T0=0,Tf=0;int accepted=0;List trace;
 for(int step=-pilot;step<L;++step){
   if(step%1000==0)checkUserInterrupt();
   int pos=std::min(n-1,int(R::unif_rand()*n));int op=outside.empty()?-1:std::min(int(outside.size())-1,int(R::unif_rand()*outside.size()));
   int a=idx[pos],b=op<0?a:outside[op],g0=group[a],g1=group[b];
   double proposed_y=draw(b);
   double ap=std::max(0.0,sumacc-loss(ys[pos],a)+loss(proposed_y,b));
   if(g0!=g1){ng[g0]--;ng[g1]++;}
   for(int j=0;j<J;++j){ResponseTerm &t=terms[j];int ca=t.id[a],cb=t.id[b];oldval[j]=t.value;double v=t.value;
     if(ca!=cb){
       if(!t.conditional)v-=.5*(std::abs(double(t.count[ca])/n-t.target[ca])+std::abs(double(t.count[cb])/n-t.target[cb]));
       t.count[ca]--;t.count[cb]++;
       if(!t.conditional)v+=.5*(std::abs(double(t.count[ca])/n-t.target[ca])+std::abs(double(t.count[cb])/n-t.target[cb]));
     }
     if(t.conditional){newg0[j]=gerror(t,g0);v+=gw[g0]*(newg0[j]-t.ge[g0]);
       if(g1!=g0){newg1[j]=gerror(t,g1);v+=gw[g1]*(newg1[j]-t.ge[g1]);}}
     newval[j]=v;t.value=v;
   }
   double Fp=fmean(), H_old=total(F,sumacc/n), H_new=total(Fp,ap/n),delta=H_new-H_old;
   if(step<0)deltas.push_back(delta);
   double T=step<0?0:T0*std::pow(Tf/T0,double(step)/std::max(1,L-1));
   bool take=step>=0 && (std::log(R::unif_rand())<std::min(0.0,-delta/T));
   if(take){idx[pos]=b;ys[pos]=proposed_y;if(op>=0)outside[op]=a;F=Fp;sumacc=ap;accepted++;
     for(int j=0;j<J;++j)if(terms[j].conditional){terms[j].ge[g0]=newg0[j];if(g1!=g0)terms[j].ge[g1]=newg1[j];}
   }else{
     if(g0!=g1){ng[g0]++;ng[g1]--;}
     for(int j=0;j<J;++j){ResponseTerm&t=terms[j];t.count[t.id[a]]++;t.count[t.id[b]]--;t.value=oldval[j];}
   }
   if(step==-1){std::vector<double> positive;for(double d:deltas)if(d>1e-14)positive.push_back(d);
     double scale=1.0/n;if(positive.size()){std::sort(positive.begin(),positive.end());int m=positive.size();scale=m%2?positive[m/2]:.5*(positive[m/2-1]+positive[m/2]);}
     T0=scale/(-std::log(.8));Tf=T0*.001;
     if(verbose) Rcout<<"Temperature pilot completed. T0="<<T0<<" Tf="<<Tf<<" F_mean="<<F<<" Mean_score="<<sumacc/n<<"\n";
   }
   int it=step+1;
   if(step>=0&&(it%log_every==0||it==L)){
     // Independent count, fidelity and accuracy checks.
     double ac=0;for(int pos=0;pos<n;++pos)ac+=loss(ys[pos],idx[pos]);if(std::abs(ac-sumacc)>1e-8*(1+std::abs(ac)))stop("Response loss update mismatch.");
     for(ResponseTerm&t:terms){std::vector<int> check(t.count.size(),0);for(int z:idx)check[t.id[z]]++;if(check!=t.count)stop("Count update mismatch.");
       double v=0;if(t.conditional){for(int g=0;g<G;++g)v+=gw[g]*gerror(t,g);}else for(size_t c=0;c<t.count.size();++c)v+=.5*std::abs(double(t.count[c])/n-t.target[c]);
       if(std::abs(v-t.value)>1e-8)stop("Fidelity update mismatch.");}
     if(verbose) Rcout<<it<<" / "<<L<<" | F_mean "<<F<<" | mean_score "<<sumacc/n
       <<" | gap "<<utility(sumacc/n)<<" | H "<<total(F,sumacc/n)
       <<" | acceptance "<<double(accepted)/it<<"\n";
     NumericVector row=NumericVector::create(_["iter"]=it,_["F_mean"]=F,_["mean_score"]=sumacc/n,
       _["utility"]=utility(sumacc/n),_["H"]=total(F,sumacc/n),
       _["accepted_last_proposal"]=double(take),_["T"]=T,_["accepted"]=accepted);
     trace.push_back(row);


   }
 }
 IntegerVector bi(n);for(int i=0;i<n;++i)bi[i]=idx[i]+1;
 return List::create(_["response_values"]=ys,_["initial_response_values"]=initialY,_["indices"]=bi,_["F"]=F,_["mean_score"]=sumacc/n,
  _["initial"]=NumericVector::create(_["F"]=Finitial,_["mean_score"]=Ainitial),
  _["initial_terms"]=wrap(initialTerms),_["trace"]=trace,_["T0"]=T0,_["Tf"]=Tf,_["pilot"]=wrap(deltas));
 END_RCPP
}
