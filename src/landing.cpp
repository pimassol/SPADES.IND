#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;
struct Term { std::vector<int> id, count; std::vector<double> target, ge; int levels; bool conditional; double value; };
// [[Rcpp::export]]
List spades_sa_cpp(IntegerMatrix X, List specs, IntegerVector group, NumericVector gw,
 NumericVector correct, double refacc, IntegerVector initial, int L, int pilot,
 int log_every, std::string checkpoint, Function savefun, bool regression, double utility_scale, bool verbose) {
 int M=X.nrow(), n=initial.size(), G=gw.size(), J=specs.size();
 std::vector<int> idx(n), outside, ng(G,0); std::vector<bool> selected(M,false);
 for(int i=0;i<n;++i){idx[i]=initial[i]-1;selected[idx[i]]=true;ng[group[idx[i]]]++;}
 for(int i=0;i<M;++i)if(!selected[i])outside.push_back(i);
 std::vector<Term> terms(J);
 auto gerror=[&](Term &t,int g)->double {
   if(ng[g]==0)return 1.0;
   double z=0;for(int v=0;v<t.levels;++v){int c=g*t.levels+v;z+=std::abs(double(t.count[c])/ng[g]-t.target[c]);}
   return .5*z;
 };
 for(int j=0;j<J;++j){ List s=specs[j]; Term &t=terms[j];
   t.id=as<std::vector<int>>(s["id"]);t.target=as<std::vector<double>>(s["target"]);
   t.conditional=as<bool>(s["conditional"]);t.levels=as<int>(s["levels"]);
   t.count.assign(t.target.size(),0);for(int a:idx)t.count[t.id[a]]++;
   t.value=0;
   if(t.conditional){t.ge.resize(G);for(int g=0;g<G;++g){t.ge[g]=gerror(t,g);t.value+=gw[g]*t.ge[g];}}
   else for(size_t c=0;c<t.count.size();++c)t.value+=.5*std::abs(double(t.count[c])/n-t.target[c]);
 }
 double sumacc=0;for(int a:idx)sumacc+=correct[a];
 auto fmean=[&]()->double{double f=0;for(Term &t:terms)f+=t.value;return f/J;};
 auto utility=[&](double a)->double{return std::abs((regression?std::sqrt(std::max(0.0,a)):a)-refacc)/utility_scale;};
 auto total=[&](double f,double a)->double{return f+utility(a);};
 double F=fmean(), Finitial=F, Ainitial=sumacc/n;
 std::vector<double> initialTerms(J);for(int j=0;j<J;++j)initialTerms[j]=terms[j].value;
 std::vector<double> oldval(J),newval(J),newg0(J),newg1(J),deltas;
 double T0=0,Tf=0;int accepted=0;List trace;int tracepos=0;
 for(int step=-pilot;step<L;++step){
   if(step%1000==0)checkUserInterrupt();
   int pos=std::min(n-1,int(R::unif_rand()*n));int op=std::min(int(outside.size())-1,int(R::unif_rand()*outside.size()));
   int a=idx[pos],b=outside[op],g0=group[a],g1=group[b];
   double ap=sumacc-correct[a]+correct[b];
   if(g0!=g1){ng[g0]--;ng[g1]++;}
   for(int j=0;j<J;++j){Term &t=terms[j];int ca=t.id[a],cb=t.id[b];oldval[j]=t.value;double v=t.value;
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
   if(take){idx[pos]=b;outside[op]=a;F=Fp;sumacc=ap;accepted++;
     for(int j=0;j<J;++j)if(terms[j].conditional){terms[j].ge[g0]=newg0[j];if(g1!=g0)terms[j].ge[g1]=newg1[j];}
   }else{
     if(g0!=g1){ng[g0]++;ng[g1]--;}
     for(int j=0;j<J;++j){Term&t=terms[j];t.count[t.id[a]]++;t.count[t.id[b]]--;t.value=oldval[j];}
   }
   if(step==-1){std::vector<double> positive;for(double d:deltas)if(d>1e-14)positive.push_back(d);
     double scale=1.0/n;if(positive.size()){std::sort(positive.begin(),positive.end());int m=positive.size();scale=m%2?positive[m/2]:.5*(positive[m/2-1]+positive[m/2]);}
     T0=scale/(-std::log(.8));Tf=T0*.001;
     if(verbose) Rcout<<"Temperature pilot completed. T0="<<T0<<" Tf="<<Tf<<" F_mean="<<F<<" Mean_score="<<sumacc/n<<"\n";
   }
   int it=step+1;
   if(step>=0&&(it%log_every==0||it==L)){
     // Independent count, fidelity and accuracy checks.
     double ac=0;for(int z:idx)ac+=correct[z];if(std::abs(ac-sumacc)>1e-8*(1+std::abs(ac)))stop("Accuracy update mismatch.");
     for(Term&t:terms){std::vector<int> check(t.count.size(),0);for(int z:idx)check[t.id[z]]++;if(check!=t.count)stop("Count update mismatch.");
       double v=0;if(t.conditional){for(int g=0;g<G;++g)v+=gw[g]*gerror(t,g);}else for(size_t c=0;c<t.count.size();++c)v+=.5*std::abs(double(t.count[c])/n-t.target[c]);
       if(std::abs(v-t.value)>1e-8)stop("Fidelity update mismatch.");}
     if(verbose) Rcout<<it<<" / "<<L<<" | F_mean "<<F<<" | mean_score "<<sumacc/n
       <<" | gap "<<utility(sumacc/n)<<" | H "<<total(F,sumacc/n)
       <<" | acceptance "<<double(accepted)/it<<"\n";
     NumericVector row=NumericVector::create(_["iter"]=it,_["F_mean"]=F,_["mean_score"]=sumacc/n,
       _["utility"]=utility(sumacc/n),_["H"]=total(F,sumacc/n),
       _["accepted_last_proposal"]=double(take),_["T"]=T,_["accepted"]=accepted);
     trace.push_back(row);tracepos++;
     IntegerVector ci(n);for(int i=0;i<n;++i)ci[i]=idx[i]+1;
     savefun(List::create(_["iter"]=it,_["current_indices"]=ci,_["trace"]=trace),checkpoint);
   }
 }
 IntegerVector bi(n);for(int i=0;i<n;++i)bi[i]=idx[i]+1;
 return List::create(_["indices"]=bi,_["F"]=F,_["mean_score"]=sumacc/n,
  _["initial"]=NumericVector::create(_["F"]=Finitial,_["mean_score"]=Ainitial),
  _["initial_terms"]=wrap(initialTerms),_["trace"]=trace,_["T0"]=T0,_["Tf"]=Tf,_["pilot"]=wrap(deltas));
}
