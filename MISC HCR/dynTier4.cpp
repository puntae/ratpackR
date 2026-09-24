#include <TMB.hpp>


template<class Type>
Type posfun(Type x, Type eps, Type &pen)
{
  if ( x >= eps ){
    return x;
  } else {
    pen += Type(0.01) * pow(x-eps,2);
    return eps/(Type(2.0)-x/eps);
  }
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data passed in
  DATA_IVECTOR(Year);
  DATA_VECTOR(C);
  DATA_MATRIX(I);
  int n = C.size();
  int Yr1 = Year(0);
  DATA_INTEGER(EstR);
  DATA_INTEGER(EstZ);
  DATA_SCALAR(MSYL);
  DATA_SCALAR(MSYINPUT);
  DATA_INTEGER(MSYRY1);
  DATA_INTEGER(MSYRY2);
  DATA_INTEGER(Ncpue);
  DATA_SCALAR(CVMSYL);
  DATA_INTEGER(MSYPriorY1);
  DATA_INTEGER(MSYPriorY2);
  DATA_SCALAR(PriorMeanR);
  DATA_SCALAR(PriorSDr);
  DATA_INTEGER(ProcessError);
  DATA_INTEGER(IsDymB0);

  // Potential estimated parameters
  PARAMETER(logR);
  PARAMETER(logK);
  PARAMETER_VECTOR(logQ);
  PARAMETER(logz);
  PARAMETER(logSigma);
  PARAMETER_VECTOR(FF);
  PARAMETER_VECTOR(Rec_dev);
  PARAMETER(logSigmaR);

  // Extract the parameters
  Type k = exp(logK);
  Type z = exp(logz);
  vector<Type> q = exp(logQ);
  Type sigma = exp(logSigma);
  Type SigmaR = exp(logSigmaR);
  if (ProcessError==0) SigmaR = 0;

  // Is MSYL (BMSY/B0) calculated from z (1) or pre-specified (0)
  Type MSYLOut;
  if (EstZ==1) MSYLOut = pow((1.0/(z+1.0)),(1.0/z)); else MSYLOut = MSYL;

  // Is R estimated (1) or computed from MSYINPUT (0)
  Type BMSY;
  Type r;
  Type MSY;
  BMSY = MSYLOut*k;
  if (EstR==1) {  r = exp(logR); } else {  r = MSYINPUT / (BMSY*(1.0-pow(MSYLOut,z)));   }
  MSY = r*BMSY*(1.0-pow(MSYLOut,z));

  // End of years
  int n1 = 0;
  n1 = n + 1;

  Type f;
  Type CompPrior1;
  Type CompPrior2;
  Type CompPrior3;

  // Declare variables
  vector<Type> B(n1);
  matrix<Type> Ihat(n,Ncpue);
  vector<Type> Chat(n);
  vector<Type> ExpOut(n);
  vector<Type> B0(n1+50);

  // project model forward
  B(0) = k;
  B0(0) = k;
  for(int t=0; t<n; t++)
  {
    Type Expl = 1.0/(1.0+exp(-FF(t)));
    B(t+1) = B(t) + r*B(t)*(1-pow((B(t)/k),z)) - Expl*B(t);
    B(t+1) = B(t+1)*exp(Rec_dev(t)*SigmaR-SigmaR*SigmaR/2.0);
    if (IsDymB0==1)
     {
      B0(t+1) = B0(t) + r*B0(t)*(1-pow((B0(t)/k),z));
      B0(t+1) = B0(t+1)*exp(Rec_dev(t)*SigmaR-SigmaR*SigmaR/2.0);
     }
    else
     B0(t+1) = k;
    Chat(t) = Expl*B(t);
    ExpOut(t) = Expl;
    // Predicted CPUE
    for (int Icpue=0;Icpue<Ncpue;Icpue++) Ihat(t,Icpue) = q(Icpue)*(B(t)+B(t+1))/2.0;
  }
  f -= sum(dnorm(log(C), log(Chat), Type(0.05), true));

 // project model under B0
  for(int t=n; t<n+50; t++) B0(t+1) = B0(t) + r*B0(t)*(1-pow((B0(t)/k),z));

  // Negative log-likelihood for the cpue indices
  for (int Icpue=0;Icpue<Ncpue;Icpue++)
   for(int t=0; t<n; t++)
    if (I(t,Icpue) > 0)
     f -= dnorm(log(I(t,Icpue)), log(Ihat(t,Icpue)), sigma, true);

  // Can fit to mean biomass relative to MSYL
  Type BioAve = 0;
  for(int t=MSYRY1-Yr1; t<=MSYRY2-Yr1; t++) BioAve += B(t);
  BioAve = BioAve / float(MSYRY2-MSYRY1+1);

  // Penalty on that mean biomass matches MSYL
  CompPrior1 = 1.0/(CVMSYL*CVMSYL)*(log(BioAve) - log(BMSY))*(log(BioAve) - log(BMSY));

  // Prior on r
  CompPrior2 = dnorm(r,Type(PriorMeanR),Type(PriorSDr),true);

  // Prior on rec_devs
  CompPrior3 = 0;
  for (int t=0; t<n;t++)
   CompPrior3 += dnorm(Rec_dev(t),Type(0.0),Type(1.0),true);

  // Add penalties
  f += CompPrior1;  f -= CompPrior2; f -= CompPrior3;

  ADREPORT(r);
  ADREPORT(k);
  ADREPORT(z);
  ADREPORT(q);
  ADREPORT(MSY);
  ADREPORT(MSYLOut);
  ADREPORT(MSYLOut*k);
  ADREPORT(BioAve);
  ADREPORT(sigma);
  //ADREPORT(log(B)); // uncertainty
  REPORT(f);            // plot
  REPORT(B);            // plot
  REPORT(B0);            // plot
  REPORT(Chat);         // plot
  REPORT(ExpOut);       // plot
  REPORT(Ihat);         // plot

  return f;
}




// dvariable posfun(const dvariable&x,const double eps,dvariable& pen)
//     {
//       if (x>=eps) {
//         return x;
//       } else {
//         pen+=.01*square(x-eps);
//         return eps/(2-x/eps);
// } }


