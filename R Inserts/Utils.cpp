#include <RcppArmadillo.h>
// #include <Rcpp.h>

  // [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// ==============================================================================================

//' @title SetO -omputes the ogive corresponding to the given parameter
//'
// [[Rcpp::export]]
NumericVector SetO(double Sig, double Mean, int MaxAge)
{
 // Local variables
 NumericVector V(MaxAge+1);

 // Knife-edge function
 if (abs(Mean)<  0.0001)
  {
   for (int L=0;L<=MaxAge;L++) V(L) = 1;
   return(V);
  }

 // Knife-edged function
 if (Mean < 0)
  {
   for (int L=0;L<=MaxAge;L++)
    if (L > -1*Mean) V(L) = 0; else V(L) = 1;
   return(V);
  }

 // Knife-edged function
 if (Sig <=0)
  {
   for (int L=0;L<=MaxAge;L++)
    if (L < Mean) V(L) = 0; else V(L) = 1;
   return(V);
  }

 // real logistic
 for (int L=0;L<=MaxAge;L++)
  if ( ((float(L)-Mean)/Sig > 10) )
   V(L) = 1;
  else
   if ( ((float(L)-Mean)/Sig < -10) )
    V(L) = 0;
   else
    V(L) = 1.0/(1.0+exp(-(float(L)-Mean)/Sig));
 V(0) = 0;
 V(MaxAge-1) = 1;
 V(MaxAge) = 1;
 return(V);
}

// ==============================================================================================

//' @title Surv - computes survival as function of age
//'
// [[Rcpp::export]]
double Surv(NumericVector M, int A)
{
 // Local variables
 double Alpha,Beta,Surv2;

 if (M(2) != -1) { Surv2 = exp(-M(2)); return(Surv2); }

 Beta = (M(1) - M(0))/16.0;
 Alpha = M(0) - 4.0*Beta;
 if (A <= 4) Surv2 = exp(-M(0)); else Surv2 = exp(-(Alpha+Beta*float(A)));
 return(Surv2);
}

// ==============================================================================================

//' @title Trform - adjust an ogive to transition form, that is so that V(L) =
//'                 the proportion of animals in a given class at age A-1 which make the
//'                  transition to a different class age A
//'
// [[Rcpp::export]]
NumericVector Trform(NumericVector V, int MaxAge)
{
 // Local variables
 double RM,D;

 RM = V(0);
 for (int L=1;L<=MaxAge;L++)
  if (RM < 1.0)
   { D = RM; RM = V(L); V(L) = (RM - D)/(1.0 -D); }
  else
   { RM = 1.0; V(L) = 1.0; }

 return(V);
}

// ==============================================================================================

//' @title InitP: Solves for the initial exploitation rate
//'
//' @export
// [[Rcpp::export]]
List FUN2(double ROI, int J, List BiolPars, NumericVector KTOT)
 {

  // Local variables (passed)
  int MAXAGE, Nstk;
  double FEC;
  Nstk = as<int>(BiolPars["Nstk"]);
  MAXAGE = as<int>(BiolPars["MAXAGE"]);
  FEC = as<double>(BiolPars["FEC"]);

  NumericVector A(Nstk);
  A = as<NumericVector>(BiolPars["A"]);
  NumericVector Z(Nstk);
  Z = as<NumericVector>(BiolPars["Z"]);
  NumericVector FMATUR(MAXAGE+1);
  FMATUR = as<NumericVector>(BiolPars["FMATUR"]);
  NumericVector SUR(MAXAGE+1);
  SUR = as<NumericVector>(BiolPars["SUR"]);
  NumericVector SELFREF(MAXAGE+1);
  SELFREF = as<NumericVector>(BiolPars["SELFREF"]);
  NumericVector SELFREM(MAXAGE+1);
  SELFREM = as<NumericVector>(BiolPars["SELFREM"]);

  // Other local variables
  double B, B2, B3, P1PL, PMAT;
  NumericMatrix RM(MAXAGE+1,Nstk);
  NumericMatrix RF(MAXAGE+1,Nstk);

  //Calculate the numbers at age relative to the number of 0 yr olds {C.4}
  // NB Population is in equilibrium so males=females as SUR is sex independent
  RF(0,J) = 1.0; RM(0,J) = 1.0;
  for (int L=0;L<MAXAGE;L++)
   {
    RF(L+1,J) = SUR(L)*RF(L,J)*(1.0-ROI*SELFREF(L));
    RM(L+1,J) = SUR(L)*RM(L,J)*(1.0-ROI*SELFREM(L));
   }
  // Adjust for last age class being pooled (and fully recruited)
  RF(MAXAGE,J) = RF(MAXAGE,J) / (1.0-SUR(MAXAGE)*(1.0-ROI));
  RM(MAXAGE,J) = RM(MAXAGE,J) / (1.0-SUR(MAXAGE)*(1.0-ROI));

  //std::cout << RM << std::endl;
  // Add 1+ & mature female totals
  P1PL = 0; PMAT = 0;
  for (int L=1;L<=MAXAGE;L++)
   {
    P1PL = P1PL + (RM(L,J) + RF(L,J));
    PMAT = PMAT + RF(L,J)*FMATUR(L);
   }
  // Find no. of births B {5.5} and hence 1+ population (P1PL)
  B = 0;
  B3 = 1.0 - (1.0/(FEC*PMAT)-1.0)/A(J);
  if (B3 > 0)
   {
    B2 = KTOT(J) * pow(B3,(1.0/Z(J)));
    B = B2 / P1PL;
   }
  P1PL = P1PL*B;

  return List::create( _["B"]=B,_["P1PL"]=P1PL,_["RM"]=RM,_["RF"]=RF);
 }


// ==============================================================================================

//' @title InitP: Solves for the initial exploitation rate
//'
//' @export
// [[Rcpp::export]]
List InitP(double PPRJ, int J, List BiolPars, NumericMatrix RM, NumericMatrix RF,NumericVector KTOT)
 {

  // Local variables (passed)
  int MAXAGE, Nstk;
  MAXAGE = as<int>(BiolPars["MAXAGE"]);
  Nstk = as<int>(BiolPars["Nstk"]);


  // Local variables
  //NumericMatrix RM2(MAXAGE+1,Nstk);
  //NumericMatrix RF2(MAXAGE+1,Nstk);
  double ROI,P1PL,B,FMIN,FMAX;
  int Done;

  ROI = 0;
  List L1 = FUN2(ROI, J, BiolPars, KTOT);
  B = L1["B"];
  P1PL = L1["P1PL"];

  // Do we need to
  if (abs(P1PL-PPRJ) > 0.0000001*PPRJ)
   {
    FMIN = 0; FMAX = 1; Done = 0;
    for (int II=1;II<=50 & Done==0 ;II++)
     {
      // Bisect F
      ROI = (FMIN + FMAX) * 0.5;
      List L2 = FUN2(ROI, J, BiolPars, KTOT);
      B = L2["B"];
      P1PL = L2["P1PL"];

      // Check for convergence
      if(abs(P1PL-PPRJ)< 0.0000001*PPRJ) Done = 1;
      if (P1PL > PPRJ) FMIN = ROI; else FMAX = ROI;
     }
   }
   //std::cout << "B " << J << " " << ROI << " " << P1PL << " " << PPRJ << " " << B << std::endl;
   List L3 = FUN2(ROI, J, BiolPars, KTOT);
   NumericMatrix RM2 = L3["RM"];
   NumericMatrix RF2 = L3["RF"];

 // Rescale the numbers at age to actual values:(FUN2 set RF & UNRF to sizes scaled by B)
 //for (int L=0; L<=MAXAGE; L++)
 //  std::cout << "B1 " << RM2(L,J) << " " << RF2(L,J) << std::endl;
 for (int L=0; L<=MAXAGE; L++)
  { RM(L,J) = RM2(L,J) * B; RF(L,J) = RF2(L,J) * B; }
 //for (int L=0; L<=MAXAGE; L++)
 //  std::cout << "B " << RM(L,J) << " " << RF(L,J) << std::endl;

  return List::create( _["RM"]=RM,_["RF"]=RF);
 }


// ==============================================================================================

//' @title SETKA
//'
//' @export
// [[Rcpp::export]]
List SETKA(List BiolPars,List Control, List PopVars)
 {
  Function tst("tst");
  List TT;



  // Local variables (passed)
  int MAXAGE, Nstk, Nsuba, OPTEARLY;
  double FEC;
  Nstk = as<int>(BiolPars["Nstk"]);
  Nsuba = as<int>(BiolPars["Nsuba"]);
  MAXAGE = as<int>(BiolPars["MAXAGE"]);
  FEC = as<double>(BiolPars["FEC"]);
  OPTEARLY = as<int>(Control["OPTEARLY"]);

  NumericVector KMAT(Nstk);
  KMAT = as<NumericVector>(PopVars["KMAT"]);
  NumericVector INITDEP(Nstk);
  INITDEP = as<NumericVector>(PopVars["INITDEP"]);
  NumericVector SUR(MAXAGE+1);
  SUR = as<NumericVector>(BiolPars["SUR"]);

  // Local variables
  double  KTOT1P, PPRJ;
  NumericVector KTOT(Nstk);
  NumericMatrix PMATF(MAXAGE+1,Nstk);
  NumericMatrix PTOT(MAXAGE+1,Nstk);
  NumericMatrix RM(MAXAGE+1,Nstk);
  NumericMatrix RF(MAXAGE+1,Nstk);
  Cube<double> PP(MAXAGE+1,Nstk,Nsuba);
  List L;

  // Initialise annual population vectors:
  PP.fill(0); KTOT.fill(0); KTOT1P = 0; PTOT.fill(0); PMATF.fill(0);

  for (int J=0;J<Nstk;J++)
   {
    //Set mature female carrying capacity
    //std::cout << J << std::endl;
    PMATF(0,J) = KMAT(J);

    // Scale the 1st age class so the # of mature females (=# mature males) in pristine stock J is KMAT(J).
    //  Note: FEC = #mature females / # female births = #mature/#births
    RM(0,J) = KMAT(J) * FEC;
    RF(0,J) = RM(0,J);

    // Set up pristine nos. by age (male & female)      {Eqn 5.1, 5.2 with C=0}
    for (int L=1;L<MAXAGE;L++)
     {
      RM(L,J) = SUR(L-1) * RM(L-1,J);
      RF(L,J) = RM(L,J);
	 }
    RM(MAXAGE,J) = SUR(MAXAGE-1)*RM(MAXAGE-1,J)/(1.0-SUR(MAXAGE));
    RF(MAXAGE,J) = RM(MAXAGE,J);

    // Find K by stock
    KTOT(J) = 0;
    for (int L=1;L<=MAXAGE;L++) KTOT(J) += RM(L,J)+ RF(L,J);
    PTOT(0,J) = KTOT(J);

    // If Population is not pristine when projection begins set up the initial age-distribution in IYRPRJ
    if (OPTEARLY == 1)
     {
      PPRJ = INITDEP(J)*KTOT(J);
      //std::cout << INITDEP(J) << " " << KTOT(J) << std::endl;
      L = InitP(PPRJ,J,BiolPars, RM, RF, KTOT);
      //std::cout << J << " " << KMAT(J) << " " << KTOT(J) << " " << J << " " << INITDEP(J) << std::endl;
      NumericMatrix RM2 = L["RM"]; RM = RM2;
      NumericMatrix RF2 = L["RF"]; RF = RF2;
     }

   } // J

   //std::cout << KTOT << std::endl;

  //TT = tst(5);
  return List::create( _["RM"]=RM,_["RF"]=RF,_["KTOT"]=KTOT);
 }

// ==============================================================================================

//' @title MSYPAR is the function that is solved to find Z
//'
//' @export
// [[Rcpp::export]]
List MSYPAR(List BiolPars,List Control)
 {

  // Local variables
  int MAXAGE, MINMATA, Nstk, OPTFA, OPTDDA, OPMSYLA;
  double MSIGA, MATA1, DFF, TOL, MSYLA;
  NumericVector MORTA;
  NumericVector MSYRA;

  // Set up tolerances
  DFF = 0.0001; TOL = 0.0001;

  // Extract biological parameters
  Nstk = as<int>(BiolPars["Nstk"]);
  MAXAGE = as<int>(BiolPars["MAXAGE"]);
  MINMATA = as<int>(BiolPars["MINMAT"]);
  MSIGA = as<double>(BiolPars["MSIG"]);
  MATA1 = as<double>(BiolPars["MAT1"]);
  MORTA = as<NumericVector>(BiolPars["Mort"]);
  MSYRA = as<NumericVector>(BiolPars["MSYR"]);
  MSYLA = as<double>(BiolPars["MSYL"]);
  OPTFA= as<int>(Control["OPTF"]);
  OPTDDA= as<int>(Control["OPTDD"]);
  OPMSYLA = as<int>(Control["OPMSYL"]);

  // Local derived variables
  NumericVector SUR(MAXAGE+1);
  NumericVector RECF(MAXAGE+1);
  NumericVector FMATUR(MAXAGE+1);
  NumericVector P(MAXAGE+1);
  NumericVector Z(Nstk);
  NumericVector A(Nstk);
  NumericVector UF(2);
  NumericVector UMAT(2);
  NumericVector U1PLUS(2);
  NumericVector PARS(3);
  NumericVector Init(2);

  double PA, RMAT, RREC, R1PLUS, FMAT, U1, UM;
  double FEC, FECMSY, TERM, MSYLAD;

  Function Zcalc("Zcalc");
  Function f("uniroot");
  List unitroorout;

  // set RECF =fraction of unrecruited animals of age A which recruit
  // at age A+1, except RECF(0) = fraction recruited of age 0. Note
  // that selectvity equals maturity
  RECF = SetO(MSIGA,MATA1,MAXAGE);

  // Set up maturity ogive FMATUR = proportion mature of age A
  FMATUR = SetO(MSIGA,MATA1,MAXAGE);
  for (int Iage=0; Iage<=MINMATA; Iage++) { FMATUR(Iage) = 0; RECF(Iage) = 0; }

  // Set up natural mortality-at-age array SUR &  calculate the relative
  // population size starting with unity in the 1st age class (A=0)
  // Calculate the relative mature population RMAT (no mature of age 0)
  PA = 1; RMAT = 0; RREC = 0; R1PLUS = 0;
  for (int L=0; L<=(MAXAGE-1);L++)
   {
    SUR(L) = Surv(MORTA,L);
    RMAT = RMAT + PA*FMATUR(L);
    RREC = RREC + PA*RECF(L);
    if (L > 0) R1PLUS = R1PLUS + PA;
    PA = PA * SUR(L);
   }
  SUR(MAXAGE) = Surv(MORTA,MAXAGE);

  // Adjust for last age class being pooled (and always fully mature)
  PA = PA/(1 - SUR(MAXAGE));
  RMAT = RMAT + PA;
  RREC = RREC + PA;
  R1PLUS = R1PLUS + PA;

  // Set the birth rate so as to give balance at equilibrium
  FEC = 1/RMAT;

  // Set up the recruitment ogive in transition form:
  // RECF(A+1) = fraction of unrecruited animals of age A which recruit
  //at age A+1, except RECF(0) = fraction recruited of age 0  {Eqn A4.2}
  RECF = Trform(RECF,MAXAGE);

  for (int J=0;J<Nstk;J++)
   {
    UF(0) = 1.0 - MSYRA(J) + DFF*0.5;
    UF(1) = UF(0) - DFF;
    for (int I=0;I<=1;I++)
     {
      // Set up unrecruited & recruited components relative to # of age 0. Also sum number mature
      if (OPTFA == 0) // Fishing pattern is 1+
       {
        P(0) = 1.0; FMAT = 0; U1 = P(1);
        for (int L=1; L<=(MAXAGE-1);L++)
         {
          if (L != 1)
           P(L) = SUR(L-1)*UF(I)*P(L-1);
          else
           P(L) = SUR(L-1)*P(L-1);
          FMAT = FMAT + P(L)*FMATUR(L);
          U1 = U1 + P(L);
         }
        // Pooled age class.  NB last 2 age classes are all mature & all recruited
        P(MAXAGE) = SUR(MAXAGE-1) * P(MAXAGE-1) *UF(I)/(1.0 - SUR(MAXAGE)*UF(I));
       }
      else
       {
        // Fishing pattern is recruited
        P(0) = 1.0; FMAT = 0; U1 = P(1);
        for (int L=1; L<=(MAXAGE-1);L++)
         {
          P(L) = SUR(L-1)*(1-RECF(L-1)*(1-UF(I)))*P(L-1);
          FMAT = FMAT + P(L)*FMATUR(L);
          U1 = U1 + P(L);
         }
        // Pooled age class.  NB last 2 age classes are all mature & all recruited
        P(MAXAGE) = SUR(MAXAGE-1) * P(MAXAGE-1) *UF(I)/(1.0 - SUR(MAXAGE)*UF(I));
	   }
      FMAT = FMAT + P(MAXAGE);
      U1 = U1 + P(MAXAGE);

      // Save the birth rate which balances with this age-structure
      UF(I) = 1.0/FMAT;
      UMAT(I) = FMAT;
      U1PLUS(I) = U1;
    } // I

    UM = (UMAT(0)+UMAT(1)) * 0.5;
    U1 = (U1PLUS(0) + U1PLUS(1)) * 0.5;

    //  Setup MSYLAD (JCRM 1:270 eqn 20)
    if (OPTDDA == OPMSYLA)
     // MSYLA defined on density dependent component of population
     MSYLAD = MSYLA;
    else
     if (OPMSYLA == 0 & OPTDDA == 1)
      // MSYLA on 1+;  DD on mature
      MSYLAD = MSYLA * R1PLUS * UM * FEC / U1;
     else
      if (OPMSYLA == 1 & OPTDDA == 0)
       // MSYLA on mature; DD on 1+
       MSYLAD = MSYLA * U1 / (R1PLUS * UM * FEC);

     // TERM (1st 2 terms in {} in eqn A7.3)
     if (OPTDDA == OPTFA)
       // MSYRA defined on density dependent component of population
       TERM = 0;
     else
      {
       TERM = ((U1PLUS(1)-U1PLUS(0))/U1 - (UMAT(1)+UMAT(0))/UM) / DFF;
       if (OPTFA == 1 & OPTDDA == 0) TERM = -TERM;
      }
    //   Place values to be passed into PARS.  PARS2 = Df(Fmsy)/DF / f(Fmsy)-f(0)
    PARS(0) = 1.0 + TERM*MSYRA(J);
    FECMSY = (UF(0) + UF(1)) * 0.5;
    PARS(1) = MSYRA(J) * (UF(1) - UF(0))/(DFF*(FECMSY - FEC));
    PARS(2) = MSYLAD;

    // Call uniroot
    Init(0) = -5;
    Init(1) =  5;
    unitroorout = f(Zcalc, _["interval"]=Init, _["Pars"]=PARS);
    Z(J) = unitroorout["root"];
    A(J) = (FECMSY/FEC - 1.0)/(1.0 - pow(MSYLA,Z(J)));
  } /// J

  return List::create( _["FMATUR"]=FMATUR,_["FEC"]=FEC,_["SUR"]=SUR,_["A"]=A,_["Z"]=Z);

}

// ==============================================================================================

//' @title Zcalc is the function that is solved to find Z
//'
//'
//' The root of this function = density dependent exponent Z
//'     PARS(1) = 1 + MSYRA(J) (dP/dF/P - dPD/dF/PD)
//'     PARS(2) = MSYRA(J) * df(Fmsy)/dF / (f(Fmsy)-f(0))
//'     PARS(3) = MSYLAD
//'     PARS(4) = Z(J)//' @param BasicData is List with all the key parameters
//' @export
// [[Rcpp::export]]
double Zcalc(double ZZ,NumericVector Pars)
{
 // Local variables
 double Zcalc2;
 NumericVector Zcalc3(5);

 //Function f("rnorm");
 //Zcalc3 = f(5, Named("sd")=2, _["mean"]=10);
 //std::cout << Zcalc3 << std::endl;

 if (ZZ==0)
  Zcalc2 = Pars(0) - Pars(1)*log(Pars(2));
 else
  Zcalc2 = Pars(0) - Pars(1)*(pow(Pars(2),(-ZZ))-1.0)/ZZ;
 return Zcalc2;
}


