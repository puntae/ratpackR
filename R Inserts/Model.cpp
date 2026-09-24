#include <RcppArmadillo.h>
// #include <Rcpp.h>

  // [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// ==============================================================================================

//' @title SPR calculates the SPR for an input Fully selected F (internal routine; Tier 2)
//'
//' @description SPR calculates the the yield-per-recruit and spawning
//'     potential ratio for a particular input fully selected instantaneous
//'     fishing mortality. In all cases where there are two sexes females are in
//'     column 0 (the first column). This is implemented in C++ for speed.
//'
//' @param BasicData is List with all the key parameters
//' @param FFis a fully-selected instantaneous fishing mortality
//' @return the spr
//' @export
// [[Rcpp::export]]
double SPR(List BasicData, double F)
{
  // Local variables
  int Nsex, nagebins, Nage, Nfleet;
  double spr,Mbase;

  // Extract
  Nsex = as<int>(BasicData["nsex"]);
  nagebins = as<int>(BasicData["nagebins"]);
  Nage = nagebins-1;                                            // Easy to work with
  Nfleet = as<int>(BasicData["nfleet"]);
  Mbase = as<double>(BasicData["baseM"]);

  // Define the Matrices
  NumericVector Neqn(nagebins);

  // Extract material from BasicData (Wpristine is the starting W)
  NumericVector Fecund(nagebins);
  Fecund = as<NumericVector>(BasicData["Fecund"]);
  NumericMatrix SelAge(Nfleet,nagebins);
  SelAge = as<NumericMatrix>(BasicData["SelAge"]);
  NumericVector Frel(Nfleet);
  Frel = as<NumericVector>(BasicData["Exploit"]);

  NumericMatrix Z(Nsex,nagebins);
  for (int Isex=0;Isex<Nsex;Isex++)
   for (int Iage=0;Iage<=Nage;Iage++)
    {
     Z(Isex,Iage) = Mbase;
     for (int Ifleet=0;Ifleet<Nfleet;Ifleet++) Z(Isex,Iage) += F*Frel(Ifleet)*SelAge(Ifleet,Iage);
	}

  Neqn(0) = 1/float(Nsex);
  for (int Iage=1;Iage<=Nage;Iage++) Neqn(Iage) = Neqn(Iage-1)*exp(-Z(0,Iage-1));
  Neqn(Nage) = Neqn(Nage)/(1.0-exp(-Z(0,Nage)));
  spr = 0; for (int Iage=1;Iage<=Nage;Iage++) spr += Neqn(Iage)*Fecund(Iage);

  //std::cout << Neqn << std::endl;
  //std::cout << Z << std::endl;
  //std::cout << SelAge << std::endl;

  return spr;
}

// ==============================================================================================

//' @title SPR calculates the SPR for an input Fully selected F (internal routine; Tier 2)
//'
//' @description SPR calculates overall F corresponding a specific reduction in spawning biomass-per-recruit
//'
//' @param BasicData is List with all the key parameters
//' @return the spr
//' @export
// [[Rcpp::export]]
double Get_SPR(List BasicData, double target)
{
  double sprf0, spr, fmin, fmax, fmult;
  sprf0 = SPR(BasicData,0);

  fmin = 0; fmax = 10;
  for (int II=1;II<=20;II++)
   {
	fmult = (fmin+fmax)/2.0;
	spr = SPR(BasicData,fmult);
    //std::cout << spr << " " << target*sprf0 << " " << fmult << std::endl;
 	if (spr < target*sprf0)
	 fmax = fmult;
	else
	 fmin = fmult;
   }

  return fmult;
 }

// ==============================================================================================

//' @title SPR calculates the SPR for an input Fully selected F (internal routine; Tier 2)
//'
//' @description SPR calculates overall F corresponding a specific reduction in spawning biomass-per-recruit
//'
//' @param BasicData is List with all the key parameters
//' @return the spr
//' @export
// [[Rcpp::export]]
List Project(List BasicData) {
  // Define the output storage

  // Local variables
  double ssb, Mbase, R0, SSB0, Ftarget, Fuse, TAC_Year1;
  double Inflect, CutOff, fmin, fmax, Cpred;

  // Extract dimensions
  int Nsex, nagebins, Nage, Nfleet, Nyear, Nsim;
  Nsex = as<int>(BasicData["nsex"]);
  Nyear = as<int>(BasicData["Nyear"]);
  nagebins = as<int>(BasicData["nagebins"]);
  Nage = nagebins-1;                                            // Easy to work with
  Nfleet = as<int>(BasicData["nfleet"]);
  Nsim = as<int>(BasicData["Nsim"]);

  // Extract variables
  Mbase = as<double>(BasicData["baseM"]);
  R0 = as<double>(BasicData["R0"]);
  SSB0 = as<double>(BasicData["SSB0"]);
  Ftarget = as<double>(BasicData["Ftarget"]);
  CutOff = as<double>(BasicData["CutOff"]);
  Inflect = as<double>(BasicData["Inflect"]);
  TAC_Year1 = as<double>(BasicData["TAC_Year1"]);
  // Extract variables 2
  NumericVector NatAge(nagebins);
  NatAge = as<NumericVector>(BasicData["NatAge"]);
  NumericVector Fecund(nagebins);
  Fecund = as<NumericVector>(BasicData["Fecund"]);
  NumericMatrix SelAge(Nfleet,nagebins);
  SelAge = as<NumericMatrix>(BasicData["SelAge"]);
  NumericMatrix SelAgeRetWght(Nfleet,nagebins);
  SelAgeRetWght = as<NumericMatrix>(BasicData["SelAgeRetWght"]);
  NumericVector Frel(Nfleet);
  Frel = as<NumericVector>(BasicData["Exploit"]);
  NumericMatrix RecDevs(Nyear,Nsim);
  RecDevs = as<NumericMatrix>(BasicData["RecDevs"]);
  NumericMatrix MDevs(Nyear,Nsim);
  MDevs = as<NumericMatrix>(BasicData["MDevs"]);
  NumericVector Price(Nfleet);
  Price = as<NumericVector>(BasicData["Price"]);
  NumericVector CostPerDay(Nfleet);
  CostPerDay = as<NumericVector>(BasicData["CostPerDay"]);
  NumericVector DaysPerF1(Nfleet);
  DaysPerF1 = as<NumericVector>(BasicData["DaysPerF1"]);
  NumericVector DaysPerF2(Nfleet);
  DaysPerF2 = as<NumericVector>(BasicData["DaysPerF2"]);
  NumericVector FixedCosts(Nfleet);
  FixedCosts = as<NumericVector>(BasicData["FixedCosts"]);

  // Key derived variables variables
  NumericVector SSB(Nyear+1);
  Cube<double> N(Nsex,nagebins,Nyear+1);
  NumericMatrix FF(Nfleet,nagebins);
  NumericMatrix Z(Nsex,nagebins);
  NumericMatrix Catch(Nfleet,Nyear);
  NumericMatrix FullFish(Nfleet,Nyear);
  NumericMatrix Effort(Nfleet,Nyear);
  NumericMatrix Profit(Nfleet,Nyear);
  NumericMatrix Revenue(Nfleet,Nyear);

  for (int Isex=0;Isex<Nsex;Isex++)
   for (int Iage=0;Iage<nagebins;Iage++)
    N(Isex,Iage,0) = NatAge(Iage);

  ssb = 0; for (int Iage=0;Iage<nagebins;Iage++) ssb += N(0,Iage,0)*Fecund(Iage);
  SSB(0) = ssb;
  //std::cout <<ssb << std::endl;

  for (int Isim=0;Isim<Nsim;Isim++)
   {
	// Replace first age-class
	for (int Isex=0;Isex<Nsex;Isex++) N(Isex,0,0) = R0*exp(RecDevs(0,Isim));
    for (int Iyear=0;Iyear<Nyear;Iyear++)
     {
      //std::cout << Isim << " " << Iyear << std::endl;

      // what to do depends on whether this is the first year or not
      if (Iyear==0)
       {
		fmin = 0;
		fmax = 10;
		for (int II=0;II<25;II++)
		 {
		  Fuse = (fmin+fmax)/2.0;
          // compute F and Z
          for (int Iage=0;Iage<nagebins;Iage++)
           {
            for (int Isex=0;Isex<Nsex;Isex++) Z(Isex,Iage) = Mbase*exp(MDevs(Iyear,Isim));
             for (int Ifleet=0;Ifleet<Nfleet;Ifleet++)
	          {
	           FF(Ifleet,Iage) = Fuse*Frel(Ifleet)*SelAge(Ifleet,Iage);
	           for (int Isex=0;Isex<Nsex;Isex++) Z(Isex,Iage) += FF(Ifleet,Iage);
	          }
	       } // Iage
          // Compute catch
          Cpred = 0;
          for (int Ifleet=0;Ifleet<Nfleet;Ifleet++)
  	       for (int Isex=0;Isex<Nsex;Isex++)
	   	    for (int Iage=0;Iage<nagebins;Iage++)
	 	    Cpred += SelAgeRetWght(Ifleet,Iage)*Fuse*Frel(Ifleet)* N(Isex,Iage,Iyear)/Z(Isex,Iage)*(1.0-exp(-Z(Isex,Iage)));
	      // Now update
	      if (Cpred > TAC_Year1) fmax = Fuse; else fmin = Fuse;
		 } // II
         //std::cout << Cpred << " " << TAC_Year1 << std::endl;
	   }
	  else
	   {
        // Apply control rule to compute the Target
        if (SSB(Iyear) < 0.2*SSB0 | SSB(Iyear) < CutOff*SSB0)
         Fuse = 0;
        else
         if (SSB(Iyear) > Inflect*SSB0)
          Fuse = Ftarget;
         else
          Fuse = Ftarget*(SSB(Iyear)/SSB0-CutOff)/(Inflect-CutOff);
        //std::cout << SSB(Iyear)/SSB0 << " " << Fuse << std::endl;

        // compute F and Z
        for (int Iage=0;Iage<nagebins;Iage++)
         {
          for (int Isex=0;Isex<Nsex;Isex++) Z(Isex,Iage) = Mbase*exp(MDevs(Iyear,Isim));
           for (int Ifleet=0;Ifleet<Nfleet;Ifleet++)
	        {
	         FF(Ifleet,Iage) = Fuse*Frel(Ifleet)*SelAge(Ifleet,Iage);
	         for (int Isex=0;Isex<Nsex;Isex++) Z(Isex,Iage) += FF(Ifleet,Iage);
	        }
	      } // Iage
	   } // else

      // Compute catch and effort
      for (int Ifleet=0;Ifleet<Nfleet;Ifleet++)
       {
		FullFish(Ifleet,Iyear) += Fuse*Frel(Ifleet);
		Effort(Ifleet,Iyear) += exp(DaysPerF1(Ifleet)+DaysPerF2(Ifleet)*log(Fuse*Frel(Ifleet)+1.0e-20));
		for (int Isex=0;Isex<Nsex;Isex++)
		 for (int Iage=0;Iage<nagebins;Iage++)
		  Catch(Ifleet,Iyear) += SelAgeRetWght(Ifleet,Iage)*Fuse*Frel(Ifleet)* N(Isex,Iage,Iyear)/Z(Isex,Iage)*(1.0-exp(-Z(Isex,Iage)));
	   }

      // Project forward
      for (int Isex=0;Isex<Nsex;Isex++)
       {
        for (int Iage=1;Iage<=Nage;Iage++) N(Isex,Iage,Iyear+1) = N(Isex,Iage-1,Iyear)*exp(-Z(Isex,Iage-1));
        N(Isex,Nage,Iyear+1) += N(Isex,Nage,Iyear)*exp(-Z(Isex,Nage));
       } // Iage

      // Compute SSB
      ssb = 0; for (int Iage=0;Iage<nagebins;Iage++) ssb += N(0,Iage,Iyear+1)*Fecund(Iage);
	  SSB(Iyear+1) = ssb;

      for (int Isex=0;Isex<Nsex;Isex++) N(Isex,0,Iyear+1) = R0*exp(RecDevs(Iyear+1,Isim));


     } // Iyear
	//std::cout << Nyear << " " << ssb/SSB0 << std::endl;

  } // Isim

  // Find the expect values
  for (int Ifleet=0;Ifleet<Nfleet;Ifleet++)
   for (int Iyear=0;Iyear<Nyear;Iyear++)
     {
      FullFish(Ifleet,Iyear) /= float(Nsim);
      Effort(Ifleet,Iyear) /= float(Nsim);
	  Catch(Ifleet,Iyear) /= float(Nsim);
	  Revenue(Ifleet,Iyear) = Price(Ifleet)*Catch(Ifleet,Iyear);
	  Profit(Ifleet,Iyear) =  Revenue(Ifleet,Iyear) - CostPerDay(Ifleet)*Effort(Ifleet,Iyear) - FixedCosts(Ifleet);
	 }

  return List::create( _["Catch"]=Catch, _["Revenue"]=Revenue, _["FullFish"]=FullFish, _["Effort"]=Effort, _["SSB"]=SSB, _["Profit"]=Profit,_["FullFish"]=FullFish);

}

// ==============================================================================================
//' @title GetRecruit computes the recruitment from SSB
//'
//' @description GetRecruit computes the recruitment from SSB. Three SR relationships are available
//'         and account is taken of environmental drves
//'
//' @param BasicData are the biological parameters
//' @param BasicPars are the biological parameters
//' @param RunOptions are the options for the factors
//' @param EnvironmentalData is the list of the enviromental data
//' @param SSB is spawning biomass
//' @param SSB0 is unfished spawning biomass
//' @param year is the projection year
//' @return the recruitment corresponding to the specified SPR value
//' @export

// [[Rcpp::export]]
double GetRecruit(List BasicData, List BasicPars, List RunOptions, NumericMatrix EnvironmentalData, NumericVector SSB, double SSB0, int year, int Diag) {

  // Local variables
  double Recruit, Top,Bot, R0, Steepness, SSBuse;
  int SROpt, NenvLinks, RecLag, end_yr, Amax;

  // Stock-recruitment relationship and how many environmental links
  SROpt = as<int>(RunOptions["SROpt"]);                                  // 0=Constant; 1=BH; 1=Ricker;

  // Extract parameters
  R0 = as<double>(BasicPars["R0"]);
  Steepness = as<double>(BasicPars["Steep"]);
  RecLag = as<int>(BasicData["reclag"]);
  end_yr = as<int>(BasicData["end_yr"]);
  Amax = as<int>(BasicData["Amax"]);

  NenvLinks = as<int>(RunOptions["NenvLinks"]);                          // how many linklinks
  NumericVector EnvPars(NenvLinks);
  EnvPars = as<NumericVector>(BasicPars["EnvPars"]);
  IntegerMatrix EnvLinks(NenvLinks,3);
  EnvLinks = as<IntegerMatrix>(RunOptions["EnvLinks"]);                  // links
  Recruit = 0;

  // Specify SSB to use
  if (year+1-RecLag < 0) SSBuse = SSB(year);
  if (year+1-RecLag >= 0) SSBuse = SSB(year+1-RecLag);
  // Check for before-density dependence environmental links
  for (int Ilink=0;Ilink<NenvLinks;Ilink++)
   if (EnvLinks(Ilink,0)==1) SSBuse *= exp(EnvironmentalData(Amax+1+year-RecLag,EnvLinks(Ilink,1))*EnvPars(EnvLinks(Ilink,1)));

  // Constant recruitment
  if (SROpt==1) Recruit = R0;

  // Beverton-Holt recruitment
  if (SROpt==2)
   {
    Top = 4.0 * Steepness * R0* SSBuse/SSB0;
    Bot = (1-Steepness) + (5*Steepness-1)*SSBuse/SSB0;
    Recruit = Top/Bot;
   }

  // Ricker recruitment
  if (SROpt==3)
   {
    Top = log(5.0*Steepness)/0.8*(1.0-SSBuse/SSB0);
    Recruit = R0*SSBuse/SSB0*exp(Top);
    //std::cout << "RickerExp " << SSBuse << " " << SSB0 << " " << Recruit << " " << R0 << std::endl;
   }

  // Check for after-density dependence environmental links
  for (int Ilink=0;Ilink<NenvLinks;Ilink++)
   {
    if (EnvLinks(Ilink,0)==2) Recruit *= exp(EnvironmentalData(Amax+1+year-RecLag,EnvLinks(Ilink,1))*EnvPars(EnvLinks(Ilink,1)));
    //if (EnvLinks(Ilink,0)==2) std::cout << "Recruit Adj " << year << " " << end_yr+year+1 << " " << Amax+1+year-RecLag << " " << EnvironmentalData(Amax+1+year-RecLag,EnvLinks(Ilink,1)) << " " << EnvPars(EnvLinks(Ilink,1)) <<  " " << exp(EnvironmentalData(Amax+1+year-RecLag,EnvLinks(Ilink,1))*EnvPars(EnvLinks(Ilink,1))) << std::endl;
   }
  if (Diag==1)
   {
	std::cout << "Recruit" << year << " " << end_yr+year+1 << " " << Amax+1+year-RecLag << " " << Recruit/R0 << " ";
	for (int Ilink=0;Ilink<NenvLinks;Ilink++) std::cout << EnvironmentalData(Amax+1+year-RecLag,Ilink) << " ";
	for (int Ilink=0;Ilink<NenvLinks;Ilink++) std::cout << EnvPars (Ilink) << " ";
	std::cout << " " << std::endl;
   }

  return Recruit;
}
// ==============================================================================================
//' @title CalcWcatch2 calculates weight-at-age for all projection years
//'
//' @description CalcWcatch3 calculates weight-at-age for a projection year
//'
//' @param BasicData are the biological parameters
//' @param BasicPars are the biological parameters
//' @param RunOptions are the options for the factors
//' @param EnvironmentalData is the list of the enviromental data
//' @param WcatchInput is the input weight-at-age
//' @param Nyear is the number of the year
//' @return weight-at-age and length-at-age
//' @export
// [[Rcpp::export]]

List CalcWcatch2(List BasicData, List BasicPars, List RunOptions, NumericMatrix EnvironmentalData, NumericMatrix WcatchInput, int Nyear, int Diag) {

  // Local variables
  int Nsex, Amax, Nage, jyear,WeightOpt, NenvLinks, end_yr, EnvIndex;
  double EnvironEffect,TheLength,L1ratio,KappaU,LinfU;

  // How many environmental links
  WeightOpt = as<int>(RunOptions["WeightOpt"]);             // 0=Pre-specified; 1=related to growth increment; 2=with environmental link

  // Extract
  Nsex = as<int>(BasicData["Nsex"]);
  Amax = as<int>(BasicData["Amax"]);
  end_yr = as<int>(BasicData["end_yr"]);
  Nage = Amax-1;                                            // Easier to work with
  Cube<double> Wcatch(Nsex,Amax,Nyear+1);                   // Weight-at-age
  Cube<double> Lcatch(Nsex,Amax,Nyear+1);                   // Length-at-age

  NumericVector Linf(Nsex);
  NumericVector Kappa(Nsex);
  NumericVector L1min(Nsex);
  NumericVector aa(Nsex);
  NumericVector bb(Nsex);
  NumericVector epsLmin(Nyear+Nage);
  NumericVector epsKappa(Nyear+Nage);
  NumericVector epsLinf(Nyear+Nage);
  NumericMatrix WAAIncrement(Nsex,Amax);

  // Extract growth parameters
  Linf = as<NumericVector>(BasicData["Linf"]);
  Kappa = as<NumericVector>(BasicData["Kappa"]);
  L1min = as<NumericVector>(BasicData["L1min"]);
  aa = as<NumericVector>(BasicData["aa"]);
  bb = as<NumericVector>(BasicData["bb"]);
  epsLmin  = as<NumericVector>(BasicData["epsLmin"]);
  epsKappa  = as<NumericVector>(BasicData["epsKappa"]);
  epsLinf  = as<NumericVector>(BasicData["epsLinf"]);

  // Extract environmental parameters
  NenvLinks = as<int>(RunOptions["NenvLinks"]);                       // how many links
  NumericVector EnvPars(NenvLinks);
  EnvPars = as<NumericVector>(BasicPars["EnvPars"]);                  // parameters
  IntegerMatrix EnvLinks(NenvLinks,3);
  EnvLinks = as<IntegerMatrix>(RunOptions["EnvLinks"]);               // links

  // Initial year
  for (int Isex=0;Isex<Nsex;Isex++)
   for (int Iage=0;Iage<=Nage;Iage++)
    {
     if (WeightOpt==0) Wcatch(Isex,Iage,0) = WcatchInput(Isex,Iage);
     if (WeightOpt==1)
      {
	   Lcatch(Isex,Iage,0) = Linf(Isex) + Linf(Isex)*(L1min(Isex)-1.0)*exp(-Kappa(Isex)*float(Iage));
       Wcatch(Isex,Iage,0) = aa(Isex)*pow(Lcatch(Isex,Iage,0),bb(Isex))/1000;
      }
     if (WeightOpt==2)
      {
       jyear = Nage-Iage;
	   L1ratio = L1min(Isex)*exp(epsLmin(jyear));
       KappaU = Kappa(Isex)*exp(epsKappa(jyear));
	   LinfU = Linf(Isex)*exp(epsLinf(jyear));
       if (Diag==1) std::cout << "Year 0 values " << Isex << " " << Iage << " " << jyear << " " << epsLmin(jyear) << " " << epsKappa(jyear) << " " << epsLinf(jyear) << " " << endl;
	   Lcatch(Isex,Iage,0) = LinfU + LinfU*(L1ratio-1.0)*exp(-KappaU*float(Iage));
       Wcatch(Isex,Iage,0) = WcatchInput(Isex,Iage);
      }
    }

  // Subsequent years
  for (int Iyear=1;Iyear<=Nyear;Iyear++)
   {
    // Fixed at initial weight-at-age
    if (WeightOpt==0)
     {
      for (int Isex=0;Isex<Nsex;Isex++)
       for (int Iage=0;Iage<=Nage;Iage++) Wcatch(Isex,Iage,Iyear) = WcatchInput(Isex,Iage);
     } // WeightOpt=0
	// Base parameters
    if (WeightOpt==1)
     {
      for (int Isex=0;Isex<Nsex;Isex++)
       for (int Iage=0;Iage<=Nage;Iage++)
        {
         TheLength = Linf(Isex) + Linf(Isex)*(L1min(Isex)-1.0)*exp(-Kappa(Isex)*float(Iage));
         Lcatch(Isex,Iage,Iyear) = TheLength;
         Wcatch(Isex,Iage,Iyear) = aa(Isex)*pow(TheLength,bb(Isex))/1000;
        }
     } // WeightOpt=1

    // Dynamics
    if (WeightOpt==2)
     {
      // Set initial size
      for (int Isex=0;Isex<Nsex;Isex++)
       {
        jyear = Iyear+Nage;                                  // Birth year (at age 1)
        L1ratio = L1min(Isex)*exp(epsLmin(jyear));
		KappaU = Kappa(Isex)*exp(epsKappa(jyear));
        LinfU = Linf(Isex)*exp(epsLinf(jyear));
        if (Diag==1) std::cout << "Initial size " << Isex << " " << 0 << " " << jyear << " " << epsLmin(jyear) << " " << epsKappa(jyear) << " " << epsLinf(jyear) << " " << endl;
        TheLength = LinfU*L1ratio;
        Lcatch(Isex,0,Iyear) = TheLength;
        Wcatch(Isex,0,Iyear) = aa(Isex)*pow(TheLength,bb(Isex))/1000;
       }
      // Compute length-at-age and hence growth increment
      for (int Isex=0;Isex<Nsex;Isex++)
       for (int Iage=1;Iage<=Nage;Iage++)
        {
         jyear = Iyear+Nage-Iage;                                  // Birth year (at age 1)
         L1ratio = L1min(Isex)*exp(epsLmin(jyear));
		 KappaU = Kappa(Isex)*exp(epsKappa(jyear));
         LinfU = Linf(Isex)*exp(epsLinf(jyear));
         Lcatch(Isex,Iage,Iyear) = LinfU + LinfU*(L1ratio-1.0)*exp(-KappaU*float(Iage));
         WAAIncrement(Isex,Iage) = aa(Isex)*pow(Lcatch(Isex,Iage,Iyear),bb(Isex))/1000 - aa(Isex)*pow(Lcatch(Isex,Iage-1,Iyear-1),bb(Isex))/1000;
        }
      for (int Isex=0;Isex<Nsex;Isex++)
       {
        // Environmental effort
        EnvironEffect = 1;
        EnvIndex = Amax+Iyear;
        for (int Ilink=0;Ilink<NenvLinks;Ilink++)
         if (EnvLinks(Ilink,0)==3) EnvironEffect *=  exp(EnvironmentalData(EnvIndex,EnvLinks(Ilink,1))*EnvPars(EnvLinks(Ilink,1)));
        for (int Ilink=0;Ilink<NenvLinks;Ilink++)
         if (Diag==1 && EnvLinks(Ilink,0)==3) std::cout << "growth Annual " << Iyear << " " << end_yr+Iyear << " " << EnvIndex << " " << EnvironEffect << " " << Ilink+1 << " " << EnvLinks(Ilink,1) << " " << EnvironmentalData(EnvIndex,EnvLinks(Ilink,1)) << " " << EnvPars(EnvLinks(Ilink,1)) << std::endl;

        // Initial weight
        jyear = Iyear+Nage;
        Lcatch(Isex,0,Iyear) = Linf(Isex)*exp(epsLinf(jyear))*L1min(Isex)*exp(epsLmin(jyear));
        Wcatch(Isex,0,Iyear) = aa(Isex)*pow(Lcatch(Isex,0,Iyear),bb(Isex))/1000;
         // Increment
        for (int Iage=Nage;Iage>=1;Iage--)
         Wcatch(Isex,Iage,Iyear) = Wcatch(Isex,Iage-1,Iyear-1) + WAAIncrement(Isex,Iage)*EnvironEffect;
       }
     } // WeightOpts
  }

  return List::create( _["Wcatch"]=Wcatch, _["Lcatch"]=Lcatch);
}

// ==============================================================================================

double ProjSSB(double FullF, NumericMatrix Mbase, NumericMatrix Sel, NumericVector Matur, NumericMatrix N, NumericMatrix WAA, int Nsex, int Amax, double spawnfrac) {
  NumericMatrix F(Nsex,Amax);
  NumericMatrix Z(Nsex,Amax);
  int Nage;
  Nage = Amax-1;                                                      // Easy to work with
  double SSB;

  // Calculate F and Z
  for (int Isex=0;Isex<Nsex;Isex++)
   for (int Iage=0;Iage<=Nage;Iage++)
    {
	 F(Isex,Iage) = FullF*Sel(Isex,Iage);
	 Z(Isex,Iage) = Mbase(Isex,Iage) + F(Isex,Iage);
	}

  // SSB
  SSB = 0; for (int Iage=0;Iage<=Nage;Iage++) SSB += N(0,Iage)*Matur(Iage)*WAA(0,Iage)*exp(-spawnfrac*Z(0,Iage));
  return SSB;

}

// ==============================================================================================

//' @title Project conducts projections
//'
//' @description Project conducts the projections
//' @param BasicData are the biological parameters
//' @param BasicPars are the biological parameters
//' @param RunOptions are the options for the factors
//' @param EnvironmentalData is the list of the enviromental data
//' @param Nyear is the number of projection years
//' @param Ftarget is the target fishing mortality
//' @param Btarget is the unfished biomass

//' @return the recruitment corresponding to the specified SPR value
// [[Rcpp::export]]
List Project2(List BasicData, List BasicPars, List RunOptions, NumericMatrix EnvironmentalData, int Nyear) {

  // Local variables
  int Nsex, Amax, Nage,Diag;
  double SSB0, R0, Recruit, SigmaR, ExploitRate, spr, spawnfrac;      // Temp variables
  double Error,FullF;                                         // Temp variables
  double Price,Cost,q;                                                // Econ variables

  // Set diagnostic level
  Diag = 0;

  // Extract
  Nsex = as<int>(BasicData["Nsex"]);
  Amax = as<int>(BasicData["Amax"]);
  Nage = Amax-1;                                                      // Easy to work with
  spawnfrac = as<double>(BasicData["spawnfrac"]);
  R0 = as<double>(BasicPars["R0"]);                                   // Nsex is both sexes
  SigmaR = as<double>(BasicPars["SigmaR"]);
  Cost = as<double>(BasicPars["Cost"]);
  Price = as<double>(BasicPars["Price"]);
  q = as<double>(BasicPars["q"]);

  // Define the output storage
  Cube<double> N(Nsex,Amax,Nyear+1);
  Cube<double> Z(Nsex,Amax,Nyear);
  Cube<double> F(Nsex,Amax,Nyear);
  Cube<double> M(Nsex,Amax,Nyear);
  NumericVector SSB(Nyear);
  NumericVector Recruitment(Nyear+1);
  NumericVector Catch(Nyear);
  NumericVector FishingMort(Nyear);
  NumericVector Profit(Nyear);

  // Create a 4D Array
  arma::field<arma::cube> nonstandard_4d_array(5);

  // Extract material from BasicData (Wcatch is the starting W)
  NumericMatrix Mbase(Nsex,Amax);
  Mbase = as<NumericMatrix>(BasicData["M"]);
  NumericVector Matur(Amax);
  Matur = as<NumericVector>(BasicData["Matur"]);
  NumericMatrix Sel(Nsex,Amax);
  Sel = as<NumericMatrix>(BasicData["Sel"]);
  NumericMatrix Ninit(Nsex,Amax);
  Ninit = as<NumericMatrix>(BasicData["Ninit"]);
  NumericMatrix Wcatch(Nsex,Amax);
  Wcatch = as<NumericMatrix>(BasicData["Wcatch"]);

  // Temporary variables
  NumericMatrix Wtemp(Nsex,Amax);
  NumericMatrix Ntemp(Nsex,Amax);

  // Set up the unfished SSB
 //spr = SPR(BasicData, 0.0);
  spr = 0;
  SSB0 = R0*spr;
  //std::cout << "SSB0 " << spr << " " << R0 << " " << SSB0 << std::endl;

  // Extract weight-at-age
  List L = CalcWcatch2(BasicData,BasicPars, RunOptions, EnvironmentalData, Wcatch, Nyear, Diag);
  Cube<double> WAA = L["Wcatch"];
  Cube<double> LAA = L["Lcatch"];

  // Initialize the N matrix
  for (int Isex=0;Isex<Nsex;Isex++)
   for (int Iage=0;Iage<=Nage;Iage++)
    N(Isex,Iage,0) = Ninit(Isex,Iage);

  //Function("WriteDatFile");

  // Project ahead
  Recruitment(0) = N(0,0,0)*float(Nsex);
  for (int Iyear=0;Iyear<Nyear;Iyear++)
   {

    // Solve for F
    for (int Isex=0;Isex<Nsex;Isex++)
	 for (int Iage=0;Iage<=Nage;Iage++)
	  { Ntemp(Isex,Iage) = N(Isex,Iage,Iyear); Wtemp(Isex,Iage) = WAA(Isex,Iage,Iyear); }

    FullF = 0;

    // Calculate F and Z
    FishingMort(Iyear) = FullF;
    for (int Isex=0;Isex<Nsex;Isex++)
     for (int Iage=0;Iage<=Nage;Iage++)
      {
	   M(Isex,Iage,Iyear) = Mbase(Isex,Iage);
	   F(Isex,Iage,Iyear) = FullF*Sel(Isex,Iage);
	   Z(Isex,Iage,Iyear) = M(Isex,Iage,Iyear) + F(Isex,Iage,Iyear);
	  }

    // SSB
    SSB(Iyear) = 0; for (int Iage=0;Iage<=Nage;Iage++) SSB(Iyear) += N(0,Iage,Iyear)*Matur(Iage)*WAA(0,Iage,Iyear)*exp(-spawnfrac*Z(0,Iage,Iyear));

    // Calculate the catch
    Catch(Iyear) = 0;
    for (int Isex=0;Isex<Nsex;Isex++)
     for (int Iage=0;Iage<=Nage;Iage++)
      {
	   ExploitRate = F(Isex,Iage,Iyear)/Z(Isex,Iage,Iyear)*(1.0-exp(-Z(Isex,Iage,Iyear)));
	   Catch(Iyear) += WAA(Isex,Iage,Iyear)*N(Isex,Iage,Iyear)*ExploitRate;
      }
    Profit(Iyear) = Price*Catch(Iyear) - Cost*q*FullF;


    // Update dynamics
    for (int Isex=0;Isex<Nsex;Isex++)
     {
      for (int Iage=1;Iage<Nage;Iage++)
        N(Isex,Iage,Iyear+1) = N(Isex,Iage-1,Iyear)*exp(-Z(Isex,Iage-1,Iyear));
      N(Isex,Nage,Iyear+1) = N(Isex,Nage-1,Iyear)*exp(-Z(Isex,Nage-1,Iyear)) +  N(Isex,Nage,Iyear)*exp(-Z(Isex,Nage,Iyear));
     }

    // Now generate recruitment (BH)
    Recruit = GetRecruit(BasicData,BasicPars,RunOptions,EnvironmentalData,SSB,SSB0,Iyear,Diag);
    Error = as<double>(rnorm(1,0,SigmaR))-SigmaR*SigmaR/2.0;
    Recruit = Recruit*exp(Error)/float(Nsex);
    Recruitment(Iyear+1) = Recruit*float(Nsex);

    // Age zero recruits
    for (int Isex=0;Isex<Nsex;Isex++) N(Isex,0,Iyear+1) = Recruit;

   } // Year loop

  return List::create( _["N"]=N, _["SSB"]=SSB, _["Recruitment"]=Recruitment,_["SSB0"]=SSB0,
                       _["Catch"]=Catch, _["WAA"]=WAA, _["LAA"]=LAA,
                       _["F"]=FishingMort,_["Profit"]=Profit,_["SSB0"]=SSB0);
}

// ================================================================================================


