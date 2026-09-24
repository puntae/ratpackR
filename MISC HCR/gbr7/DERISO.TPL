DATA_SECTION
!! int Treef = 24;                               // Total number of reefs
 init_int Nreef                                  // Number of reefs
 init_int Fyear                                  // Number of years
 init_int Est_M                                  // Estimate Natural mortality (-1 for fix)
 init_int Est_Qage                               // Fix ageQ to 1 (1) or estimate (0)
 init_int Is_Schnute                             // Use the Schnute correction (1) or original Deriso formulation (0)
 init_int Arec                                   // Age-at-recruitment
 init_int NumFs                                  // Number of Fs to estimate
 init_int Offset                                 // Reefs to skip
 init_ivector B1DB0(1,Treef)                     // Is the ratio of initial biomass to B0 equal to 1
 init_vector B1target(1,Nreef)                   // Target values for initial depletion
 init_vector B1SD(1,Nreef)                       // SD of target values
 init_vector Qprior(1,Treef)                     // Reef perimeter
 init_3darray CtObs(1,Treef,1,Fyear,0,12)        // Catch data
 init_3darray AbarD(1,Treef,1,Fyear,0,12)        // Mean age
 init_3darray AsdD(1,Treef,1,Fyear,0,12)         // SD of age
 init_4darray Index(1,2,1,Treef,1,Fyear,0,12)    // The index data

 number Prow                                     // Growth parameter
 number Wrec                                     // Weight-at-age recruitment
 number Wrec1                                    // Weight before recruitment

INITIALIZATION_SECTION
 M 0.2
 Fs 0
 
PARAMETER_SECTION
 init_bounded_number M(0,1,Est_M)                // Natural mortality rate
 init_bounded_vector Fs(1,NumFs,0,1,2)           // Initial Fs
 init_bounded_vector R(1,Nreef,0,1000)           // Recuitment
 init_bounded_number vv(-5,5,2)                  // power
 init_bounded_number ZZ(0,10)                    // Mean Q
 init_bounded_matrix q(1,2,1,Nreef,0,10)         // Normal qs
 
 number SigObs;                                  // Sigma-Q
 number SigQ2;                                   // Sigma-Q
 number SSI;                                     // Likelihood (age data)
 number SSG;                                     // Likelihood (Epsilons)
 number SSF;                                     // Likelihood (Indices)
 number SSJ;                                     // Penalty (initial depletions)
 number SSH;                                     // Q prior component
 vector Abar(1,Nreef);                           // Current mean age
 vector FFR(1,Nreef);                            // Last survival rate
 vector Qage(1,Nreef);                           // Q (age data)
 vector Eps(1,Nreef);                            // Epsilons
 matrix BioOut(1,Nreef,1,Fyear)                  // Biomass output
 3darray Bio1(1,Nreef,1,Fyear,1,12)              // Biomass outputs
 4darray Bio2(1,Nreef,1,Fyear,1,12,1,6)          // Data update
 vector InitialDep(1,Nreef)                      // Initial depletion
 matrix Nvals(1,Nreef,1,3)                       // Number of data points (UVS, catch survey, Age data)
 
 sdreport_number F97;                            // 1997 fishing mortality
 sdreport_number R3B96;                          // 1996 biomass - GF1
 sdreport_number R3B00;                          // 2000 biomass - GF1
 sdreport_number R3B00D96;                       // 2000 to 1996 biomass - GF1
 sdreport_number R4B96;                          // 1996 biomass - GF2
 sdreport_number R4B00;                          // 2000 biomass - GF2
 sdreport_number R4B00D96;                       // 2000 to 1996 biomass - GF2
 
 objective_function_value obj_f

PRELIMINARY_CALCS_SECTION
 
 // Specify growth per time step
 Prow = 0.9999999;
 
 // Specify weight-at-age 
 Wrec = 62.0*(1.0-mfexp(-0.15*(float(Arec)+1.09)));
 Wrec = 3.0*pow(Wrec/62.0,2.97);
 if (Is_Schnute == 1)
  {
   Wrec1 = 62.0*(1.0-mfexp(-0.15*(float(Arec)-1.0/12.0+1.09)));
   Wrec1 = 3.0*pow(Wrec1/62.0,2.97);
  }
 else
  Wrec1 = 0;  
 
PROCEDURE_SECTION

 // Reste OBJ
 obj_f = 0;

 // This function projects the population dynamics model forward
 DoConRec();
 
 // Compute the likelihood 
 Likelihood();

FUNCTION DoConRec
 dvariable N0;                                   // Numbers at age
 dvariable AbarVir;                              // Mean age in the virgin popln
 dvariable AbarAct;                              // Mean age in exploited population
 dvariable NMidVir;                              // Total numbers counter
 dvariable EXPZ;                                 // Storage for Exp(-Z)
 dvariable TotDemVir;                            // Used to get virgin recruitment
 dvariable TotDem;                               // Used to get initial biomass
 dvariable FF;                                   // Exploitation rate
 dvariable Ccat;                                 // The catches
 dvariable BBB1;                                 // Biomass
 dvariable BBB0;                                 // More biomass
 dvariable BBB2;                                 // Biomass
 dvariable NNN1;                                 // Numbers
 dvariable NNN2;                                 // Numbers
 dvariable FFF1;                                 // Exploitation rate
 dvariable FFF2;                                 // Exploitation rate
 dvariable AbarE;                                // Mean age
 dvariable AbarE1;                               // Mean age
 dvariable Term1;                                // Temporary variables
 dvariable Term2;                                // Temporary variables
 int Age, Month, Reef, IFrf, Year;

 // Find Wmid, Wrec and Abar
 N0 = 1; AbarVir = 0; NMidVir = 0;
 for (Age=0;Age<=50;Age++)
  for (Month=1;Month<=12;Month++)
   {                         
    if (Age >= Arec) 
     { NMidVir += N0; AbarVir += N0*float(Age+(Month-1)/12.0); }
    N0 = N0 * mfexp(-M/12.0); 
   }
 AbarVir = AbarVir / NMidVir;
 
 // Virgin biomass
 EXPZ = mfexp(-M/12.0);
 TotDemVir = (1.0-(1+Prow)*EXPZ+Prow*EXPZ*EXPZ)/(1.0-Wrec1/Wrec*Prow*EXPZ);
 
 // Compute the mean weight, the mean age and the survival - depends on whether
 // the population is fished or not!
 IFrf = 0;
 for (Reef=1;Reef<=Nreef;Reef++)
  if (B1DB0(Offset+Reef) == 1) 
   { 
    Abar(Reef) = AbarVir; 
    FFR(Reef) = 1.0; 
    InitialDep(Reef) = 1.0;
   }
  else
   {
    // Extract the fishing mortality and compute biomass
    IFrf += 1;
    FF = Fs(IFrf);
    EXPZ = mfexp(-M/12.0)*(1.0-FF);
    TotDem = (1.0-(1+Prow)*EXPZ+Prow*EXPZ*EXPZ)/(1.0-Wrec1/Wrec*Prow*EXPZ);
    
    // Now find Abar
    N0 = 1; AbarAct = 0; NMidVir = 0;
    for (Age=0;Age<=50;Age++)
    for (Month=1;Month<=12;Month++)
     {                         
      if (Age >= Arec) 
       { NMidVir += N0; AbarAct += N0*float(Age+(Month-1)/12.0); }
      N0 = N0 * mfexp(-M/12.0); 
      if (Age >= Arec) N0 = N0*(1-FF);
     }
    Abar(Reef) = AbarAct / NMidVir;
    FFR(Reef) = 1.0 - FF;
    InitialDep(Reef) = TotDemVir / TotDem;
   } 

 for (Reef=1;Reef<=Nreef;Reef++)
  {
   // Fished biomass
   EXPZ = mfexp(-M/12.0)*FFR(Reef);
   TotDem = (1.0-(1+Prow)*EXPZ+Prow*EXPZ*EXPZ)/(1.0-Wrec1/Wrec*Prow*EXPZ);
   BBB1 = R(Reef) / TotDem;
   BBB0 = BBB1;
   NNN1 = R(Reef) / Wrec / (1.0 - EXPZ);
   FFF1 = EXPZ;
   AbarE1 = Abar(Reef);
   
   // Project forward for each year
   for (Year=1;Year<=Fyear;Year++)
    for (Month=1;Month<=12;Month++)
     {
      // Store the initial biomass
      Bio1(Reef,Year,Month) = BBB1;
      
      // Update dynamics / survival rate
      Ccat = CtObs(Offset+Reef,Year,Month);
      FFF2 = (1.0 - Ccat/BBB1);
      FFF2 = FFF2 * mfexp(-M/12);
      if (FFF2 < 0.001) 
       {
        FFF2 = 0.001;
        obj_f += 1000;
       }
      if (Reef == 3 && Year+1994 == 1997 && Month ==4) F97 = Ccat/BBB1;
       
      // Update basic dynamics (biomass, numbers, mean age)
      Term1 = BBB1*(1.0+Prow)*FFF2 - Prow*FFF1*FFF2*BBB0;
      Term2 = R(Reef) - R(Reef)/Wrec*Wrec1*Prow*FFF1;
      BBB2 = Term1 + Term2;
      AbarE = (AbarE1 +1.0/12.0)*NNN1*FFF2;
      NNN2 = NNN1*FFF2 + R(Reef)/Wrec;
      AbarE = (AbarE + Arec*R(Reef)/Wrec)/NNN2;
      
      // Error checking
      if (BBB2 <= 0.0001) 
       { BBB2 = 0.0001; NNN2 = BBB2/Wrec; obj_f += 1000; }
       
      // Biomasses relative to CPUE
      Bio2(Reef,Year,Month,3) = (BBB1+BBB2)/2.0;
      Bio2(Reef,Year,Month,4) = pow((BBB1+BBB2)/2.0,vv);
      Bio2(Reef,Year,Month,1) = (NNN1+NNN2)/2.0 / Qprior(Offset+Reef);
      Bio2(Reef,Year,Month,2) = pow((NNN1+NNN2)/2.0,vv);
      Bio2(Reef,Year,Month,5) = AbarE1;
      Bio2(Reef,Year,Month,6) = BBB1;
//      cout << Year << " " << Month << " " << R(Reef) << " " << NNN1 << " " << BBB1 << " " << AbarE1 << " " << BBB1/NNN1 << " " << FFF2 << " " << Ccat << endl;
      
      // Update and storage
      if (Month == 1) BioOut(Reef,Year) = (BBB1+BBB2)/2.0;
      BBB0 = BBB1;
      BBB1 = BBB2;
      NNN1 = NNN2;
      FFF1 = FFF2;
      AbarE1 = AbarE;

     }
  }
 // Key outputs 
 R3B96 = BioOut(3,1996-1994);
 R3B00 = BioOut(3,2000-1994);
 R3B00D96 = R3B00/R3B96*100;
 R4B96 = BioOut(4,1996-1994);
 R4B00 = BioOut(4,2000-1994);
 R4B00D96 = R4B00/R4B96*100;

FUNCTION Likelihood
 dvariable Temp;                                 // Temporary
 dvariable Npoint, Numy;                         // Number of data points
 dvariable T1,T2,T3;                             // Temps
 dvariable Error;                                // Residual Error
 dvariable Obs,Pred;                             // Temps
 dvariable Npnt2;                                // Counter
 int Year, Month, Reef, iv;                      // Counters
 
 // Set sigma(q)
 SigObs = 0.2*0.2;
 SigQ2 = 0.4*0.4;
 
 // Find epsilon for each year
 SSG = 0;
 for (Year=1;Year<=Fyear;Year++)
  {
   Eps(Year) = 0; Numy = 0;
   for (Month=1;Month<=12;Month++)
    for (Reef=1;Reef<=Nreef;Reef++)
     for (iv=1;iv<=2;iv++)
      if (Index(iv,Offset+Reef,Year,Month) > 0)
      {
       Obs = log(Index(iv,Offset+Reef,Year,Month));
       Pred = log(q(iv,Reef)*Bio2(Reef,Year,Month,iv));
       Eps(Year) = Eps(Year) - (Obs - Pred);
       Numy += 1;
      }
   Eps(Year) = Eps(Year) / Numy;
   SSG += pow(Eps(Year),2);
   // cout << Year+1994 << " " << Numy << " " << Eps(Year) << endl;
  }
 SSG = sqrt(SSG/float(Fyear));
 SSG = float(Fyear)*log(SSG) + float(Fyear)/2.0;

 // Index data
 SSF = 0; SSH = 0; Npoint = 0;
 for (iv=1;iv<=2;iv++)
  for (Reef=1;Reef<=Nreef;Reef++)
   {
    // Term for Qprior
    if (iv == 1)
     { Error = log(q(1,Reef))-log(ZZ); SSH = SSH + square(Error)/(2.0*SigQ2); }

    // normal bit
    Npnt2 = 0;
    for (Year=1;Year<=Fyear;Year++)
     for (Month=1;Month<=12;Month++)
      if (Index(iv,Offset+Reef,Year,Month) > 0) 
       {
        Obs = log(Index(iv,Offset+Reef,Year,Month));
        Pred = log(q(iv,Reef)*Bio2(Reef,Year,Month,iv));
        SSF += square(Obs - Pred + Eps(Year));
        Npoint += 1;
        Npnt2 += 1;
        // cout << Reef << " " << Year+1994 << " " << iv << " " << mfexp(Obs) << " " << mfexp(Pred) << " " << SSF << endl;
       }
    Nvals(Reef,iv) = Npnt2;
   }
 SSF = 0.5*Npoint*log(SigObs) + SSF/(2.0*SigObs);

 // Age data
 for (Reef = 1;Reef<=Nreef;Reef++)
  {
   Qage(Reef) = 0;
   Temp = 0; Npoint = 0;
   for (Year=1;Year<=Fyear;Year++)
    for (Month=1;Month<=12;Month++)
     if (AbarD(Offset+Reef,Year,Month) > 0 && AsdD(Offset+Reef,Year,Month) > 0)
      {
       T1 = AbarD(Offset+Reef,Year,Month);
       T2 = Bio2(Reef,Year,Month,5);
       T3 = AsdD(Offset+Reef,Year,Month);
       Qage(Reef) += T1*T2/(T3*T3);
       Temp += T2*T2/(T3*T3);
       Npoint += 1;
      } 
   if (Est_Qage == 1) 
    Qage(Reef) = 1;
   else    
    Qage(Reef) = Qage(Reef)/Temp;
   Nvals(Reef,3) = Npoint; 
  }                

 SSI = 0;
 for (Reef=1;Reef<=Nreef;Reef++)
  for (Year=1;Year<=Fyear;Year++)
   for (Month=1;Month<=12;Month++)
    if (AbarD(Offset+Reef,Year,Month) > 0 && AsdD(Offset+Reef,Year,Month) > 0)
     {
      Error = AbarD(Offset+Reef,Year,Month)-Qage(Reef)*Bio2(Reef,Year,Month,5);
      SSI += Error*Error/square(AsdD(Offset+Reef,Year,Month));
      // cout << Reef << " " << Year+1995 << " " << Month << " " << AbarD(Offset+Reef,Year,Month) << " " << AsdD(Offset+Reef,Year,Month) << " " << Qage(Reef)*Bio2(Reef,Year,Month,5) << " " << SSI << endl;
     }
 SSI = SSI / 2.0;
 
 // Target depletion
 SSJ = 0;
 for (Reef=1;Reef<=Nreef;Reef++)
  SSJ += square(InitialDep(Reef)-B1target(Reef))/square(B1SD(Reef));
 
 obj_f = obj_f + SSI + SSG + SSF + SSH + SSJ;
 cout << obj_f << " " << SSI << " " << SSG << " " << SSF << " " << SSH << " " << SSJ << endl;    


REPORT_SECTION
 dvariable Obs, Pred, Error;
 int iv,Reef,Month,Year;
 
 report <<  SSG << " " << SSF << " " << SSH << " " << SSI << " " << SSJ << endl;
 report << obj_f << endl;
 report << " " << endl;

 report << "Inititial Depletions" << endl;
 for (Reef=1;Reef<=Nreef;Reef++)
  report << Reef << " " << InitialDep(Reef) << " " << R(Reef) <<" " << vv << " " << M << endl;
 report << " " << endl;
 report << "Catchability cofficients" << endl;
 for (iv=1;iv<=2;iv++)
  for (Reef=1;Reef<=Nreef;Reef++)
    report << iv << " " << Reef << " " << q(iv,Reef) << endl;
 report << "ZZ = "<< ZZ << endl;
 report << " " << endl;
 report << "Epsilons" << endl;   
 for (Year=1;Year<=Fyear;Year++)
  report << " " << Year << " " << Eps(Year) << endl;   

 report << " " << endl;
 report << "Number of data points" << endl;
 report << Nvals << endl;
 report << " " << endl;
 
 for (iv=1;iv<=2;iv++)
  for (Reef=1;Reef<=Nreef;Reef++)
   for (Year=1;Year<=Fyear;Year++)
    for (Month=1;Month<=12;Month++)
     if (Index(iv,Offset+Reef,Year,Month) > 0) 
      {
       Obs = log(Index(iv,Offset+Reef,Year,Month));
       Pred = log(q(iv,Reef)*Bio2(Reef,Year,Month,iv))-Eps(Year);
       report << Reef << " " << Year+1994 << " " <<setw(2) << Month << " " << iv << " " << setw(12)<<setprecision(5)<< mfexp(Obs) << " " <<setw(12)<<setprecision(5) << mfexp(Pred) <<  " " <<
            (Obs-Pred)/sqrt(SigObs) << endl;
       }
 report << " " << endl;      
     
 for (Reef=1;Reef<=Nreef;Reef++)
  for (Year=1;Year<=Fyear;Year++)
   for (Month=1;Month<=12;Month++)
    if (AbarD(Offset+Reef,Year,Month) > 0 && AsdD(Offset+Reef,Year,Month) > 0)
     {
      Error = AbarD(Offset+Reef,Year,Month)-Qage(Reef)*Bio2(Reef,Year,Month,5);
      Error = Error / AsdD(Offset+Reef,Year,Month);
      report << Reef << " " << Year+1995 << " " <<setw(2)<< Month << " " << setw(8)<< AbarD(Offset+Reef,Year,Month) << " " <<setw(8)<< Qage(Reef)*Bio2(Reef,Year,Month,5) << " " << Error << endl;
     }
 report << " " << endl;
 for (Reef=1;Reef<=Nreef;Reef++)
  for (Year=1;Year<=Fyear;Year++)
   for (Month=1;Month<=12;Month++)
    report << Reef << " " << Year << " " << Month << " " << Bio2(Reef,Year,Month,6) << endl;

 report << R3B96 << " " << R3B00 << " " << R3B00D96 << " " << F97 << endl;
 report << R4B96 << " " << R4B00 << " " << R4B00D96 << endl; 


