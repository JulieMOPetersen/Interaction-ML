
*Creating spreadsheet that lists new FFQ labels with coded categories and comments - for consensus process;

libname julie "I:\Bodnar Julie\";

proc import out=julie.nuMoM2B_toadd datafile='I:\Bodnar Julie\numom_extradiet_morepree_tomerge_9.19.2022.dta' dbms=DTA replace; run;

proc import out=julie.nuMoM2B_cleaned datafile='I:\Bodnar Julie\Cleaned nuMoM2B dataset_draft_7.5.2022.dta' dbms=DTA replace; run;

proc import out=julie.nuMoM2B_cleaned220824 datafile='I:\Bodnar Julie\Cleaned nuMoM2B dataset_draft_8.24.2022_REDUCED for Ashley.dta' dbms=DTA replace; run;

proc contents data  = julie.nuMoM2B_cleaned;
run;

proc contents data  = julie.nuMoM2B_toadd;
run;

proc contents data  = julie.nuMoM2B_cleaned220824;
run;

data old;
 set julie.nuMoM2B_cleaned;
 keep numomid f_totdens_old v_totdens_old
 f_totdens80_old 
v_totdens80_old 
d_totdens_old p_totdens_old p_seaplantdens_old
fatratio_old g_nwhldens_old g_whldens_old
sodium_dens_old pct_emptyc_old;
run;

proc sort data = old; by numomid; run;
proc sort data = julie.nuMoM2B_cleaned220824; by numomid; run;
proc sort data = julie.nuMoM2B_toadd; by numomid; run;

data nuMoM2B_all_old;
 merge julie.nuMoM2B_cleaned220824 julie.nuMoM2B_toadd old;
 by numomid;

 	*flag for GA window;
	if gestage2 ge 20 then GA20wkplus = 1;
	else if gestage2 ne . then GA20wkplus = 0;

	if  pree_acog ne . and GA20wkplus = 1;
	*pregoutcome =1 livebirth,2 = 2 stillbirth, = 3 miscarriage, = 4 termination;

		fv_totdens_old  = f_totdens_old + v_totdens_old;

	rename v1_physicalactivity_tot_metminwk = METminwk;	
run;

proc means data = nuMoM2B_all_old n nmiss mean;
var momage epds_tot_v1 anx_tot stress_tot_v1 METminwk bmiprepreg
d_totdens_old p_totdens_old p_seaplantdens_old
fatratio_old g_nwhldens_old g_whldens_old
sodium_dens_old pct_emptyc_old;
run;

proc freq data = nuMoM2B_all_old;
	tables fv_totdens80 momagecat3 momrace4 momeduc4  smokerpre married insurpub
			momaccult pregplanned prediab prehtn sleepsat3 bmiprepregcat3 pree_acog/missing;
run; 

data MLInt_impute_all;
	set nuMoM2B_all_old;

if fv_totdens_old ge 2.41 then fv_totdens_old_80 = 1;
else fv_totdens_old_80 = 0;

if fv_totdens_old = . then FFQmissind = 1;
else FFQmissind = 0;

*Creating dummy variables for all categorical variables;
if momrace4 = 1 then momrace41 = 1; else momrace41 = 0;
if momrace4 = 2 then momrace42 = 1; else momrace42 = 0;
if momrace4 = 3 then momrace43 = 1; else momrace43 = 0;
if momrace4 = 4 then momrace44 = 1; else momrace44 = 0;

if momeduc4 = 1 then momeduc41 = 1; else momeduc41 = 0;
if momeduc4 = 2 then momeduc42 = 1; else momeduc42 = 0;
if momeduc4 = 3 then momeduc43 = 1; else momeduc43 = 0;
if momeduc4 = 4 then momeduc44 = 1; else momeduc44 = 0;

if smokerpre = . then smokerpre = rand('binomial', 0.1675, 1);
if pregplanned = . then pregplanned = rand('binomial', 0.6179, 1);
if sleepsat3 = . then sleepsat31 = rand('binomial', 0.1860, 1);
else if sleepsat3 = 1 then sleepsat31 = 1;
else if sleepsat3 in (2, 3) then sleepsat31 = 0;
if sleepsat3 = . and sleepsat31 ne 1 then sleepsat33 = rand('binomial', 0.3978, 1);
else if sleepsat31 = 1 then sleepsat33 = 0;
else if sleepsat3 = 3 then sleepsat33 = 1;
else if sleepsat3 in (1, 2) then sleepsat33 = 0;

if sleepsat31 = 1 then sleepsat3 = 1;
else if sleepsat33 = 1 then sleepsat3 = 3;
else if sleepsat31 = 0 and sleepsat33 = 0 then sleepsat3 = 2;

if sleepsat31 = 0 and sleepsat33 = 0 then sleepsat32 = 1; else sleepsat32 = 0;

if epds_tot_v1 = . then epds_tot_v1 = 5.7457703;
if anx_tot = . then anx_tot = 33.8160555;
if stress_tot_v1 = . then stress_tot_v1 = 12.7427426;
if METminwk = . then METminwk = 720.6691222;
if bmiprepreg = . then bmiprepreg = 25.5745412;

if bmiprepreg <25 then bmiprepregcat3 = 1;
else if 25 le bmiprepreg <30 then bmiprepregcat3 = 2;
else if bmiprepreg ge 30 then bmiprepregcat3 = 3;

if bmiprepregcat3 = 1 then bmiprepregcat31 = 1; else bmiprepregcat31 = 0;
if bmiprepregcat3 = 2 then bmiprepregcat32 = 1; else bmiprepregcat32 = 0;
if bmiprepregcat3 = 3 then bmiprepregcat33 = 1; else bmiprepregcat33 = 0;

if momaccult = 1 then momaccult1 = 1; else momaccult1 = 0;
if momaccult = 2 then momaccult2 = 1; else momaccult2 = 0;
if momaccult = 3 then momaccult3 = 1; else momaccult3 = 0;
if momaccult = 4 then momaccult4 = 1; else momaccult4 = 0;
if momaccult = 5 then momaccult5 = 1; else momaccult5 = 0;
if momaccult = 6 then momaccult6 = 1; else momaccult6 = 0;

if momagecat3 = 1 then momagecat31 = 1; else momagecat31 = 0;
if momagecat3 = 2 then momagecat32 = 1; else momagecat32 = 0;
if momagecat3 = 3 then momagecat33 = 1; else momagecat33 = 0;

if momrace4 = 2 then black = 1; else black =0;
if momeduc4 in (3, 4) then college = 1; else college = 0;

if bmiprepreg <18.5 then bmiprepreg61 = 1; else bmiprepreg61 = 0;
if 18.5 le bmiprepreg <25 then bmiprepreg62 = 1; else bmiprepreg62 = 0;
if 25 le bmiprepreg <30 then bmiprepreg63 = 1; else bmiprepreg63 = 0;
if 30 le bmiprepreg <35 then bmiprepreg64 = 1; else bmiprepreg64 = 0;
if 35 le bmiprepreg <40 then bmiprepreg65 = 1; else bmiprepreg65 = 0;
if bmiprepreg ge 40 then bmiprepreg66 = 1; else bmiprepreg66 = 0;

if bmiprepreg <18.5 then bmiprepreg6 = 1; 
else if 18.5 le bmiprepreg <25 then bmiprepreg6 = 0; 
else if 25 le bmiprepreg <30 then bmiprepreg6 = 3; 
else if 30 le bmiprepreg <35 then bmiprepreg6 = 4; 
else if 35 le bmiprepreg <40 then bmiprepreg6 = 5; 
else if bmiprepreg ge 40 then bmiprepreg6 = 6; 

if d_totdens = . then d_totdens = 0.9513927;
if p_totdens = . then p_totdens = 2.4976945;
if p_seaplantdens = . then p_seaplantdens = 0.8771856;
if fatratio = . then fatratio = 1.9460437;
if g_nwhldens = . then g_nwhldens = 2.1848304;
if g_whldens = . then g_whldens = 0.5490443;
if sodium_dens = . then sodium_dens = 1.6121092;
if pct_addsug = . then pct_addsug = 0.1146959;
if pct_satfat = . then pct_satfat = 0.1108599;

if  FFQmissind ne 1;

run;

*Information for Tables 1 and 2;

proc means data = MLInt_impute_all min max;
var f_totdens_old;
class f_totdens80_old;
run;

proc means data = MLInt_impute_all min max;
var v_totdens_old;
class v_totdens80_old;
run;

proc freq data = MLInt_impute_all;
	tables (f_totdens80_old v_totdens80_old)*pree_acog/nocum;
run; 

proc freq data = MLInt_impute_all;
	tables (momagecat3 momrace4 momeduc4  smokerpre married insurpub
			momaccult pregplanned prediab prehtn sleepsat3 bmiprepregcat3 )*(f_totdens80_old v_totdens80_old)/norow nopercent nocum;
run; 
*race 1= White NH, 2 = Black NH, 3 = Hispanic, 4 = Other;
*momeduc4 1 = Some or completed high school, 2 = some undergraduate or assoc/technical degree, 3 = Bachelors, 4 = Masters, 5 = Gradaute;
*label define accult 1 "Born in US/Parents Born in US" 2 "Born in US/one parent immigrant" 3 "Born in US/Both parents immigr" 
4 "Born outside US-came as a child <=5 yrs" 5 "Born outside US-came as an older child 6-17 yrs" 6 "Born outside US-came as an adult";

proc means data = MLInt_impute_all n nmiss median q1 q3;
class f_totdens80_old;
var epds_tot_v1 anx_tot stress_tot_v1 METminwk 
f_totdens_old v_totdens_old
d_totdens_old p_totdens_old p_seaplantdens_old
fatratio_old g_nwhldens_old g_whldens_old 
sodium_dens_old pct_emptyc_old;
run;

proc means data = MLInt_impute_all n nmiss median q1 q3;
class v_totdens80_old;
var epds_tot_v1 anx_tot stress_tot_v1 METminwk 
f_totdens_old v_totdens_old
d_totdens_old p_totdens_old p_seaplantdens_old
fatratio_old g_nwhldens_old g_whldens_old 
sodium_dens_old pct_emptyc_old;
run;

data MLInt_impute_old_ffqavail;
set MLInt_impute_all;
keep numomid fv_totdens_old_80 fv_totdens80 pree_acog momage epds_tot_v1 anx_tot stress_tot_v1 METminwk 
momrace41 momrace42 momrace43 momrace44 momeduc41 momeduc42 momeduc43 momeduc44 sleepsat31 sleepsat32 sleepsat33 
bmiprepreg bmiprepreg61 bmiprepreg62 bmiprepreg63 bmiprepreg64 bmiprepreg65 bmiprepreg66 bmiprepregcat3 bmiprepreg6
smokerpre married insurpub
momaccult1 momaccult2 momaccult3 momaccult4 momaccult5 momaccult6 
d_totdens_old p_totdens_old p_seaplantdens_old
fatratio_old g_nwhldens_old g_whldens_old
f_totdens80_old f_totdens_old
v_totdens80_old v_totdens_old
sodium_dens_old pct_emptyc_old pct_addsug pct_satfat 
d_totdens g_nwhldens g_whldens p_seaplantdens
p_totdens sodium_dens fatratio 
pregplanned prediab prehtn
black
college
momagecat31 momagecat32 momagecat33
momagecat3 momrace4 momeduc4
momaccult
sleepsat3
;
run;

proc export data = MLInt_impute_old_ffqavail
	outfile = "I:\Bodnar Julie\nuMoM2b\Interaction ML\data\MLInteraction_Imputed_ffqavail_oldnutr_2022-09-27.csv" 
	DBMS = csv replace;
run;

proc freq data = MLInt_impute_old_ffqavail;
tables (f_totdens80_old v_totdens80_old)*pree_acog;
run;
