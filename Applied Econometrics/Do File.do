cls
clear 
global REG "E:\Edu\Masters\ECON 599\Term Paper\Data"
cap log close 

set more off 

use "$REG/2006.dta"

*drop if AGEP<25 | AGEP>64 /*Using age 25 to 64*/
keep if WKSWRK>=49 & HRSWRK>=30 /*Worked at least 30hrs/wk for more than 49wks*/
*drop if FPTWK==2 /*Only full time workers*/
drop if HDGREE==88
drop if CIP==88
drop if NAICS==88
drop if NOCS==88
drop if WAGES==.

/*Generating log of earnings*/
drop if WAGES<=1
gen lnwage=log(WAGES)
drop if lnwage==. 

/*Gender*/
gen FEMALE= 1 if SEX==1
replace FEMALE= 0 if SEX==2

/*Age Groups*/
gen AGE = .
replace AGE = 1 if AGEGRP==9 | AGEGRP==10 
label define AGE 1 "25-34 years", modify
replace AGE = 2 if AGEGRP==11 | AGEGRP==12 
label define AGE 2 "35-44 years", modify
replace AGE = 3 if AGEGRP==13 | AGEGRP==14
label define AGE 3 "45-54 years", modify
replace AGE = 4 if AGEGRP==15 | AGEGRP==16
label define AGE 4 "55-64 years", modify
drop if AGE == .
drop AGEGRP
rename  AGE AGEGRP
label variable AGEGRP "AGE GROUPS"
label values AGEGRP AGE
tabulate AGEGRP, nofreq generate(AGEGRP)

/*Visible Minority*/
replace VISMIN = 1 if VISMIN!=13 /*Visible Minority*/
label define VISMIN 1 "Visible MInority", modify
replace VISMIN = 0 if VISMIN==13 /*Not a Visible Miniority*/
label define VISMIN 0 "Not a Visible MInority", modify
label variable VISMIN "VISIBLE MINORITY"
label values VISMIN VISMIN

/*Occupation*/
replace NOCS = 1 if NOCS==1 | NOCS==2
replace NOCS = 2 if NOCS>=3 & NOCS<=5
replace NOCS = 3 if NOCS==6 
replace NOCS = 4 if NOCS>=7 & NOCS<=9
replace NOCS = 5 if NOCS==10
replace NOCS = 6 if NOCS==11 
replace NOCS = 7 if NOCS>=12 & NOCS<=17
replace NOCS = 8 if NOCS>=18 & NOCS<=22
replace NOCS = 9 if NOCS==23
replace NOCS = 10 if NOCS>=24 & NOCS<=25
tabulate NOCS, nofreq generate(NOCS)

tabulate HDGREE, nofreq generate(HDGREE)
tabulate CIP, nofreq generate(CIP)
tabulate NAICS, nofreq generate(NAICS)
tabulate KOL, nofreq generate(KOL)

#delimit;
estpost tabstat FEMALE KOL VISMIN PR CITIZEN MARST HDGREE CIP NAICS NOCS AGEGRP 
WAGES, s(n mean median sd min max) columns(statistics); 
esttab using "$REG/Des_summary06.csv",replace cells("count mean p50 sd min max")
noobs nomtitle nonumber varlabels(`e(labels)');

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS20 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST [aw=WEIGHT] if FEMALE==0;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;
est store MT2006;

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS20 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST [aw=WEIGHT] if FEMALE==1;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;
est store FT2006;

oaxaca lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS20 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST 
[aw=WEIGHT], by(FEMALE) noisily pooled;
est store DT2006;

esttab MT2006 FT2006 using "$REG/RegT2006.csv", replace mtitle("Male" "Female") 
starlevels(* 0.01) stats(N r2, labels ("Observations" "R-Squared")) 
b(%4.3f) wide plain;

esttab DT2006 using "$REG/DT2006.csv", replace b(%4.3f) starlevels(* 0.01) 
stats(N r2, labels ("Observations" "R-Squared")) wide plain;

replace HDGREE1 = . if HDGREE1==0;
replace HDGREE2 = . if HDGREE2==0;
replace HDGREE3 = . if HDGREE3==0;
replace HDGREE4 = . if HDGREE4==0;
replace HDGREE5 = . if HDGREE5==0;
replace HDGREE6 = . if HDGREE6==0;
replace HDGREE7 = . if HDGREE7==0;
replace HDGREE8 = . if HDGREE8==0;
replace HDGREE9 = . if HDGREE9==0;
replace HDGREE10 = . if HDGREE10==0;
replace HDGREE11 = . if HDGREE11==0;
replace HDGREE12 = . if HDGREE12==0;
replace HDGREE13 = . if HDGREE13==0;
bysort HDGREE: egen EduWagesM=mean(WAGES) if FEMALE==0; 
bysort HDGREE: egen EduWagesF=mean(WAGES) if FEMALE==1;

/*Share of full-time for each educational attainment for Male*/
estpost tabstat EduWagesM, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Male06.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of full-time for each educational attainment for Female*/
estpost tabstat EduWagesF, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Female06.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

drop if CIP==13;
replace CIP1 = . if CIP1==0;
replace CIP2 = . if CIP2==0;
replace CIP3 = . if CIP3==0;
replace CIP4 = . if CIP4==0;
replace CIP5 = . if CIP5==0;
replace CIP6 = . if CIP6==0;
replace CIP7 = . if CIP7==0;
replace CIP8 = . if CIP8==0;
replace CIP9 = . if CIP9==0;
replace CIP10 = . if CIP10==0;
replace CIP11 = . if CIP11==0;
replace CIP12 = . if CIP12==0;

bysort CIP: egen CIPWagesM=mean(WAGES) if FEMALE==0; 
bysort CIP: egen CIPWagesF=mean(WAGES) if FEMALE==1;

/*Share of field of study for Male*/
estpost tabstat CIPWagesM, stats(count mean) by(CIP);
esttab using "$REG/Field_Share_Male06.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of field of study for Female*/
estpost tabstat CIPWagesF, stats(count mean) by(CIP);
esttab using "$REG/Field_Share_Female06.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');
#delimit cr

clear 

use "$REG/2011.dta"

keep if WKSWRK==6 /*Worked at least 30hrs/wk for more than 49wks*/
*drop if FPTWK==2 /*Only full time workers*/
drop if HDGREE==88
drop if CIP2011==88
drop if NAICS==88
drop if NOCS==88

/*Generating log weekly earnings*/
drop if WAGES<=1 | WAGES==8888888
gen lnwage=log(WAGES)
drop if lnwage==.

/*Gender*/
gen FEMALE= 1 if SEX==1
replace FEMALE= 0 if SEX==2

/*Age Groups*/
gen AGE = .
replace AGE = 1 if AGEGRP==9 | AGEGRP==10 
label define AGE 1 "25-34 years", modify
replace AGE = 2 if AGEGRP==11 | AGEGRP==12 
label define AGE 2 "35-44 years", modify
replace AGE = 3 if AGEGRP==13 | AGEGRP==14
label define AGE 3 "45-54 years", modify
replace AGE = 4 if AGEGRP==15 | AGEGRP==16
label define AGE 4 "55-64 years", modify
drop if AGE == .
drop AGEGRP
rename  AGE AGEGRP
label variable AGEGRP "AGE GROUPS"
label values AGEGRP AGE
tabulate AGEGRP, nofreq generate(AGEGRP)

/*Visible Minority*/
replace VISMIN = 1 if VISMIN!=13 /*Visible Minority*/
label define VISMIN 1 "Visible MInority", modify
replace VISMIN = 0 if VISMIN==13 /*Not a Visible Miniority*/
label define VISMIN 0 "Not a Visible MInority", modify
label variable VISMIN "VISIBLE MINORITY"
label values VISMIN VISMIN

tabulate NOCS, nofreq generate(NOCS)
tabulate HDGREE, nofreq generate(HDGREE)
tabulate CIP2011, nofreq generate(CIP)
tabulate NAICS, nofreq generate(NAICS)
tabulate KOL, nofreq generate(KOL)

#delimit;
*Summary Statistics;
estpost tabstat FEMALE KOL VISMIN PR CITIZEN MARST HDGREE CIP2011 NAICS NOCS AGEGRP 
WAGES, s(n mean median sd min max) columns(statistics); 
esttab using "$REG/Des_summary11.csv",replace cells("count mean p50 sd min max")
noobs nomtitle nonumber varlabels(`e(labels)');

qui reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST [aw=WEIGHT] if FEMALE==0;
est store MT2011;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

qui reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST [aw=WEIGHT] if FEMALE==1;
est store FT2011;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

oaxaca lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR CITIZEN MARST 
[aw=WEIGHT], by(FEMALE) noisily pooled;
est store DT2011;

esttab MT2011 FT2011 using "$REG/RegT2011.csv", replace mtitle("Male" "Female") 
starlevels(* 0.01) stats(N r2, labels ("Observations" "R-Squared")) 
b(%4.3f) wide plain;

esttab DT2011 using "$REG/DT2011.csv", replace b(%4.3f) starlevels(* 0.01) 
stats(N r2, labels ("No. of Obs." "R-Squared")) wide plain;

replace HDGREE1 = . if HDGREE1==0;
replace HDGREE2 = . if HDGREE2==0;
replace HDGREE3 = . if HDGREE3==0;
replace HDGREE4 = . if HDGREE4==0;
replace HDGREE5 = . if HDGREE5==0;
replace HDGREE6 = . if HDGREE6==0;
replace HDGREE7 = . if HDGREE7==0;
replace HDGREE8 = . if HDGREE8==0;
replace HDGREE9 = . if HDGREE9==0;
replace HDGREE10 = . if HDGREE10==0;
replace HDGREE11 = . if HDGREE11==0;
replace HDGREE12 = . if HDGREE12==0;
replace HDGREE13 = . if HDGREE13==0;
bysort HDGREE: egen EduWagesM=mean(WAGES) if FEMALE==0; 
bysort HDGREE: egen EduWagesF=mean(WAGES) if FEMALE==1;

/*Share of full-time for each educational attainment for Male*/
estpost tabstat EduWagesM, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Male11.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of full-time for each educational attainment for Female*/
estpost tabstat EduWagesF, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Female11.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

drop if CIP2011==13;
replace CIP1 = . if CIP1==0;
replace CIP2 = . if CIP2==0;
replace CIP3 = . if CIP3==0;
replace CIP4 = . if CIP4==0;
replace CIP5 = . if CIP5==0;
replace CIP6 = . if CIP6==0;
replace CIP7 = . if CIP7==0;
replace CIP8 = . if CIP8==0;
replace CIP9 = . if CIP9==0;
replace CIP10 = . if CIP10==0;
replace CIP11 = . if CIP11==0;
replace CIP12 = . if CIP12==0;

bysort CIP2011: egen CIPWagesM=mean(WAGES) if FEMALE==0; 
bysort CIP2011: egen CIPWagesF=mean(WAGES) if FEMALE==1;

/*Share of field of study for Male*/
estpost tabstat CIPWagesM, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Male11.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of field of study for Female*/
estpost tabstat CIPWagesF, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Female11.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');
#delimit cr

clear

cls
clear 
global REG "E:\Edu\Masters\ECON 599\Term Paper\Data"
cap log close 

set more off 

use "$REG/2016.dta"

keep if WKSWRK==6 /*Worked at least 30hrs/wk for more than 49wks*/
*keep if FPTWK==1 /*Only full time workers*/
drop if HDGREE==88
drop if CIP2011==88
drop if NAICS==88
drop if NOCS==88


/*Generating log weekly earnings*/
drop if Wages<=1
gen lnwage=log(Wages)
drop if lnwage==.

/*Gender*/
gen FEMALE= 1 if Sex==1
replace FEMALE= 0 if Sex==2

/*Age Groups*/
gen AGE = .
replace AGE = 1 if AGEGRP==9 | AGEGRP==10 
label define AGE 1 "25-34 years", modify
replace AGE = 2 if AGEGRP==11 | AGEGRP==12 
label define AGE 2 "35-44 years", modify
replace AGE = 3 if AGEGRP==13 | AGEGRP==14
label define AGE 3 "45-54 years", modify
replace AGE = 4 if AGEGRP==15 | AGEGRP==16
label define AGE 4 "55-64 years", modify
drop if AGE == .
drop AGEGRP
rename  AGE AGEGRP
label variable AGEGRP "AGE GROUPS"
label values AGEGRP AGE
tabulate AGEGRP, nofreq generate(AGEGRP)

/*Visible Minority*/
rename  VisMin VISMIN
replace VISMIN = 1 if VISMIN!=13 /*Visible Minority*/
label define VISMIN 1 "Visible MInority", modify
replace VISMIN = 0 if VISMIN==13 /*Not a Visible Miniority*/
label define VISMIN 0 "Not a Visible MInority", modify
label variable VISMIN "VISIBLE MINORITY"
label values VISMIN VISMIN


tabulate NOCS, nofreq generate(NOCS)
tabulate HDGREE, nofreq generate(HDGREE)
tabulate CIP2011, nofreq generate(CIP)
tabulate NAICS, nofreq generate(NAICS)
tabulate KOL, nofreq generate(KOL)

#delimit;
*Summary Statistics;
estpost tabstat FEMALE KOL VISMIN PR Citizen MarStH HDGREE CIP2011 NAICS NOCS AGEGRP 
Wages, s(n mean median sd min max) columns(statistics); 
esttab using "$REG/Des_summary16.csv",replace cells("count mean p50 sd min max")
noobs nomtitle nonumber varlabels(`e(labels)');

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH [aw=WEIGHT] if FEMALE==0;
est store MT2016;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH [aw=WEIGHT] if FEMALE==1;
est store FT2016;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

oaxaca lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH 
[aw=WEIGHT], by(FEMALE) noisily pooled;
est store DT2016;

esttab MT2016 FT2016 using "$REG/RegT2016.csv", replace mtitle("Male" "Female") 
stats(N r2, labels ("Observations" "R-Squared")) 
b(%4.3f) wide plain;

esttab DT2016 using "$REG/DT2016.csv", replace b(%4.3f)
stats(N r2, labels ("Observations" "R-Squared"));


replace HDGREE1 = . if HDGREE1==0;
replace HDGREE2 = . if HDGREE2==0;
replace HDGREE3 = . if HDGREE3==0;
replace HDGREE4 = . if HDGREE4==0;
replace HDGREE5 = . if HDGREE5==0;
replace HDGREE6 = . if HDGREE6==0;
replace HDGREE7 = . if HDGREE7==0;
replace HDGREE8 = . if HDGREE8==0;
replace HDGREE9 = . if HDGREE9==0;
replace HDGREE10 = . if HDGREE10==0;
replace HDGREE11 = . if HDGREE11==0;
replace HDGREE12 = . if HDGREE12==0;
replace HDGREE13 = . if HDGREE13==0;
bysort HDGREE: egen EduWagesM=mean(Wages) if FEMALE==0; 
bysort HDGREE: egen EduWagesF=mean(Wages) if FEMALE==1;

/*Share of full-time for each educational attainment for Male*/
estpost tabstat EduWagesM, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of full-time for each educational attainment for Female*/
estpost tabstat EduWagesF, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Female16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

drop if CIP2011==13;
replace CIP1 = . if CIP1==0;
replace CIP2 = . if CIP2==0;
replace CIP3 = . if CIP3==0;
replace CIP4 = . if CIP4==0;
replace CIP5 = . if CIP5==0;
replace CIP6 = . if CIP6==0;
replace CIP7 = . if CIP7==0;
replace CIP8 = . if CIP8==0;
replace CIP9 = . if CIP9==0;
replace CIP10 = . if CIP10==0;
replace CIP11 = . if CIP11==0;
replace CIP12 = . if CIP12==0;

bysort CIP2011: egen CIPWagesM=mean(Wages) if FEMALE==0; 
bysort CIP2011: egen CIPWagesF=mean(Wages) if FEMALE==1;

/*Share of field of study for Male*/
estpost tabstat CIPWagesM, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

estpost tabstat CIPWagesM, stats(count mean) by(NOCS);
esttab using "$REG/Field_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of field of study for Female*/
estpost tabstat CIPWagesF, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Female16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');
#delimit cr

clear

use "$REG/2016.dta"

keep if WKSWRK==6 /*Worked at least 30hrs/wk for more than 49wks*/
*keep if FPTWK==1 /*Only full time workers*/
drop if HDGREE==88
drop if CIP2011==88
drop if NAICS==88
drop if NOCS==88


/*Generating log weekly earnings*/
drop if Wages<=1
gen lnwage=log(Wages)
drop if lnwage==.

/*Gender*/
gen FEMALE= 1 if Sex==1
replace FEMALE= 0 if Sex==2

/*Age Groups*/
gen AGE = .
replace AGE = 1 if AGEGRP==9 | AGEGRP==10 
label define AGE 1 "25-34 years", modify
replace AGE = 2 if AGEGRP==11 | AGEGRP==12 
label define AGE 2 "35-44 years", modify
replace AGE = 3 if AGEGRP==13 | AGEGRP==14
label define AGE 3 "45-54 years", modify
replace AGE = 4 if AGEGRP==15 | AGEGRP==16
label define AGE 4 "55-64 years", modify
drop if AGE == .
drop AGEGRP
rename  AGE AGEGRP
label variable AGEGRP "AGE GROUPS"
label values AGEGRP AGE
tabulate AGEGRP, nofreq generate(AGEGRP)

/*Visible Minority*/
rename  VisMin VISMIN
replace VISMIN = 1 if VISMIN!=13 /*Visible Minority*/
label define VISMIN 1 "Visible MInority", modify
replace VISMIN = 0 if VISMIN==13 /*Not a Visible Miniority*/
label define VISMIN 0 "Not a Visible MInority", modify
label variable VISMIN "VISIBLE MINORITY"
label values VISMIN VISMIN


tabulate NOCS, nofreq generate(NOCS)
tabulate HDGREE, nofreq generate(HDGREE)
tabulate CIP2011, nofreq generate(CIP)
tabulate NAICS, nofreq generate(NAICS)
tabulate KOL, nofreq generate(KOL)

#delimit;
*Summary Statistics;
estpost tabstat FEMALE KOL VISMIN PR Citizen MarStH HDGREE CIP2011 NAICS NOCS AGEGRP 
Wages, s(n mean median sd min max) columns(statistics); 
esttab using "$REG/Des_summary16.csv",replace cells("count mean p50 sd min max")
noobs nomtitle nonumber varlabels(`e(labels)');

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH [aw=WEIGHT] if FEMALE==0;
est store MT2016;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

reg lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH [aw=WEIGHT] if FEMALE==1;
est store FT2016;
test HDGREE2 HDGREE3 HDGREE4 HDGREE5 HDGREE6 HDGREE7 HDGREE8 HDGREE9 HDGREE10 
HDGREE11 HDGREE12 HDGREE13;
test CIP1 CIP2 CIP4 CIP5 CIP6 CIP7 CIP8 CIP9 CIP10 CIP11;
estat ovtest;

oaxaca lnwage HDGREE2-HDGREE13 CIP1 CIP2 CIP4-CIP11 NAICS2-NAICS19 NOCS2-NOCS10 
AGEGRP2-AGEGRP4 KOL2-KOL4 VISMIN PR Citizen MarStH 
[aw=WEIGHT], by(FEMALE) noisily pooled;
est store DT2016;

esttab MT2016 FT2016 using "$REG/RegT2016.csv", replace mtitle("Male" "Female") 
stats(N r2, labels ("Observations" "R-Squared")) 
b(%4.3f) wide plain;

esttab DT2016 using "$REG/DT2016.csv", replace b(%4.3f)
stats(N r2, labels ("Observations" "R-Squared"));


replace HDGREE1 = . if HDGREE1==0;
replace HDGREE2 = . if HDGREE2==0;
replace HDGREE3 = . if HDGREE3==0;
replace HDGREE4 = . if HDGREE4==0;
replace HDGREE5 = . if HDGREE5==0;
replace HDGREE6 = . if HDGREE6==0;
replace HDGREE7 = . if HDGREE7==0;
replace HDGREE8 = . if HDGREE8==0;
replace HDGREE9 = . if HDGREE9==0;
replace HDGREE10 = . if HDGREE10==0;
replace HDGREE11 = . if HDGREE11==0;
replace HDGREE12 = . if HDGREE12==0;
replace HDGREE13 = . if HDGREE13==0;
bysort HDGREE: egen EduWagesM=mean(Wages) if FEMALE==0; 
bysort HDGREE: egen EduWagesF=mean(Wages) if FEMALE==1;

/*Share of full-time for each educational attainment for Male*/
estpost tabstat EduWagesM, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of full-time for each educational attainment for Female*/
estpost tabstat EduWagesF, stats(count mean) by(HDGREE);
esttab using "$REG/Edu_Share_Female16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

drop if CIP2011==13;
replace CIP1 = . if CIP1==0;
replace CIP2 = . if CIP2==0;
replace CIP3 = . if CIP3==0;
replace CIP4 = . if CIP4==0;
replace CIP5 = . if CIP5==0;
replace CIP6 = . if CIP6==0;
replace CIP7 = . if CIP7==0;
replace CIP8 = . if CIP8==0;
replace CIP9 = . if CIP9==0;
replace CIP10 = . if CIP10==0;
replace CIP11 = . if CIP11==0;
replace CIP12 = . if CIP12==0;

bysort CIP2011: egen CIPWagesM=mean(Wages) if FEMALE==0; 
bysort CIP2011: egen CIPWagesF=mean(Wages) if FEMALE==1;

/*Share of field of study for Male*/
estpost tabstat CIPWagesM, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

#delimit;
estpost tabstat CIPWagesM, stats(count mean) by(NOCS);
esttab using "$REG/Field_Share_Male16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');

/*Share of field of study for Female*/
estpost tabstat CIPWagesF, stats(count mean) by(CIP2011);
esttab using "$REG/Field_Share_Female16.rtf", replace cells("count mean") 
noobs nomtitle nonumber varlabels(`e(labels)');
#delimit cr
