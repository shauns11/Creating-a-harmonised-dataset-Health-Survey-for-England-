**********************************************************************************************************
*Estimating trends in cardiovascular disease risk: 
*protocol for the creation of a harmonised dataset for the ExPoSE study (The Health Survey for England)
**********************************************************************************************************

use "C:\Users\rmjdshc\OneDrive - University College London\Expose\Datasets\HSE1998-2017_15102024.dta", clear
keep id year sex age_h cholval13 glyhb_h omsysval highbp bpmedc_h bpmedd_h cigsta3 topqual3 bmival2 diabete2 ///
point1 gor wt_int wt_nurse wt_blood hdlval1a  pilluse pillevr samptype ///
sys1om sys2om sys3om dias1om dias2om dias3om htval wtval2 hdlval1a wt_int_s1 wt_nurse_s1 wt_blood_s1 cluster nuroutc
sort year id
gen id2=_n     // unique 

*variable for estimating linear trends.
gen yr_tr=0
replace yr_tr=0 if year==1998
replace yr_tr=1 if year==1999
replace yr_tr=2 if year==2000
replace yr_tr=3 if year==2001
replace yr_tr=4 if year==2002
replace yr_tr=5 if year==2003
replace yr_tr=6 if year==2004
replace yr_tr=7 if year==2005
replace yr_tr=8 if year==2006
replace yr_tr=9 if year==2007
replace yr_tr=10 if year==2008
replace yr_tr=11 if year==2009
replace yr_tr=12 if year==2010
replace yr_tr=13 if year==2011
replace yr_tr=14 if year==2012
replace yr_tr=15 if year==2013
replace yr_tr=16 if year==2014
replace yr_tr=17 if year==2015
replace yr_tr=18 if year==2016
replace yr_tr=19 if year==2017
label variable yr_tr "survey year for linear trend"
tab1 yr_tr

***********************************
*Flowchart
***********************************

count                                                  // 171,088
count if inlist(year,1999,2000,2004)
tab1 year
*171,088: 2005 includes boost (2673) and core (7630).
*drop the boost in 2005
drop if year==2005 & samptype==2                        //  2673 
count                                                   // 168,415
tab1 year
count if inrange(age,16,39)|inrange(age,75,120)         //  75,980
count if inrange(age,40,74)                             // 92,435
count    												// 168,415

*168,415
tab1 year                   						// Just the core in 2005

*gor.
replace gor=99 if gor==-2
replace gor=99 if missing(gor)

*age.
rename age_h age

*convert negative values in HSE (-9,-8,-1) to missing.
*identify complete cases.
replace glyhb_h=. if glyhb_h<0
replace cigsta3=. if cigsta3<0
replace diabete2=. if diabete2<0

*Current Smoking status.
generate smk=.
replace smk=0 if inlist(cigsta3,2,3)
replace smk=1 if cigsta3==1
label define smklbl 0 "No" 1 "Yes"
label values smk smklbl
tab1 smk

*Total diabetes (requires known HbA1c).
generate tdm=.
label var tdm "Total diabetes" 
replace tdm=0 if inrange(glyhb_h,0,6.4) & (diabete2==2)
replace tdm=1 if inrange(glyhb_h,6.5,17)|(diabete2==1 & inrange(glyhb_h,0,6.4)) 
replace tdm=. if glyhb_h==.|diabete2==.
label define diablbl 0 "No" 1 "Yes"
label values tdm diablbl
tab1 tdm

*Diagnosed diabetes
generate dm=.
label var dm "Diagnosed diabetes" 
replace dm=0 if (diabete2==2)
replace dm=1 if (diabete2==1) 
replace dm=. if diabete2==.
label define diab2lbl 0 "No" 1 "Yes"
label values dm diab2lbl

************************
**CVD RISK COMPONENTS**
**SBP**
************************

replace sys1om=. if sys1om<60
replace sys1om=. if sys1om>270
gen sbp1=sys1om
label var sbp1 "Systolic Blood Pressure [mmHg] - reading 1"

replace sys2om=. if sys2om<60
replace sys2om=. if sys2om>270
gen sbp2=sys2om
label var sbp2 "Systolic Blood Pressure [mmHg] - reading 2"

replace sys3om=. if sys3om<60
replace sys3om=. if sys3om>270
gen sbp3=sys3om
label var sbp3 "Systolic Blood Pressure [mmHg] - reading 3"
 
replace dias1om=. if dias1om<60
replace dias1om=. if dias1om>270
gen dbp1=dias1om
label var dbp1 "Diastolic Blood Pressure [mmHg] - reading 1"
 
replace dias2om=. if dias2om<60
replace dias2om=. if dias2om>270
gen dbp2=dias2om
label var dbp2 "Diastolic Blood Pressure [mmHg] - reading 2"
 
replace dias3om=. if dias3om<60
replace dias3om=. if dias3om>270
gen dbp3=dias3om
label var dbp3 "Diastolic Blood Pressure [mmHg] - reading 3"

replace sbp1=. if sys1om-dias1om<15
replace dbp1=. if sys1om-dias1om<15
replace sbp2=. if sys2om-dias2om<15
replace dbp2=. if sys2om-dias2om<15
replace sbp3=. if sys3om-dias3om<15
replace dbp3=. if sys3om-dias3om<15
egen sbp_mean = rowmean(sbp2 sbp3)
label var sbp_mean "Mean Systolic Blood Pressure [mmHg], 2nd & 3rd reading"
rename sbp_mean sbp_mean2
egen dbp_mean = rowmean(dbp2 dbp3)
label var dbp_mean "Mean Diastolic Blood Pressure [mmHg], 2nd & 3rd reading"
rename dbp_mean dbp_mean2
summ sbp_mean2 dbp_mean2
rename sbp_mean2 sbp
*compare with archived variable.
summ sbp omsysval if (omsysval>0)
drop omsysval

************************
**CVD RISK COMPONENTS**
**BMI**
************************

rename htval height
replace height=. if height<120 | height>220
label var height "Height [cm] - Average of available readings"
rename wtval2 weight
replace weight=. if weight<25 & sex==2 /*implausible women*/
replace weight=. if weight<35 & sex==1 /*implausible men*/
replace weight=. if weight>250 /*implausible*/
label var weight "Weight [kg] - Average of available readings"
gen bmi=weight/(height/100)^2
replace bmi=. if bmi<10 | bmi>131
label var bmi "Body Mass Index [kg/m2], calculated"
*compare with archived variable.
summ bmi bmival2 if bmival2>0
drop bmival2

************************
**CVD RISK COMPONENTS**
**Total cholesterol**
************************

gen chol_tot=cholval13
replace chol_tot=. if cholval13<0
replace chol_tot=. if cholval13<1.75 | cholval13>20.00 
label var chol_tot "Total cholesterol [mmol/l]"
sum chol_tot
*  (D) Valid Total Cholesterol Result mmol/L (incl those on lipid-lowering drugs) (sample received a...

gen chol_hdl=hdlval1a
replace chol_hdl=. if hdlval1a<0
replace chol_hdl=. if hdlval1a<0.40 | hdlval1a>5.00 
label var chol_hdl "High-density lipoprotein (HDL) cholesterol [mmol/l]"
sum chol_hdl
drop hdlval1a

replace chol_tot=. if chol_tot<chol_hdl & chol_hdl !=.
replace chol_tot=. if chol_hdl>chol_tot & chol_hdl !=.

rename chol_tot tc
*compare with archived variable.
summ tc cholval13 if cholval13>0 
drop cholval13

*******************************************
*WHO CVD risk score.
*******************************************

*Binary variables (diagnosed diabetes).
tab1 diabete2, nolab
gen hxdiabbin=.
replace hxdiabbin=0 if diabete2==2
replace hxdiabbin=1 if diabete2==1
tab1 hxdiabbin

*smoking status.
gen smallbin=smk
gen ages = age
gen ccode = "GBR"

***WHOCVDRisk: lab score (40-74).
***N=35,405.
***N=33,629 (2006 just has CVD group).
preserve
drop if year==2005
keep if inrange(age,40,74) & tc!=. & smallbin!=. & (sbp!=.) & hxdiabbin!=.
summ sex age smallbin hxdiabbin sbp tc 
tab1 year
drop if year==2006 & samptype==2
tab1 year
restore

***WHOCVDRisk: lab score (40-74).
***Analysis of missing.
preserve
drop if year==2005
drop if year==2006 & samptype==2
keep if inrange(age,40,74) & inlist(year,1998,2003,2006,2009,2010,2011,2012,2013,2014,2015,2016,2017) 
egen b= rowmiss(tc smallbin sbp hxdiabbin)
tab b
count if b>0
keep if b>0
summ tc smallbin sbp hxdiabbin
count if missing(sbp)|missing(tc)
restore

***WHOCVDRisk: non-lab score (40-74).
***N=61,629.
preserve
keep if inrange(age,40,74) & bmi!=. & smallbin!=. & (sbp!=.) 
summ sex age bmi smallbin sbp 
tab1 year
restore

clonevar tchol = tc

*************************
*WHO CVD risk programme
*************************

whocvdrisk

rename cal2_who_cvdx_m1 cvdrisk_lab
rename cal2_who_cvdx_m2 cvdrisk_office

*************************************************************
*deal with 2006 here (65+ not asked CVD module, so no diabetes)
*************************************************************

replace cvdrisk_lab =. if year==2006 & samptype==2

gen WHOflagLab=0
gen WHOflagOffice=0

*Lab.
replace WHOflagLab = 1 if inrange(age,40,74) & (tchol>1.99 & tchol!=.) & smallbin!=. & (sbp!=.) & hxdiabbin!=. ///
& inlist(year,1998,2003,2006,2009,2010,2011,2012,2013,2014,2015,2016,2017)

*Non-lab
replace WHOflagOffice = 1 if inrange(age,40,74) & smallbin!=. & (sbp!=.) & inrange(bmi,10,80) ///
& inlist(year,1998,2001,2002,2003,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
tab WHOflagLab WHOflagOffice

*Analytical samples:
*Lab:     33,628.
*Non-Lab: 61,629.
preserve
tab1 year
summ age if (cvdrisk_lab!=. & inrange(age,40,74) & WHOflagLab==1)   
summ age if (cvdrisk_office!=. & inrange(age,40,74) & WHOflagOffice==1 )  
restore

gen who_lab = (cvdrisk_lab!=. & inrange(age,40,74) & WHOflagLab==1)  
gen who_nonlab = (cvdrisk_office!=. & inrange(age,40,74) & WHOflagOffice==1)  
tab1 who_lab who_nonlab

*************************
*WHO (non-lab): n=61629
*************************

preserve
keep if who_nonlab
summ age
tab1 sex topqual3
svyset, clear
svyset [pw=wt_nurse],psu(point1) strata(gor)
svy:mean age,over(sex year)
svy:mean sbp,over(sex year)
svy:mean smk,over(sex year)
svy:mean bmi,over(sex year)
svy:mean cvdrisk_office,over(sex year)
restore

*************************
*WHO (lab): n= 33,628
*************************

preserve
keep if who_lab
summ age
tab1 sex topqual3
svyset, clear
svyset [pw=wt_blood],psu(point1) strata(gor)
svy:mean age,over(sex year)
svy:mean sbp,over(sex year)
svy:mean smk,over(sex year)
svy:mean tc,over(sex year)
svy:mean hxdiabbin,over(sex year)
svy:mean cvdrisk_lab,over(sex year)
restore


**************************
*Dataset for Globorisk.
**************************

gen iso="GBR"
format iso %-3s  // left-aligned

gen hseyear=year
drop year
gen year=2020        // baseline year

*sex variable, must be 0 = man and 1 = woman!
gen sex2=sex
recode sex2 (1=0) (2=1)

keep id2 hseyear year who_lab who_nonlab wt_nurse wt_blood point1 gor age sex sex2  ///
sbp smk tc dm cvdrisk_office cvdrisk_lab bmi iso hxdiabbin yr_tr

summ sex2 age sbp tc dm hxdiabbin smk bmi cvdrisk_office cvdrisk_lab who_lab who_nonlab

preserve
keep if who_lab==1
keep id2 year sex2 age sbp tc dm smk iso 
save "C:\Users\rmjdshc\OneDrive - University College London\Expose\Datasets\Globorisk_dataset_forR_lab.dta", replace
restore


preserve
keep if who_nonlab==1
keep id2 year sex2 age sbp smk bmi iso 
save "C:\Users\rmjdshc\OneDrive - University College London\Expose\Datasets\Globorisk_dataset_forR_office.dta", replace
restore

sort id2 

***********************************************
*R script for calculating Globorisk is below.
***********************************************

merge 1:1 id2 using "C:\Users\rmjdshc\OneDrive - University College London\Expose\Datasets\scores1.dta", keepusing(GloboLab) nogen
merge 1:1 id2 using "C:\Users\rmjdshc\OneDrive - University College London\Expose\Datasets\scores2.dta", keepusing(GloboOffice) nogen
count

********************************.
*Globorisk (non-lab): n=61629
********************************.

preserve
keep if who_nonlab
svyset, clear
svyset [pw=wt_nurse],psu(point1) strata(gor)
svy:mean GloboOffice,over(sex hseyear)
restore

*************************.
*Globorisk (lab): n= 33628
*************************.

preserve
keep if who_lab
svyset, clear
svyset [pw=wt_blood],psu(point1) strata(gor)
svy:mean GloboLab,over(sex hseyear)
restore

gen male=sex==1
gen female=sex==2

*percentage
gen cvdrisk_office_p = cvdrisk_office*100
gen GloboOffice_p = GloboOffice*100
gen cvdrisk_lab_p = cvdrisk_lab*100
gen GloboLab_p = GloboLab*100


**********
*Non-lab.
**********

preserve
keep if who_nonlab==1
svyset, clear
svyset [pw=wt_nurse],psu(point1) strata(gor)

*first vs last survey year.
svy:mean cvdrisk_office,over(sex hseyear)
lincom  _b[c.cvdrisk_office@1.sex#1998bn.hseyear] - _b[c.cvdrisk_office@1.sex#2017.hseyear]
lincom  _b[c.cvdrisk_office@2.sex#1998bn.hseyear] - _b[c.cvdrisk_office@2.sex#2017.hseyear]

svy:mean GloboOffice,over(sex hseyear)
lincom _b[c.GloboOffice@1.sex#1998bn.hseyear] - _b[c.GloboOffice@1.sex#2017.hseyear]
lincom _b[c.GloboOffice@2.sex#1998bn.hseyear] - _b[c.GloboOffice@2.sex#2017.hseyear]

*correlations
svy,subpop(male): regress cvdrisk_office GloboOffice
di e(r2)^0.5
svy,subpop(female): regress cvdrisk_office GloboOffice
di e(r2)^0.5

*linear trend (percentage)
svy,subpop(male): regress cvdrisk_office_p yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(female): regress cvdrisk_office_p yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(male): regress GloboOffice_p  yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(female): regress GloboOffice_p  yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
restore

**********
*lab.
**********

preserve
keep if who_lab==1
svyset, clear
svyset [pw=wt_blood],psu(point1) strata(gor)

*first vs last survey year.
svy:mean cvdrisk_lab,over(sex hseyear)
lincom  _b[c.cvdrisk_lab@1.sex#1998bn.hseyear] - _b[c.cvdrisk_lab@1.sex#2017.hseyear] 
lincom  _b[c.cvdrisk_lab@2.sex#1998bn.hseyear] - _b[c.cvdrisk_lab@2.sex#2017.hseyear]

svy:mean GloboLab,over(sex hseyear)
lincom _b[c.GloboLab@1.sex#1998bn.hseyear] - _b[c.GloboLab@1.sex#2017.hseyear] 
lincom _b[c.GloboLab@2.sex#1998bn.hseyear] - _b[c.GloboLab@2.sex#2017.hseyear]

svy,subpop(male): regress GloboLab cvdrisk_lab
di e(r2)^0.5
svy,subpop(female): regress GloboLab cvdrisk_lab
di e(r2)^0.5

svy,subpop(male): regress cvdrisk_lab_p yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(female): regress cvdrisk_lab_p yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(male): regress GloboLab_p  yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
svy,subpop(female): regress GloboLab_p  yr_tr
margins, at(yr_tr=(0(1)19)) post coeflegend
lincom _b[20._at] - _b[1bn._at]
restore


*************************************
*Changes in CVD components (lab)
*************************************

preserve
keep if who_lab==1
svyset, clear
svyset [pw=wt_blood],psu(point1) strata(gor)
*BP.
svy:mean sbp,over(sex hseyear) 
lincom _b[c.sbp@1bn.sex#1998bn.hseyear] - _b[c.sbp@1bn.sex#2017.hseyear]
lincom _b[c.sbp@2.sex#1998bn.hseyear] - _b[c.sbp@2.sex#2017.hseyear]
svy,subpop(male): regress sbp yr_tr
svy,subpop(female): regress sbp yr_tr
*Diabetes
svy:mean hxdiabbin,over(sex hseyear) 
lincom   _b[c.hxdiabbin@1bn.sex#1998bn.hseyear] - _b[c.hxdiabbin@1bn.sex#2017.hseyear]
lincom  _b[c.hxdiabbin@2.sex#1998bn.hseyear] - _b[c.hxdiabbin@2.sex#2017.hseyear]
svy,subpop(male): regress hxdiabbin yr_tr
svy,subpop(female): regress hxdiabbin yr_tr
*smoke.
svy:mean smk,over(sex hseyear) 
lincom  _b[c.smk@1bn.sex#1998bn.hseyear] - _b[c.smk@1bn.sex#2017.hseyear]
lincom  _b[c.smk@2.sex#1998bn.hseyear] - _b[c.smk@2.sex#2017.hseyear]
svy,subpop(male): regress smk yr_tr
svy,subpop(female): regress smk yr_tr
*TC.
svy:mean tc,over(sex hseyear) 
lincom  _b[c.tc@1bn.sex#1998bn.hseyear] - _b[c.tc@1bn.sex#2017.hseyear]
lincom _b[c.tc@2.sex#1998bn.hseyear] - _b[c.tc@2.sex#2017.hseyear] 
svy,subpop(male): regress tc yr_tr
svy,subpop(female): regress tc yr_tr
restore

*************************************
*changes in CVD components (Non-lab)
*************************************

preserve
keep if who_nonlab==1
svyset, clear
svyset [pw=wt_nurse],psu(point1) strata(gor)
*BP.
svy:mean sbp,over(sex hseyear) 
lincom _b[c.sbp@1bn.sex#1998bn.hseyear] - _b[c.sbp@1bn.sex#2017.hseyear]
lincom _b[c.sbp@2.sex#1998bn.hseyear] - _b[c.sbp@2.sex#2017.hseyear]
svy,subpop(male): regress sbp yr_tr
svy,subpop(female): regress sbp yr_tr
*smoke.
svy:mean smk,over(sex hseyear) 
lincom _b[c.smk@1bn.sex#1998bn.hseyear] - _b[c.smk@1bn.sex#2017.hseyear]
lincom _b[c.smk@2.sex#1998bn.hseyear] - _b[c.smk@2.sex#2017.hseyear] 
svy,subpop(male): regress smk yr_tr
svy,subpop(female): regress smk yr_tr
*BMI
svy:mean bmi,over(sex hseyear) 
lincom _b[c.bmi@1bn.sex#1998bn.hseyear] - _b[c.bmi@1bn.sex#2017.hseyear]
lincom _b[c.bmi@2.sex#1998bn.hseyear] - _b[c.bmi@2.sex#2017.hseyear]
svy,subpop(male): regress bmi yr_tr
svy,subpop(female): regress bmi yr_tr
restore

*interpretation: linear trend: annual decrease in CVD risk (expressed as a %).
preserve
keep if who_lab==1
svyset, clear
svyset [pw=wt_blood],psu(point1) strata(gor)
svy,subpop(male): regress cvdrisk_lab_p yr_tr
margins,at(yr_tr=(0(1)20))
restore

****************
*FINISHED.
****************






















***************
*R script
***************

*setwd ("C:/Users/rmjdshc/OneDrive - University College London/Expose/Datasets/")
*library(haven)
*library(globorisk)
*lab_data<-read_dta("Globorisk_dataset_forR_lab.dta")
*head(lab_data)
*office_data<-read_dta("Globorisk_dataset_forR_office.dta")
*head(office_data)

*###############################################################################################
*#Globo-lab uses information on age, sex, SBP, diabetes, smoking status, and total cholesterol. 
*# estimate 10-year risk of CVD using laboratory calculator 
*# 6 variables.
*# outcome = fatal + non-fatal
*################################################################################################

*GloboLab<-globorisk(
*  sex = lab_data$sex2,
*  age = lab_data$age,
*  sbp = lab_data$sbp,
*  tc = lab_data$tc,
*  dm = lab_data$dm,
*  smk = lab_data$smk,
*  iso = lab_data$iso,
*  year = lab_data$year,
*  version = "lab",
*  type = "risk"
*)
*summary(GloboLab)
*lab_data$GloboLab<-GloboLab
*head(lab_data)
*#export stata dataset.
*haven::write_dta(lab_data,"scores1.dta")

*###############################################.
*#Globo-office model, BMI replaces tc & and dm.
*#Office (5 vars).
*#age must be 40-80.
*###############################################.

*GloboOffice<-globorisk(
*  sex = office_data$sex2,
*  age = office_data$age,
*  sbp = office_data$sbp,
*  smk = office_data$smk,
*  bmi = office_data$bmi,
*  iso = office_data$iso,
*  year = office_data$year,
*  version = "office",
*  type = "risk"
*)
*summary(GloboOffice)
*office_data$GloboOffice<-GloboOffice
*head(office_data)
*summary(office_data$age)
*#export stata dataset.
*haven::write_dta(office_data,"scores2.dta")












































