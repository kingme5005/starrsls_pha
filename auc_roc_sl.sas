

*sl output;
proc import out=predprob_sample1_test
			datafile = "/harvard/aking/DataQC/pha/data/predprob_sample1_test.csv"
			dbms=csv replace;
		getnames= yes;
		datarow=2;
run;
proc import out=predprob_sample2_test
			datafile = "/harvard/aking/DataQC/pha/data/predprob_sample2_test.csv"
			dbms=csv replace;
		getnames= yes;
		datarow=2;
run;
proc import out=predprob_sample3_test
			datafile = "/harvard/aking/DataQC/pha/data/predprob_sample3_test.csv"
			dbms=csv replace;
		getnames= yes;
		datarow=2;
run;
proc import out=predprob_sample4_test
			datafile = "/harvard/aking/DataQC/pha/data/predprob_sample4_test.csv"
			dbms=csv replace;
		getnames= yes;
		datarow=2;
run;







proc logistic data=predprob_sample1_test rocoptions(weighted);
	weight weight1;
	model outcome_6msa (event='1')=/nofit outroc=harvrd04.roc_sample1_sl_test;
	roc pred=pred_prob;
	ods output rocassociation=auc_sample1_test;
run;
data harvrd04.roc_sample1_sl_test;
	set harvrd04.roc_sample1_sl_test;
	format _sensit_ 7.6 _1mspec_ 7.6;
	keep _sensit_ _1mspec_;
run;



proc logistic data=predprob_sample2_test rocoptions(weighted);
	weight weight1;
	model outcome_6msa (event='1')=/nofit outroc=harvrd04.roc_sample2_sl_test;
	roc pred=pred_prob;
	ods output rocassociation=auc_sample2_test;
run;
data harvrd04.roc_sample2_sl_test;
	set harvrd04.roc_sample2_sl_test;
	format _sensit_ 7.6 _1mspec_ 7.6;
	keep _sensit_ _1mspec_;
run;



proc logistic data=predprob_sample3_test rocoptions(weighted);
	weight weight1;
	model outcome_6msa (event='1')=/nofit outroc=harvrd04.roc_sample3_sl_test;
	roc pred=pred_prob;
	ods output rocassociation=auc_sample3_test;
run;
data harvrd04.roc_sample3_sl_test;
	set harvrd04.roc_sample3_sl_test;
	format _sensit_ 7.6 _1mspec_ 7.6;
	keep _sensit_ _1mspec_;
run;



proc logistic data=predprob_sample4_test rocoptions(weighted);
	weight weight1;
	model outcome_6msa (event='1')=/nofit outroc=harvrd04.roc_sample4_sl_test;
	roc pred=pred_prob;
	ods output rocassociation=auc_sample4_test;
run;
data harvrd04.roc_sample4_sl_test;
	set harvrd04.roc_sample4_sl_test;
	format _sensit_ 7.6 _1mspec_ 7.6;
	keep _sensit_ _1mspec_;
run;



*/*superlearner model;
data predprob_sample1_test;
	set predprob_sample1_test;
	id=1;
	
run;
proc univariate data=predprob_sample1_test;
	weight weight1;
	var pred_prob;
	output out=pSL_cut_points pctlpts =1 to 100 by 1 pctlpre = total;
run;
data pSL_cut_points;
	set pSL_cut_points;
	id=1;
run;
data pp_sl1;
	merge 	predprob_sample1_test
			pSL_cut_points;
	by id;
	
	if pred_prob ge total95 then ventile_cat=1;
	else if pred_prob ge total90 then ventile_cat=2;
	else if pred_prob ge total85 then ventile_cat=3;
	else if pred_prob ge total80 then ventile_cat=4;
	else if pred_prob ge total75 then ventile_cat=5;
	else if pred_prob ge total70 then ventile_cat=6;
	else if pred_prob ge total65 then ventile_cat=7;
	else if pred_prob ge total60 then ventile_cat=8;
	else if pred_prob ge total55 then ventile_cat=9;
	else if pred_prob ge total50 then ventile_cat=10;
	else if pred_prob ge total45 then ventile_cat=11;
	else if pred_prob ge total40 then ventile_cat=12;
	else if pred_prob ge total35 then ventile_cat=13;
	else if pred_prob ge total30 then ventile_cat=14;
	else if pred_prob ge total25 then ventile_cat=15;
	else if pred_prob ge total20 then ventile_cat=16;
	else if pred_prob ge total15 then ventile_cat=17;
	else if pred_prob ge total10 then ventile_cat=18;
	else if pred_prob ge total5 then ventile_cat=19;
	else ventile_cat=20;
	
	if ventile_cat le 2 then ventile_cat2=1;
	if ventile_cat le 3 then ventile_cat3=1;
	if ventile_cat le 4 then ventile_cat4=1;
	if ventile_cat le 5 then ventile_cat5=1;
	if ventile_cat le 6 then ventile_cat6=1;
	if ventile_cat le 7 then ventile_cat7=1;
	if ventile_cat le 8 then ventile_cat8=1;
	if ventile_cat le 9 then ventile_cat9=1;
	if ventile_cat le 10 then ventile_cat10=1;
	if ventile_cat le 11 then ventile_cat11=1;
	if ventile_cat le 12 then ventile_cat12=1;
	if ventile_cat le 13 then ventile_cat13=1;
	if ventile_cat le 14 then ventile_cat14=1;
	if ventile_cat le 15 then ventile_cat15=1;
	if ventile_cat le 16 then ventile_cat16=1;
	if ventile_cat le 17 then ventile_cat17=1;
	if ventile_cat le 18 then ventile_cat18=1;
	if ventile_cat le 19 then ventile_cat19=1;

run; 


data predprob_sample2_test;
	set predprob_sample2_test;
	id=1;
	
run;
proc univariate data=predprob_sample2_test;
	weight weight1;
	var pred_prob;
	output out=pSL_cut_points pctlpts =1 to 100 by 1 pctlpre = total;
run;
data pSL_cut_points;
	set pSL_cut_points;
	id=1;
run;
data pp_sl2;
	merge 	predprob_sample2_test
			pSL_cut_points;
	by id;
	
	if pred_prob ge total95 then ventile_cat=1;
	else if pred_prob ge total90 then ventile_cat=2;
	else if pred_prob ge total85 then ventile_cat=3;
	else if pred_prob ge total80 then ventile_cat=4;
	else if pred_prob ge total75 then ventile_cat=5;
	else if pred_prob ge total70 then ventile_cat=6;
	else if pred_prob ge total65 then ventile_cat=7;
	else if pred_prob ge total60 then ventile_cat=8;
	else if pred_prob ge total55 then ventile_cat=9;
	else if pred_prob ge total50 then ventile_cat=10;
	else if pred_prob ge total45 then ventile_cat=11;
	else if pred_prob ge total40 then ventile_cat=12;
	else if pred_prob ge total35 then ventile_cat=13;
	else if pred_prob ge total30 then ventile_cat=14;
	else if pred_prob ge total25 then ventile_cat=15;
	else if pred_prob ge total20 then ventile_cat=16;
	else if pred_prob ge total15 then ventile_cat=17;
	else if pred_prob ge total10 then ventile_cat=18;
	else if pred_prob ge total5 then ventile_cat=19;
	else ventile_cat=20;
	
	if ventile_cat le 2 then ventile_cat2=1;
	if ventile_cat le 3 then ventile_cat3=1;
	if ventile_cat le 4 then ventile_cat4=1;
	if ventile_cat le 5 then ventile_cat5=1;
	if ventile_cat le 6 then ventile_cat6=1;
	if ventile_cat le 7 then ventile_cat7=1;
	if ventile_cat le 8 then ventile_cat8=1;
	if ventile_cat le 9 then ventile_cat9=1;
	if ventile_cat le 10 then ventile_cat10=1;
	if ventile_cat le 11 then ventile_cat11=1;
	if ventile_cat le 12 then ventile_cat12=1;
	if ventile_cat le 13 then ventile_cat13=1;
	if ventile_cat le 14 then ventile_cat14=1;
	if ventile_cat le 15 then ventile_cat15=1;
	if ventile_cat le 16 then ventile_cat16=1;
	if ventile_cat le 17 then ventile_cat17=1;
	if ventile_cat le 18 then ventile_cat18=1;
	if ventile_cat le 19 then ventile_cat19=1;

run; 

*sample3;
data predprob_sample3_test;
	set predprob_sample3_test;
	id=1;
	
run;
proc univariate data=predprob_sample3_test;
	weight weight1;
	var pred_prob;
	output out=pSL_cut_points pctlpts =1 to 100 by 1 pctlpre = total;
run;
data pSL_cut_points;
	set pSL_cut_points;
	id=1;
run;
data pp_sl3;
	merge 	predprob_sample3_test
			pSL_cut_points;
	by id;
	
	if pred_prob ge total95 then ventile_cat=1;
	else if pred_prob ge total90 then ventile_cat=2;
	else if pred_prob ge total85 then ventile_cat=3;
	else if pred_prob ge total80 then ventile_cat=4;
	else if pred_prob ge total75 then ventile_cat=5;
	else if pred_prob ge total70 then ventile_cat=6;
	else if pred_prob ge total65 then ventile_cat=7;
	else if pred_prob ge total60 then ventile_cat=8;
	else if pred_prob ge total55 then ventile_cat=9;
	else if pred_prob ge total50 then ventile_cat=10;
	else if pred_prob ge total45 then ventile_cat=11;
	else if pred_prob ge total40 then ventile_cat=12;
	else if pred_prob ge total35 then ventile_cat=13;
	else if pred_prob ge total30 then ventile_cat=14;
	else if pred_prob ge total25 then ventile_cat=15;
	else if pred_prob ge total20 then ventile_cat=16;
	else if pred_prob ge total15 then ventile_cat=17;
	else if pred_prob ge total10 then ventile_cat=18;
	else if pred_prob ge total5 then ventile_cat=19;
	else ventile_cat=20;
	
	if ventile_cat le 2 then ventile_cat2=1;
	if ventile_cat le 3 then ventile_cat3=1;
	if ventile_cat le 4 then ventile_cat4=1;
	if ventile_cat le 5 then ventile_cat5=1;
	if ventile_cat le 6 then ventile_cat6=1;
	if ventile_cat le 7 then ventile_cat7=1;
	if ventile_cat le 8 then ventile_cat8=1;
	if ventile_cat le 9 then ventile_cat9=1;
	if ventile_cat le 10 then ventile_cat10=1;
	if ventile_cat le 11 then ventile_cat11=1;
	if ventile_cat le 12 then ventile_cat12=1;
	if ventile_cat le 13 then ventile_cat13=1;
	if ventile_cat le 14 then ventile_cat14=1;
	if ventile_cat le 15 then ventile_cat15=1;
	if ventile_cat le 16 then ventile_cat16=1;
	if ventile_cat le 17 then ventile_cat17=1;
	if ventile_cat le 18 then ventile_cat18=1;
	if ventile_cat le 19 then ventile_cat19=1;

run; 

*sample4;
data predprob_sample4_test;
	set predprob_sample4_test;
	id=1;
	
run;
proc univariate data=predprob_sample4_test;
	weight weight1;
	var pred_prob;
	output out=pSL_cut_points pctlpts =1 to 100 by 1 pctlpre = total;
run;
data pSL_cut_points;
	set pSL_cut_points;
	id=1;
run;
data pp_sl4;
	merge 	predprob_sample4_test
			pSL_cut_points;
	by id;
	
	if pred_prob ge total95 then ventile_cat=1;
	else if pred_prob ge total90 then ventile_cat=2;
	else if pred_prob ge total85 then ventile_cat=3;
	else if pred_prob ge total80 then ventile_cat=4;
	else if pred_prob ge total75 then ventile_cat=5;
	else if pred_prob ge total70 then ventile_cat=6;
	else if pred_prob ge total65 then ventile_cat=7;
	else if pred_prob ge total60 then ventile_cat=8;
	else if pred_prob ge total55 then ventile_cat=9;
	else if pred_prob ge total50 then ventile_cat=10;
	else if pred_prob ge total45 then ventile_cat=11;
	else if pred_prob ge total40 then ventile_cat=12;
	else if pred_prob ge total35 then ventile_cat=13;
	else if pred_prob ge total30 then ventile_cat=14;
	else if pred_prob ge total25 then ventile_cat=15;
	else if pred_prob ge total20 then ventile_cat=16;
	else if pred_prob ge total15 then ventile_cat=17;
	else if pred_prob ge total10 then ventile_cat=18;
	else if pred_prob ge total5 then ventile_cat=19;
	else ventile_cat=20;
	
	if ventile_cat le 2 then ventile_cat2=1;
	if ventile_cat le 3 then ventile_cat3=1;
	if ventile_cat le 4 then ventile_cat4=1;
	if ventile_cat le 5 then ventile_cat5=1;
	if ventile_cat le 6 then ventile_cat6=1;
	if ventile_cat le 7 then ventile_cat7=1;
	if ventile_cat le 8 then ventile_cat8=1;
	if ventile_cat le 9 then ventile_cat9=1;
	if ventile_cat le 10 then ventile_cat10=1;
	if ventile_cat le 11 then ventile_cat11=1;
	if ventile_cat le 12 then ventile_cat12=1;
	if ventile_cat le 13 then ventile_cat13=1;
	if ventile_cat le 14 then ventile_cat14=1;
	if ventile_cat le 15 then ventile_cat15=1;
	if ventile_cat le 16 then ventile_cat16=1;
	if ventile_cat le 17 then ventile_cat17=1;
	if ventile_cat le 18 then ventile_cat18=1;
	if ventile_cat le 19 then ventile_cat19=1;

run; 




ods excel file = "/harvard/aking/DataQC/pha/tables/sl_6msa_aucroc.xlsx";
proc print data=auc_sample1_test;
title "SL model1: 5 suicidality predictors + age/sex/rank";
proc print data=auc_sample2_test;
title "SL model2: model 1 + remaining PHA survey variables";
proc print data=auc_sample3_test;
title "SL model3: model 2 + medical data and demos";
proc print data=auc_sample4_test;
title "SL model4: model 3 + remaining HADS and geospatial data";
proc print data=harvrd04.roc_sample1_sl_test;
title "SL model1: 5 suicidality predictors + age/sex/rank";
proc print data=harvrd04.roc_sample2_sl_test;
title "SL model2: model 1 + remaining PHA survey variables";
proc print data=harvrd04.roc_sample3_sl_test;
title "SL model3: model 2 + medical data and demos";
proc print data=harvrd04.roc_sample4_sl_test;
title "SL model4: model 3 + remaining HADS and geospatial data";
run;
ods excel close;


*/*;
ods html file = "/harvard/aking/DataQC/pha/tables/sl_6msa_ctab.html";
proc surveyfreq data=pp_sl1 missing;
title "SL model1: 5 suicidality predictors + age/sex/rank";
weight weight1;
	tables ventile_cat*outcome_6msa/row col;
	tables ventile_cat2*outcome_6msa/ row col ;
	tables ventile_cat3*outcome_6msa/ row col ;
	tables ventile_cat4*outcome_6msa/ row col ;
	tables ventile_cat5*outcome_6msa/ row col ;
	tables ventile_cat6*outcome_6msa/ row col ;
	tables ventile_cat7*outcome_6msa/ row col ;
	tables ventile_cat8*outcome_6msa/ row col ;
	tables ventile_cat9*outcome_6msa/ row col ;
	tables ventile_cat10*outcome_6msa/ row col ;
	tables ventile_cat11*outcome_6msa/ row col ;
	tables ventile_cat12*outcome_6msa/ row col ;
	tables ventile_cat13*outcome_6msa/ row col ;
	tables ventile_cat14*outcome_6msa/ row col ;
	tables ventile_cat15*outcome_6msa/ row col ;
	tables ventile_cat16*outcome_6msa/ row col ;
	tables ventile_cat17*outcome_6msa/ row col ;
	tables ventile_cat18*outcome_6msa/ row col ;
	tables ventile_cat19*outcome_6msa/ row col ;
proc surveyfreq data=pp_sl2 missing;
title "model2: SL model 1 + medical data and demos";
weight weight1;
	tables ventile_cat*outcome_6msa/row col;
	tables ventile_cat2*outcome_6msa/ row col ;
	tables ventile_cat3*outcome_6msa/ row col ;
	tables ventile_cat4*outcome_6msa/ row col ;
	tables ventile_cat5*outcome_6msa/ row col ;
	tables ventile_cat6*outcome_6msa/ row col ;
	tables ventile_cat7*outcome_6msa/ row col ;
	tables ventile_cat8*outcome_6msa/ row col ;
	tables ventile_cat9*outcome_6msa/ row col ;
	tables ventile_cat10*outcome_6msa/ row col ;
	tables ventile_cat11*outcome_6msa/ row col ;
	tables ventile_cat12*outcome_6msa/ row col ;
	tables ventile_cat13*outcome_6msa/ row col ;
	tables ventile_cat14*outcome_6msa/ row col ;
	tables ventile_cat15*outcome_6msa/ row col ;
	tables ventile_cat16*outcome_6msa/ row col ;
	tables ventile_cat17*outcome_6msa/ row col ;
	tables ventile_cat18*outcome_6msa/ row col ;
	tables ventile_cat19*outcome_6msa/ row col ;
proc surveyfreq data=pp_sl3 missing;
title "SL model3: model 2 + medical data and demos";
weight weight1;
	tables ventile_cat*outcome_6msa/row col;
	tables ventile_cat2*outcome_6msa/ row col ;
	tables ventile_cat3*outcome_6msa/ row col ;
	tables ventile_cat4*outcome_6msa/ row col ;
	tables ventile_cat5*outcome_6msa/ row col ;
	tables ventile_cat6*outcome_6msa/ row col ;
	tables ventile_cat7*outcome_6msa/ row col ;
	tables ventile_cat8*outcome_6msa/ row col ;
	tables ventile_cat9*outcome_6msa/ row col ;
	tables ventile_cat10*outcome_6msa/ row col ;
	tables ventile_cat11*outcome_6msa/ row col ;
	tables ventile_cat12*outcome_6msa/ row col ;
	tables ventile_cat13*outcome_6msa/ row col ;
	tables ventile_cat14*outcome_6msa/ row col ;
	tables ventile_cat15*outcome_6msa/ row col ;
	tables ventile_cat16*outcome_6msa/ row col ;
	tables ventile_cat17*outcome_6msa/ row col ;
	tables ventile_cat18*outcome_6msa/ row col ;
	tables ventile_cat19*outcome_6msa/ row col ;
proc surveyfreq data=pp_sl4 missing;
title "SL model4: model 3 + remaining HADS and geospatial data";
weight weight1;
	tables ventile_cat*outcome_6msa/row col;
	tables ventile_cat2*outcome_6msa/ row col ;
	tables ventile_cat3*outcome_6msa/ row col ;
	tables ventile_cat4*outcome_6msa/ row col ;
	tables ventile_cat5*outcome_6msa/ row col ;
	tables ventile_cat6*outcome_6msa/ row col ;
	tables ventile_cat7*outcome_6msa/ row col ;
	tables ventile_cat8*outcome_6msa/ row col ;
	tables ventile_cat9*outcome_6msa/ row col ;
	tables ventile_cat10*outcome_6msa/ row col ;
	tables ventile_cat11*outcome_6msa/ row col ;
	tables ventile_cat12*outcome_6msa/ row col ;
	tables ventile_cat13*outcome_6msa/ row col ;
	tables ventile_cat14*outcome_6msa/ row col ;
	tables ventile_cat15*outcome_6msa/ row col ;
	tables ventile_cat16*outcome_6msa/ row col ;
	tables ventile_cat17*outcome_6msa/ row col ;
	tables ventile_cat18*outcome_6msa/ row col ;
	tables ventile_cat19*outcome_6msa/ row col ;
run;
ods html close;

proc datasets library=harvrd04;
	delete 	roc_:;
run;

