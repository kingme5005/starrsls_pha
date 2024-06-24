

ods html file = "/harvard/aking/DataQC/pha/tables/demo_table.html";
proc means data=work1 mean min max nmiss;
	var mast_pm_v010;
	class age_cat;
proc means data=work1 mean min max nmiss;
	var army_time;
	class army_time_cat;
proc surveyfreq data=work1; 
	*weight  weight1;
	tables 	outcome_6msa*age_cat
			outcome_6msa*age_catb
			outcome_6msa*army_time_cat
			outcome_6msa*army_time_catb
			outcome_6msa*mast_pm_v001
			outcome_6msa*race_cat 
			outcome_6msa*ed_cat
			outcome_6msa*mhx_cat
			outcome_6msa*rank_3cat
			
			outcome_6msa*command_cat
			/ row wchisq;
				
run;
ods html close;




