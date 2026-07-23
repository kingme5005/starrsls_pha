/* Source: Demo_newform.sas (unmodified analysis logic).
   Only change from upstream: the ODS HTML destination path was rewritten
   from the author's absolute /harvard/aking/... path to a relative file so
   the run is self-contained. work1 is supplied by autoexec.sas. */

ods html file = "demo_table.html";
proc means data=work1 mean min max nmiss;
	var mast_pm_v010;
	class age_cat;
proc means data=work1 mean min max nmiss;
	var army_time;
	class army_time_cat;
proc surveyfreq data=work1;
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
