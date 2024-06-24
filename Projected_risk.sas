libname pha "/harvard/aking/DataQC/pha/data/";
%include '/home/aking/assign_libnames.sas';
options nocenter nofmterr mprint symbolgen mlogic mprint;



proc lifetest data=work1 intervals=(0 to 6 by 1) method=lt;
	time fu_6m*outcome_6msa(0);
		
	ods output 	lifetableestimates=lte_outstat  ;
run;

ods excel file = "/harvard/aking/DataQC/pha/tables/proj_risk_ctab.xlsx";
proc print data=lte_outstat; run;

ods excel close;

