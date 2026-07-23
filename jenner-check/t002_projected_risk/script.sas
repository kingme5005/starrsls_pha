/* Source: Projected_risk.sas (unmodified core analysis).
   Changes from upstream: the external libname / %include / options line
   referencing the author's external autoexec were removed, and the ODS
   destination was pointed at a relative file so the run is self-contained.
   Upstream reads work1 from an external libname
   (/harvard/aking/DataQC/pha/data/); here work1 is built inline with the
   two columns the PROC LIFETEST life-table analysis reads -- fu_6m
   (follow-up month, 0..6) and outcome_6msa (event flag; 0 = censored).
   Values are synthetic. The PROC LIFETEST life-table specification is
   exactly the author's: interval survival estimates from 0 to 6 by 1 with
   the actuarial (life-table) method, over the 6-month follow-up time fu_6m
   with outcome_6msa=0 as the censoring value. */

data work1;
    input fu_6m outcome_6msa;
    datalines;
6 0
3 1
6 0
1 1
5 0
2 1
6 0
4 0
6 0
1 1
6 0
5 0
2 1
6 0
3 1
6 0
6 0
4 1
6 0
2 0
5 1
6 0
6 0
3 0
1 1
6 0
6 0
5 0
2 1
6 0
;
run;

ods html file = "proj_risk_ctab.html";

proc lifetest data=work1 intervals=(0 to 6 by 1) method=lt;
	time fu_6m*outcome_6msa(0);
run;

ods html close;
