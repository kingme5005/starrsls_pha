/* cap input rows for the captured run */
options obs=100;

/* Demo_newform.sas reads a dataset named work1 from an external libname
   (/harvard/aking/DataQC/pha/data/) that is not part of the repo. This
   autoexec builds a small in-memory work1 with the columns the script's
   PROC MEANS and PROC SURVEYFREQ read, so the analysis runs standalone.
   Values are synthetic. */
data work1;
    input outcome_6msa age_cat age_catb army_time army_time_cat army_time_catb
          mast_pm_v010 mast_pm_v001 race_cat ed_cat mhx_cat rank_3cat command_cat;
    datalines;
1 1 0 36 1 0 4 1 1 2 1 1 1
0 2 1 12 2 1 2 0 2 1 0 2 2
0 1 0 60 3 1 5 1 3 3 1 3 1
1 3 1 24 1 0 1 0 1 2 0 1 3
0 2 1 48 2 1 3 1 2 1 1 2 2
0 1 0 6  1 0 2 0 3 3 0 3 1
1 2 1 72 3 1 4 1 1 2 1 1 2
0 3 1 18 2 1 1 0 2 1 0 2 3
0 1 0 30 1 0 5 1 3 3 1 3 1
1 2 1 54 3 1 2 0 1 2 0 1 2
0 1 0 42 2 1 3 1 2 1 1 2 1
0 3 1 9  1 0 1 0 3 3 0 3 3
1 2 1 66 3 1 4 1 1 2 1 1 2
0 1 0 15 2 1 2 0 2 1 0 2 1
0 2 1 21 1 0 5 1 3 3 1 3 2
1 1 0 33 3 1 1 0 1 2 0 1 1
0 3 1 45 2 1 3 1 2 1 1 2 3
0 2 1 3  1 0 2 0 3 3 0 3 2
1 1 0 78 3 1 4 1 1 2 1 1 1
0 2 1 27 2 1 1 0 2 1 0 2 2
;
run;
