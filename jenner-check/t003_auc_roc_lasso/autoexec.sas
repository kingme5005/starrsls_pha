/* cap input rows for the captured run */
options obs=100;

/* auc_roc_lasso.sas builds its model-1 input by PROC IMPORT of an external
   CSV (sample1_lasso_pp_test1.csv, which carries a predicted-probability
   column s1) merged with pha.test_sample (keep= weight1 outcome_6msa) from
   an external libname, then renames s1=pred_prob. None of those external
   files are in the repo. This autoexec supplies sample1_lasso_pp_test with
   the three columns the downstream analysis actually reads -- pred_prob
   (the LASSO predicted probability), weight1 (survey weight), and
   outcome_6msa (the 6-month outcome) -- so the modeling pipeline runs
   standalone. Values are synthetic. */
data sample1_lasso_pp_test;
    input pred_prob weight1 outcome_6msa;
    datalines;
0.02 1.4 0
0.05 0.9 0
0.11 1.1 0
0.08 1.3 0
0.31 0.8 1
0.44 1.2 1
0.03 1.0 0
0.19 1.5 0
0.62 0.7 1
0.27 1.1 1
0.06 1.3 0
0.14 0.9 0
0.51 1.0 1
0.09 1.2 0
0.38 0.8 1
0.04 1.4 0
0.22 1.1 0
0.71 0.6 1
0.17 1.3 0
0.33 0.9 1
0.07 1.0 0
0.12 1.2 0
0.48 0.8 1
0.10 1.1 0
0.29 0.9 1
0.05 1.5 0
0.15 1.0 0
0.58 0.7 1
0.20 1.2 0
0.41 0.8 1
;
run;
