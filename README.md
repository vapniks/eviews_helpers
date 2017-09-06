# eviews_helpers
Various eviews programs for data management and statistical routines

These programs were written some time ago and some routines may be redundant with the newer versions of Eviews.

  * run_regression.prg : program to run regression, perform diagnostics, and write output to spool object
  * johansen.prg : subroutine to apply the Pantula principle for finding the optimal model type for testing for cointegration (see pages 323-327 of Applied Econometrics by Dimitrios Asteriou & Stephen Hall)
  * var_lag_length.prg : program to help find optimal lag length in var model
  * basic_stats_and_tests.prg : program to transform series, and get basic statistics and unit root tests.
  * unit_root.prg : program to find if series contains a unit root and print output to spool object
  * transform_series.prg : program to transform series, and get basic statistics and unit root tests
  * test_models.prg : program to test forecasting ability of many models
  * open_workfiles.prg : program to open lots of workfiles at once
  * close_workfiles.prg : program to close lots of workfiles at once
  * save_workfiles.prg : program to save lots of workfiles at once
  * make_ids.prg : program to set id series' for lots of workfiles at the same time
  * convert_excel_to_eviews.prg : program to load all workfiles in an excel spreadsheet into eviews and save the eviews worksheets into the same folder
