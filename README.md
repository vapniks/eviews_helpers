# eviews_helpers
Various eviews programs for data management and statistical routines

These programs were written some time ago and some routines may be redundant with the newer versions of Eviews.

  * run_regression.prg : run regression, perform diagnostics, and write output to spool object
  * johansen.prg : apply the Pantula principle for finding the optimal model type for testing for cointegration (see pages 323-327 of Applied Econometrics by Dimitrios Asteriou & Stephen Hall)
  * var_lag_length.prg : help find optimal lag length in var model
  * basic_stats_and_tests.prg : transform series, and get basic statistics and unit root tests.
  * unit_root.prg : find if series contains a unit root and print output to spool object
  * transform_series.prg : transform series, and get basic statistics and unit root tests
  * test_models.prg : test forecasting ability of many models
  * open_workfiles.prg : open lots of workfiles at once
  * close_workfiles.prg : close lots of workfiles at once
  * save_workfiles.prg : save lots of workfiles at once
  * make_ids.prg : set id series' for lots of workfiles at the same time
  * convert_excel_to_eviews.prg : load all workfiles in an excel spreadsheet into eviews and save the eviews worksheets into the same folder
