' program to help find optimal lag length in var model
' VAR models with number of lags from 1 to 10 lags will be created and output placed in var_lags_test (a spool object)
' they can then be compared by eye to find the best one.

!maxlag = 10
' place endogenous variables in following variable, surrounded by quotes
%endogenous = ""
' place exogenous variables in following variable, surrounded by quotes
%exogenous = "c @trend"

spool var_lags_test
var bestvar

for !lag = !maxlag to 1 step -1
	%name = "lag_" + @str(!lag)
	bestvar.ls 1 !lag {%endogenous} @ {%exogenous}
	var_lags_test.insert(name=%name) bestvar
next
