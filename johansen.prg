' subroutine to apply the Pantula principle for finding the optimal model type for 
' testing for cointegration (see pages 323-327 of Applied Econometrics by Dimitrios Asteriou &
' Stephen Hall)

' 5% critical values for trace statistic of cointegration test are stored in following matrix.
' Row number corresponds to model number (currently only using model types II, III & IV, since I & V are unrealistic).
' Column number corresponds to the number of restrictions = number of variables - number of cointegrating relationships.
' These values can be obtained from the "Numerical Distribution Functions of Likelihood Ratio Tests for Cointegration"
' paper by MacKinnon, Haug and Michelis. 
' In the tables in that paper:
'                                 k = number of exogenous variables, 
'                                 p = number of endogenous variables, 
'                                 r = number of cointegrating relationships. 
' The bottom number in each entry in the tables should be used as this corresponds to the critical value for the 
' trace statistic. In eviews it assumed that k = 0, so use the first column of each table.

matrix(5,12) mhmcritval
' critical values for model 2 with k = 0, and r = 0, 1 & 2 respectively
mhmcritval(2,1) = 9.17   
mhmcritval(2,2) = 20.25  
mhmcritval(2,3) = 35.19  
mhmcritval(2,4) = 54.09  
mhmcritval(2,5) = 76.96  
mhmcritval(2,6) = 103.84 
mhmcritval(2,7) = 134.70 
mhmcritval(2,8) = 169.54 
mhmcritval(2,9) = 208.41 
mhmcritval(2,10) = 251.31 
mhmcritval(2,11) = 298.16 
mhmcritval(2,12) = 348.98 
' critical values for model 3 with k = 0, and r = 0, 1 & 2 respectively
mhmcritval(3,1) =   8.19  
mhmcritval(3,2) =   18.11 
mhmcritval(3,3) =   31.88 
mhmcritval(3,4) =   49.64 
mhmcritval(3,5) =   71.44 
mhmcritval(3,6) =   97.26 
mhmcritval(3,7) =   127.05
mhmcritval(3,8) =   160.87
mhmcritval(3,9) =   198.72
mhmcritval(3,10) =  240.58
mhmcritval(3,11) =  286.39
mhmcritval(3,12) =  336.22
' critical values for model 4 with k = 0, and r = 0, 1 & 2 respectively
mhmcritval(4,1) =   12.52 
mhmcritval(4,2) =   25.86 
mhmcritval(4,3) =   42.92 
mhmcritval(4,4) =   63.87 
mhmcritval(4,5) =   88.79 
mhmcritval(4,6) =   117.69
mhmcritval(4,7) =   150.55
mhmcritval(4,8) =   187.44
mhmcritval(4,9) =   228.32
mhmcritval(4,10) =  273.20
mhmcritval(4,11) =  322.03
mhmcritval(4,12) =  374.84

SUBROUTINE pantula(var vartest, group testseries, spool output, scalar !maxlag)
	!numseries = testseries.@count
	vartest.ls 1 1 testseries 
	vartest.laglen(!maxlag, vname = laglencriteria)
	!bestlag = laglencriteria(3)
	output.insert(name="model2") testseries.coint(b,!bestlag,save=cointstatsB)
	output.insert(name="model3") testseries.coint(c,!bestlag,save=cointstatsC)
	output.insert(name="model4") testseries.coint(d,!bestlag,save=cointstatsD)
	!numcoint = 0
	%bestmodel = "b"
	WHILE !numcoint < !numseries
		IF cointstatsB(!numcoint+1,3) > mhmcritval(2,!numseries - !numcoint) THEN 
			IF cointstatsC(!numcoint+1,3) > mhmcritval(3,!numseries - !numcoint) THEN 
				IF cointstatsD(!numcoint+1,3) <= mhmcritval(4,!numseries - !numcoint) THEN 
					%bestmodel = "d"
					EXITLOOP
				ENDIF
			ELSE
				%bestmodel = "c"
				EXITLOOP
			ENDIF
		ELSE
			%bestmodel = "b"
			EXITLOOP
		ENDIF
		!numcoint = !numcoint + 1
	WEND
	' !numcoint is equal to number of cointegrating vectors, and !bestmodel is equal to the number of the best model 
	' create vec model with these specifications, or var if no cointegrating vectors found
	IF !numcoint > 0 THEN 
		vartest.ec({%bestmodel}, !numcoint) 1 !bestlag testseries	    
	ELSE
		%dseries = ""
		FOR !i = 1 TO testseries.@count STEP 1
		    	%temp = %dseries + "d(" + testseries.@seriesname(!i) + ") "
			%dseries = %temp
		NEXT
		vartest.ls 1 !bestlag {%dseries}
		vartest.laglen(!maxlag, vname = laglencriteria2)
		!bestlag = laglencriteria2(3)
		vartest.ls 1 !bestlag {%dseries}
		delete laglencriteria2
	ENDIF

	' don't need these anymore
	delete laglencriteria cointstatsB cointstatsC cointstatsD
ENDSUB

' %name should be defined before including this file and should contain a unique name for this combination of series

' add extra series to the subroutine arguments if necessary
SUBROUTINE test_model(spool outputspool, group testseries)
	' maximum number of lags to test in lag length test
	!maxlag = 10
	%varname = %name + "var"
	var {%varname}
	call pantula({%varname}, testseries, outputspool, !maxlag)
	%modelbname = %name + "model2"
	%modelcname = %name + "model3"
	%modeldname = %name + "model4"
	%modelbdisplayname = %name + " : Model 2"
	%modelcdisplayname = %name + " : Model 3"
	%modelddisplayname = %name + " : Model 4"
	%modelbcomment = "Cointegration results assuming intercept (no trend) in CE, no intercept or trend in VAR."
	%modelccomment = "Cointegration results assuming intercept in CE and VAR, no trends in CE or VAR."
	%modeldcomment = "Cointegration results assuming intercept in CE and VAR, trend in CE no trend in VAR."
	IF @eqna(%bestmodel,"b") THEN 
		%temp = %modelbdisplayname + ", best model, " + @str(!numcoint) + " vec."
		%modelbdisplayname = %temp
		IF !numcoint = 1 THEN 
			%temp = "Cointegration results assuming intercept (no trend) in CE, no intercept or trend in VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vector found." 
			%modelbcomment = %temp
		ELSE
			%temp = "Cointegration results assuming intercept (no trend) in CE, no intercept or trend in VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vectors found." 
			%modelbcomment = %temp
		ENDIF
	ELSE 
		IF @eqna(%bestmodel,"c") THEN 
			%temp = %modelcdisplayname + ", best model, " + @str(!numcoint) + " vec."
			%modelcdisplayname = %temp
			IF !numcoint = 1 THEN 
				%temp = "Cointegration results assuming intercept in CE and VAR, no trends in CE or VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vector found." 
				%modelccomment = %temp
			ELSE
				%temp = "Cointegration results assuming intercept in CE and VAR, no trends in CE or VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vectors found." 
				%modelccomment = %temp
			ENDIF
		ELSE 
			IF @eqna(%bestmodel,"d") THEN
				%temp = %modelddisplayname + ", best model, " + @str(!numcoint) + " vec."
				%modelddisplayname = %temp
				IF !numcoint = 1 THEN 
					%temp =  "Cointegration results assuming intercept in CE and VAR, trend in CE no trend in VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vector found." 
					%modeldcomment = %temp
				ELSE
					%temp =  "Cointegration results assuming intercept in CE and VAR, trend in CE no trend in VAR.\n" + "Best model, " + @str(!numcoint) + " cointegrating vectors found." 
					%modeldcomment = %temp
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	outputspool.displayname model2 %modelbdisplayname
	outputspool.displayname model3 %modelcdisplayname
	outputspool.displayname model4 %modelddisplayname
	outputspool.comment model2 %modelbcomment
	outputspool.comment model3 %modelccomment
	outputspool.comment model4 %modeldcomment
	outputspool.name model2 {%modelbname}
	outputspool.name model3 {%modelcname}
	outputspool.name model4 {%modeldname}
	outputspool.options comments displaynames
ENDSUB

