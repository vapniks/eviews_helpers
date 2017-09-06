' program to test forecasting ability of many models
' takes list of worksheets as arguments



%trainsample = "1969 1999"
%testsample = "2000 2006" 

close aggregate
wfcreate(wf=aggregate) u 100
table(100,5) bestmodels
bestmodels(1,1) = "model"
bestmodels(1,2) = "best for"
bestmodels(1,3) = "count"
FOR !i = 2 TO 41 STEP 1
	bestmodels(!i,1) = !i-1
	bestmodels(!i,3) = 0
NEXT


FOR %workfile {%0} {%1} {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9}
	IF @len(%workfile) > 0 THEN 
		wfselect {%workfile}
		table(100,10) resultstable
		resultstable(1,1) = "model"
		resultstable(1,2) = "dependent var"
		resultstable(1,3) = "independent vars"
		resultstable(1,4) = "AIC"
		resultstable(1,5) = "DW"
		resultstable(1,6) = "MSE"
		resultstable(1,7) = "MAE"
		!resultsrow = 2
		series tempseries1
		series tempseries2
		series log_sales = log(sales_or_revenue)

		%trendvars = ""
		%lagvarname = "log_sales"
		%depvar = "log_sales"

		FOR !j = 1 TO 4 STEP 1
			%trendvars = %trendvars + " @trend^" + @str(!j) + " "
			%lagvars = ""
			FOR !i = 1 TO 5 STEP 1
				%tempvars = %lagvars + %lagvarname +"(-" + @str(!i) + ") "
				%lagvars = %tempvars
				%indvars = " c " + %trendvars + %lagvars
				resultstable(!resultsrow,1) = !resultsrow - 1
				resultstable(!resultsrow,2) = %depvar
				resultstable(!resultsrow,3) = %indvars
				%allvars = %depvar + " " + %indvars
				smpl {%trainsample}
				!modelnumber = !resultsrow - 1
				IF !modelnumber < 10 THEN 
					%modelname = "model0" + @str(!modelnumber)
				ELSE
					%modelname = "model" + @str(!modelnumber)
				ENDIF
				%equationname = %modelname + "_equation"
				%predictionname = %modelname + "_predictions"
				%upperboundname = %modelname + "_upperbound"
				%lowerboundname = %modelname + "_lowerbound"
				%modelgraphname = %modelname + "_graph"
				equation {%equationname}.ls {%allvars}
				resultstable(!resultsrow,4) = {%equationname}.@aic
				resultstable(!resultsrow,5) = {%equationname}.@dw
				smpl {%testsample}
				{%equationname}.forecast tempseries2 tempseries1
				series {%predictionname} = @exp(tempseries2)
				series {%upperboundname} = @exp(tempseries2 + 2*tempseries1)
				series {%lowerboundname} = @exp(tempseries2 - 2*tempseries1)
				group {%modelgraphname} @exp({%depvar}) {%predictionname} {%lowerboundname} {%upperboundname}
				{%modelgraphname}.line
				close {%modelgraphname}
				tempseries1 = ({%predictionname} - @exp({%depvar}))^2
				resultstable(!resultsrow,6) = @mean(tempseries1)
				tempseries1 = @abs({%predictionname} - @exp({%depvar}))
				resultstable(!resultsrow,7) = @mean(tempseries1)
				!resultsrow = !resultsrow + 1
			NEXT
		NEXT

		' find best AIC (Akaike information criterion), MSE (mean square error),
		' and MAE (mean absolute error) for these models
		!bestAIC = @val(resultstable(2,4))
		!bestAICrow = 2
		!bestMSE = @val(resultstable(2,6))
		!bestMSErow = 2
		!bestMAE = @val(resultstable(2,7))
		!bestMAErow = 2
		FOR !i = 2 TO !resultsrow-1 STEP 1
			IF resultstable(!i,4) < !bestAIC THEN 
				!bestAIC = @val(resultstable(!i,4))
				!bestAICrow = !i
			ENDIF
			IF resultstable(!i,6) < !bestMSE THEN 
				!bestMSE = @val(resultstable(!i,6))
				!bestMSErow = !i
			ENDIF
			IF resultstable(!i,7) < !bestMAE THEN 
				!bestMAE = @val(resultstable(!i,7))
				!bestMAErow = !i
			ENDIF
		NEXT
		resultstable(!bestAICrow,4) = @str(!bestAIC) + "*"
		resultstable(!bestMSErow,6) = @str(!bestMSE) + "*"
		resultstable(!bestMAErow,7) = @str(!bestMAE) + "*"
		!overallbestMSE = !bestMSE
		!overallbestMSErow = !bestMSErow
		!overallbestMAE = !bestMAE
		!overallbestMAErow = !bestMAErow



		' work out new set of models with different transformation of dependent variable
		%trendvars = ""
		%lagvarname = "sales_or_revenue"
		%depvar = "sales_or_revenue"
		FOR !j = 1 TO 4 STEP 1
			%trendvars = %trendvars + " @trend^" + @str(!j) + " "
			%lagvars = ""
			FOR !i = 1 TO 5 STEP 1
				%tempvars = %lagvars + %lagvarname +"(-" + @str(!i) + ") "
				%lagvars = %tempvars
				%indvars = " c " + %trendvars + %lagvars
				resultstable(!resultsrow,1) = !resultsrow - 1
				resultstable(!resultsrow,2) = %depvar
				resultstable(!resultsrow,3) = %indvars
				%allvars = %depvar + " " + %indvars
				smpl {%trainsample}
				!modelnumber = !resultsrow - 1
				IF !modelnumber < 10 THEN 
					%modelname = "model0" + @str(!modelnumber)
				ELSE
					%modelname = "model" + @str(!modelnumber)
				ENDIF
				%equationname = %modelname + "_equation"
				%predictionname = %modelname + "_predictions"
				%upperboundname = %modelname + "_upperbound"
				%lowerboundname = %modelname + "_lowerbound"
				%modelgraphname = %modelname + "_graph"

				equation {%equationname}.ls {%allvars}
				resultstable(!resultsrow,4) = {%equationname}.@aic
				resultstable(!resultsrow,5) = {%equationname}.@dw
				smpl {%testsample}
				{%equationname}.forecast {%predictionname} tempseries1
				series {%upperboundname} = {%predictionname} + 2*tempseries1
				series {%lowerboundname} = {%predictionname} - 2*tempseries1

				group {%modelgraphname} {%depvar} {%predictionname} {%lowerboundname} {%upperboundname}
				{%modelgraphname}.line
				close {%modelgraphname}
				tempseries1 = ({%predictionname} - {%depvar})^2
				resultstable(!resultsrow,6) = @mean(tempseries1)
				tempseries1 = @abs({%predictionname} - {%depvar})
				resultstable(!resultsrow,7) = @mean(tempseries1)
				!resultsrow = !resultsrow + 1
			NEXT
		NEXT

		' find best AIC (Akaike information criterion), MSE (mean square error),
		' and MAE (mean absolute error) for these models
		!bestAIC = @val(resultstable(22,4))
		!bestAICrow = 22
		!bestMSE = @val(resultstable(22,6))
		!bestMSErow = 22
		!bestMAE = @val(resultstable(22,7))
		!bestMAErow = 22
		FOR !i = 22 TO !resultsrow-1 STEP 1
			IF resultstable(!i,4) < !bestAIC THEN 
				!bestAIC = @val(resultstable(!i,4))
				!bestAICrow = !i
			ENDIF
			IF resultstable(!i,6) < !bestMSE THEN 
				!bestMSE = @val(resultstable(!i,6))
				!bestMSErow = !i
			ENDIF
			IF resultstable(!i,7) < !bestMAE THEN 
				!bestMAE = @val(resultstable(!i,7))
				!bestMAErow = !i
			ENDIF
		NEXT
		resultstable(!bestAICrow,4) = @str(!bestAIC) + "*"
		resultstable(!bestMSErow,6) = @str(!bestMSE) + "*"
		resultstable(!bestMAErow,7) = @str(!bestMAE) + "*"


		' find overall best MSE and MAE
		IF !bestMSE < !overallbestMSE THEN 
			!overallbestMSE = !bestMSE
			!overallbestMSErow = !bestMSErow
		ENDIF
		resultstable(!overallbestMSErow,6) = @str(!overallbestMSE) + "**"
		IF !bestMAE < !overallbestMAE THEN 
			!overallbestMAE = !bestMAE
			!overallbestMAErow = !bestMAErow
		ENDIF
		resultstable(!overallbestMAErow,7) = @str(!overallbestMAE) + "**"
		
		' indicate best model for this workfile in aggregate workfile
		wfselect aggregate
		%temp = bestmodels(!overallbestMSErow,2) + %workfile + " "
		bestmodels(!overallbestMSErow,2) = %temp
		!temp = @val(bestmodels(!overallbestMSErow,3)) + 1
		bestmodels(!overallbestMSErow,3) = !temp
		IF !overallbestMSErow <> !overallbestMAErow THEN
			%temp = bestmodels(!overallbestMAErow,2) + %workfile + " "
			bestmodels(!overallbestMAErow,2) = %temp
			!temp = @val(bestmodels(!overallbestMAErow,3)) + 1
			bestmodels(!overallbestMAErow,3) = !temp
		ENDIF
		close tempseries1
		close tempseries2
	ENDIF
NEXT

