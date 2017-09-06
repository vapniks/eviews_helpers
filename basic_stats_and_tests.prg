' Program to transform series, and get basic statistics and unit root tests.
' Program arguments should be a list of series to use (upto 20, if more are needed change the for loop in this file).
' Set the variables below to 0 or 1 depending on which transformations and statistics you want.
' Result will be a new group object for each transformation chosen (containing the associated transformed variables),
' spool objects for associated with each group, containing descriptives statistics and correlations, 
' and a pair of spool objects containing results of unit root tests for raw and log data if these were chosen.




' Variables to indicate which transformations to test.
' Set to 1 any which should be tested, and 0 any which shouldn't.
!raw = 1
!logs = 1
!rawdiffs = 1
!logdiffs = 1
!recips = 0
' indicate whether to transform data so that negative values and 0 values can be used in logs
' (will set them to 0 then add small value)
!useall = 0
' Variables to indicate what tests to do.
' Set to 1 any which should be tested, and 0 any which shouldn't.
!descriptives = 1
!linegraphs = 1
!correlograms = 1
!pearsoncorr = 1
!spearmancorr = 1
!unitroots = 1
' create groups to hold the different series 
if !raw = 1 then
	group raw_series
	spool raw_series_stats
endif
if !logs = 1 then
	group lg_series
	spool lg_series_stats
endif
if !rawdiffs = 1 then
	group d1raw_series
	spool d1raw_series_stats
endif
if !logdiffs = 1 then
	group d1lg_series
	spool d1lg_series_stats
endif
if !recips = 1 then
	group recip_series
	spool recip_series_stats
endif
' need these objects for unit root tests 
include unitroot_subroutine.prg
if !unitroots = 1 then

	if !raw = 1 then
		spool unit_root_tests_raw
	endif
	if !logs = 1 then
		spool unit_root_tests_lg
	endif
	scalar alpha = 0.05
	scalar diff
endif
' Loop over series. 
' Replace ... with space seperated list of series'
' (uncomment lines corresponding to series which you don't need or want)
FOR %i {%0} {%1} {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20}  
	if @len(%i) > 0 then
		if !raw = 1 then
			raw_series.add {%i}
			if !unitroots = 1 then
				call unitroot(unit_root_tests_raw,{%i},alpha,diff)
				%newname = %i + "_d" + @str(diff)
				unit_root_tests_raw.name tempname %newname
				if diff < 3 and diff > 0 then
					%newname = %i + "_unitroot_d" + @str(diff - 1)
					unit_root_tests_raw.name btempname %newname
				endif
			endif
			if !correlograms = 1 then
				%newname = %i + "_correl"
				raw_series_stats.insert(name=%newname) {%i}.correl(24)
			endif
		endif
		if !logs = 1 then
			%name = "lg" + "_" + %i
			if !useall = 1 then
			series {%name} = log(({%i}+abs({%i}))/2+0.000000001)
			else
				series {%name} = log({%i})				
			endif
			lg_series.add {%name}
			if !unitroots = 1 then
				call unitroot(unit_root_tests_lg,{%name},alpha,diff)
				%newname = %i + "_d" + @str(diff)
				unit_root_tests_lg.name tempname %newname
				if diff < 3 and diff > 0 then
					%newname = %i + "_unitroot_d" + @str(diff - 1)
					unit_root_tests_lg.name btempname %newname
				endif
			endif
			if !correlograms = 1 then
				%newname = %name + "_correl"
				lg_series_stats.insert(name=%newname) {%name}.correl(24)
			endif
		endif
		if !rawdiffs = 1 then
			%name = "d1" + "raw_" + %i
			series {%name} = d({%i})
			d1raw_series.add {%name}
			if !correlograms = 1 then
				%newname = %name + "_correl"
				d1raw_series_stats.insert(name=%newname) {%name}.correl(24)
			endif
		endif
		if !logdiffs = 1 then
			%name = "d1lg" + "_" + %i
			if !useall = 1 then
				series {%name} = d(log(({%i}+abs({%i}))/2+0.000000001))
			else
				series {%name} = d(log({%i}))
		        endif
			d1lg_series.add {%name}
			if !correlograms = 1 then
				%newname = %name + "_correl"
				d1lg_series_stats.insert(name=%newname) {%name}.correl(24)
			endif
		endif
		if !recips = 1 then
			%name = "recip" + "_" + %i
			series {%name} = 1/{%i}
			recip_series.add {%name}
			if !correlograms = 1 then
				%newname = %name + "_correl"
				recip_series_stats.insert(name=%newname) {%name}.correl(24)
			endif
		endif
	endif
NEXT
' don't need these anymore
if !unitroots = 1 then
	delete alpha
	delete diff
	delete unit_root_results
endif

' Descriptive statistics (mean, median, max, min, standard deviation Jarque Bera normality test etc.)
if !descriptives = 1 then
	if !raw = 1 then
		raw_series_stats.insert(name="descriptives") raw_series.stats(i)
	endif
	if !logs = 1 then
		lg_series_stats.insert(name="descriptives") lg_series.stats(i)
	endif
	if !rawdiffs = 1 then
		d1raw_series_stats.insert(name="descriptives") d1raw_series.stats(i)
	endif
	if !logdiffs = 1 then
		d1lg_series_stats.insert(name="descriptives") d1lg_series.stats(i)
	endif
	if !recips = 1 then
		recips_series_stats.insert(name="descriptives") recips_series.stats(i)
	endif
endif
' Line graphs of series
if !linegraphs = 1 then
	if !raw = 1 then
		raw_series_stats.insert(name="line_graphs") raw_series.line(m)
	endif
	if !logs = 1 then
		lg_series_stats.insert(name="line_graphs") lg_series.line(m)
	endif
	if !rawdiffs = 1 then
		d1raw_series_stats.insert(name="line_graphs") d1raw_series.line(m)
	endif
	if !logdiffs = 1 then
		d1lg_series_stats.insert(name="line_graphs") d1lg_series.line(m)
	endif
	if !recips = 1 then
		recips_series_stats.insert(name="line_graphs") recips_series.line(m)
	endif
endif
' Pearson correlations (with t-test and p-value for significance)
if !pearsoncorr = 1 then
	if !raw = 1 then
		freeze(temptable) raw_series.cor corr prob
		raw_series_stats.insert(name="pearson_correlations") temptable
		delete temptable
	endif
	if !logs = 1 then
		freeze(temptable) lg_series.cor corr prob
		lg_series_stats.insert(name="pearson_correlations") temptable
		delete temptable
	endif
	if !rawdiffs = 1 then
		freeze(temptable) d1raw_series.cor corr prob
		d1raw_series_stats.insert(name="pearson_correlations") temptable
		delete temptable
	endif
	if !logdiffs = 1 then
		freeze(temptable) d1lg_series.cor corr prob
		d1lg_series_stats.insert(name="pearson_correlations") temptable
		delete temptable
	endif
	if !recips = 1 then
		freeze(temptable) recip_series.cor corr prob 
		recips_series_stats.insert(name="pearson_correlations") temptable
		delete temptable
	endif
endif
' Spearman correlations (with p-value for significance)
if !spearmancorr = 1 then
	if !raw = 1 then
		freeze(temptable) raw_series.cor rcorr rprob
		raw_series_stats.insert(name="spearman_correlations") temptable
		delete temptable
	endif
	if !logs = 1 then
		freeze(temptable) lg_series.cor rcorr rprob
		lg_series_stats.insert(name="spearman_correlations") temptable
		delete temptable
	endif
	if !rawdiffs = 1 then
		freeze(temptable) d1raw_series.cor rcorr rprob
		d1raw_series_stats.insert(name="spearman_correlations") temptable
		delete temptable
	endif
	if !logdiffs = 1 then
		freeze(temptable) d1lg_series.cor rcorr rprob
		d1lg_series_stats.insert(name="spearman_correlations") temptable
		delete temptable
	endif
	if !recips = 1 then
		freeze(temptable) recip_series.cor rcorr rprob 
		recips_series_stats.insert(name="spearman_correlations") temptable
		delete temptable
	endif
endif

