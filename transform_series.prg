' Program to transform series, and get basic statistics and unit root tests.
' Program arguments should be a list of series to use.
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
!recips = 1
' Variables to indicate what tests to do.
' Set to 1 any which should be tested, and 0 any which shouldn't.
!descriptives = 1
!pearsoncorr = 1
!spearmancorr = 1
!unitroots = 1

' groups to hold the different series 
' (uncomment lines corresponding to series which you don't need or want)
if !raw = 1 then
	group raw_series
	spool raw_series_stats
endif
if !logs = 1 then
	group lg_series
	spool lg_series_stats
endif
if !rawdiffs = 1 then
	group d1_series
	spool d1_series_stats
endif
if !logdiffs = 1 then
	group d1_lg_series
	spool d1_lg_series_stats
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
					%newname = %i + "_unit_root_d" + @str(diff - 1)
					unit_root_tests_raw.name btempname %newname
				endif
			endif
		endif
		if !logs = 1 then
			%name = "lg" + "_" + %i
			series {%name} = log({%i})
			lg_series.add {%name}
			if !unitroots = 1 then
				call unitroot(unit_root_tests_lg,{%name},alpha,diff)
				%newname = %i + "_d" + @str(diff)
				unit_root_tests_lg.name tempname %newname
				if diff < 3 and diff > 0 then
					%newname = %i + "_unit_root_d" + @str(diff - 1)
					unit_root_tests_lg.name btempname %newname
				endif
			endif
		endif
		if !rawdiffs = 1 then
			%name = "d1" + "_" + %i
			series {%name} = d({%i})
			d1_series.add {%name}
		endif
		if !logdiffs = 1 then
			%name = "d1_lg" + "_" + %i
			series {%name} = d(log({%i}))
			d1_lg_series.add {%name}
		endif
		if !recips = 1 then
			%name = "recip" + "_" + %i
			series {%name} = 1/{%i}
			recip_series.add {%name}
		endif
	endif
NEXT

if !unitroots = 1 then
	delete alpha
	delete diff
	delete unit_root_results
endif

' Descriptive statistics (mean, median, max, min, standard deviation Jarque Bera normality test etc.)
' (uncomment lines corresponding to series which you don't need or want)
if !descriptives = 1 then
	if !raw = 1 then
		raw_series_stats.insert(name="descriptives") raw_series.stats(i)
	endif
	if !logs = 1 then
		lg_series_stats.insert(name="descriptives") lg_series.stats(i)
	endif
	if !rawdiffs = 1 then
		d1_series_stats.insert(name="descriptives") d1_series.stats(i)
	endif
	if !logdiffs = 1 then
		d1_lg_series_stats.insert(name="descriptives") d1_lg_series.stats(i)
	endif
	if !recips = 1 then
		recip_series_stats.insert(name="descriptives") recip_series.stats(i)
	endif
endif



' Pearson correlations (with t-test and p-value for significance)
' (uncomment lines corresponding to series which you don't need or want)
if !pearsoncorr = 1 then
	if !raw = 1 then
		freeze(temptable) raw_series.cor corr prob
		raw_series_stats.insert(name="pearson_corr") temptable
		delete temptable
	endif
	if !logs = 1 then
		freeze(temptable) lg_series.cor corr prob
		lg_series_stats.insert(name="pearson_corr") temptable
		delete temptable
	endif
	if !rawdiffs = 1 then
		freeze(temptable) d1_series.cor corr prob
		d1_series_stats.insert(name="pearson_corr") temptable
		delete temptable
	endif
	if !logdiffs = 1 then
		freeze(temptable) d1_lg_series.cor corr prob
		d1_lg_series_stats.insert(name="pearson_corr") temptable
		delete temptable
	endif
	if !recips = 1 then
		freeze(temptable) recip_series.cor corr prob 
		recip_series_stats.insert(name="pearson_corr") temptable
		delete temptable
	endif
endif

' Spearman correlations (with t-test and p-value for significance)
' (uncomment lines corresponding to series which you don't need or want)
if !spearmancorr = 1 then
	if !raw = 1 then
		freeze(temptable) raw_series.cor rcorr rprob
		raw_series_stats.insert(name="spearman_corr") temptable
		delete temptable
	endif
	if !logs = 1 then
		freeze(temptable) lg_series.cor rcorr rprob
		lg_series_stats.insert(name="spearman_corr") temptable
		delete temptable
	endif
	if !rawdiffs = 1 then
		freeze(temptable) d1_series.cor rcorr rprob
		d1_series_stats.insert(name="spearman_corr") temptable
		delete temptable
	endif
	if !logdiffs = 1 then
		freeze(temptable) d1_lg_series.cor rcorr rprob
		d1_lg_series_stats.insert(name="spearman_corr") temptable
		delete temptable
	endif
	if !recips = 1 then
		freeze(temptable) recip_series.cor rcorr rprob
		recip_series_stats.insert(name="spearman_corr") temptable
		delete temptable
	endif
endif

