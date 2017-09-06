' program to find if series' contains unit root and print output to spool
' arguments to program are:
' 	    %0 = spool object to send output to
'	    %1, %2, ... = series' to test (up to 20)



' subroutine to see if series contains unit-root. 
' Arguments:
' output :  a spool object to save the diagnostics output to
' x : the series to be tested
' alpha : the alpha-value for significance testing
' diff : the lowest difference level at which the series is stationary will be saved in this variable, if this is greater than 2 then 3 is returned.
'         values are ; 0 (stationary in levels), 1 (stationary in 1st differences), 2 (stationary in 2nd differences), 3 (unit-root in 2nd differences)
subroutine unitroot(spool output, series x, scalar alpha, scalar diff)
	' check for unit-root in first differences,
	' first try with trend and intercept
	x.uroot(pp, dif=0, const, trend, save=unit_root_results)
	if unit_root_results(4,1) < alpha then
		' no unit-root found with intercept and trend, now try without trend
		x.uroot(pp, dif=0, const, save=unit_root_results)
		if unit_root_results(4,1) < alpha then
			'still no unit-root so try without intercept
			x.uroot(pp, dif=0, save=unit_root_results)
			if unit_root_results(4,1) < alpha then
				'no unit-root even when intercept and trend are ommitted
				'append to output spool
				output.insert(name = "tempname") x.uroot(pp, dif=0)
				output.comment tempname "no unit-root in levels"
			else
				'no unit-root only if we include intercept
				'append this to output spool
				output.insert(name="tempname") x.uroot(pp, dif=0, const)
				output.comment tempname "no unit-root in levels with intercept"
			endif
		else
			'no unit-root only if we include both intercept and trend
			' append to output spool
			output.insert(name="tempname") x.uroot(pp, dif=0, const)
			output.comment tempname "no unit-root in levels with intercept and trend"
		endif
		diff = 0
		' unit-root was found in levels, now try same process for first differences
	else
		x.uroot(pp, dif=1, const, trend, save=unit_root_results)
		if unit_root_results(4,1) < alpha then
			x.uroot(pp, dif=1, const, save=unit_root_results)
			if unit_root_results(4,1) < alpha then
				x.uroot(pp, dif=1, save=unit_root_results)
				if unit_root_results(4,1) < alpha then
					output.insert(name="btempname") x.uroot(pp, dif=0, const, trend)
					output.comment btempname "unit-root in levels, no unit-root in first-differences"
					output.insert(name="tempname") x.uroot(pp, dif=1)
				else
					output.insert(name="btempname") x.uroot(pp, dif=0, const, trend)
					output.comment btempname "unit-root in levels, no unit-root in first-differences with intercept"
					output.insert(name="tempname")  x.uroot(pp, dif=1, const)
				endif
			else
				output.insert(name="btempname") x.uroot(pp, dif=0, const, trend)
				output.comment btempname "unit-root in levels, no unit-root in first-differences with intercept and trend"
				output.insert(name="tempname") x.uroot(pp, dif=1, const, trend)
			endif
			diff = 1
		' unit-root was found in first-differences, now try same again with second-differences
		else
			x.uroot(pp, dif=2, const, trend, save=unit_root_results)
			if unit_root_results(4,1) < alpha then
				x.uroot(pp, dif=2, const, save=unit_root_results)
				if unit_root_results(4,1) < alpha then
					x.uroot(pp, dif=2, save=unit_root_results)
					if unit_root_results(4,1) < alpha then
						output.insert(name="btempname") x.uroot(pp, dif=1, const, trend)
						output.comment btempname "unit-root in first-differences, no unit-root in second-differences"
						output.insert(name="tempname") x.uroot(pp, dif=2)
					else
						output.insert(name="btempname") x.uroot(pp, dif=1, const, trend)
						output.comment btempname "unit-root in first-differences, no unit-root in second-differences with intercept"
						output.insert(name="tempname") x.uroot(pp, dif=2, const)
					endif
				else
					output.insert(name="btempname") x.uroot(pp, dif=1, const, trend)
					output.comment btempname "unit-root in first-differences, no unit-root in second-differences with intercept and trend"
					output.insert(name="tempname") x.uroot(pp, dif=2, const, trend)
				endif
				diff = 2
			' unit-root was found in second-differences
			else
				output.insert(name="tempname") x.uroot(pp, dif=2, const, trend)
				output.comment tempname "unit-root found in second-differences!!!"
				diff = 3
			endif
		endif
	endif
	close x
endsub

scalar alpha = 0.05
scalar diff

if @isempty(%1)=0 then
	call unitroot({%0}, {%1}, alpha, diff)
	%newname = %1 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %1 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%2)=0 then
	call unitroot({%0}, {%2}, alpha, diff)
	%newname = %2 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %2 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%3)=0 then
	call unitroot({%0}, {%3}, alpha, diff)
	%newname = %3 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %3 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%4)=0 then
	call unitroot({%0}, {%4}, alpha, diff)
	%newname = %4 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %4 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%5)=0 then
	call unitroot({%0}, {%5}, alpha, diff)
	%newname = %5 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %5 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%6)=0 then
	call unitroot({%0}, {%6}, alpha, diff)
	%newname = %6 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %6 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%7)=0 then
	call unitroot({%0}, {%7}, alpha, diff)
	%newname = %7 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %7 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%8)=0 then
	call unitroot({%0}, {%8}, alpha, diff)
	%newname = %8 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %8 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%9)=0 then
	call unitroot({%0}, {%9}, alpha, diff)
	%newname = %9 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %9 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%10)=0 then
	call unitroot({%0}, {%10}, alpha, diff)
	%newname = %10 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %10 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%11)=0 then
	call unitroot({%0}, {%11}, alpha, diff)
	%newname = %11 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %11 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%12)=0 then
	call unitroot({%0}, {%12}, alpha, diff)
	%newname = %12 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %12 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%13)=0 then
	call unitroot({%0}, {%13}, alpha, diff)
	%newname = %13 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %13 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%14)=0 then
	call unitroot({%0}, {%14}, alpha, diff)
	%newname = %14 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %14 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%15)=0 then
	call unitroot({%0}, {%15}, alpha, diff)
	%newname = %15 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %15 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%16)=0 then
	call unitroot({%0}, {%16}, alpha, diff)
	%newname = %16 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %16 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%17)=0 then
	call unitroot({%0}, {%17}, alpha, diff)
	%newname = %17 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %17 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%18)=0 then
	call unitroot({%0}, {%18}, alpha, diff)
	%newname = %18 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %18 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%19)=0 then
	call unitroot({%0}, {%19}, alpha, diff)
	%newname = %19 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %19 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

if @isempty(%20)=0 then
	call unitroot({%0}, {%20}, alpha, diff)
	%newname = %20 + "_stationary_d" + @str(diff)
	{%0}.name tempname %newname
	if diff < 3 and diff > 0 then
		%newname = %20 + "_unit_root_d" + @str(diff - 1)
		{%0}.name btempname %newname
	endif
endif

delete alpha
delete diff
delete unit_root_results

