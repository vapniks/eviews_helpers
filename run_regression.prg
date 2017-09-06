' program to run regression and diagnostics
' INPUT PARAMETERS: name for equation object, dependent variable, independent variables
' Up to 21 input paramters are allowed

' Set this to 1 to include try to fix autocorrelation problems by adding successive autoregressive terms.
' If you set this variable to 1 you should also set the !num_ar_terms variable to a number greater than 1.
!fix_autocorrelation = 0
' If !fixautocorrelations = 1, then the following variable indicates the maximum number of ar terms to try,
' otherwise it indicates how many ar terms to include in the model
!num_ar_terms = 0
' Set this variable to the maximum number of autocorrelation lags to check for
!autocorr_test_order = 4
' Set this to 1 to calculate VIF (variance inflation factors) of independent variables
!calculate_vifs = 1
' Set this to 1 to calculate correlations between independent variables
!calculate_correlations = 1
' Set this to the required p-value
!pvalue = 0.05

equation {%0}.ls {%1} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
freeze(temptable) {%0}.auto(!autocorr_test_order)
%arterms = ""
%spoolname = %0 + "_diagnostics"
spool {%spoolname}
if !fix_autocorrelation = 1 then
	table(!num_ar_terms+1,3) autocorr_temp
	autocorr_temp(1,1) = "Number of ar terms"
	autocorr_temp(1,2) = "Obs*R-squared"
	autocorr_temp(1,3) = "p-value"
	!i = 1
	' while we still have a problem with autocorrelation
	while temptable(4,5) < !pvalue and !i <= !num_ar_terms
		delete {%0} temptable
		%arterms = %arterms + " ar(" + @str(!i) + ")"
		equation {%0}.ls {%1} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} {%arterms}
		!i = !i + 1
		freeze(temptable) {%0}.auto(!autocorr_test_order)
		autocorr_temp(!i,1) = !i-1
		autocorr_temp(!i,2) = temptable(4,2)
		autocorr_temp(!i,3) = temptable(4,5)
	wend

else
	for !i = 1 to !num_ar_terms
		%arterms = %arterms + " ar(" + @str(!i) + ")"
	next
	delete {%0}
	equation {%0}.ls {%1} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} {%arterms}
	freeze(autocorr_tests) {%0}.auto(!autocorr_test_order)
endif

{%spoolname}.insert(name="regression") {%0}
{%spoolname}.insert(name="normality") {%0}.hist
{%spoolname}.insert(name="heteroscedasticity") {%0}.white(c)
if !fix_autocorrelation = 1 then
	table(!i,3) autocorr_tests
	for !j = 1 to !i
		autocorr_tests(!j,1) = autocorr_temp(!j,1)
		autocorr_tests(!j,2) = autocorr_temp(!j,2)
		autocorr_tests(!j,3) = autocorr_temp(!j,3)
	next
	delete autocorr_temp
endif
{%spoolname}.insert(name="autocorrelation") autocorr_tests
delete temptable autocorr_tests


if !calculate_vifs = 1 then
	table(21,2) temptable1
	temptable1(1,1) = "Variable"
	temptable1(1,2) = "VIF value"
	!vifsum = 0
	!num = 0
	if @isempty(%2)=0 then
		equation tempreg.ls {%2} c {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(2,1) = %2
		temptable1(2,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%3)=0 then
		equation tempreg.ls {%3} c {%2} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(3,1) = %3
		temptable1(3,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%4)=0 then
		equation tempreg.ls {%4} c {%2} {%3} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(4,1) = %4
		temptable1(4,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%5)=0 then
		equation tempreg.ls {%5} c {%2} {%3} {%4}  {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(5,1) = %5
		temptable1(5,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%6)=0 then
		equation tempreg.ls {%6} c {%2} {%3} {%4} {%5}  {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(6,1) = %6
		temptable1(6,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%7)=0 then
		equation tempreg.ls {%7} c {%2} {%3} {%4} {%5} {%6}  {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(7,1) = %7
		temptable1(7,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%8)=0 then
		equation tempreg.ls {%8} c {%2} {%3} {%4} {%5} {%6} {%7}  {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(8,1) = %8
		temptable1(8,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%9)=0 then
		equation tempreg.ls {%9} c {%2} {%3} {%4} {%5} {%6} {%7} {%8}  {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(9,1) = %9
		temptable1(9,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%10)=0 then
		equation tempreg.ls {%10} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9}  {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(10,1) = %10
		temptable1(10,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%11)=0 then
		equation tempreg.ls {%11} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10}  {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(11,1) = %11
		temptable1(11,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%12)=0 then
		equation tempreg.ls {%12} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11}  {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(12,1) = %12
		temptable1(12,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%13)=0 then
		equation tempreg.ls {%13} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12}  {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(13,1) = %13
		temptable1(13,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%14)=0 then
		equation tempreg.ls {%14} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13}  {%15} {%16} {%17} {%18} {%19} {%20} 
		temptable1(14,1) = %14
		temptable1(14,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%15)=0 then
		equation tempreg.ls {%15} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14}  {%16} {%17} {%18} {%19} {%20} 
		temptable1(15,1) = %15
		temptable1(15,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%16)=0 then
		equation tempreg.ls {%16} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15}  {%17} {%18} {%19} {%20} 
		temptable1(16,1) = %16
		temptable1(16,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%17)=0 then
		equation tempreg.ls {%17} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16}  {%18} {%19} {%20} 
		temptable1(17,1) = %17
		temptable1(17,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%18)=0 then
		equation tempreg.ls {%18} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17}  {%19} {%20} 
		temptable1(18,1) = %18
		temptable1(18,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%19)=0 then
		equation tempreg.ls {%19} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18}  {%20} 
		temptable1(19,1) = %19
		temptable1(19,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	if @isempty(%20)=0 then
		equation tempreg.ls {%20} c {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19}  
		temptable1(20,1) = %20
		temptable1(20,2) = 1/(1-tempreg.@r2)
		!vifsum = !vifsum + 1/(1-tempreg.@r2)
		!num = !num + 1
	endif
	temptable1(!num+2,1) = "mean:"
	temptable1(!num+2,2) = !vifsum/!num
	table(!num+2,2) temptable2
	for !i = 1 to !num+2
		temptable2(!i,1) = temptable1(!i,1)
		temptable2(!i,2) = temptable1(!i,2)
	next
	{%spoolname}.insert(name="VIF") temptable2
	delete temptable1 temptable2 tempreg
endif
if !calculate_correlations = 1 then
	group tempgroup {%2} {%3} {%4} {%5} {%6} {%7} {%8} {%9} {%10} {%11} {%12} {%13} {%14} {%15} {%16} {%17} {%18} {%19} {%20} 
	freeze(temptable) tempgroup.cor corr prob
	{%spoolname}.insert(name="correlations") temptable
	delete temptable
	delete tempgroup
endif


