' program to load all workfiles in an excel spreadsheet into eviews
' and save the eviews worksheets into the same folder

' set %path variable to path of file
%path = "C:\Users\ben\Documents\Teaching\Virgis (Econometrics)\"
' set %filename variable to excel filename
%filename = "Data.xls"
%loadpath = %path + %filename
' place list of worksheets at start of for loop after %i
FOR %i wwy swy rdsa pg pfe pep nyt k jnj hsy gt 
	wfopen %loadpath range = %i
	%savepath = %path + %i + ".wf1"
	wfsave(2) %savepath
NEXT
