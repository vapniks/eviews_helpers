' program to save lots of workfiles at once

' place path to directory in which workfiles should be saved in the following string
%path = "C:\Users\ben\Documents\Teaching\Virgis (Econometrics)\"

' place individual workfile names after %i in following FOR loop
FOR %i gt hsy jnj k nyt pep pfe pg rdsa swy wwy
	wfselect %i
	%fullpath = %path + %i
	wfsave %fullpath
NEXT