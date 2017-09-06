' program to open lots of workfiles at once

' place path to directory containing workfiles in the following string
%path = "C:\Users\ben\Documents\Teaching\Virgis (Econometrics)\"

' place individual workfile names after %i in following FOR loop
FOR %i gt hsy jnj k nyt pep pfe pg rdsa swy wwy
	%fullpath = %path + %i
	open %fullpath
NEXT