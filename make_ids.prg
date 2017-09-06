' program to set id series' for lots of workfiles at the same time

' place name of idseries in following string
%idseries = "data_year"
' place names of workfiles after %i in following FOR loop
FOR %i gt hsy jnj k nyt pep pfe pg rdsa swy wwy
	wfselect %i
	pagestruct @date({%idseries})
NEXT