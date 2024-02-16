#!/bin/csh

# Do the following for HTRU and for LOFAR

foreach tol ( 1.01 1.05 1.10 1.15 1.20 1.25 1.50 1.75 2.00 ) 
	echo $tol
	# do it the dedisp way
	./dedisp_steps $tol 1 1 > dedisp_$tol
	# do it the heimdall way
	./dedisp_steps_new -dmtol $tol > new_$tol
end

