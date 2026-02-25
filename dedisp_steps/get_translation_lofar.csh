#!/bin/csh

# EFK 22/01/2026
# Output the DM steps needed from my calculation for the specified dm_tol. 
# Also give the corresponding value for heimdall's calculation

set cmdargs = "-fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536"
set option = ""
set outfile = evan_vals_lofar
if ( -e evan_vals_lofar ) rm evan_vals_lofar
if ( -e lina_vals_lofar ) rm lina_vals_lofar
foreach val ( 1 2 )
  foreach dm ( 0 100 )
    echo "DM = "$dm" pc/cc"
    foreach requested_dmtol ( 1.25 1.20 1.15 1.10 1.05 1.01 1.005 )
      set dmstep = `./dedisp_steps_new $cmdargs -dmstart $dm -dmtol $requested_dmtol $option | grep "response 1.0" | head -1 | awk -v dm=$dm '{print $2-dm}'`
      echo $requested_dmtol $dmstep | awk '{printf "%.3f %.4f\n",$1,$2}' >> $outfile
    end
  end
  set option = "-lina"
  set outfile = lina_vals_lofar
end
paste evan_vals_lofar lina_vals_lofar > vals_lofar

# Now give the dm_tol from my calculation that is equivalent to 
# the heimdall step size coming out of its calculation
if ( -e equiv_vals_lofar ) rm equiv_vals_lofar
set number = 1
foreach dm ( 0 100 )
  echo "DM = "$dm" pc/cc"
  foreach requested_dmtol ( 1.25 1.20 1.15 1.10 1.05 1.01 1.005 )
    set step = `awk '{print $4}' vals_lofar | head -$number | tail -1`
#    echo "Finding dmtol for heimdall stepsize of "$step" pc/cc"
    foreach iteration ( `seq 0 1000` )
      set tol = `echo $requested_dmtol $iteration | awk '{print $1+0.001*$2}'`
      set dmstep = `./dedisp_steps_new $cmdargs -dmstart $dm -dmtol $tol | grep "response 1.0" | head -1 | awk -v dm=$dm '{print $2-dm}'`
      set check = `echo $dmstep $step $tol | awk '{if ($1>=$2) print "y"; else print "n"}'`
      if ($check == "y") then
        echo $dmstep $tol >> equiv_vals_lofar
        goto next
      endif
    end
    next:
    set number = `echo $number | awk '{print $1+1}'`
  end
end

paste vals_lofar equiv_vals_lofar > vals_for_fitting_lofar


