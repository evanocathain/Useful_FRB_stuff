#!/bin/csh

# 18/09/2013
# Evan Keane
# A script to search for various fake signals for SPs
# using different SP search software

set dd = /projects/p002_swin/psrhome/psrsoft/usr/bin/dedisperse_all # NOT the dedisperse_all which is in $PATH!
#set fake = /lustre/home/ekeane/psrsoft/usr/src/sixproc/src/fake
set fake = /home/ekeane/psrsoft/usr/src/sixproc/src/fake
set foff = -3.000000
set fch1 = 1516.5000
set thresh = 4.0
setenv _POSIX2_VERSION 199209 # So that stupid system call in seek works

#foreach seed ( `seq 0 100` )
#foreach seed ( `seq 5 100` )
foreach seed ( 1 )

echo "SEED" $seed

set fake_options = "-nbits 8 -nchans 96 -tsamp 125.000 -fch1 $fch1 -foff $foff -nifs 1 -tobs 10.0 -seed $seed" # -nosmear"

$fake -period 100.0 -width 0.000125 -snrpeak 0.0001 -dm 0 $fake_options -headerless > fake2.fil

#foreach dm ( 500 700 900 1100 1300 1500 )
foreach dm ( 1000 ) # DM really doesn't matter -> pick a good high FRB value

    set dm_min = `echo $dm | awk '{print $1-100}'`
    set dm_max = `echo $dm | awk '{print $1+100}'`

#    foreach SNR ( 5 6 7 8 9 10 11 12 ) # From the minimum sigma recorded to just above the minimum you would confidently report
#    foreach SNR ( 2.0 2.5 3.0 3.5 4.5 5.0 ) # 4 done already!
    foreach SNR ( 3.0 )

	set period = 10.000000 # seconds, Same as Tobs -> 1 pulse in the file
	set trial_period = $period
	set tsamp = 0.000125
	set width_bins = 16  # Try 16 - a boxcar that is used, and 24 - between boxcars
	set width = `echo $tsamp $width_bins | awk '{printf "%.6f",$1*$2}'` # in seconds
	set width_smeared = `echo $width $tsamp | awk -v dm=$dm -v foff=$foff -v fch1=$fch1 '{tdm=8300.000*dm*(-foff)/(fch1^3); printf "%.6f", sqrt($1*$1 + $2*$2 + tdm*tdm)}'` # NB this is the smearing time as calculated in $fake
	set width_smeared_bins = `echo $width_smeared $tsamp | awk '{printf "%d", int($1/$2)}'`

#    foreach tol ( 1.05 1.25 )
	set tol = 1.25

	foreach delta_phase ( `seq 0 1 $width_smeared_bins` ) 

	    set trial_period = `echo $period $tsamp $delta_phase | awk '{printf "%.6f", $1+($3*(2.0*$2))}'`
	    set phase = `echo $trial_period $width_smeared $tsamp | awk '{P=$1/$3; w=$2/$3; phase = 0.5*(P-w)/w; printf "%.6f", phase - int(phase)}'`
	    echo $trial_period $width $width_smeared $phase
	    echo "Creating filterbank file"
	    $fake -period $trial_period -width $width -snrpeak $SNR -dm $dm $fake_options > fake3.fil
	    cat fake3.fil fake2.fil > fake.fil


		echo "Searching with heimdall"
		heimdall -f fake.fil -dm $dm 2000 -detect_thresh $thresh
#		exit
#		heimdall -f fake.fil -dm $dm_min $dm_max -dm_tol $tol -detect_thresh $thresh
		cat *.cand > $dm"_"$SNR"_"$tol"_"$phase".h" && rm ./*.cand
		
		echo "Searching with dedisperse_all"
#		$dd fake.fil -d $dm_min $dm_max -tol $tol -G -dec 1024 -sig $thresh -file $dm"_"$SNR"_"$tol"_"$phase".dd" >& /dev/null && rm ./*.pulse && rm ./*.tim
		$dd fake.fil -d $dm $dm -tol $tol -G -dec 1024 -sig $thresh -file $dm"_"$SNR"_"$tol"_"$phase".dd" >& /dev/null && rm ./*.pulse && rm ./*.tim
#		exit

		echo "Searching with destroy"
		dedisperse fake.fil -d $dm > fake.tim
		~/code/destroy_gutted/destroy -spthresh 6.0 fake.tim >& /dev/null && mv pulses.pls $dm"_"$SNR"_"$tol"_"$phase".d" && rm pulses.hst

		echo "Searching with seek"
		seek fake.tim -pulse -nofft >& /dev/null && mv fake.pls $dm"_"$SNR"_"$tol"_"$phase".s" && rm fake.hst
#		exit



       end # end of delta_phase loop
		
      #end # end of tol loop
       
       # Move results files and make plot
       # NB PLOTTING FILE IS HARD-CODED A BIT -> NEED TO GENERALISE THAT
       mkdir $dm"_"$SNR"_"$tol"_"$seed"_results"
       mv $dm"_"$SNR"_"$tol*.h $dm"_"$SNR"_"$tol"_"$seed"_results"/
       mv $dm"_"$SNR"_"$tol*.dd $dm"_"$SNR"_"$tol"_"$seed"_results"/
       mv $dm"_"$SNR"_"$tol*.d $dm"_"$SNR"_"$tol"_"$seed"_results"/
       mv $dm"_"$SNR"_"$tol*.s $dm"_"$SNR"_"$tol"_"$seed"_results"/
       cp make_plot_vals.csh $dm"_"$SNR"_"$tol"_"$seed"_results"/
       cp plot.gp $dm"_"$SNR"_"$tol"_"$seed"_results"/
       cd $dm"_"$SNR"_"$tol"_"$seed"_results"/
       csh make_plot_vals.csh
#       gnuplot -persist plot.gp
#       gnuplot plot.gp
       cd ..

    end # end of SNR loop

end # end of DM loop



end # end of seed loop

