#!/bin/csh

# Evan Keane
# 14/08/2014

# A script to grab the relevant piece of filterbank files where the
# archival FRBs were found, and output the data in both archive and
# ascii text format

if ( $#argv != 1 ) then
    echo "Usage: csh script.csh <FRB_name>"
    goto death
endif
set FRB = $1
set vals = `grep $FRB FRB.info`
set stem     = $vals[2]
set dm       = $vals[3]
set skip     = $vals[4]
set tsamp    = $vals[5]
set read     = 2.0        # Read 2 seconds out, around the FRB time
set data_dir = <PATH_TO_DIRECTORY_WITH_FIL_FILES>  # Where fil files are
set destroy  = <PATH_TO_DESTROY_BINARY>            # Where destroy is
set nsamps   = `echo $read $tsamp | awk '{print $1/$2}'`

echo $FRB
foreach beam ( 1 2 3 4 5 6 7 8 9 A B C D )

    echo "Beam" $beam
    dspsr -cepoch start -S $skip -T $read -c $read -b $nsamps -D $dm -t 12 -O $beam $data_dir/$stem$beam"1.fil"
    paz -z "5 6 7 8 25 26 27 94 95" $beam".ar" -m
    pdv -t -j "B 500" $beam".ar" | awk 'NR!=1{print $2,$3,$4}' > $beam".cube"
    pdv -t -j "D, F" $beam".ar" | awk 'NR!=1{print $4}' > $beam".txt"
    $destroy -ascii -dm $dm -n $nsamps $beam".txt"
    mv pulses.pls $beam".pls"

end

death:
exit
