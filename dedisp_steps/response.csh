#!/bin/csh

# Make the response curves, using more correct and general calculations
# First up Parkes L band (HTRU + SUPERB) specs
#./dedisp_steps_new -dmstart 0.0 > evan_pks_dm0
#./dedisp_steps_new -dmstart 100.0 > evan_pks_dm100
#./dedisp_steps_new -dmstart 1000.0 > evan_pks_dm1000
# Next up LOFAR HBA specs
#./dedisp_steps_new -dmstart 0.0 -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 > evan_lofar_dm0
#./dedisp_steps_new -dmstart 100.0 -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 > evan_lofar_dm100
##./dedisp_steps_new -dmstart 1000.0 -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 > evan_lofar_dm1000

# Make the response curves, using the heimdall algorithm
# First up Parkes L band (HTRU + SUPERB) specs
./dedisp_steps_new -dmstart 0.0 -lina > lina_pks_dm0
./dedisp_steps_new -dmstart 100.0 -lina > lina_pks_dm100
#./dedisp_steps_new -dmstart 1000.0 -lina > lina_pks_dm1000
# Next up LOFAR HBA specs, sub-bands 12 to 499
./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 0.0 -lina > lina_lofar_dm0
./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 100.0 -lina > lina_lofar_dm100
#./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 1000.0 -lina > lina_lofar_dm1000
# Lastly, LOFAR HBA specs, sub-bands 12 to 499 but with dmtol=1.005
./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 0.0 -dmtol 1.005 -lina > lina_lofar_1.005_dm0
./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 100.0 -dmtol 1.005 -lina > lina_lofar_1.005_dm100
#./dedisp_steps_new -fch1 197.55859375 -foff -0.0244140625 -nchans 3904 -tsamp 0.00065536 -dmstart 1000.0 -dmtol 1.005 -lina > lina_lofar_1.005_dm1000

# Make the plots
#gnuplot plot_response.gp
