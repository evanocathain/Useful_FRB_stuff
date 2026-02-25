set term postscript enhanced color solid
set output "response_pks_evan.ps"

set key right bottom box opaque font ", 20" width -4

set title "Corrected, dmtol=1.25, Parkes L-band (HTRU + SUPERB) specs" font ",20"
set ylabel "Relative response" font ",20"
set xlabel "Delta DM (pc/cc)" font ",20"
#plot [0:1.5][0.88:1.01]"evan_pks_dm0" u 2:10 wi li lw 3 title "DM start = 0 pc/cc", "evan_pks_dm100" u ($2-100.0):10 wi li lw 3 title "DM start = 100 pc/cc", "evan_pks_dm1000" u ($2-1000.0):10 wi li lt -1 lw 3 title "DM start = 1000 pc/cc" 
#exit

#set output "response_evan_lofar.ps"
#set title "Corrected, dmtol=1.25, LOFAR HBA specs"
#plot [0:0.05][0.88:1.01]"evan_lofar_dm0" u 2:10 wi li lw 2 title "DM start = 0 pc/cc", "evan_lofar_dm100" u ($2-100.0):10 wi li lw 2 title "DM start = 100 pc/cc"

set output "response_lina_pks.ps"
set y2label "Relative response, Heimdall calculation" font ",20"
set title "Heimdall, dmtol=1.25, Parkes L-band (HTRU + SUPERB) specs" font ",20"
factor=0.762 # that is 0.735/0.965
set yrange[0.15:1.01]
set ytics nomirror
set y2tics nomirror
val=0.735
set link y2 via (y-0.735)*(1.000-0.967)/(1.000-0.735)+0.967 inverse (y-0.967)*(1.000-0.735)/(1.000-0.967)+0.735
#set y2range[0.967:1.01]
#plot [0:1.5]"< awk '{if ($10>0.967) print $0,($10-0.967)*(1.000-0.735)/(1.000-0.967)+0.735}' lina_pks_dm0" u 2:11 wi li lw 3 title "DM start = 0 pc/cc", "< awk '{if ($10>0.968) print $0,($10-0.968)*(1.000-0.735)/(1.000-0.968)+0.735}' lina_pks_dm100" u ($2-100.0):11 wi li lw 2 title "DM start = 100 pc/cc", "< awk '{if ($10>0.968) print $0,($10-0.968)*(1.000-0.735)/(1.000-0.968)+0.735}' lina_pks_dm1000" u ($2-1000.0):11 wi li lt -1 lw 2 title "DM start = 1000 pc/cc", "lina_pks_dm100" u ($2-100.0):10 wi dots lt 2 axis x1y2 notitle, "lina_pks_dm1000" u ($2-1000.0):10 wi dots lt -1 axis x1y2 notitle
#"lina_pks_dm0" u 2:10 wi li lw 1 lt 1 notitle
plot [0:1.5]"< awk '{if ($10>0.967) print $0}' lina_pks_dm0" u 2:10 wi li lw 3 axis x1y2 title "DM start = 0 pc/cc", "< awk '{if ($10>0.968) print $0}' lina_pks_dm100" u ($2-100.0):10 wi li lw 3 axis x1y2 title "DM start = 100 pc/cc", "< awk '{if ($10>0.968) print $0}' lina_pks_dm1000" u ($2-1000.0):10 wi li lt -1 lw 3 axis x1y2 title "DM start = 1000 pc/cc", "lina_pks_dm100" u ($2-100.0):10 wi dots lt 2 axis x1y2 notitle, "lina_pks_dm1000" u ($2-1000.0):10 wi dots lt -1 axis x1y2 notitle, "lina_pks_dm0" u 2:10 wi li lw 1 lt 1 axis x1y2 notitle
exit

#set output "response_lina_lofar.ps"
#set title "Heimdall, dmtol=1.25, LOFAR HBA specs"
#plot [0:0.05][0.88:1.01]"< awk '{if ($10>0.9675) print $0}' lina_lofar_dm0" u 2:10 wi li lw 2 title "DM start = 0 pc/cc", "< awk '{if ($10>0.9675) print $0}' lina_lofar_dm100" u ($2-100.0):10 wi li lw 2 title "DM start = 100 pc/cc"

#set output "response_lina_lofar_1.005.ps"
#set title "Heimdall, dmtol=1.005, LOFAR HBA specs"
#plot [0:0.1][0.99:1.001]"< awk '{if ($10>0.997) print $0}' lina_lofar_1.005_dm0" u 2:10 wi li lw 2 title "DM start = 0 pc/cc", "< awk '{if ($10>0.997) print $0}' lina_lofar_1.005_dm100" u ($2-100.0):10 wi li lw 2 title "DM start = 100 pc/cc"
