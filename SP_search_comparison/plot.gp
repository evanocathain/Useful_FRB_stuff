#csh make_plot_vals.csh
#set term x11
set term postscript enhanced color solid
set output "comparison.ps"

set key at 1.0,5 font "Times, 20" spacing 3

#set title "H vs dd, DM=1000, S/N = 12"
set xlabel "Boxcar Phase (arbitrary zero point)" font "Times, 22"
set ylabel "Recovered S/N" font "Times, 22"
set mxtics 2
set mytics 2

#set label "2^{23} samples, 1000 DMs, 10 widths" front at 0.2,6.5 textcolor lt -1

threshold_live(x) = 10.0    # threshold, live pipeline
threshold_slow(x) = 8.0     # threshold, offline pipeline
noisefloor_1(x)   = 6.81    # false alarm prob, 2^21 samps, 1000 DMs, 10 widths
noisefloor_2(x)   = 7.01    # false alarm prob, 2^21 samps, 1000 DMs, 10 widths

#ff(x) = f(x)/sqrt(2.0)
#g(x) = f(x)*sqrt(59.0/64.0) # Theoretical maximum, if in phase
#h(x) = g(x)/sqrt(2.0)       # Theoretical minimum, if 0.5 out of phase

#set style fill transparent solid 0.5 noborder
#plot [0.0:1.0][0:15]"< awk '{print ($1+0.195)%1,$2}' h_vals" lt rgb "#008B8B" pt 7 title "Heimdall", "< awk '{print ($1+0.195)%1,$2}' dd_vals" lt rgb "#00008B" pt 7 title "DedisperseAll", "< awk '{print ($1+0.195)%1,$2}' d_vals" lt rgb "#DDA0DD" pt 7 title "Destroy", "< awk '{print ($1+0.195)%1,$2}' s_vals" lt 5 pt 7 title "Seek", threshold_live(x) lt -1 lw 2 notitle, threshold_slow(x) lt -1 lw 2 notitle, noisefloor_1(x) lt rgb "#DC143C" lw 2 notitle, noisefloor_2(x) notitle lt rgb "#DC143C" lw 2

plot [0.0:1.0][0:18]"< awk '{print ($1+0.195)%1,$2,$3}' h_ave_rms" wi ye lt rgb "#008B8B" pt 7 title "Heimdall", "< awk '{print ($1+0.195)%1,$2,$3}' dd_ave_rms" wi ye lt rgb "#00008B" pt 7 title "DedisperseAll", "< awk '{print ($1+0.195)%1,$2,$3}' d_ave_rms" wi ye lt rgb "#DDA0DD" pt 7 title "Destroy", "< awk '{print ($1+0.195)%1,$2,$3}' s_ave_rms" wi ye lt rgb "#FF7F50" pt 7 title "Seek", threshold_live(x) lt -1 lw 2 notitle, threshold_slow(x) lt -1 lw 2 notitle, noisefloor_1(x) lt rgb "#DC143C" lw 2 notitle, noisefloor_2(x) notitle lt rgb "#DC143C" lw 2

