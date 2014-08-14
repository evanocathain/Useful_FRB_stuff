#!/bin/csh

foreach code ( h d dd s )
#    foreach value ( `seq 0 100` )
#    cp ../1000_4.0_1.25_$value"_results/"$code"_vals" $value"_"$code"_vals"
#    end
#    cat *$code"_vals" | awk '{increment=$2; sum[NR%61]+=increment; print $1, sum[NR%61]}' | tail -60 > $code"_sum"
    cat *"_"$code"_vals" | awk -v nrealisations=100.0 '{sum[NR%60]+=$2; sumsq[NR%60]+=$2*$2; count[NR%60]++; ave[NR%60]=sum[NR%60]/count[NR%60]; rms[NR%60] = sqrt((sumsq[NR%60]/count[NR%60]) - (ave[NR%60]^2)); print $1, ave[NR%60], rms[NR%60]}' | tail -60 > $code"_ave_rms"
#    cat *"_"$code"_vals" > $code"_vals"
end

