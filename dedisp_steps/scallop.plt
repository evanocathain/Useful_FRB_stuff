#Evan Keane
#13/08/2014

# An example of how you might look at the scalloped DM response.

set term x11

set xrange[0:1]
set yrange[0.7:1.1]
set yrange[0.97:1.01]
set xrange[0:0.4]

set ylabel "Relative S/N"
set xlabel "DM (pc/cm^3)"

f(x) = 1/sqrt(1.25)
g(x) = 1/sqrt(1.10)
h(x) = 1/sqrt(1.05)
i(x) = 1/sqrt(1.01)

ff(x) = 1/sqrt(1.25)

#plot "tol_1.25" u 2:(1.0/(sqrt($1)*0.115)), ff(x)
#plot "tol_1.25" u 2:(1.0/(sqrt($1)*0.115)) lt 1, f(x) lt 1, "tol_1.10" u 2:(1.0/(sqrt($1)*0.115)) lt 3, g(x) lt 3
plot "tol_1.05" u 2:(1.0/(sqrt($1)*0.1151)) lt 5, h(x) lt 5, "tol_1.01" u 2:(1.0/(sqrt($1)*0.1151)) lt 6, i(x) lt 6 notitle
