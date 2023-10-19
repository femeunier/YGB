rm(list = ls())

g0 = 0.02
a1 = 6

cs = 380
Gamma = 42.75
Ds = seq(0,1,length.out = 1000)


D0 = 0.016
DOprim = 0.35

An = 100

g = g0 + a1*An/((cs - Gamma)*(1 + Ds/D0))
gprim = g0 + a1*An/((cs - Gamma)*(1 + Ds/DOprim))

plot(Ds,g)
lines(Ds,gprim)
