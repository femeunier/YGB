rm(list = ls())

# Simulations
system2("rsync",
        c("-avz","hpc:/kyukon/scratch/gent/vo/000/gvo00074/felicien/YGB/SA/SA_reference/analy/analysis.RData",
          "./outputs/"))

load(file.path(getwd(),"outputs","analysis.RData"))

years <- 25:60
month <- which(datum$month %in% c(1,2,10:12))
pos <- intersect(years,month)

plot(pos,datum$emean$gpp[pos],ylim = c(0,4),type = 'p')
lines(years,datum$emean$gpp[years],ylim = c(0,4),col = 'black',type = 'l')

lines(pos,datum$emean$reco[pos],ylim = c(0,4),type = 'p',col = 'red')
lines(years,datum$emean$reco[years],ylim = c(0,4),col = 'red',type = 'l')

lines(pos,datum$emean$nep[pos],ylim = c(0,4),type = 'p',col = 'green')
lines(years,datum$emean$nep[years],ylim = c(0,4),col = 'green',type = 'l')

print(mean(datum$emean$gpp[pos]))
print(mean(datum$emean$reco[pos]))
print(mean(datum$emean$nep[pos]))
