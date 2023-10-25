calc.MCWD <- function(diff){
  month = 1:12
  wettest.month = which.max(diff)
  month.ord = 1 + (month - wettest.month)
  month.ord = ifelse(month.ord <= 0,
                     month.ord + 12,
                     month.ord)

  diff.ord = diff[month.ord]
  CWD = NA*month.ord
  CWD[1] <- min(0,diff.ord[1])
  CWD = calc.CWD(diff.ord,CWD[1])

  MCWD = min(CWD)
  return(MCWD)
}
