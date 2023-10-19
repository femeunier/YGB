calc.CWD <- function(diff.P.E,init.CWD = 0){

  stopifnot(length(diff.P.E) == 12)
  CWD = rep(NA,length(diff.P.E))
  for (i in seq(1,length(diff.P.E))){
    if (i == 1){
      CWD[i] <- init.CWD
    } else{
      CWD[i] <- pmin(0,diff.P.E[i] + init.CWD)
      init.CWD <- CWD[i]
    }
  }
  return(CWD)
}
