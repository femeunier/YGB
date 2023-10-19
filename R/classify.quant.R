classify.quant <- function(values,quant.cat){
  cat <- rep(NA,length(values))
  quant.cat.ext <- c(-Inf,quant.cat,Inf)

  for (i in seq(2,length(quant.cat.ext))){
    cat[values <= quant.cat.ext[i] & values > quant.cat.ext[i - 1]] <- (i - 1)
  }

  return(cat)
}
