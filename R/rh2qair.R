rh2qair <- function(rh, T, press = 101325.0){
  Tc <- T - 273.15
  es <-  6.112 * exp((17.67 * Tc)/(Tc + 243.5))
  e <- rh * es
  p_mb <- press / 100.0
  qair <- (0.622 * e) / (p_mb - (0.378 * e))
  ##  qair <- rh * 2.541e6 * exp(-5415.0 / T) * 18/29
  return(qair)
}
