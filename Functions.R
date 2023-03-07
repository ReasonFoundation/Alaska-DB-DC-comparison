cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 1:length(cumvalue)) {
    if(i == 1){
      cumvalue[i] <- cashflow[i]*(1 + interest)
    } else {
      #cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
      cumvalue[i] <- (cumvalue[i - 1] + cashflow[i])*(1 + interest)
    }
  }
  return(cumvalue)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
PMT <- function(r, nper, pv, t = 1) {
  a <- PMT0(r, nper, pv*(1+r)^t)
  return(a)
}