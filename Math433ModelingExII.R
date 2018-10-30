#Math433 modeling Ex II
library(deSolve)

# condomUse.prep is the probability someone on PrEP uses a condom
# condomeUse.noprep is the probability someone not on PrEP uses a condom
# prop.prep is the proportion of the population on PrEP
# prop.noprep is the proportion of the population not on PrEP
# length.time is the time interval of interest

# prop.prep and prop.noprep must sum to 1

simHIV <- function(condomUse.reduction, prop.prep, length.time){
  
  # See google sheet for information on betas and gammas and values
  
  prop.noprep <- 1-prop.prep
  condomUse.noprep <- .35
  condomUse.prep <- (1-condomUse.reduction)*condomUse.noprep
  
  parameters <- c(beta1 = 0.0061875*(1-condomUse.prep)*(1-condomUse.prep), 
                  beta2 = 0.12825*(1-condomUse.noprep)*(1-condomUse.noprep), 
                  beta3 = 0.1175625*(1-condomUse.noprep)*(1-condomUse.noprep), 
                  beta4 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep), 
                  beta5 = 0.12825*(1-condomUse.noprep)*(1-condomUse.noprep), 
                  beta6 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep),
                  beta7 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep),
                  beta8 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep),
                  beta9 = 0.0061875*(1-condomUse.noprep)*(1-condomUse.prep),
                  beta10 = 0.1175625*(1-condomUse.prep)*(1-condomUse.noprep),
                  beta11 = 0.0388125*(1-condomUse.prep)*(1-condomUse.noprep),
                  beta12 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep),
                  beta13 = 0.0388125*(1-condomUse.noprep)*(1-condomUse.noprep),
                  beta14 = 0.0061875*(1-condomUse.noprep)*(1-condomUse.prep))
  
  state <- c(Sp = prop.prep*(1-.11*.05-.76*.15-.23*.15-.11*.95), 
             Sn = prop.noprep*(1-.11*.05-.76*.15-.23*.15-.11*.95),
             Iop= .11*.05, 
             Ih = .76*.15, 
             Ib = .23*.15,
             Ion = .11*.95,
             countHIV = 0,
             countSTI = 0)
  
  SI<- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      
      
      
      dSp = -beta1*Sp*Iop - beta10*Sp*Ion - beta11*Sp*Ib
      dSn = -beta2*Sn*Ih - beta3*Sn*Ion - beta4*Sn*Ib - beta7*Sn*Ib - beta8*Sn*Ib - beta9*Sn*Iop
      dIop = beta1*Sp*Iop + beta10*Sp*Ion + beta11*Sp*Ib
      dIh = beta2*Sn*Ih + beta7*Sn*Ib - beta6*Ih*Ion - beta13*Ih*Ib - beta14*Ih*Iop
      dIb = beta4*Sn*Ib + beta5*Ion*Ih + beta6*Ih*Ion + beta12*Ion*Ib + beta13*Ih*Ib + beta14*Ih*Iop
      dIon = beta3*Sn*Ion + beta8*Sn*Ib + beta9*Sn*Iop - beta5*Ion*Ih - beta12*Ion*Ib
      dcountHIV = beta2*Sn*Ih + beta4*Sn*Ib + beta5*Ion*Ih + beta7*Sn*Ib + beta12*Ion*Ib
      dcountSTI = beta1*Sp*Iop + beta3*Sn*Ion + beta4*Sn*Ib + beta6*Ih*Ion + beta8*Sn*Ib + beta9*Sn*Iop + beta10*Sp*Ion + beta11*Sp*Ib +  beta13*Ih*Ib + beta14*Ih*Iop
      
      list(c(dSp, dSn, dIop, dIh, dIb, dIon, dcountHIV, dcountSTI))
    })
  }
  
  times<- seq(0, length.time, by=0.01)
  
  out<- ode(y= state, times = times, func= SI, parms= parameters)
  matplot.0D(out, ylab = "Proportion of population", xlab = "Years from start", main = "Population distribution over time")
  return(c(out[dim(out)[1],8], out[dim(out)[1],9]))
}