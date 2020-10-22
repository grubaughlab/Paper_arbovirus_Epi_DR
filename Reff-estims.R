# FILE- estimated Reff for viral outbreaks using reported case data

# This script uses a generalized linear model to estimate the exponential
# growth rate and effective reproduction number (Reff) from reported cases of
# arboviruses

# Data needed: Time series data of cases

## Interval ranges ##
# Dengue: min serial interval = 4 + 9 = 13; max = 6 + 11 = 17
# Chik: min serial interval = 2 + 7 = 9; max = 12 + 9 = 21
# Zika: min serial interval = 3 + 7 = 10; max = 14 + 8 = 23

## Example for dengue:
f = 6/17      # ratio of incubation period to serial interval
v = 17/7      # serial interval (incubation + infectious periods) in weeks

df = "<DataFrame with cases as a time series>"
time = "<Column name with desired time interval"

R0.estim(df, time, f, v)

R0.estim = function(df, time, f, v){
  cases = as.data.frame(table(df[time]))
  peak = which.max(cases$Freq)  # find outbreak peak
  CumCases = cumsum(cases[1:peak,2]) # tabulate cumulative cases through peak
  
  t = 1:peak # set temporal range
  
  # fit linear regression
  mod = glm(log(CumCases) ~ t)
  
  # record model estimates and fit
  r = mod$coefficients[2]
  AIC = mod$aic
  
  # calculate R0
  R0 = (r^2)*(1-f)*f*(v^2) + r*v + 1
  
  # return exponential growth rate, AIC, and Reff ("R0")
  return(list(r, AIC, R0))
}
