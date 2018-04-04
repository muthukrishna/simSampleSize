require(simr)

simSampleSize <- function(pilot, fit, test_var, test_method, along, max_runs = 100, nsim=1000, pwr = 0.8, cutoff = 0.01) {
  count <- 0
  ps <- powerSim(fit, fixed(test_var,test_method), nsim=nsim)
  ss <- list(length(unique(pilot[[along]]))) #list of sample sizes simulated
  pwr_calc <- list(ps$x/ps$n)
  
  next_ss <- tail(ss, 1)[[1]] #next sample size
  lower_ss <- next_ss
  print(paste0("ss=",next_ss,";pwr=",ps$x/ps$n))
  
  if (tail(pwr_calc, 1)[[1]] > pwr) {
    return(do.call(rbind, Map(data.frame, Sample_Size = ss, Power = pwr_calc))) 
  } else {
    while (tail(pwr_calc, 1)[[1]] < pwr) {
      next_ss <- next_ss * 2
      extended <- extend(fit, along=along, n=next_ss)
      ps <- powerSim(extended, fixed(test_var,test_method), nsim=nsim)
      ss <- append(ss, next_ss)
      print(paste0("ss=",next_ss,";pwr=",ps$x/ps$n))
      pwr_calc <- append(pwr_calc, ps$x/ps$n)
    }
  }
  
  lower_ss <- tail(ss, 2)[[1]]
  higher_ss <- tail(ss, 1)[[1]]

  while (!(tail(pwr_calc, 1)[[1]] >= (pwr - cutoff) & tail(pwr_calc, 1)[[1]] <= (pwr + cutoff)) & (count < max_runs) & (lower_ss != higher_ss)) {
    count <- count + 1
    print(paste0("count=",count))
    next_ss <- round((lower_ss + higher_ss)/2)
    extended <- extend(fit, along=along, n=next_ss)
    ps <- powerSim(extended, fixed(test_var,test_method), nsim=nsim)
    ss <- append(ss, next_ss)
    pwr_calc <- append(pwr_calc, ps$x/ps$n)
    print(paste0("ss=",next_ss,";pwr=",ps$x/ps$n))
    if (ps$x/ps$n > pwr) {
      higher_ss <- next_ss
    } else {
      lower_ss <- next_ss
    }
  }
  
  return(do.call(rbind, Map(data.frame, Sample_Size = ss, Power = pwr_calc)))
}