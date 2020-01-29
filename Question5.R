evencount <- function(x) 
  {
    k <- 0 # assign 0 to k
    for (n in x) {
      if(n %% 2 == 0)
        {
          k <- k+1 # %% is the modulo operator
          }
      }
    return(k)
    }
evencount(x <- c(1,2,3,7,9))
