

g <- function(x)
{
  1/2*sqrt(10-x^3)
  
}

p0 <- 1.5
FPIteration <-  function(g,p0,tol = 1e-5, maxiter = 100)
{
  #step1
  i <- 1
  
  #step2
  p_seq <- c()
  while (i <= maxiter){
    
    #step3
    p <-  g(p0)
    p_seq[i] <- p
    #step4
    if(abs(p-p0) <tol)
    {
      cat(p)
      return(data.frame(p_seq))
    }
    #step5
    i <-  i + 1
    
    #step6
    p0 <- p
    
  }
  #step7
  return(p)
}
FPIteration(g, p0)
