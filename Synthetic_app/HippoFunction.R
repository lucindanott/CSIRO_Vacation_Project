###############################################################################
############################ HIPPO CUTOFF FUNCTION ############################ 
###############################################################################

# This Function recalls the hippocampus cutoff function

# Input: Data 
# Output: returns the cutoff for hippocampus


HippoFunction <- function(dat){
  
  dat1 <- dat[,c("Diagnosis", "Sum.hippo")]
  dat2 <- na.omit(dat1)
  
  AD_data <- filter(dat2, Diagnosis == "AD")
  HC_data <- filter(dat2, Diagnosis == "HC")
  m1 <- mean(AD_data$Sum.hippo)
  sd1 <- sd(AD_data$Sum.hippo)
  m2 <- mean(HC_data$Sum.hippo)
  sd2 <- sd(HC_data$Sum.hippo)
  
  x <- seq(-10, 10, by = 0.1)+1
  intersect <- function(m1, sd1, m2, sd2){
    B <- (m1/sd1^2 - m2/sd2^2)
    A <- 0.5*(1/sd2^2 - 1/sd1^2)
    C <- 0.5*(m2^2/sd2^2- m1^2/sd1^2) - log((sd1/sd2))
    if (A != 0){
      (-B + c(1,-1)*sqrt(B^2 - 4*A*C))/(2*A)
    } else{-C/B}
  }
  
  y <- dnorm(x,m1,sd1)
  y2 <- dnorm(x,m2,sd2)
  hippoframe <- data.frame(x,y,y2)
  
  Hippo_intersect <-intersect(m1, sd1, m2, sd2)
  hippocampus_threshold <- Hippo_intersect[2]
  
  ops <- list(hippocampus_threshold = hippocampus_threshold, 
              hippoframe = hippoframe)
  return(ops)
}



