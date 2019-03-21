```
#Merge files by column

m <- merge(x=data1, y=data2, by.x='column in data1', by.y='column in data2')
```
```
#View levels and amount in each level of a column 

table(data$column)
```
```
#Create a positive definite matrix 

Posdef <- function (n, ev = runif(n, 0, 10))
{
   Z <- matrix(ncol=n, rnorm(n^2))
   decomp <- qr(Z)
   Q <- qr.Q(decomp)
   R <- qr.R(decomp)
   d <- diag(R)
   ph <- d / abs(d)
   O <- Q %*% diag(ph)
   Z <- t(O) %*% diag(ev) %*% O
   return(Z)
}

pdmat <- Posdef(n=2, ev=1:2)
#eigen(pdmat)$val

pdmat2 <- pdmat*1000
```
```
#Table containing N, mean, SD, SE, CI
#Able to group by factor using groupvars
#Note, use quotes around measurevar and groupvars

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
```
