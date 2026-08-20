## data structure
## scalar
a <- 2.0 # numeric
b <- 4L # integer
d <- "aquatic"

a
b
d

## vector
va <- c(1.0, 2.3, 3) # numeric vector
vb <- c("a", "b", "c") # character vector
vc <- c("1", "2", "3.5")
# vd <- c(1.0, "b") # don't

va
vb
vc

## matrix
va <- c(1.0, 2.3, 3) # numeric vector
vb <- c(3, 2, 5.6) # numeric vector

ma <- cbind(va, vb)
mb <- rbind(va, vb)
mc <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

ma
mb
mc
