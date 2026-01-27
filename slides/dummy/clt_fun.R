par(mfrow=c(1,2))
# sampe n = 100 observations from the exponential distribution with mean = 5
x <- data.frame(X = rexp(100, rate = 5))

# plot it
hist(x$X, main = "Exponential Distribution")
abline(v= mean(x$X),col="red", lwd=5)

datalist = list()
# repeate this 120 times
# I created a for loop to do this which repeats the line above 120 times
for (i in 1:1200) {
  # during each loop I create a variable x_1, x_2, etc. using the paste command
  nam <- paste("x", i, sep = "_") 
  # I create a list containing each of the 120 dataframes, each having 100 observations
  datalist[[i]] = assign(nam, data.frame((rexp(100, rate = 5))))
}

# this syntax merges all of the list objects into one
X100 <- do.call(cbind, datalist)

# then I use the command "sapply" to compute the means of each column 
# i.e., the mean of each 100 observations
# sapply needs a data frame as input so before using sapply convert the list to a df
X100_means <- data.frame(sapply(X100, FUN=mean))

# the command "colnames" changes the name of the variable
# here I am changing the fist variable name to "CLT_FUN" hence the [1]
colnames(X100_means)[1] <- "CLT_FUN"

# Now, if the CLT worked, this histogram of the mean of the means should be normal
hist(X100_means$CLT_FUN)
abline(v= mean(X100_means$CLT_FUN),col="red", lwd=5)

