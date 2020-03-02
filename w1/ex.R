## Generate samples of sizes n = 10(10)1000 (i.e, n = 10, 20, ..., 1000) 
## from the uniform distribution, U(10, 20), using R function runif() (see, help(runif)),
## compute from each of them means and standard deviations, and generate two plots with n 
## on the x axis and the sample means and standard devaitions on on the y-axis. 
## Comment your findings.
graphics.off()
generated_list = list()
for (n in seq(10, 1000, by=10)) {
  generated_list[[length(generated_list) + 1]] = runif(n, 10, 20)
}

mean_vec = lapply(X = generated_list, FUN=mean)
sd_vec = lapply(X = generated_list, FUN=sd)

length(mean_vec)
length(sd_vec)

par(mfrow=c(2,1))
plot(x = index(mean_vec), y = mean_vec, type = "l", col="red", xlab="Sample size", ylab="Means")
plot(x = index(sd_vec), y = sd_vec, type = "l", col="green", xlab="Sample size", ylab="Standard deviations")

