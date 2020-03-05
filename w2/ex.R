# 1. Produce a histogram of the whole population using R hist() function.
# 2. Using R function sample() generate 500 samples and compute each of the samples the sample means, medians, standard deviations, and mean absolute deviations (with respect to media) saved in appropriate vectors.
# 3. Compute means and standard deviations of these sample statistics and compare to the population values.
# 4. Produce histograms of the sample means, medians, standard deviations, and mean absolute deviations. Impose normal curves on them using R function curve() (see help(curve)). Comment your findings.

graphics.off()

par(mfrow = c(2,1))
# P1
raw_data = read.table("rp_ex2_data.txt", skip=1)$V2
hist(x=raw_data, main = "Histogram", xlab = "Value" )

# P2
generated_samples = c()
for (i in seq(1,500)) {
  samples = sample(raw_data, 50)
  samples_vector = c()
  samples_vector["mean"] = mean(samples)
  samples_vector["median"] = median(samples)
  samples_vector["sd"] = sd(samples)
  samples_vector["mad_median"] = mean(abs(samples - samples_vector["median"]))
  generated_samples[[length(generated_samples) + 1]] = samples_vector
}
 
# P3
population_vector = c()
population_vector["mean"] = mean(raw_data)
population_vector["median"] = median(raw_data)
population_vector["sd"] = sd(raw_data)
population_vector["mad_median"] = mean(abs(raw_data - population_vector["median"]))

plot(index(generated_samples), lapply(X=generated_samples, FUN= function(x) {x["mean"]}))
par(new=TRUE)
lines(x=c(0,length(generated_samples)), y = c(population_vector["mean"],population_vector["mean"]), col="green")

# P4
graphics.off()
par(mfrow=c(2,2))

sample_means_array = unlist(lapply(X=generated_samples, FUN= function(x) {x["mean"]}))
curve(dnorm(x, mean=mean(sample_means_array), sd = sd(sample_means_array)), from = min(sample_means_array), to = max(sample_means_array))
hist(freq=FALSE, sample_means_array, xlab = "Sample mean", main = "Histogram of sample means", add = TRUE)

sample_median_array = unlist(lapply(X=generated_samples, FUN= function(x) {x["median"]}))
curve(dnorm(x, mean=mean(sample_median_array), sd = sd(sample_median_array)), from = min(sample_median_array), to = max(sample_median_array))
hist(freq=FALSE, sample_median_array, xlab = "Sample median", main = "Histogram of sample median", add = TRUE)

sample_sd_array = unlist(lapply(X=generated_samples, FUN= function(x) {x["sd"]}))
curve(dnorm(x, mean=mean(sample_sd_array), sd = sd(sample_sd_array)), from = min(sample_sd_array), to = max(sample_sd_array))
hist(freq=FALSE, sample_sd_array, xlab = "Sample sd", main = "Histogram of sd median", add = TRUE)

sample_mad_median_array = unlist(lapply(X=generated_samples, FUN= function(x) {x["mad_median"]}))
curve(dnorm(x, mean=mean(sample_mad_median_array), sd = sd(sample_mad_median_array)), from = min(sample_mad_median_array), to = max(sample_mad_median_array))
hist(freq=FALSE, sample_mad_median_array, xlab = "Sample mad over median", main = "Histogram of mad over median", add = TRUE)