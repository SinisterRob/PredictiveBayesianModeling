library(truncnorm)

p.grid = seq(0,1,length = 1000)
# binomial likelihood with uniform(0,1)
# prior on p leads to a beta posterior.
beta_posterior = dbeta(p.grid,7,4)

# plot the unnormalized posterior
plot(p.grid, beta_posterior, type = "l", xlab = "p", ylab= "", lwd = 2, cex.lab=1.5, cex.axis=1.5)

# truncated normal prior
truncnorm_weakinf = dtruncnorm(p.grid, a=0, b=1, mean = 0.7, sd = 0.2)
truncnorm_inf = dtruncnorm(p.grid, a=0, b=1, mean = 0.71, sd = 0.01)

# plot the priors (always a good habit)
plot(p.grid,truncnorm_weakinf, type = "l", ylab = "", xlab = "p",
     lwd = 2, cex.lab=1.5, cex.axis=1.5)
plot(p.grid,truncnorm_inf, type = "l")

# the likelihood contains information from the data
likelihood = dbinom(6, 9, p.grid)

# compute the posterior using a weakly informative
# prior and an informative prior.
truncnorm_posterior1 = likelihood*truncnorm_weakinf
truncnorm_posterior2 = likelihood*truncnorm_inf

# Compare posterior when using weakly vs. non-informative priors.
# Divide by the sum to normalize, denominator in Bayes rule
plot(p.grid, truncnorm_posterior1/sum(truncnorm_posterior1), type = "l",
     lwd = 2, cex.lab=1.5, cex.axis=1.5, ylab = "", xlab = "p")
lines(p.grid, beta_posterior/sum(beta_posterior), col = "blue", lwd = 2)

# Comparing impact of priors
plot(p.grid, truncnorm_posterior2/sum(truncnorm_posterior2), type = "l",
     lwd = 2, cex.lab=1.5, cex.axis=1.5, ylab = "", xlab = "p", col = "red")
lines(p.grid, truncnorm_posterior1/sum(truncnorm_posterior1), col = "black")
lines(p.grid, beta_posterior/sum(beta_posterior), col = "blue", lwd = 2)



