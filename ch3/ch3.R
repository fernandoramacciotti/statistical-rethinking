## Statistical Rethinking
## Chapter 3

rm(list=ls())
library(rethinking)

# Vampire toy example -------------------------------------
PrPV <- .95
PrPM <- .01
PrV  <- .001
PrP  <- PrPV * PrV + PrPM * (1 - PrV)
(PrVP <- PrPV * PrV / PrP)


# Sampling from a grid-approx posterior  ----------------------------------

p.grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, length(p.grid)) ## uniform
likelihood <- dbinom(6, size = 9, prob = p.grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

## draw 10,000 samples from posterior
samples <- sample(p.grid, prob = posterior, size = 1e4, replace = TRUE)
dens(samples)
plot(samples)


# Sampling to summarize ---------------------------------------------------

## probability that proportion of water is less than 0.5
sum(posterior[p.grid < 0.5])
sum(samples < 0.5) / 1e4  ## same calculation from samples

## Percentiles Intervals
p_grid <- seq( from=0 , to=1 , length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom(3 , size=3 , prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid , size=1e4 , replace=TRUE , prob=posterior)
dens(samples)  ## not symmetric
PI(samples, prob = .5)
HPDI(samples, prob = .5)

## Point estimates

## MAP
p.grid[which.max(posterior)]   ## 1.0
chainmode(samples, adj = .01)  ## 0.989

## Loss function
loss <- sapply(p.grid, function(d) sum(posterior * abs(d - p.grid)))
p.grid[which.min(loss)]                      ## 0.84
p.grid[which.min(loss)] - median(samples)    ## -.001


# Sampling to simulate prediction -----------------------------------------

