## Statistical Rethinking
## Chapter 2

rm(list=ls())
library(rethinking)


#  Grid Approximation -----------------------------------------------------

## defining a grid
p.grid <- seq(from = 0, to = 1, length.out = 50)

## define prior
prior <- rep(1, length(p.grid))

## compute likelihood
likelihood <- dbinom(6, size = 100, prob = p.grid)

## product of likelihood and prior
unstd.posterior <- likelihood * prior

## standarize posterior
posterior <- unstd.posterior / sum(unstd.posterior)

## plotting
plot(p.grid, posterior, 
     type = 'b', 
     xlab = 'probability of water', 
     ylab = 'posterior probability', 
     ylim = c(0, 1))
points(p.grid, prior)
mtext(paste(length(p.grid), 'points'))



# Quadratic Approximation -------------------------------------------------

## log de normal lembra uma parabola

globe.qa <- map(
    alist(
        w ~ dbinom(9, p),    ## binomial likelihood
        p ~ dunif(0, 1)      ## uniform prior
    ),
    data = list(w = 6)
)

## summary of quadratic approximation
precis(globe.qa)

## Analytical solution - conjugate prior -> BetaBinom

w <- 6     ## observed water frequency
n <- 9     ## total trial times
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1, col = 'blue') # analytical
curve(dnorm(x, .67, .16), lty = 2, add = TRUE)              # Quadratic approximation (dashed line)
mtext(paste0('n=', n))


