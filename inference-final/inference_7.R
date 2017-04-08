n <- 10000; means <- cumsum(rexp(n)) / (1  : n); library(ggplot2)
ge <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
ge <- ge + geom_hline(yintercept = 0) + geom_line(size = 2) 
ge <- ge + labs(x = "Number of obs", y = "Cumulative mean")

n <- 10000; means <- cumsum(rnorm(n)) / (1  : n); 
gr <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
gr <- gr + geom_hline(yintercept = 0) + geom_line(size = 2) 
gr <- gr + labs(x = "Number of obs", y = "Cumulative mean")


n <- 10000; means <- cumsum(rpois(n, 1)) / (1  : n); 
gp <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
gp <- gp + geom_hline(yintercept = 0) + geom_line(size = 2) 
gp <- gp + labs(x = "Number of obs", y = "Cumulative mean")

multiplot(ge,gr,gp, cols=3)
means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2) 
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g



nosim <- 1000
cfunc <- function(x, n) exp(n) # * (mean(x) - 3.5) / 1.71
dat <- data.frame(
	x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
			   nosim), 1, cfunc, 10),
			   apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
			   	     nosim), 1, cfunc, 20),
			   apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
			   	     nosim), 1, cfunc, 30)
	),
	size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dexp, size = 2)
g + facet_grid(. ~ size)
g

nosim <- 1000
cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5) 
dat <- data.frame(
	x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE), 
			   nosim), 1, cfunc, 10),
			   apply(matrix(sample(0:1, nosim * 20, replace = TRUE), 
			   	     nosim), 1, cfunc, 20),
			   apply(matrix(sample(0:1, nosim * 30, replace = TRUE), 
			   	     nosim), 1, cfunc, 30)
	),
	size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
g

nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 0.9) / sqrt(.1 * .9)
dat <- data.frame(
	x = c(apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 10, replace = TRUE), 
			   nosim), 1, cfunc, 10),
			   apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 20, replace = TRUE), 
			   	     nosim), 1, cfunc, 20),
			   apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 30, replace = TRUE), 
			   	     nosim), 1, cfunc, 30)
	),
	size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
g

library(UsingR);data(father.son); x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) / 12

round(1 / sqrt(10 ^ (1 : 6)), 3)

.56 + c(-1, 1) * qnorm(.975) * sqrt(.56 * .44 / 100)
binom.test(56, 100)$conf.int

n <- 20; pvals <- seq(.1, .9, by = .05); nosim <- 1000
coverage <- sapply(pvals, function(p){
	phats <- rbinom(nosim, prob = p, size = n) / n
	ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
	ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
	mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

n <- 100; pvals <- seq(.1, .9, by = .05); nosim <- 1000
coverage2 <- sapply(pvals, function(p){
	phats <- rbinom(nosim, prob = p, size = n) / n
	ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
	ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
	mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage2)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ ylim(.75, 1.0)

n <- 20; pvals <- seq(.1, .9, by = .05); nosim <- 1000
coverage <- sapply(pvals, function(p){
	phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
	ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
	ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
	mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ ylim(.75, 1.0)

x <- 5; t <- 94.32; lambda <- x / t
round(lambda + c(-1, 1) * qnorm(.975) * sqrt(lambda / t), 3)
poisson.test(x, T = 94.32)$conf

lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda){
	lhats <- rpois(nosim, lambda = lambda * t) / t
	ll <- lhats - qnorm(.975) * sqrt(lhats / t)
	ul <- lhats + qnorm(.975) * sqrt(lhats / t)
	mean(ll < lambda & ul > lambda)
})

ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(lambda){
	lhats <- rpois(nosim, lambda = lambda * t) / t
	ll <- lhats - qnorm(.975) * sqrt(lhats / t)
	ul <- lhats + qnorm(.975) * sqrt(lhats / t)
	mean(ll < lambda & ul > lambda)
})
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(0, 1.0)



