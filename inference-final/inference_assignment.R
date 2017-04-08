n <- 40
lambda = 0.2
d <- rexp(n, lambda)
th_mean = 1/0.2 # theoretical mean

# theoretical exponential distribution very large n and same lambda
set.seed(8653)
nd <- rnorm(100000, 1/lambda)
ed <- rexp(10000, lambda)
th_dist = density(nd) # to compare with 
th_mean <- round(1/lambda,4)  # for vertical line
## Code to Simulate the sample means and plot them
par(mfrow=c(1,4))
for (nsim in c(50,100,500,1000)){ 
  title<-paste("Simulations: ", nsim)
  # simulate the means
  mns = NULL
  for (i in 1 : nsim) mns = c(mns, mean(rexp(n, lambda)))
  
  mean_mns <- mean(mns)  
  dens_mns <-density(mns) # density for the sample means
  var_mns  <- round(var(mns),3)
  
  hist(mns, probability = TRUE, col="green", 
          ylim = c(0, .65), xlim = c(3,8),
       main=title, xlab="Sample Mean")
  lines(dens_mns, lwd=2, col="steelblue")
  abline(v=mean_mns, col="steelblue")
  text(x=mean_mns+0.01, y = 0.55, labels = round(mean_mns,4), col="blue")

  lines(th_dist, col="black", lwd=2)
  abline( v= 1/lambda, col="black")
  text(x=th_mean-0.01, y = 0.45,labels = th_mean, col="black")
  var_lbl<- paste("Var: ",var_mns)
  text(x=6,y=.6,labels=var_lbl)
}
var(nd)
hist(nd,breaks=10)
nd <- rnorm(100000, 1/lambda, 1/lambda)
ed <- rexp(100000, 1/lambda^2)
dens_nd <- density(nd)
dens_mns <- density(mns)
plot(density(nd), col="blue")
plot(density(mns), col="red")
plot(density(ed), col="black")
vrs = NULL
for (i in 1 : nsim) vrs = c(mns, var(rexp(n, lambda)))
hist(vrs, probability = TRUE, ylim=c(0.0, 0.6), col="white")

library(ggplot2)
ge <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
ge <- ge + geom_hline(yintercept = 0) + geom_line(size = 2) 
ge <- ge + labs(x = "Number of obs", y = "Cumulative mean")

#### Code for t.test between ToothGrowth supplements
# OJ and VC


g_vc <- ToothGrowth$len[ToothGrowth$supp=="VC"];  
g_oj <- ToothGrowth$len[ToothGrowth$supp=="OJ"]; 
difference <- g_vc - g_oj

mn <- mean(difference); 
s <- sd(difference); 
n <- 30
## Calculating directly
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n) 
## using R's built in function 
t.test(difference)
## using R's built in function, another format 
t.test(g_oj, g_vc, paired = FALSE)
## using R's built in function, another format
t.test(len ~ as.factor(dose), paired = FALSE, data = ToothGrowth)

## Below are the results (after a little formatting)

n1 <- length(g_vc); 
n2 <- length(g_oj)
sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2)) 
md <- mean(g_vc) - mean(goj)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
	md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd, 
	t.test(g_oj, g_vc, paired = FALSE, ar.equal = TRUE)$conf, 
	t.test(g_oj, g_vc, paired = FALSE)$conf
)
