library(plyr)
library(ggplot2)
library(scales)
library(IDPmisc)
library(gridExtra)
library(cowplot)

x <- seq(0.01, 1, 0.01)
y <- seq(0.01, 1, 0.01)
data <- expand.grid(x, y)
colnames(data) <- c("p1", "p2")
head(data)

plotData <- function(d, title, name, 
                     low_col = "red",
                     mid_col = "#eeeeee", 
                     high_col = "#66cc00") {
  x <- data.frame(p1 = data$p1, p2 = data$p2, n = d)
  ggplot(x, aes(p1, p2, z = n)) +  
    stat_summary_hex() + 
    scale_fill_gradient2(
      name = name,
      na.value = '#ff3333',
      high = high_col,
      mid = mid_col,
      low = low_col
    ) +
    labs(x = "Control", y = "Test") +
    ggtitle(title) +
    geom_abline(intercept = 0.10, slope = 1, colour='#666666') +
    geom_abline(intercept = -0.10, slope = 1, colour= '#666666') +
    theme(
      #panel.grid.minor = element_blank(), 
      #panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) 
}

computeConvergence <- function(eps, p1, p2) {
  a <- (1.66 ^ 2)  * ( eps * p1 * (1 - p1) + (1 - eps) *  p2 * (1 - p2))
  b <- (abs(p1 - p2)^2) * eps * (1 - eps)
  a / b
}

evaluateConvergence <- function(epsilon, name) {
  n <- mdply(data, function(p1, p2) {
    eps <- epsilon(p1, p2)
    c <- computeConvergence(eps, p1, p2)
    if (is.infinite(c)) NA else c
  })
  names(n) <- c('p1', 'p2', 'n')
  n
}

evaluateRegret <- function(epsilon, name) {
  r <- mdply(data, function(p1, p2) {
    eps <- epsilon(p1, p2)
    if (p1 < p2) eps * (p2 - p1) else (1 - eps) * abs(p1 - p2)
  })
  names(r) <- c('p1', 'p2', 'r')
  r
}

ab.epsilonFn <- function(p1, p2) {
  0.5
}

epsilon.epsilonFn <- function(p1, p2) {
  if (p1 > p2) 0.95 else 0.05
}

prob.epsilonFn <- function(p1, p2) {
  p1 / (p1 + p2)
}

hybrid.epsilonFn <- function(p1, p2) {
  epsExploit <- if (p1 >= p2) 1.0 else 0.0
  epsExplore <- 0.5
  epsProb <- p1 / (p1 + p2)
  alpha <- 0.1
  beta <- 0.1
  eps <- alpha * epsExploit + beta * epsExplore + (1 - (alpha + beta)) * epsProb
  eps
}

ab.regret <- evaluateRegret(ab.epsilonFn, "Testare A/B")
epsilon.regret <- evaluateRegret(epsilon.epsilonFn, "Greedy Epsilon")
prob.regret <- evaluateRegret(prob.epsilonFn, "Probabilistic Bandit")
hybrid.regret <- evaluateRegret(hybrid.epsilonFn, "Hybrid Bandit")
 
ab.convergence <- evaluateConvergence(ab.epsilonFn, "Testare A/B")
epsilon.convergence <- evaluateConvergence(epsilon.epsilonFn, "Greedy Epsilon")
prob.convergence <- evaluateConvergence(prob.epsilonFn, "Probabilistic Bandit")
hybrid.convergence <- evaluateConvergence(hybrid.epsilonFn, "Hybrid Bandit")

analyzeDiff <- function(A, B, title) {
  t <- ((A - B) / A)
  x <- data.frame(p1 = data$p1, p2 = data$p2, diff = t)
  names(x) <- c("p1", "p2", "diff")
  plotData(x, title)
}


x <-  (prob.regret$r - hybrid.regret$r) / prob.regret$r
plotData(x,"Hybrid Bandit vs Bayesian Bandit - Regret", low_col="red", name = "Diferenta relativa (x ori)")
summary(x)

x <- (prob.convergence$n - hybrid.convergence$n) / hybrid.convergence$n
plotData(x,"Hybrid Bandit vs Bayesian Bandit - Convergenta", high_col="#66cc00", name = "Iteratii suplimentare (x ori)")
summary(x)

x <- (hybrid.convergence$n - ab.convergence$n) / ab.convergence$n
plotData(x,"Hybrid Bandit vs AB - Convergenta", high_col="red", name = "Iteratii suplimentare (x ori)")
summary(x)

x <- (hybrid.convergence$n - ab.convergence$n) / ab.convergence$n
plotData(x,"Hybrid Bandit vs AB - Convergenta", high_col="red", name = "Iteratii suplimentare (x ori)")
summary(x)
