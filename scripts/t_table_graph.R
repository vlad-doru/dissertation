library(plyr)
library(ggplot2)
library(scales)

x <- seq(0.01, 1, 0.01)
y <- seq(0.01, 1, 0.01)
data <- expand.grid(x, y)
colnames(data) <- c("p1", "p2")
head(data)

plotNValue <- function(data, title) {
  mybreaks <- c(0, 1000, 10000)
  ggplot(data, aes(p1, p2, z = n)) +  
    stat_contour() + 
    geom_tile(aes(fill = n)) + 
    scale_fill_gradient(
      aes(fill = n),
      trans = "log", 
      name = "Timp convergență",
      breaks= mybreaks,
      labels = mybreaks,
      low = "#3399ff",
      high = "#ff3333"
    ) +
    labs(x = "Grup 1 - Prob", y = "Grup 2 - Prob") +
    ggtitle(title)
}

evaluateAssigner <- function(assigner, name) {
  n <- mdply(data, function(p1, p2) {
    r <- assigner(p1, p2)
    if (is.infinite(r)) assigner(p1, abs(p1+ 0.01)) else r
  })
  names(n) <- c('p1', 'p2', 'n')
  print(plotNValue(n, name))
  n
}


analyzeDiff <- function(A, B, title) {
  t <- ((B$n - A$n) / A$n) * 100
  x <- data.frame(p1 = A$p1, p2 = A$p2, n = t)
  names(x) <- c("p1", "p2", "n")
  print(head(x))
  print(plotDiff(x, title))
}

plotDiff <- function(diff, title) {
  ggplot(diff, aes(p1, p2, z = n)) +  
    stat_contour() + 
    geom_tile(aes(fill = n)) + 
    scale_fill_gradient(
      name = "Performanță (%)",
      low = "#3399ff",
      high = "#ff3333"
    ) +
    labs(x = "Grup 1 - Prob", y = "Grup 2 - Prob") + 
    ggtitle(title)
}

computeN <- function(eps, p1, p2) {
  a <- (1.66 ^ 2)  * ( eps * p1 * (1 - p1) + (1 - eps) *  p2 * (1 - p2))
  b <- (abs(p1 - p2)^2) * eps * (1 - eps)
  a / b
}

ABAssigner <- function(p1, p2) {
  eps <- 0.5
  computeN(eps, p1, p2)
}

ProbBandit <- function(p1, p2) {
  eps <- p1 / (p1 + p2)
  computeN(eps, p1, p2)
}

EpsilonBandit <- function(p1, p2) {
  eps <- if (p1 >= p2) 0.95 else 0.05
  computeN(eps, p1, p2)
}

HybridBandit <- function(p1, p2) {
  epsExploit <- if (p1 >= p2) 1.0 else 0.0
  epsExplore <- 0.5
  epsProb <- p1 / (p1 + p2)
  alpha <- 0.1
  beta <- 0.1
  eps <- alpha * epsExploit + beta * epsExplore + (1 - (alpha + beta)) * epsProb
  computeN(eps, p1, p2)
}

abEval <- evaluateAssigner(ABAssigner, "Testare AB")
probEval <- evaluateAssigner(ProbBandit, "Bayesian Bandit")
epsilonEval <- evaluateAssigner(EpsilonBandit, "Greedy Epsilon")
hybridEval <- evaluateAssigner(HybridBandit, "Hybrid Bandit")

analyzeDiff(abEval, epsilonEval, "Testare AB vs Greedy Epsilon")
analyzeDiff(abEval, probEval, "Testare AB vs Probabilistic Bandit")
analyzeDiff(abEval, hybridEval, "Testare AB vs Hybrid Bandit")
analyzeDiff(probEval, hybridEval, "Probabilistic Bandit vs Hybrid Bandit")

