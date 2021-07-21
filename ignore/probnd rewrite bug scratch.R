v1 <- v2 <- c()

for (i in 1:200){
  v1[i] <- prob.nd(n = i, nd = 1, k = 1, p_tie = 0.1)
  v2[i] <- prob.nd(n = i, nd = 1, k = 1, p_tie = 0.1)
}


# > prob.nd(n = 5, nd = 5, k = 0, p_tie = 0)
# [1] 0.03125
# > prob.nd2(n = 5, nd = 5, k = 0, p_tie = 0)
# [1] 0.9375

n <- 5
nd <- 5
k <- 0
p_tie <- 0

factorial(n) /
  (factorial(nd + k) * factorial(k) * factorial(n - nd - 2*k)) *
  ((1 - p_tie) / 2)^(nd + 2*k) *
  p_tie^(n - nd - 2*k)
choose(n, nd + 2*k) * ifelse(k == 0, 1, prod((nd + k + 1):(nd + 2*k))) / factorial(k) *
  ((1 - p_tie) / 2)^(nd + 2*k) *
  p_tie^(n - nd - 2*k)