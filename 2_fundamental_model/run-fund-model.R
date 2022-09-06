rm(list=ls())
load("pe.RData")

library(rstan)
library(ggmcmc)
library(latex2exp)

standardize <- function(mat){
  mat <- t(mat) - colMeans(mat)
  t(mat / sqrt(rowSums(mat^2)))
}

P = length(cluster)
TT = length(15:19)
N = P*TT

Pop_weight <- matrix(unlist(lapply(pe_sum,\(l) rowSums(l[,1:(ncol(l-2))]))),nrow=P)
Pop_weight <- t(t(Pop_weight)/colSums(Pop_weight))
dimnames(Pop_weight) <- list(names(cluster),paste0("pe",14:19))
Alpha_init <- pe_twoparty_vote_share[1,-(1+P)]-
  sum(Pop_weight[,1] * pe_twoparty_vote_share[1,-(1+P)])+0.5

data <- list(P = P,TT = TT,N = N,
             K = ncol(X),
             L = ncol(Z),
             X = standardize(X)[1:N,,drop=F],
             Z = standardize(Z)[1:N,,drop=F],
             X_pred = standardize(X)[(N+1):(N+P),,drop=F],
             Z_pred = standardize(Z)[(N+1):(N+P),,drop=F],
             Y = c(t(pe_twoparty_vote_share[2:(1+TT),-(1+P)])),
             Alpha_init = Alpha_init,
             Pop_weight = Pop_weight[,c(2:6,6)])

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file="2_fundamental_model/fund-model-simple.stan")
fit <- sampling(
  stanmodel,
  data=data,
  pars=c("alpha0"),include=FALSE,
  seed=1234,thin=1,chains=4,iter=10000,warmup=2000
  # seed=1234,iter=600,warmup=200
)

res <- data.frame(summary(fit)$summary)
View(res)

# ms2 <- rstan::extract(fit, pars="alpha", permuted=FALSE, inc_warmup=F)
# plot(1:8000,ms2[,4,1],type="l")

# ggmcmc(ggs(fit, inc_warmup = F, stan_include_auxiliar = T), 
#        plot='traceplot',file = "2_fundamental_model/fund-traceplot.pdf")

# ms <- rstan::extract(fit)

{ # get samples
  ms_fund_alpha1 <- rstan::extract(fit, pars="alpha", permuted=FALSE, inc_warmup=F)
  ms_fund_beta1 <- rstan::extract(fit, pars="beta", permuted=FALSE, inc_warmup=F)
  ms_fund_gamma1 <- rstan::extract(fit, pars="gamma", permuted=FALSE, inc_warmup=F)
  ms_fund_y_pred1 <- rstan::extract(fit, pars="y_pred", permuted=FALSE, inc_warmup=F)
}

ms_fund_alpha <- array(dim=c(8000*3,15,6))
ms_fund_beta <- array(dim=c(8000*3,12))
ms_fund_gamma <- array(dim=c(8000*3,6))
ms_fund_y_pred <- array(dim=c(8000*3,15))

chain1 <- 1:8000
chain2 <- 8001:16000
chain3 <- 16001:24000

ms_fund_alpha[chain1,,] <- array(ms_fund_alpha1[1:8000,1,],dim=c(8000,15,6))
ms_fund_alpha[chain2,,] <- array(ms_fund_alpha1[1:8000,2,],dim=c(8000,15,6))
ms_fund_alpha[chain3,,] <- array(ms_fund_alpha1[1:8000,3,],dim=c(8000,15,6))

ms_fund_beta[chain1,] <- array(ms_fund_beta1[1:8000,1,],dim=c(8000,12))
ms_fund_beta[chain2,] <- array(ms_fund_beta1[1:8000,2,],dim=c(8000,12))
ms_fund_beta[chain3,] <- array(ms_fund_beta1[1:8000,3,],dim=c(8000,12))

ms_fund_gamma[chain1,] <- array(ms_fund_gamma1[1:8000,1,],dim=c(8000,6))
ms_fund_gamma[chain2,] <- array(ms_fund_gamma1[1:8000,2,],dim=c(8000,6))
ms_fund_gamma[chain3,] <- array(ms_fund_gamma1[1:8000,3,],dim=c(8000,6))

ms_fund_y_pred[chain1,] <- array(ms_fund_y_pred1[1:8000,1,],dim=c(8000,15))
ms_fund_y_pred[chain2,] <- array(ms_fund_y_pred1[1:8000,2,],dim=c(8000,15))
ms_fund_y_pred[chain3,] <- array(ms_fund_y_pred1[1:8000,3,],dim=c(8000,15))

colnames(ms_fund_y_pred) <- names(cluster)
ms_fund_y_pred_cluster <- matrix(nrow=nrow(ms_fund_y_pred), ncol=length(cluster2))
colnames(ms_fund_y_pred_cluster) <- names(cluster2)
for(i in seq_along(cluster2)){
  ms_fund_y_pred_cluster[,i] <- colSums(t(ms_fund_y_pred[,cluster2[[i]]]) * 
                                  Pop_weight[cluster2[[i]],"pe19"] / 
                                  sum(Pop_weight[cluster2[[i]],"pe19"]))
}

## save RData
rm(list=ls()[!(ls() %in% 
                 c("ms_fund_alpha","ms_fund_beta","ms_fund_gamma",
                   "ms_fund_y_pred","ms_fund_y_pred_cluster"))])

save.image(file="samples-fundamental.RData")
