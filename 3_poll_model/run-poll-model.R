rm(list=ls())
load("pe.RData")
load("samples-fundamental.RData")

library(rstan)
library(magrittr)
library(gridExtra)
library(grid)
library(Gmedian)
library(compositions)
library(ggmcmc)

################################################################################

until <- "2022-01-05"
poll_multiple_day_subset <- poll_multiple_day[poll_multiple_day$date <= until,]

data <- list(I = length(cluster2),
             J = as.numeric(as.Date("2022-3-9")-as.Date("2021-11-27")+1),
             K = nrow(poll_multiple_day_subset),
             i = match(poll_multiple_day_subset$subregion,names(cluster2)),
             j = match(poll_multiple_day_subset$date,seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
             w = ceiling(1:103/3),
             w_size = 35,
             wording = match(poll_multiple_day_subset$survey_q,c("vote","appropriate","support")),
             method = match(poll_multiple_day_subset$method,c("cordless ARS","cordless interview")),
             response = poll_multiple_day_subset[,c("lee","yoon","sim","ahn")],
             mean_prior = apply(y_pred_cluster,2,mean),
             sd_prior = apply(y_pred_cluster,2,sd),
             zero_vec = c(0,0,0))

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file="poll_model/poll-model.stan")
fit_poll <- sampling(
  stanmodel,
  data=data,
  pars=c("beta_star_temp","delta_star","d_wording_temp","d_method_temp",
         "demo_pred","lee","yoon","sim","ahn","prediction",
         "beta_star"),include=FALSE,
  # seed=12345,thin=1,chains=4,iter=10000,warmup=2000
  # seed=12345,iter=2000,warmup=500,chains=8
  seed=1234,iter=15000,warmup=5000,chains=4
)

res2 <- data.frame(summary(fit_poll)$summary)
# View(res2)
summary(res2$Rhat)

save.image(paste0("samples-poll-",until,"-new.RData"))
