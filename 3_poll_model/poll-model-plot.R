## plots from the poll model
## we made `2xxxxx-new.rds` files from the `run-poll-model.R` file. 
## Conduct that code and save it using `save.image` function.

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
library(latex2exp)

dblue <- rgb(0, 78/255, 162/255)
rred <- rgb(230/255, 30/255, 43/255)
jyellow <- rgb(255/255, 204/255, 0)
porange <- rgb(234/255, 85/255, 4/255)
party_colors <- list(dblue,rred,jyellow,porange)
party_colors50 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 100))
party_colors30 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 70))
party_colors10 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 50))

window_size <- 3
pe20result <- read.csv("0_data/20pe_cluster2.csv")

pe19_vote_count <- 0

rowSums(pe_sum$pe19[,1:(ncol(pe_sum$pe19)-2)]) %>% 
  stack() -> pe19count_temp
pe19count_temp_ind <- vector(length=nrow(pe19count_temp))
for(i in 1:nrow(pe19count_temp)){
  pe19count_temp_ind[i] <- names(cluster2)[which(sapply(cluster2, FUN=function(X) pe19count_temp$ind[i] %in% X))]
}
pe19_vote_count <- tapply(pe19count_temp$values,pe19count_temp_ind,sum)
pe19_vote_count <- as.vector(pe19_vote_count[names(cluster2)])

################################################################################
# 0. data preparation
################################################################################


##################
# "2021-12-29"
##################

until <- "2021-12-29"

fit_poll1 <- readRDS("3_poll_model/211229-new.rds")
fit_poll1 <- fit_poll1$fit_poll

{ # get samples
  ms_poll_pi1 <- rstan::extract(fit_poll1, pars="pi", permuted=FALSE, inc_warmup=F)
  ms_poll_beta1 <- rstan::extract(fit_poll1, pars="beta", permuted=FALSE, inc_warmup=F)
  ms_poll_delta1 <- rstan::extract(fit_poll1, pars="delta", permuted=FALSE, inc_warmup=F)
  ms_poll_wording1 <- rstan::extract(fit_poll1, pars="d_wording", permuted=FALSE, inc_warmup=F)
  ms_poll_method1 <- rstan::extract(fit_poll1, pars="d_method", permuted=FALSE, inc_warmup=F)
}

par(mfrow=c(2,4))
for(chain in 1:8) plot(1:4500,ms_poll_pi1[,chain,1],type="l",main=chain)

chains <- list(1:4000,1:2000,1:4000,1:4500,
            2001:4500,3001:4500,1:4500,2001:4500)

ms_poll_pi <- array(dim=c(length(unlist(chains)),7,103,4))
ms_poll_beta <- array(dim=c(length(unlist(chains)),7,35,4))
ms_poll_delta <- array(dim=c(length(unlist(chains)),102,4))
ms_poll_wording <- array(dim=c(length(unlist(chains)),3,4))
ms_poll_method <- array(dim=c(length(unlist(chains)),2,4))

for(ch in 1:8){
  new_index <- chains[[ch]]-chains[[ch]][1]+1+ifelse(ch >= 2, length(unlist(chains[1:(ch-1)])), 0)
  sample_length <- length(chains[[ch]])

  ms_poll_pi[new_index,,,] <- array(ms_poll_pi1[chains[[ch]],ch,],dim=c(sample_length,7,103,4))
  ms_poll_beta[new_index,,,] <- array(ms_poll_beta1[chains[[ch]],ch,],dim=c(sample_length,7,35,4))
  ms_poll_delta[new_index,,] <- array(ms_poll_delta1[chains[[ch]],ch,],dim=c(sample_length,102,4))
  ms_poll_wording[new_index,,] <- array(ms_poll_wording1[chains[[ch]],ch,],dim=c(sample_length,3,4))
  ms_poll_method[new_index,,] <- array(ms_poll_method1[chains[[ch]],ch,],dim=c(sample_length,2,4))
}

pi_0025_1 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.025))))
pi_0975_1 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.975))))
pi_05_1 <- lapply(1:7, \(i) sapply(1:103, \(j) ilrInv(Gmedian(ilr(ms_poll_pi[,i,j,])))))

national_poll_ms <- array(0,dim=dim(ms_poll_pi)[c(1,3,4)])
for(i in 1:7){
  national_poll_ms <- national_poll_ms + ms_poll_pi[,i,,] * pe19_vote_count[i]/sum(pe19_vote_count)
}

nat_0025_1 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.025)))
nat_0975_1 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.975)))
nat_05_1 <- sapply(1:103, \(j) ilrInv(Gmedian(ilr(national_poll_ms[,j,]))))

# rhat <- array(dim=c(7,103,4))
# for(i in 1:7){
#   for(j in 1:103){
#     for(k in 1:4){
#       rhat[i,j,k] <- rstan::Rhat(ms_poll_pi[,i,j,k])
#     }
#   }
# }
# summary(rhat)



##################
# "2022-01-19"
##################

until <- "2022-01-19"

fit_poll1 <- readRDS("3_poll_model/220119-new.rds")
fit_poll1 <- fit_poll1$fit_poll

{ # get samples
  ms_poll_pi1 <- rstan::extract(fit_poll1, pars="pi", permuted=FALSE, inc_warmup=F)
  ms_poll_beta1 <- rstan::extract(fit_poll1, pars="beta", permuted=FALSE, inc_warmup=F)
  ms_poll_delta1 <- rstan::extract(fit_poll1, pars="delta", permuted=FALSE, inc_warmup=F)
  ms_poll_wording1 <- rstan::extract(fit_poll1, pars="d_wording", permuted=FALSE, inc_warmup=F)
  ms_poll_method1 <- rstan::extract(fit_poll1, pars="d_method", permuted=FALSE, inc_warmup=F)
}

par(mfrow=c(2,4))
for(chain in 1:8) plot(1:4500,ms_poll_pi1[,chain,1],type="l",main=chain)

chains <- list(1:3500,501:3200,1:4500,1:4500,
               1:1,1:4500,1:4500,1001:3200)

ms_poll_pi <- array(dim=c(length(unlist(chains)),7,103,4))
ms_poll_beta <- array(dim=c(length(unlist(chains)),7,35,4))
ms_poll_delta <- array(dim=c(length(unlist(chains)),102,4))
ms_poll_wording <- array(dim=c(length(unlist(chains)),3,4))
ms_poll_method <- array(dim=c(length(unlist(chains)),2,4))

for(ch in 1:8){
  new_index <- chains[[ch]]-chains[[ch]][1]+1+ifelse(ch >= 2, length(unlist(chains[1:(ch-1)])), 0)
  sample_length <- length(chains[[ch]])
  
  ms_poll_pi[new_index,,,] <- array(ms_poll_pi1[chains[[ch]],ch,],dim=c(sample_length,7,103,4))
  ms_poll_beta[new_index,,,] <- array(ms_poll_beta1[chains[[ch]],ch,],dim=c(sample_length,7,35,4))
  ms_poll_delta[new_index,,] <- array(ms_poll_delta1[chains[[ch]],ch,],dim=c(sample_length,102,4))
  ms_poll_wording[new_index,,] <- array(ms_poll_wording1[chains[[ch]],ch,],dim=c(sample_length,3,4))
  ms_poll_method[new_index,,] <- array(ms_poll_method1[chains[[ch]],ch,],dim=c(sample_length,2,4))
}

pi_0025_2 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.025))))
pi_0975_2 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.975))))
pi_05_2 <- lapply(1:7, \(i) sapply(1:103, \(j) ilrInv(Gmedian(ilr(ms_poll_pi[,i,j,])))))

national_poll_ms <- array(0,dim=dim(ms_poll_pi)[c(1,3,4)])
for(i in 1:7){
  national_poll_ms <- national_poll_ms + ms_poll_pi[,i,,] * pe19_vote_count[i]/sum(pe19_vote_count)
}

nat_0025_2 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.025)))
nat_0975_2 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.975)))
nat_05_2 <- sapply(1:103, \(j) ilrInv(Gmedian(ilr(national_poll_ms[,j,]))))


# rhat <- array(dim=c(7,103,4))
# for(i in 1:7){
#   for(j in 1:103){
#     for(k in 1:4){
#       rhat[i,j,k] <- rstan::Rhat(ms_poll_pi[,i,j,k])
#     }
#   }
# }
# summary(rhat)



##################
# "2022-02-09"
##################

until <- "2022-02-09"

fit_poll1 <- readRDS("3_poll_model/220209-new.rds")
fit_poll1 <- fit_poll1$fit_poll

{ # get samples
  ms_poll_pi1 <- rstan::extract(fit_poll1, pars="pi", permuted=FALSE, inc_warmup=F)
  ms_poll_beta1 <- rstan::extract(fit_poll1, pars="beta", permuted=FALSE, inc_warmup=F)
  ms_poll_delta1 <- rstan::extract(fit_poll1, pars="delta", permuted=FALSE, inc_warmup=F)
  ms_poll_wording1 <- rstan::extract(fit_poll1, pars="d_wording", permuted=FALSE, inc_warmup=F)
  ms_poll_method1 <- rstan::extract(fit_poll1, pars="d_method", permuted=FALSE, inc_warmup=F)
}

par(mfrow=c(2,4))
for(chain in 1:8) plot(1:4500,ms_poll_pi1[,chain,1],type="l",main=chain)

chains <- list(1:4500,1001:4500,1:1,1:2500,
               1:1,1:4500,1:4500,501:4500)

ms_poll_pi <- array(dim=c(length(unlist(chains)),7,103,4))
ms_poll_beta <- array(dim=c(length(unlist(chains)),7,35,4))
ms_poll_delta <- array(dim=c(length(unlist(chains)),102,4))
ms_poll_wording <- array(dim=c(length(unlist(chains)),3,4))
ms_poll_method <- array(dim=c(length(unlist(chains)),2,4))

for(ch in 1:8){
  new_index <- chains[[ch]]-chains[[ch]][1]+1+ifelse(ch >= 2, length(unlist(chains[1:(ch-1)])), 0)
  sample_length <- length(chains[[ch]])
  
  ms_poll_pi[new_index,,,] <- array(ms_poll_pi1[chains[[ch]],ch,],dim=c(sample_length,7,103,4))
  ms_poll_beta[new_index,,,] <- array(ms_poll_beta1[chains[[ch]],ch,],dim=c(sample_length,7,35,4))
  ms_poll_delta[new_index,,] <- array(ms_poll_delta1[chains[[ch]],ch,],dim=c(sample_length,102,4))
  ms_poll_wording[new_index,,] <- array(ms_poll_wording1[chains[[ch]],ch,],dim=c(sample_length,3,4))
  ms_poll_method[new_index,,] <- array(ms_poll_method1[chains[[ch]],ch,],dim=c(sample_length,2,4))
}

pi_0025_3 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.025))))
pi_0975_3 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.975))))
pi_05_3 <- lapply(1:7, \(i) sapply(1:103, \(j) ilrInv(Gmedian(ilr(ms_poll_pi[,i,j,])))))

national_poll_ms <- array(0,dim=dim(ms_poll_pi)[c(1,3,4)])
for(i in 1:7){
  national_poll_ms <- national_poll_ms + ms_poll_pi[,i,,] * pe19_vote_count[i]/sum(pe19_vote_count)
}

nat_0025_3 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.025)))
nat_0975_3 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.975)))
nat_05_3 <- sapply(1:103, \(j) ilrInv(Gmedian(ilr(national_poll_ms[,j,]))))


# rhat <- array(dim=c(7,103,4))
# for(i in 1:7){
#   for(j in 1:103){
#     for(k in 1:4){
#       rhat[i,j,k] <- rstan::Rhat(ms_poll_pi[,i,j,k])
#     }
#   }
# }
# summary(rhat)

##################
# "2022-03-02"
##################

until <- "2022-03-02"

fit_poll1 <- readRDS("3_poll_model/220302-new.rds")
fit_poll1 <- fit_poll1$fit_poll

{ # get samples
  ms_poll_pi1 <- rstan::extract(fit_poll1, pars="pi", permuted=FALSE, inc_warmup=F)
  ms_poll_beta1 <- rstan::extract(fit_poll1, pars="beta", permuted=FALSE, inc_warmup=F)
  ms_poll_delta1 <- rstan::extract(fit_poll1, pars="delta", permuted=FALSE, inc_warmup=F)
  ms_poll_wording1 <- rstan::extract(fit_poll1, pars="d_wording", permuted=FALSE, inc_warmup=F)
  ms_poll_method1 <- rstan::extract(fit_poll1, pars="d_method", permuted=FALSE, inc_warmup=F)
}

par(mfrow=c(2,4))
for(chain in 1:8) plot(1:4000,ms_poll_pi1[,chain,1],type="l",main=chain)

chains <- list(1:4000,1:4000,1:4000,1:4000,
               1:4000,1:4000,1:3000,1:4000)

ms_poll_pi <- array(dim=c(length(unlist(chains)),7,103,4))
ms_poll_beta <- array(dim=c(length(unlist(chains)),7,35,4))
ms_poll_delta <- array(dim=c(length(unlist(chains)),102,4))
ms_poll_wording <- array(dim=c(length(unlist(chains)),3,4))
ms_poll_method <- array(dim=c(length(unlist(chains)),2,4))

for(ch in 1:8){
  new_index <- chains[[ch]]-chains[[ch]][1]+1+ifelse(ch >= 2, length(unlist(chains[1:(ch-1)])), 0)
  sample_length <- length(chains[[ch]])
  
  ms_poll_pi[new_index,,,] <- array(ms_poll_pi1[chains[[ch]],ch,],dim=c(sample_length,7,103,4))
  ms_poll_beta[new_index,,,] <- array(ms_poll_beta1[chains[[ch]],ch,],dim=c(sample_length,7,35,4))
  ms_poll_delta[new_index,,] <- array(ms_poll_delta1[chains[[ch]],ch,],dim=c(sample_length,102,4))
  ms_poll_wording[new_index,,] <- array(ms_poll_wording1[chains[[ch]],ch,],dim=c(sample_length,3,4))
  ms_poll_method[new_index,,] <- array(ms_poll_method1[chains[[ch]],ch,],dim=c(sample_length,2,4))
}

pi_0025_4 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.025))))
pi_0975_4 <- lapply(1:7, \(i) sapply(1:4, \(k) apply(ms_poll_pi[,i,,k],2,function(x) quantile(x,0.975))))
pi_05_4 <- lapply(1:7, \(i) sapply(1:103, \(j) ilrInv(Gmedian(ilr(ms_poll_pi[,i,j,])))))

national_poll_ms <- array(0,dim=dim(ms_poll_pi)[c(1,3,4)])
for(i in 1:7){
  national_poll_ms <- national_poll_ms + ms_poll_pi[,i,,] * pe19_vote_count[i]/sum(pe19_vote_count)
}

nat_0025_4 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.025)))
nat_0975_4 <- sapply(1:4, \(k) apply(national_poll_ms[,,k],2,function(x) quantile(x,0.975)))
nat_05_4 <- sapply(1:103, \(j) ilrInv(Gmedian(ilr(national_poll_ms[,j,]))))

# rhat <- array(dim=c(7,103,4))
# for(i in 1:7){
#   for(j in 1:103){
#     for(k in 1:4){
#       rhat[i,j,k] <- rstan::Rhat(ms_poll_pi[,i,j,k])
#     }
#   }
# }
# summary(rhat)


rm(list=ls()[!(ls() %in% 
                 c("pi_0025_4","pi_0975_4","pi_05_4","nat_0025_4","nat_0975_4","nat_05_4",
                   "pi_0025_3","pi_0975_3","pi_05_3","nat_0025_3","nat_0975_3","nat_05_3",
                   "pi_0025_2","pi_0975_2","pi_05_2","nat_0025_2","nat_0975_2","nat_05_2",
                   "pi_0025_1","pi_0975_1","pi_05_1","nat_0025_1","nat_0975_1","nat_05_1") )])

save.image(file="poll-quantiles.RData")

rm(list=ls())
load("pe.RData")
load("samples-fundamental.RData")
load("poll-quantiles.RData")

dblue <- rgb(0, 78/255, 162/255)
rred <- rgb(230/255, 30/255, 43/255)
jyellow <- rgb(255/255, 204/255, 0)
porange <- rgb(234/255, 85/255, 4/255)
party_colors <- list(dblue,rred,jyellow,porange)
party_colors50 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 100))
party_colors30 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 70))
party_colors10 <- lapply(party_colors, function(cc) rgb(col2rgb(cc)[1],col2rgb(cc)[2],col2rgb(cc)[3],max = 255,alpha = 50))

window_size <- 3
pe20result <- read.csv("0_data/20pe_cluster2.csv")

pe19_vote_count <- 0

rowSums(pe_sum$pe19[,1:(ncol(pe_sum$pe19)-2)]) %>% 
  stack() -> pe19count_temp
pe19count_temp_ind <- vector(length=nrow(pe19count_temp))
for(i in 1:nrow(pe19count_temp)){
  pe19count_temp_ind[i] <- names(cluster2)[which(sapply(cluster2, FUN=function(X) pe19count_temp$ind[i] %in% X))]
}
pe19_vote_count <- tapply(pe19count_temp$values,pe19count_temp_ind,sum)
pe19_vote_count <- as.vector(pe19_vote_count[names(cluster2)])

################################################################################
# 1. pi
################################################################################

until <- "2021-12-29"
until <- "2022-01-19"
until <- "2022-02-09"
until <- "2022-03-02"

{
  tt <- which(c("2021-12-29","2022-01-19","2022-02-09","2022-03-02") == until)
  op <- par(mfrow = c(4,2),
            oma = c(5,4,2,0) + 0.1,
            mar = c(0,0,1,1) + 0.1)
  for(i in 1:7){
    plot(0,0,xlim=c(1,103),ylim=c(0,1),pch=NA,
         xaxt="n",yaxt="n")
    title(names(cluster2)[i], line = -2)
    if(i >= 6) {
      axis(1, rev(seq(103,1,by=-7)), 14:0, cex.axis = .7)
    }else{
      axis(1, rev(seq(103,1,by=-7)), labels=F)
    }
    if(i %% 2 == 1){
      axis(2, seq(0,1,by=0.2),seq(0,100,by=20), las=1)
    }else{
      axis(2, seq(0,1,by=0.2),labels=F)  
    }
    
    if(i==1){
      mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
      mtext('Four-party vote share (%)', side = 2, line = 3, outer=T)
      legend(x=0,y=1.08, title="",
             legend=c("Lee","Yoon","Sim","Ahn"),
             lwd=5, col=unlist(party_colors),lty=1,pch=NA,
             bty = "n")
    }
    
    poll_multiple_day_subset <- poll_multiple_day[poll_multiple_day$date <= until,]
    region_index = (poll$subregion == names(cluster2)[i])
    region_index2 = (poll_multiple_day_subset$subregion == names(cluster2)[i])
    
    for(k in 1:4){
      points(match(poll_multiple_day_subset$date,seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103))[region_index2],
             poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")][,k]/rowSums(poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")]), 
             col=party_colors10[[k]],
             pch=20,lwd=0,
             cex=sqrt(rowSums(poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")])/quantile(rowSums(poll_multiple_day_subset[,c("lee","yoon","sim","ahn")]),0.95))*2)
      
      polygon(c(1:103,103:1),c(eval(parse(text=paste0("pi_0025_",tt)))[[i]][,k],
                               eval(parse(text=paste0("pi_0975_",tt)))[[i]][,k][103:1]),
              col=party_colors50[[k]],border = NA)
    }
    for(k in 1:4){
      until.index <- which.max(until<=seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103))
      lines(1:until.index,eval(parse(text=paste0("pi_05_",tt)))[[i]][k,1:until.index],
            col=party_colors[[k]],lwd=2)
      lines(until.index:103,eval(parse(text=paste0("pi_05_",tt)))[[i]][k,until.index:103],
            col=party_colors[[k]],lwd=1.5,lty=3)
      points(103,eval(parse(text=paste0("pi_05_",tt)))[[i]][k,103],
             col=party_colors[[k]],pch=20)
    }
    
  }
  title(paste0("Forecast made at ",until," (",13-3*tt," week",ifelse(tt==4,"","s")," left)"),outer=T,line=-0.2)
  par(op)
} # 13*11 inches

# National trend plot
{
  pe19_vote_count <- 0
  
  rowSums(pe_sum$pe19[,1:(ncol(pe_sum$pe19)-2)]) %>% 
    stack() -> pe19count_temp
  pe19count_temp_ind <- vector(length=nrow(pe19count_temp))
  for(i in 1:nrow(pe19count_temp)){
    pe19count_temp_ind[i] <- names(cluster2)[which(sapply(cluster2, FUN=function(X) pe19count_temp$ind[i] %in% X))]
  }
  pe19_vote_count <- tapply(pe19count_temp$values,pe19count_temp_ind,sum)
  pe19_vote_count <- as.vector(pe19_vote_count[names(cluster2)])
  
  national_poll_ms <- array(0,dim=dim(ms_poll_pi)[c(1,3,4)])
  for(i in 1:7){
    national_poll_ms <- national_poll_ms + ms_poll_pi[,i,,] * pe19_vote_count[i]/sum(pe19_vote_count)
  }
  
  par(mfrow=c(1,1),
      oma = c(0,0,0,0) + 0,
      mar = c(5,4,4,2) + 0.1)
  plot(0,0,xlim=c(1,103),ylim=c(0,1),pch=NA,
       xaxt="n",yaxt="n",xlab="Dates",ylab="Vote share (%)")
  title(paste0("National Trend and Forecast of the Four-Party Vote Share (~",until,")"))
  axis(1, rev(seq(103,1,by=-7)), 
       format(seq(as.Date("2021-11-27"),as.Date("2022-03-09"),length.out=103), "%b %d")[rev(seq(103,1,by=-7))],
       cex.axis = .7)
  axis(2, seq(0,1,by=0.2),seq(0,100,by=20), las=1)
  index_national <- (national_poll$date <= until)
  for(k in 1:4){
    points(match(as.Date(national_poll$date[index_national]),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
           national_poll[index_national,c("lee","yoon","sim","ahn")][,k], 
           col=party_colors30[[k]],
           pch=20,lwd=0,
           cex=sqrt(national_poll$sample_size[index_national]/quantile(national_poll$sample_size[index_national],0.95))*2)
    
    polygon(c(1:103,103:1),c(apply(national_poll_ms[,,k],2,function(x) quantile(x,0.1)),
                             apply(national_poll_ms[,,k],2,function(x) quantile(x,0.9))[103:1]),
            col=party_colors50[[k]],border = NA)
    polygon(c(1:103,103:1),c(apply(national_poll_ms[,,k],2,function(x) quantile(x,0.025)),
                             apply(national_poll_ms[,,k],2,function(x) quantile(x,0.975))[103:1]),
            col=party_colors50[[k]],border = NA)
    # lines(c(103,103),quantile(national_poll_ms[,103,k],c(0.1,0.9)),col=party_colors[[k]],lwd=2)
  }
  for(k in 1:4) lines(1:103,apply(national_poll_ms[,,k],2,function(x) quantile(x,0.5)),
                      col=party_colors[[k]],lwd=2)
  for(k in 1:3) points(103,c(0.484249418,0.491658932,0.024091649)[k],pch=4,cex=2,col=party_colors[[k]])
  
  legend(x=0,y=1.05, title="",
         legend=c("Lee","Yoon","Sim","Ahn"),
         lwd=5, col=unlist(party_colors),lty=1,pch=NA,
         bty = "n")
  
  abline(v=match(as.Date("2021-12-16"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  text(x=match(as.Date("2021-12-16"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
       y=1,labels="Yoon says sorry",pos=4,srt=-90,cex=.8)
  abline(v=match(as.Date("2022-1-6"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  text(x=match(as.Date("2022-1-6"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
       y=1,labels="Yoon-Lee (Junseok) one-team declaration",pos=4,srt=-90,cex=.8)
  abline(v=match(as.Date("2022-2-20"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  text(x=match(as.Date("2022-2-20"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
       y=1,labels="Ahn \"no unification\"",pos=4,srt=-90,cex=.8)
  abline(v=match(as.Date("2022-3-3"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  text(x=match(as.Date("2022-3-3"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
       y=1,labels="Black out starts, Yoon-Ahn unification",pos=4,srt=-90,cex=.8)
  abline(v=match(as.Date("2022-3-9"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  text(x=match(as.Date("2022-3-9"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
       y=1,labels="The 20th PE",pos=4,srt=-90,cex=.8)
}

################################################################################
# 2 & 3. beta and delta_swing
################################################################################

{
  w = ceiling(1:103/3)
  w_size = max(w)
  op <- par(mfrow = c(4,2),
            oma = c(5,4,2,0) + 0.1,
            mar = c(0,1,1,2) + 0.1)
  for(i in 1:7){
    plot(0,0,xlim=c(1,103),ylim=c(0,1),pch=NA,
         xaxt="n",yaxt="n")
    title(TeX(paste0("$\\xi_{pw(t)}$ (",names(cluster2)[i],")")), line = -2)
    if(i >= 7) {
      axis(1, rev(seq(103,1,by=-7)), 14:0, cex.axis = .7)
    }else{
      axis(1, rev(seq(103,1,by=-7)), labels=F)
    }
    # if(i %% 2 == 1){
      axis(2, seq(0,1,by=0.2),seq(0,1,by=0.2), las=1)
    # }else{
      # axis(2, seq(0,1,by=0.2),labels=F)  
    # }
    
    if(i==1){
      mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
      # mtext(TeX('$\\beta_{pt}$ or $\\delta_t^{swing}$'), side = 2, line = 2.2, outer=T)
      legend(x=0,y=1.08, title="",
             legend=c("Lee","Yoon","Sim","Ahn"),
             lwd=5, col=unlist(party_colors),lty=1,pch=NA,
             bty = "n")
    }
    
    for(k in 1:4){
      # polygon(c(1:103,103:1),c(apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.1))[w],
      #                          apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.9))[rev(w)]),
      #         col=party_colors50[[k]],border = NA)
      polygon(c(1:103,103:1),c(apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.025))[w],
                               apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.975))[rev(w)]),
              col=party_colors50[[k]],border = NA)
    }
    q5 <- sapply(1:35, \(j) ilrInv(Gmedian(ilr(ms_poll_beta[,i,j,]))))
    for(k in 1:4) lines(1:103, q5[k,w],
                        col=party_colors[[k]],lwd=2)
    # for(k in 1:3) points(103,pe20result[i,k+1],pch=4,cex=2,col=party_colors[[k]])
      
  }

  ##############################################################################
  
  plot(0,0,xlim=c(1,103),ylim=c(0,0.5),pch=NA,
       xaxt="n",yaxt="n")
  title(TeX('$\\delta_t^{swing}$'), line = -2)
  axis(1, rev(seq(103,1,by=-7)), 14:0, cex.axis = .7)
  axis(2, seq(0,0.5,by=0.1),las=1)
  
  for(k in 1:4){
    # polygon(c(1:102,102:1),c(apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.1)),
    #                          apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.9))[rev(1:102)]),
    #         col=party_colors50[[k]],border = NA)
    polygon(c(1:102,102:1),c(apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.025)),
                             apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.975))[rev(1:102)]),
            col=party_colors50[[k]],border = NA)
  }
  q5 <- sapply(1:102, \(j) ilrInv(Gmedian(ilr(ms_poll_delta[,j,]))))
  for(k in 1:4) lines(1:102,q5[k,],
                      col=party_colors[[k]],lwd=2)
  abline(h=0.25,lwd=2,col="gray",lty=2)
  
  # abline(v=match(as.Date("2021-12-16"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  # text(x=match(as.Date("2021-12-16"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
  #      y=0.5,labels="Yoon says sorry",pos=4,srt=-90,cex=.8)
  # abline(v=match(as.Date("2022-1-6"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  # text(x=match(as.Date("2022-1-6"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
  #      y=0.5,labels="Yoon-Lee (Junseok) one-team declaration",pos=4,srt=-90,cex=.8)
  # abline(v=match(as.Date("2022-2-20"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  # text(x=match(as.Date("2022-2-20"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
  #      y=0.5,labels="Ahn \"no unification\"",pos=4,srt=-90,cex=.8)
  # abline(v=match(as.Date("2022-3-3"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  # text(x=match(as.Date("2022-3-3"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
  #      y=0.5,labels="Black out starts, Yoon-Ahn unification",pos=4,srt=-90,cex=.8)
  # abline(v=match(as.Date("2022-3-9"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)))
  # text(x=match(as.Date("2022-3-9"),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
  #      y=0.5,labels="The 20th PE",pos=4,srt=-90,cex=.8)
  
  par(op)
} # 13*11 inches


{ ### smaller plot
  layout(matrix(c(1,2,8,8,3,4,8,8,5,6,7,9), 3, 4, byrow = TRUE))
  w = ceiling(1:103/3)
  w_size = max(w)
  op <- par(#mfrow = c(4,2),
            oma = c(5,2,2,3) + 0.1,
            mar = c(0,1,1,0) + 0.1)
  for(i in 1:7){
    plot(0,0,xlim=c(1,103),ylim=c(0,1),pch=NA,
         xaxt="n",yaxt="n")
    title(TeX(paste0("$\\xi_{pw(t)}$ (",names(cluster2)[i],")")), line = -2)
    if(i >= 5) {
      axis(1, rev(seq(103,1,by=-14)), seq(14,0,by=-2), cex.axis = .9)
    }else{
      axis(1, rev(seq(103,1,by=-14)), labels=F)
    }
    if(i %in% c(1,3,5)){
      axis(2, seq(0,1,by=0.2),seq(0,1,by=0.2), las=1)
    }else{
      axis(2, seq(0,1,by=0.2),labels=F)
    }
    
    if(i==1){
      mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
    #   # mtext(TeX('$\\beta_{pt}$ or $\\delta_t^{swing}$'), side = 2, line = 2.2, outer=T)
    #   legend(x=0,y=1.08, title="",
    #          legend=c("Lee","Yoon","Sim","Ahn"),
    #          lwd=5, col=unlist(party_colors),lty=1,pch=NA,
    #          bty = "n")
    }
    
    for(k in 1:4){
      # polygon(c(1:103,103:1),c(apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.1))[w],
      #                          apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.9))[rev(w)]),
      #         col=party_colors50[[k]],border = NA)
      polygon(c(1:103,103:1),c(apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.025))[w],
                               apply(ms_poll_beta[,i,,k],2,function(x) quantile(x,0.975))[rev(w)]),
              col=party_colors50[[k]],border = NA)
    }
    q5 <- sapply(1:35, \(j) ilrInv(Gmedian(ilr(ms_poll_beta[,i,j,]))))
    for(k in 1:4) lines(1:103, q5[k,w],
                        col=party_colors[[k]],lwd=2)
    # for(k in 1:3) points(103,pe20result[i,k+1],pch=4,cex=2,col=party_colors[[k]])
    
  }
  
  ##############################################################################
  
  plot(0,0,xlim=c(1,103),ylim=c(0,0.5),pch=NA,
       xaxt="n",yaxt="n")
  title(TeX('$\\delta_t^{swing}$'), line = -2)
  axis(3, rev(seq(103,1,by=-7)), 14:0, cex.axis = 1)
  axis(4, seq(0,0.5,by=0.1),las=1)
  
  for(k in 1:4){
    # polygon(c(1:102,102:1),c(apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.1)),
    #                          apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.9))[rev(1:102)]),
    #         col=party_colors50[[k]],border = NA)
    polygon(c(1:102,102:1),c(apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.025)),
                             apply(ms_poll_delta[,,k],2,function(x) quantile(x,0.975))[rev(1:102)]),
            col=party_colors50[[k]],border = NA)
  }
  q5 <- sapply(1:102, \(j) ilrInv(Gmedian(ilr(ms_poll_delta[,j,]))))
  for(k in 1:4) lines(1:102,q5[k,],
                      col=party_colors[[k]],lwd=2)
  abline(h=0.25,lwd=2,col="gray",lty=2)
  
  plot(0,0,xlim=c(1,103),ylim=c(0,0.5),pch=NA,axes = 0)
  # mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
  # mtext(TeX('$\\beta_{pt}$ or $\\delta_t^{swing}$'), side = 2, line = 2.2, outer=T)
  legend("topleft", title="",
         legend=c("Lee","Yoon","Sim","Ahn"),
         lwd=5, col=unlist(party_colors),lty=1,pch=NA,
         bty = "n",cex=1.5)
  
  par(op)
} # 8*13 inches

################################################################################
# 4. delta_wording, method
################################################################################

{ #method
  par(mfrow=c(1,5),
      oma = c(3, 6, 3, 2),
      mar = c(3, 0, 3, 1) + 0.1)
  layout(matrix(c(1,2,3,4,5,6),nrow=2,byrow=T))#,widths=c(3,3,1.2,3,3,3))
  
  

#{ # wording
  # par(mfrow=c(1,3),
  #     oma = c(3, 8, 3, 2),
  #     mar = c(0, 0, 0, 1) + 0.1)
  
  for(i in 1:3){
    plot(0,0,xlim=c(0.22,0.28),ylim=c(0.5,5),yaxt="n",xaxt="n",
         xlab="",ylab="")
    abline(h=1:4,lty=2,col="lightgray")
    axis(1, seq(0.22,0.28,by=0.01))
    mtext(TeX(paste0("$\\eta^{wording}_i$ (Wording = ",c("Vote","Appropriate","Support")[i],")")),line=1,cex=1.0)
    if(i == 1) {
      axis(2, 4:1,c("Lee ","Yoon ","Sim ","Ahn "),las=1,cex.axis=1.4)
      axis(2, 5,"Candidate",las=1,cex.axis=1.2,font=2,lwd=NA,cex.axis=1.2)
    }
    abline(v=0.25,col="gray",lwd=2,lty=2)
    q5 <- sapply(1:3, \(j) ilrInv(Gmedian(ilr(ms_poll_wording[,j,]))))
    for(k in 1:4){
      lines(y=c(5-k,5-k),
            x=quantile(ms_poll_wording[,i,k],c(0.025,0.975)),  
            lwd=1)
      lines(y=c(5-k,5-k),
            x=quantile(ms_poll_wording[,i,k],c(0.1,0.9)),  
            lwd=3)
      points(y=5-k,
             x=q5[k,i],  
             pch=20,cex=1.2)
      dx <- (density(ms_poll_wording[,i,k]))$x
      dy <- (density(ms_poll_wording[,i,k]))$y
      q99 <- quantile(ms_poll_wording[,i,k],0.99)
      q1 <- quantile(ms_poll_wording[,i,k],0.01)
      lines(dx[dx <= q99 & dx >= q1],dy[dx <= q99 & dx >= q1]*0.002+5-k,col="gray")
    }
  }
  legend("topright", legend=c("80% CI", "95% CI", "Median"),
         lwd=c(1,3,NA), pch=c(NA,NA,20), col=rep("black",3),
         bty = "n")
  # title(paste0("Wording (~",until,")"),outer=T)
  
  for(i in 1:2){
    plot(0,0,xlim=c(0.22,0.28),ylim=c(0.5,5),yaxt="n",xaxt="n",
         xlab="",ylab="")
    abline(h=1:4,lty=2,col="lightgray")
    axis(1, seq(0.21,0.29,by=0.01))
    mtext(TeX(paste0("$\\eta^{method}_j$ (Method = ",c("ARS","Interview")[i],")")),line=1,cex=1.0)
    if(i == 1){
      axis(2, 4:1,c("Lee ","Yoon ","Sim ","Ahn "),las=1,cex.axis=1.4)
      axis(2, 5,"Candidate",las=1,cex.axis=1.2,font=2,lwd=NA)
    }
    abline(v=0.25,col="gray",lwd=2,lty=2)
    q5 <- sapply(1:2, \(j) ilrInv(Gmedian(ilr(ms_poll_method[,j,]))))
    for(k in 1:4){
      lines(y=c(5-k,5-k),
            x=quantile(ms_poll_method[,i,k],c(0.025,0.975)),  
            lwd=1)
      lines(y=c(5-k,5-k),
            x=quantile(ms_poll_method[,i,k],c(0.1,0.9)),  
            lwd=3)
      points(y=5-k,
             x=q5[k,i],  
             pch=20,cex=1.2)
      dx <- (density(ms_poll_method[,i,k]))$x
      dy <- (density(ms_poll_method[,i,k]))$y
      q99 <- quantile(ms_poll_method[,i,k],0.99)
      q1 <- quantile(ms_poll_method[,i,k],0.01)
      lines(dx[dx <= q99 & dx >= q1],dy[dx <= q99 & dx >= q1]*0.001+5-k,col="gray")
    }
  }
  # title(paste0("Method (~",until,")"),outer=T)
  legend("topright", legend=c("80% CI", "95% CI", "Median"),
         lwd=c(1,3,NA), pch=c(NA,NA,20), col=rep("black",3),
         bty = "n")
  #}
  plot.new()
} # 8*12 inches


################################################################################
# 5. summary
################################################################################

# point forecast
poll_forecast_median <- data.frame(lee=rep(0,7),yoon=rep(0,7),sim=rep(0,7),ahn=rep(0,7))
for(i in 1:7) poll_forecast_median[i,] <- apply(ms_poll_pi[,i,103,],2,median)
poll_forecast_median <- rbind(poll_forecast_median, apply(national_poll_ms[,103,],2,median))
rownames(poll_forecast_median) <- c(names(cluster2),"NATIONAL")

# confidence interval length / 2
interval95 <- poll_forecast_median*0 
for(i in 1:7) interval95[i,] <- apply(ms_poll_pi[,i,103,],2,function(x) (quantile(x,0.975)-quantile(x,0.025))/2)
interval95[8,] <- apply(national_poll_ms[,103,],2,function(x) (quantile(x,0.975)-quantile(x,0.025))/2)

poll_forecast_median; pe20result
interval95

################################################################################
# 6. Chances for winning
################################################################################
win_prob_demo <- c(0.5661034,0.3316667,0.3988,0.2906154)
win_prob_rep <- 1 - win_prob_demo
xlabels <- c("Dec 29", "Jan 19", "Feb 09", "Mar 02")
plot(1:4, win_prob_demo, col=dblue, pch=19, 
     xaxt="n",ylim=c(0.2,0.8),yaxt="n",ylab="Chance of winning",xlab="Dates")
lines(1:4, win_prob_demo, col=dblue)
lines(1:4, win_prob_rep, col=rred)
points(1:4, win_prob_rep, pch=19, col=rred)
abline(h=.5,lty=2,col="gray")
axis(1, 1:4, xlabels)
axis(2, 2:8/10, las=1)


################################################################################
# ** For manuscript (SDG + GJJ)
################################################################################

{
  op <- par(mfcol = c(4,2),
            oma = c(5,4,2,0) + 0.1,
            mar = c(0,0,1,1) + 0.1)
  
  for(pp in 1:8){
    plot(0,0,xlim=c(1,103),ylim=c(0,1),pch=NA,
         xaxt="n",yaxt="n")
    title(paste0(ifelse(pp<=4,"SDG","GJJ")," (",
                 13-3*pp+ifelse(pp<=4,0,12),
                 " week",ifelse(pp==4 || pp==8,"","s")," left)"), line = -2)
    if(pp %in% c(4,8)) {
      axis(1, rev(seq(103,1,by=-7)), 14:0, cex.axis = .9)
    }else{
      axis(1, rev(seq(103,1,by=-7)), labels=F)
    }
    
    if(pp <= 4){
      axis(2, seq(0,1,by=0.2),seq(0,100,by=20), las=1)
    }else{
      axis(2, seq(0,1,by=0.2),labels=F)  
    }
    
    if(pp==1){
      mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
      mtext('Four-party vote share (%)', side = 2, line = 3, outer=T)
      legend(x=0,y=1.08, title="",
             legend=c("Lee","Yoon","Sim","Ahn"),
             lwd=5, col=unlist(party_colors),lty=1,pch=NA,
             bty = "n")
    }
    
    i = 2 # SDG
    if(pp >= 5) i = 7 # GJJ
    
    until <- rep(c("2021-12-29","2022-01-19","2022-02-09","2022-03-02"),2)[pp]
    poll_multiple_day_subset <- poll_multiple_day[poll_multiple_day$date <= until,]
    region_index = (poll$subregion == names(cluster2)[i])
    region_index2 = (poll_multiple_day_subset$subregion == names(cluster2)[i])
    for(k in 1:4){
      points(match(poll_multiple_day_subset$date,seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103))[region_index2],
             poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")][,k]/rowSums(poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")]), 
             col=party_colors10[[k]],
             pch=20,lwd=0,
             cex=sqrt(rowSums(poll_multiple_day_subset[region_index2,c("lee","yoon","sim","ahn")])/quantile(rowSums(poll_multiple_day_subset[,c("lee","yoon","sim","ahn")]),0.95))*2)
      
      polygon(c(1:103,103:1),c(eval(parse(text=paste0("pi_0025_",((pp-1) %% 4) + 1)))[[i]][,k],
                               eval(parse(text=paste0("pi_0975_",((pp-1) %% 4) + 1)))[[i]][,k][103:1]),
              col=party_colors50[[k]],border = NA)
    }
    for(k in 1:4){
      until.index <- which.max(until<=seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103))
      lines(1:until.index,eval(parse(text=paste0("pi_05_",((pp-1) %% 4) + 1)))[[i]][k,1:until.index],
            col=party_colors[[k]],lwd=2)
      lines(until.index:103,eval(parse(text=paste0("pi_05_",((pp-1) %% 4) + 1)))[[i]][k,until.index:103],
            col=party_colors[[k]],lwd=1.5,lty=3)
      points(103,eval(parse(text=paste0("pi_05_",((pp-1) %% 4) + 1)))[[i]][k,103],
            col=party_colors[[k]],pch=20)
    }
  }
  par(op)
} # 13*11 inches

{ # national
  op <- par(#mfcol = c(4,2),
            mfrow = c(2,2),
            # oma = c(5,4,2,0) + 0.1,
            # mar = c(0,1,1,2) + 0.1)
            oma = c(5,4,2,0) + 0.1,
            mar = c(0,0,1,1) + 0.1)
  
  for(large in 1:2){
    for(pp in 1:4){
      if(large==1){
        plot(0,0,xlim=c(1,103),ylim=c(0,0.6),pch=NA,
             xaxt="n",yaxt="n")
      }else{
        plot(0,0,xlim=c(1,103),ylim=c(.3,.6),pch=NA,
             xaxt="n",yaxt="n")
      }
      
      title(paste0("National Trend (",
                   13-3*pp,
                   " week",ifelse(pp==4,"","s")," left)"), line = -2)
      if(pp >= 3) {
        axis(1, rev(seq(103,1,by=-7)), 14:0, cex.axis = .9)
      }else{
        axis(1, rev(seq(103,1,by=-7)), labels=F)
      }
      
      if(large==1 && pp %in% c(1,3)){
        axis(2, seq(0,1,by=ifelse(large==1,0.1,0.05)),seq(0,100,by=ifelse(large==1,10,5)), las=1)
      }else{
        # axis(2, seq(0,1,by=ifelse(large==1,0.1,0.05)),seq(0,100,by=ifelse(large==1,10,5)), las=1)  
        axis(2, seq(0,1,by=ifelse(large==1,0.1,0.05)),labels=F, las=1)  
      }
      
      if(pp==1){
        mtext('Weeks prior to the election', side = 1, line = 3, outer=T)
        mtext('Four-party vote share (%)', side = 2, line = 3, outer=T)
        if(large==1){
          legend(x=0,y=0.4, title="",
                 legend=c("Lee","Yoon","Sim","Ahn"),
                 lwd=5, col=unlist(party_colors),lty=1,pch=NA,
                 bty = "n")
        }else{
          legend(x=0,y=.62, title="",
                 legend=c("Lee","Yoon"),
                 lwd=5, col=unlist(party_colors)[1:2],lty=1,pch=NA,
                 bty = "n")
        }
      }
      
      until <- c("2021-12-29","2022-01-19","2022-02-09","2022-03-02")[pp]
      index_national <- (national_poll$date <= until)
      for(k in 1:4){
        points(match(as.Date(national_poll$date[index_national]),seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103)),
               national_poll[index_national,c("lee","yoon","sim","ahn")][,k], 
               col=party_colors10[[k]],
               pch=20,lwd=0,
               cex=sqrt(national_poll$sample_size[index_national]/quantile(national_poll$sample_size[index_national],0.95))*2)
        
        polygon(c(1:103,103:1),c(eval(parse(text=paste0("nat_0025_",pp)))[,k],
                                 eval(parse(text=paste0("nat_0975_",pp)))[,k][103:1]),
                col=party_colors50[[k]],border = NA)
      }
      for(k in 1:4){
        until.index <- which.max(until<=seq(as.Date("2021-11-27"),as.Date("2022-3-9"),length.out=103))
        lines(1:until.index,eval(parse(text=paste0("nat_05_",pp)))[k,1:until.index],
              col=party_colors[[k]],lwd=2)
        lines(until.index:103,eval(parse(text=paste0("nat_05_",pp)))[k,until.index:103],
              col=party_colors[[k]],lwd=1.5,lty=3)
        points(103,eval(parse(text=paste0("nat_05_",pp)))[k,103],
               col=party_colors[[k]],pch=20)
      } 
      # for(k in 1:3) points(103,c(0.484249418,0.491658932,0.024091649)[k],pch=4,cex=2,col=party_colors[[k]])
      
    }
  }
  par(op)
} # 13*11 inches, for 2x2 plot--8*12

res_summary <- matrix(NA,ncol=12,nrow=8)
rownames(res_summary) <- c(names(cluster2),"national")
colnames(res_summary) <- rep(c("med","L","U"),4)
for(i in 1:7){
  res_summary[i,((0:3)*3+1)] <- pi_05_4[[i]][,103]
  res_summary[i,((0:3)*3+2)] <- pi_0025_4[[i]][103,]
  res_summary[i,((0:3)*3+3)] <- pi_0975_4[[i]][103,]
}

res_summary[8,((0:3)*3+1)] <- nat_05_4[,103]
res_summary[8,((0:3)*3+2)] <- nat_0025_4[103,]
res_summary[8,((0:3)*3+3)] <- nat_0975_4[103,]

res_summary



