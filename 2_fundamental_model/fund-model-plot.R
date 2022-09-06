## plots from the fundamentals model

rm(list=ls())
load("pe.RData")
load("samples-fundamental.RData")

library(magrittr)
library(latex2exp)

P = length(cluster)

{## normal votes
  par(mfrow=c(1,2))
  highlight <- which(sapply(1:P, \(p){
    temp <- apply(ms_fund_alpha[,p,1:5],2,median)
    abs(temp[5]-temp[1])
  }) > 0.1)

  plot(15:20, apply(ms_fund_alpha[,1,1:6],2,median), type="l",
       ylim=c(0,1),ylab=TeX('Normal vote ($\\alpha_{pt}$)'),
       xlim=c(15,20.8),xlab="",xaxt = "n",
       main="Normal vote of the Democratic Party",
       col=ifelse(1 %in% highlight,"black","gray"),
       lwd=ifelse(1 %in% highlight,2,1),
       bty="L")
  for(p in 2:P){
    lines(15:20, apply(ms_fund_alpha[,p,1:6],2,median), 
          col=ifelse(p %in% highlight,"black","gray"),
          lwd=ifelse(p %in% highlight,2,1))
  }
  axis(1, at=15:20, labels=paste0(15:20,"th PE"))
  text(20,apply(ms_fund_alpha[,,6],2,median),names(cluster), pos=4, 
       col=ifelse(1:P %in% highlight, "black", "gray"),xpd=NA)
  abline(h=.5,lty=2,lwd=2, col="black")
} #8*15

{
  CIs <- c(.95, .8)
  n_x = ncol(X)
  n_z = ncol(Z)
  
  coeff_name = c("is_current","gdp4","gdp5","gni4","gni5","twice1","twice2",
                 "approval","net_approval","D_primary","P_primary","impeach",
                 "cpi4","cpi5","unemp4","unemp5","home","homeground")
  coeff_order = sapply(seq_along(coeff_name), \(i) which(c(colnames(X),colnames(Z))[i]==coeff_name) )
  
  
  coeffs_q <- apply(cbind(ms_fund_beta,ms_fund_gamma)[,coeff_order],2,
                    \(x) quantile(x,c((1-CIs[1])/2,
                                      (1-CIs[2])/2,
                                      0.5,
                                      1-(1-CIs[2])/2,
                                      1-(1-CIs[1])/2)))
  colnames(coeffs_q) <- coeff_name
  oldpar <- par(mar = par()$mar + c(0,6,0,0))
  range = c(min(coeffs_q), max(coeffs_q))
  plot(coeffs_q[3,],c((n_x+n_z+2):(n_z+3),n_z:1), pch=20,
       yaxt="n",ylab="",
       xlab="Estimation of the coefficients",
       xlim=c(-max(abs(range)),max(abs(range))),
       ylim=c(1,n_x+n_z+3),
       main="Posterior estimation of coefficients")
  abline(v=0,lty=2,lwd=2,col="gray")
  abline(h=c(1:n_z,(n_z+3):(n_x+n_z+2)),lty=2,lwd=1,col="lightgray")
  for(i in 1:n_x){
    lines(coeffs_q[c(1,5),i],n_x+n_z+3-c(i,i))
    lines(coeffs_q[c(2,4),i],n_x+n_z+3-c(i,i),lwd=3)
    points(coeffs_q[3,i],n_x+n_z+3-i,pch=20,cex=1.2)
  }
  for(i in 1:n_z){
    lines(coeffs_q[c(1,5),n_x+i],n_z+1-c(i,i))
    lines(coeffs_q[c(2,4),n_x+i],n_z+1-c(i,i),lwd=3)
    points(coeffs_q[3,n_x+i],n_z+1-i,pch=20,cex=1.2)
  }
  axis(2, at=c((n_x+n_z+2):(n_z+3),n_z:1),
       labels=colnames(coeffs_q),rep,las=1,cex.axis=0.9,
       font=1)
  axis(2, at=c(n_z+1,n_z+n_x+3),
       label=c("Provincial covariates","National covariates"),
       font=2,las=2,lwd=NA,cex.axis=0.9)
  
  legend("topright", legend=c(paste0(CIs*100,"% CI"), "Median"), 
         lwd=c(1,3,NA), pch=c(NA,NA,20), col=rep("black",3),
         bty = "n")
  par(oldpar)
} #8*15


## prediction plot (with two credible interval)
## use vote count from the 19th pe to project 20th pe
{ ## cluster
  oldpar <- par(oma = c(5,4,3,1),
                mar = c(0,5.5,0,0))
  
  CIs <- c(.95, .8)
  y20_prediction <- apply(ms_fund_y_pred,2,
                          \(x) quantile(x,c((1-CIs[1])/2,
                                            (1-CIs[2])/2,
                                            0.5,
                                            1-(1-CIs[2])/2,
                                            1-(1-CIs[1])/2)))
  y20_rank <- order(y20_prediction[3,])
  
  pe19_vote_count <- rowSums(pe_sum$pe19[,1:(ncol(pe_sum$pe19)-2)])
  y20_national_prediction <- colSums(t(ms_fund_y_pred) * pe19_vote_count/sum(pe19_vote_count))
  y20_national_q <- quantile(y20_national_prediction, c((1-CIs[1])/2,
                                                        (1-CIs[2])/2,
                                                        0.5,
                                                        1-(1-CIs[2])/2,
                                                        1-(1-CIs[1])/2))
  
  

  par(mfrow=c(1,2))

  plot(y20_prediction[3,y20_rank],1:15, pch=20,cex=2,
       yaxt="n",ylab="",
       xlab="",
       xlim=c(0,1))
  mtext('Two-party vote share of the Democratic Party', side = 1, line = 3, outer=T)
  title("The 20th PE forecasts from the fundamentals-based model",outer=T)
  axis(2, at=1:15,labels=names(cluster)[y20_rank],las=1)
  # axis(1, at=seq(0,1,by=.2),labels=NA)
  abline(v=.5,lty=2,lwd=2,col="gray")
  abline(h=1:P,lty=2,lwd=1,col="lightgray")
  for(p in 1:P){
    lines(y20_prediction[c(1,5),y20_rank[p]],c(p,p))
    lines(y20_prediction[c(2,4),y20_rank[p]],c(p,p),lwd=3)
  }
  
  pe20result <- read.csv("0_data/20pe_cluster2.csv",header = T)

  CIs <- c(.95, .8)
  y20_prediction <- apply(ms_fund_y_pred_cluster,2,
                          function(x) quantile(x,c((1-CIs[1])/2,
                                                   (1-CIs[2])/2,
                                                   0.5,
                                                   1-(1-CIs[2])/2,
                                                   1-(1-CIs[1])/2)))
  y20_rank <- order(y20_prediction[3,])
  
  pe19_vote_count <- 0
  
  rowSums(pe_sum$pe19[,1:(ncol(pe_sum$pe19)-2)]) %>% 
    stack() -> pe19count_temp
  pe19count_temp_ind <- vector(length=nrow(pe19count_temp))
  for(i in 1:nrow(pe19count_temp)){
    pe19count_temp_ind[i] <- names(cluster2)[which(sapply(cluster2, FUN=function(X) pe19count_temp$ind[i] %in% X))]
  }
  pe19_vote_count <- tapply(pe19count_temp$values,pe19count_temp_ind,sum)
  pe19_vote_count <- as.vector(pe19_vote_count[names(cluster2)])
  y20_national_prediction <- colSums(t(ms_fund_y_pred_cluster) * pe19_vote_count/sum(pe19_vote_count))
  y20_national_q <- quantile(y20_national_prediction, c((1-CIs[1])/2,
                                                        (1-CIs[2])/2,
                                                        0.5,
                                                        1-(1-CIs[2])/2,
                                                        1-(1-CIs[1])/2))
  
  plot(c(y20_national_q[3],y20_prediction[3,y20_rank]),0:7, pch=20,cex=2,
       yaxt="n",ylab="",
       xlab="Two-party vote share of the Democratic party",
       xlim=c(0,1),ylim=c(-0.2,8))
  axis(2, at=0:7,labels=c("National",names(cluster2)[y20_rank]),las=1)
  abline(v=.5,lty=2,lwd=2,col="gray")
  abline(h=0:7,lty=2,lwd=1,col="lightgray")
  for(p in 1:7){
    lines(y20_prediction[c(1,5),y20_rank[p]],c(p,p))
    lines(y20_prediction[c(2,4),y20_rank[p]],c(p,p),lwd=3)
    q99 <- quantile(ms_fund_y_pred_cluster[,y20_rank[p]],0.99)
    q1 <- quantile(ms_fund_y_pred_cluster[,y20_rank[p]],0.01)
    dx <- (density(ms_fund_y_pred_cluster[,y20_rank[p]]))$x
    dy <- (density(ms_fund_y_pred_cluster[,y20_rank[p]]))$y
    lines(dx[dx <= q99 & dx >= q1],dy[dx <= q99 & dx >= q1]*0.07+p,col="gray")
  }
  
  lines(y20_national_q[c(1,5)],c(0,0))
  lines(y20_national_q[c(2,4)],c(0,0),lwd=3)
  q99 <- quantile(y20_national_prediction,0.99)
  q1 <- quantile(y20_national_prediction,0.01)
  dx <- (density(y20_national_prediction))$x
  dy <- (density(y20_national_prediction))$y
  lines(dx[dx <= q99 & dx >= q1],dy[dx <= q99 & dx >= q1]*0.07,col="darkgray")

  legend("bottomright", legend=c(paste0(CIs*100,"% CI"), "Median"), 
         lwd=c(1,3,NA), pch=c(NA,NA,20),
         bty = "n")
  
  par(oldpar)
} # 14*13 // 8*15 (small)
