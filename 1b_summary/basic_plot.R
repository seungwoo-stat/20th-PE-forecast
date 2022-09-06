# basic plots for exploratory data analysis

rm(list=ls())
load("pe.RData")

## plot: two-party democratic vote share

# par(xpd = TRUE, mar = c(0,0,1,1) + 0.1)
cols <- rainbow(length(cluster)+5)[rank(pe_twoparty_vote_share[6,-(length(cluster)+1)])]
{
  oldpar <- par(xpd = TRUE, mar = par()$mar + c(0,0,0,12),mfrow=c(1,1))
  plot(1:6, pe_twoparty_vote_share[,length(cluster)+1],type="l",col="gray",lwd=10,
       main="Two-party vote share of the Democratic Party",
       ylab="Vote share",ylim=c(0,1),yaxt="n",
       xlab="",xaxt = "n")
  matlines(1:6,pe_twoparty_vote_share[,1:length(cluster)],col=cols,lty=1,lwd=2)
  matpoints(1:6,pe_twoparty_vote_share[,1:length(cluster)],col=cols,pch=20)
  axis(1, at = 1:6, labels = rep("", 6))
  axis(1, at = 1:6, line = 1, lwd = NA,
       labels=paste0(" \n",14:19,"th PE\n(",seq(1992,2017,5),")"))
  
  # axis(1, at=1:6, labels=paste0(" \n",14:19,"th PE\n(",seq(1992,2017,5),")"))
  axis(2, at=pretty((1:10)/10), labels=paste0(pretty((1:10)/10)*100,"%"),las=TRUE)
  legend(6.3,0.83,
         c("National",names(sort(-pe_twoparty_vote_share[6,-(length(cluster)+1)]))),
         lty=1,col=c("gray",rainbow(length(cluster)+5)[length(cluster):1]),
         lwd=c(10,rep(2,length(cluster))),bty="n")
  # text(6,pe_twoparty_vote_share[1,],
  #      labels=colnames(pe_twoparty_vote_share), col=c(cols,"gray"),
  #      pos=4)
  par(oldpar) # default value
  abline(h=.5, lty=2, lwd=2, col="gray")
} # 8*15 size


## include 20th ##
# cols <- rainbow(length(cluster)+5)[rank(pe_twoparty_vote_share[6,-(length(cluster)+1)])]
{
  oldpar <- par(xpd = TRUE, mar = par()$mar + c(0,0,0,10))
  plot(1:7, rbind(pe_twoparty_vote_share,pe20$dem_vote_share)[,length(cluster)+1],
       type="l",col="gray",lwd=10,
       main="Two-party vote share of the democratic party",
       ylab="Vote percentage",ylim=c(0,1),yaxt="n",
       xlab="",xaxt = "n")
  matlines(1:7,rbind(pe_twoparty_vote_share,pe20$dem_vote_share)[,1:length(cluster)],col=cols,lty=1,lwd=2)
  matpoints(1:7,rbind(pe_twoparty_vote_share,pe20$dem_vote_share)[,1:length(cluster)],col=cols,pch=20)
  axis(1, at=1:7, labels=paste0(14:20,"th PE"))
  axis(2, at=pretty((1:10)/10), labels=paste0(pretty((1:10)/10)*100,"%"),las=TRUE)
  legend(7.3,0.83,
         c("National",names(sort(-pe_twoparty_vote_share[6,-(length(cluster)+1)]))),
         lty=1,col=c("gray",rainbow(length(cluster)+5)[length(cluster):1]),
         lwd=c(10,rep(2,length(cluster))),bty="n")
  # text(6,pe_twoparty_vote_share[1,],
  #      labels=colnames(pe_twoparty_vote_share), col=c(cols,"gray"),
  #      pos=4)
  par(oldpar) # default value
  abline(h=.5, lty=2, lwd=2, col="gray")
}



