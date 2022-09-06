## design matrix generation - for fundamentals model
## X: national-level predictors
## Z: province-level predictors
## this file should be conducted by `save_RData.R` file

# rm(list=ls())
# load("pe.RData")

library(readxl)

P <- length(cluster)  # num. of provinces
TT <- length(15:20)   # num. of elections

##============================================================================##
## X: national-level predictors (ncol=length(15:20)*P=90)
##============================================================================##

# is current president democ? from 14th to 19th
is_current <- c(-1,1,1,-1,-1,1) 
# is the incumbent been in the blue house for twice?
twice1 <- c(1,0,1,0,1,0)*is_current # yes/no
twice2 <- c(2,1,2,1,2,1)*is_current # how many times
# approval rate of the current president at the second quarter of 5th yr
# from Gallup Korea
# (4th quarter of the 4th year for Park Geun-hye, who got impeached)
approval <- c(7,26,24,25,12,39)*is_current            # positive
net_approval <- c(-67,-27,-42,-33,-68,-14)*is_current # positive-negative
# national poll for the Democrats 100 days before the election 
# (60 days for the 19th PE)
# WE DON'T USE THIS VARIABLE
# poll <- c(29.9/(29.9+18.3),20.4/(20.4+30.2),7.2/(7.2+60.7),
#           15/(15+40),32/(32+1),36/(36+36))
# real GDP and GNI growth of the 4th and 5th year from KOSIS
# https://kosis.kr/statHtml/statHtml.do?orgId=301&tblId=DT_200Y006&conn_path=I2
gdp4 <- c(7.9,4.9,5.3,3.7,2.8,-0.9)*is_current
gdp5 <- c(6.2,7.7,5.8,2.4,2.9, 4.0)*is_current
gni4 <- c(6.7,4.0,4.0,1.6,6.3,-0.2)*is_current
gni5 <- c(4.0,8.6,5.7,2.9,4.4, 3.5)*is_current
# primary result
D_primary <- c(0.78,0.72,0.56,0.72,0.73,0.56)
P_primary <- c(0.60,0.79,0.51,0.91,0.74,0.54)
# impeachment by the national congress?
impeach <- c(0,0,1,0,1,0)*is_current

# make matrix X
X_temp <- cbind(is_current,twice1,twice2,approval,net_approval,#poll,
                gdp4,gdp5,gni4,gni5,D_primary,P_primary,impeach)
X <- matrix(rep(X_temp,each=P),nrow=P*length(15:20))
colnames(X) <- colnames(X_temp)
X

##============================================================================##
## Z: province-level predictors (ncol=length(15:20)*P=90)
##============================================================================##

Z_penum <- rep(15:20,each=P)
Z_cluster <- rep(names(cluster),TT)
Z_penum
Z_cluster

# home province of each candidate (adjusting for lags)
home <- rep(0,P*TT)
home[Z_penum == 15 & Z_cluster=="Gyeongsangnam-do"] = 1 
home[Z_penum == 15 & Z_cluster=="Chungcheongnam-do"] = -1
home[Z_penum == 16 & Z_cluster=="Gyeongsangnam-do"] = 1 
home[Z_penum == 16 & Z_cluster=="Jeollanam-do"] = -1
home[Z_penum == 17 & Z_cluster=="Chungcheongnam-do"] = 1 
home[Z_penum == 17 & Z_cluster=="Gyeongsangnam-do"] = -1
home[Z_penum == 17 & Z_cluster=="Jeollabuk-do"] = 1 
home[Z_penum == 17 & Z_cluster=="Gyeongsangbuk-do"] = -1
home[Z_penum == 18 & Z_cluster=="Gyeongsangbuk-do"] = 1 
home[Z_penum == 18 & Z_cluster=="Jeollabuk-do"] = -1
home[Z_penum == 18 & Z_cluster=="Gyeongsangnam-do"] = 1 
home[Z_penum == 18 & Z_cluster=="Daegu"] = -1
home[Z_penum == 19 & Z_cluster=="Daegu"] = 1 
home[Z_penum == 19 & Z_cluster=="Gyeongsangnam-do"] = -1
home[Z_penum == 20 & Z_cluster=="Gyeongsangbuk-do"] = 1 
home[Z_penum == 20 & Z_cluster=="Seoul"] = -1
# political homeground for each candidate; governor (adjusting lags)
homeground <- rep(0,P*TT)
homeground[Z_penum == 17 & Z_cluster=="Seoul"] = -1
homeground[Z_penum == 18 & Z_cluster=="Seoul"] = 1
homeground[Z_penum == 19 & Z_cluster=="Gyeongsangnam-do"] = -1
homeground[Z_penum == 20 & Z_cluster=="Gyeongsangnam-do"] = 1
homeground[Z_penum == 20 & Z_cluster=="Gyeonggi-do"] = 1 
# CPI growth
cpi <- read.csv("0_data/csi.csv")
all(cpi$pe_num == Z_penum)
all(cpi$cluster == Z_cluster)
cpi4 <- cpi$csi_increase_4 * is_current
cpi5 <- cpi$csi_increase_5 * is_current
# unemployment rate
## 임기 4,5년차 실업률
## 구직기간 1주 기준으로 작성된 자료는 2014년까지 제공되고,
## 구직기간 4주 기준으로 작성된 자료는 99~부터 제공되어 
## 두 자료의 분기별 실업률을 단순선형회귀로 보정하였음.
## 이후 분기별 값을 단순 평균내어 시도별 4년차 z, 5년차 z 생성.
## 참고로 17년 선거부터는 15~16년으로 두 해 전 자료 사용하고, 
## 그 전 선거까지는 (예. 12년) 11~12년 선거 년도 포함한 두 해 자료 사용.
unemp45 <- data.frame(read_excel("0_data/unemployment.xlsx"))
all(unemp45$pe_num == Z_penum)
all(unemp45$cluster == Z_cluster)
unemp4 <- unemp45$unemp4 * is_current
unemp5 <- unemp45$unemp5 * is_current

# make matrix Z
Z <- cbind(home,homeground,cpi4,cpi5,unemp4,unemp5)
Z


