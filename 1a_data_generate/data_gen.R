### data_gen.R 
# read presidential election (pe) data set from github 
# this file should be conducted by `save_RData.R` file

rm(list=ls())

library(readxl)

pe20 <- read_excel("0_data/20pe.xlsx")[1:16,]

## download raw data from github
url <- "https://raw.githubusercontent.com/seungwoo-stat/south-korea-election/main/csv/"
pe19 <- read.csv(paste0(url,"19pe.csv"), check.names = FALSE)
pe18 <- read.csv(paste0(url,"18pe.csv"), check.names = FALSE)
pe17 <- read.csv(paste0(url,"17pe.csv"), check.names = FALSE)
pe16 <- read.csv(paste0(url,"16pe.csv"), check.names = FALSE)
pe15 <- read.csv(paste0(url,"15pe.csv"), check.names = FALSE)
pe14 <- read.csv(paste0(url,"14pe.csv"), check.names = FALSE)
pe <- list(pe14,pe15,pe16,pe17,pe18,pe19)
names(pe) <- paste0("pe",14:19)

## define cluster of regions
# Sejong-si -> Chungcheonnam-do
# Ulsan -> Gyeongsangnam-do
cluster <- list("Seoul" = "Seoul",
                "Gyeonggi-do" = "Gyeonggi-do",
                "Incheon" = "Incheon",
                "Daejeon"="Daejeon",
                "Chungcheongbuk-do"="Chungcheongbuk-do",
                "Chungcheongnam-do"=c("Sejong-si","Chungcheongnam-do"),
                "Gwangju"="Gwangju",
                "Jeollabuk-do"="Jeollabuk-do",
                "Jeollanam-do"="Jeollanam-do",
                "Daegu"="Daegu",
                "Gyeongsangbuk-do"="Gyeongsangbuk-do",
                "Busan"="Busan",
                "Gyeongsangnam-do"=c("Ulsan","Gyeongsangnam-do"),
                "Gangwon-do"="Gangwon-do",
                "Jeju-do"="Jeju-do")

## vote count by clusters
pe_sum <- vector("list", length=length(pe))
names(pe_sum) <- names(pe)
for(i in 1:length(pe)){
  pe_sum[[i]] <- matrix(nrow=length(cluster), ncol=ncol(pe[[i]])-2)
  for(j in seq_along(cluster)){
    pe_sum[[i]][j,] <- colSums(pe[[i]][pe[[i]]$primary_cluster %in% cluster[[j]],-(1:2)])
  }
  rownames(pe_sum[[i]]) <- names(cluster)
  colnames(pe_sum[[i]]) <- colnames(pe[[i]])[-(1:2)]
}

## two-party vote share of the democratic party
## Democratic no: 14(2) 15(2) 16(2) 17(1) 18(2) 19(1) 
democ_index <- c(2,2,2,1,2,1)
twoparty_vote_share <- \(mat,index) {
  if(is.vector(mat)) return(mat[index]/sum(mat[1:2]))
  mat[,index]/rowSums(mat[,1:2])
}
pe_twoparty_vote_share <- matrix(ncol=length(cluster)+1,nrow=length(pe))
for(i in seq_along(pe)){
  pe_twoparty_vote_share[i,1:length(cluster)] <- twoparty_vote_share(pe_sum[[i]],democ_index[i])
}
rownames(pe_twoparty_vote_share) <- names(pe)
colnames(pe_twoparty_vote_share) <- c(names(cluster),"national")

for(i in seq_along(pe)){
  pe_twoparty_vote_share[i,length(cluster)+1] <- 
    twoparty_vote_share(colSums(pe[[i]][,-(1:2)]),democ_index[i])
}
pe_twoparty_vote_share
