## merge and save poll data to `poll.csv` and `metadata_poll.txt`
## this file should be conducted by `save_RData.R` file

# rm(list=ls())

library(readxl)

## read meta data, remove inappropriate rows
poll_info <- read_excel("1a_data_generate/polls_final/여론조사결과.xlsx",skip=1)
poll_info$분석여부[is.na(poll_info$분석여부)] <- "YES"
poll_info <- poll_info[poll_info$분석여부 %in% c("YES","MERGE","CHECK"),]

# poll_info <- poll_info[poll_info$분석여부 %in% c("YES","MERGE","CHECK","MERGE2"),]
# poll_info[poll_info$분석여부 %in% c("CHECK","MERGE","MERGE2"),]$조사대상
# poll_info[poll_info$분석여부 %in% c("YES"),]$조사대상

## extract meta data
reg_num <- poll_info$등록번호
house <- poll_info$조사기관명
start_dates <- as.Date(poll_info$조사시작일)
end_dates <- as.Date(poll_info$조사종료일)
poll_unit <- poll_info$조사대상
method1 <- poll_info$조사방법1
method2 <- poll_info$조사방법2
method3 <- poll_info$조사방법3
method1_ratio <- poll_info$비율...10
method2_ratio <- poll_info$비율...12
method3_ratio <- poll_info$비율...14
checks <- poll_info$분석여부
surveyq <- poll_info$질문유형
resp_rate <- poll_info$응답률
url <- poll_info$링크

## read merged cells
merge_csv <- read.csv(paste0("1a_data_generate/polls_final/MERGE.csv"),
                      check.names=FALSE)
merge_csv <- merge_csv[merge_csv[,2]!="",] # remove rows with NA
colnames(merge_csv) <- c("num","dist",trimws(colnames(merge_csv))[-(1:2)])
merge_csv$num[is.na(merge_csv$num)] <- 0

## integrate poll csv files to one data.frame
poll_full <- matrix(nrow=1,ncol=21)

for(i in seq_along(reg_num)){
  if(checks[i] == "YES"){
    # read poll csv file and rename columns
    poll_one <- suppressWarnings(read.csv(paste0("1a_data_generate/polls_final/",reg_num[i],".csv"),
                                          check.names=FALSE))
    poll_one <- poll_one[poll_one[,1]!="",] # remove rows with NA
    colnames(poll_one) <- trimws(colnames(poll_one))
    
    #remove following rows:
    wrong_index <- poll_one[,1] %in% c("대전,세종,충남북,강원", "광주,전남북,제주")
    poll_one <- poll_one[!wrong_index,]
    
    # set region from metadata and subregion from csv file
    region=rep(poll_unit[i],nrow(poll_one))
    subregion=poll_one[,1]
  }else if(checks[i] == "CHECK"){
    poll_one <- suppressWarnings(read.csv(paste0("1a_data_generate/polls_final/",reg_num[i],".csv"),
                                          check.names=FALSE))
    poll_one <- poll_one[poll_one[,1]!="",] # remove rows with NA
    colnames(poll_one) <- trimws(colnames(poll_one))
    poll_one <- poll_one[poll_one[,1]=="전체",]
    region=rep(poll_unit[i],nrow(poll_one))
    subregion=poll_unit[i] # not to "전체"
  }else{ # checks[i] == "MERGE"
    poll_one <- merge_csv[merge_csv$num == reg_num[i],]
    region=poll_one$dist
    subregion=poll_one$dist
  }
  
  poll_one_mod <- data.frame(reg_num=reg_num[i],
                             house=house[i],
                             region=region,
                             subregion=subregion,
                             start=start_dates[i],
                             end=end_dates[i],
                             method1=method1[i],
                             method2=method2[i],
                             method3=method3[i],
                             method1_ratio=method1_ratio[i],
                             method2_ratio=method2_ratio[i],
                             method3_ratio=method3_ratio[i],
                             survey_q=surveyq[i],
                             resp_rate=resp_rate[i],
                             url=url[i],
                             sample_size=poll_one$조사완료,
                             sample_size_weight=poll_one$가중치적용,
                             lee=poll_one$이재명,
                             yoon=poll_one$윤석열,
                             sim=poll_one$심상정,
                             ahn=poll_one$안철수)
  poll_one_mod[,c("lee","yoon","sim","ahn")] <- 
    apply(poll_one_mod[,c("lee","yoon","sim","ahn")],2,\(x) as.numeric(sub("%","",x)))
  
  # combine gangwon and jeju
  if(all(c("강원","제주") %in% poll_one_mod$subregion)){
    index1 <- poll_one_mod$subregion=="강원"
    index2 <- poll_one_mod$subregion=="제주"
    n1 <- poll_one_mod$sample_size_weight[index1]
    n2 <- poll_one_mod$sample_size_weight[index2]
    gjj <- poll_one_mod[index1,c("lee","yoon","sim","ahn")]*n1/(n1+n2) + 
      poll_one_mod[index2,c("lee","yoon","sim","ahn")]*n2/(n1+n2)
    poll_one_mod$subregion[index1] <- "강원,제주"
    poll_one_mod$sample_size[index1] <- poll_one_mod$sample_size[index1] + 
      poll_one_mod$sample_size[index2]
    poll_one_mod$sample_size_weight[index1] <- poll_one_mod$sample_size[index1]
    poll_one_mod[index1,c("lee","yoon","sim","ahn")] <- gjj
    poll_one_mod <- poll_one_mod[!index2,]
  }
  
  # combine 광주,전남 and 전북
  if(all(c("전북","광주,전남") %in% poll_one_mod$subregion)){
    index1 <- poll_one_mod$subregion=="전북"
    index2 <- poll_one_mod$subregion=="광주,전남"
    n1 <- poll_one_mod$sample_size_weight[index1]
    n2 <- poll_one_mod$sample_size_weight[index2]
    hng <- poll_one_mod[index1,c("lee","yoon","sim","ahn")]*n1/(n1+n2) + 
      poll_one_mod[index2,c("lee","yoon","sim","ahn")]*n2/(n1+n2)
    poll_one_mod$subregion[index1] <- "광주,전남북"
    poll_one_mod$sample_size[index1] <- poll_one_mod$sample_size[index1] + 
      poll_one_mod$sample_size[index2]
    poll_one_mod$sample_size_weight[index1] <- poll_one_mod$sample_size[index1]
    poll_one_mod[index1,c("lee","yoon","sim","ahn")] <- hng
    poll_one_mod <- poll_one_mod[!index2,]
  } 
  
  if(i==1){
    poll_full = poll_one_mod
  }else{
    poll_full <- rbind(poll_full,poll_one_mod)
  }
  
  ## unify subregion names
  poll_full$subregion[poll_full$subregion %in% c("서울","서울특별시")] <- "SEO"
  poll_full$subregion[poll_full$subregion == "경기,인천"] <- "SDG"
  poll_full$subregion[poll_full$subregion == "대전,세종,충남북"] <- "CCG"
  poll_full$subregion[poll_full$subregion %in% c("광주,전남북","광주,전북,전남")] <- "HNG"
  poll_full$subregion[poll_full$subregion == "대구,경북"] <- "DGG"
  poll_full$subregion[poll_full$subregion == "부산,울산,경남"] <- "BUG"
  poll_full$subregion[poll_full$subregion == "강원,제주"] <- "GJJ"
}

## normalize four candidate's intentions to sum to 1

poll_full$etc <- 100-rowSums(poll_full[,c("lee","yoon","sim","ahn")])
poll_full$etc[poll_full$etc < 0] <- 0
poll_full[,c("lee","yoon","sim","ahn")] <- 
  t(apply(poll_full[,c("lee","yoon","sim","ahn")],1,\(x) x/sum(x))) 

## remove redundant columns
poll_full <- poll_full[,!names(poll_full) %in% c("region","sample_size_weight")]

## remove Koreans letters
for(met in paste0("method",1:3)){
  poll_full[,met] <- sub("무선", "cordless", poll_full[,met])
  poll_full[,met] <- sub("유선", "corded", poll_full[,met])
  poll_full[,met] <- sub("전화면접", " interview", poll_full[,met])
}

poll_full[,"survey_q"] <- sub("적합", "appropriate", poll_full[,"survey_q"])
poll_full[,"survey_q"] <- sub("지지", "support", poll_full[,"survey_q"])
poll_full[,"survey_q"] <- sub("투표", "vote", poll_full[,"survey_q"])


## METADATA on poll.csv
metadata_poll <- "
Among more than 400 polls conducted during the last 100 days of the presidential election,
we removed polls having small survey population. 
For example, survey #9099 only surveyed people living in Jeju-do, although we need samples from
both Jeju-do and Gangwon-do (GJJ cluster).

However, we merged some polls that were conducted by the same house, on the same date, with same method.
For example, if Busan, Ulsan, and Gyeongsangnam-do surveys were conducted separately on the
same date, method, and house, we merged them according to the population weights announed by the
Ministry of the Interior and Safety of Korea. Below, we provide survey numbers that we merged.

list of merged polls:
- 8578 and 8579
- 8694 and 8695
- 8765 and 8766
- 8866 and 8867
- 8999, 9001, and 9003

Note that response rates are not accurate for merged polls. 
(one can easily calculate accurate resp.rates, but we did not since we didn't use them for our analysis.)
"

## save files
poll <- poll_full
write.csv(poll_full, "0_data/poll.csv", row.names=FALSE)
fileConn <- file("0_data/metadata_poll.txt")
writeLines(metadata_poll, fileConn)
close(fileConn)

################################################################################
## National polls generate; only for plotting
poll_info <- poll_info[poll_info$분석여부 %in% c("YES"),]
national_poll <- matrix(ncol=5,nrow=nrow(poll_info))
colnames(national_poll) <- c("lee","yoon","sim","ahn","sample_size")
reg_num <- poll_info$등록번호

for(i in seq_along(reg_num)){
  # read poll csv file and rename columns
  poll_one <- suppressWarnings(read.csv(paste0("1a_data_generate/polls_final/",reg_num[i],".csv"),
                                        check.names=FALSE))
  poll_one <- poll_one[poll_one[,1]!="",] # remove rows with NA
  colnames(poll_one) <- trimws(colnames(poll_one))
  
  national_poll[i, ] <- c(sum(as.numeric(sub("%","",poll_one$이재명)) * poll_one$가중치적용) / sum(poll_one$가중치적용),
                          sum(as.numeric(sub("%","",poll_one$윤석열)) * poll_one$가중치적용) / sum(poll_one$가중치적용),
                          sum(as.numeric(sub("%","",poll_one$심상정)) * poll_one$가중치적용) / sum(poll_one$가중치적용),
                          sum(as.numeric(sub("%","",poll_one$안철수)) * poll_one$가중치적용) / sum(poll_one$가중치적용),
                          sum(poll_one$가중치적용))
}

# normalize
national_poll[,1:4] <- national_poll[,1:4]/rowSums(national_poll[,1:4])

national_poll <- data.frame(national_poll)
national_poll$start <- as.Date(poll_info$조사시작일)
national_poll$end <- as.Date(poll_info$조사종료일)
national_poll$days <- as.numeric(national_poll$end - national_poll$start + 1)

date_temp <- do.call("c", sapply(1:nrow(national_poll), \(i) with(national_poll,seq(start[i],end[i],length.out=days[i]))))
sample_size_temp <- do.call("c", sapply(1:nrow(national_poll), \(i) with(national_poll,rep(sample_size[i]/days[i],days[i]))))

national_poll <- national_poll[do.call("c", sapply(1:nrow(national_poll), \(i) rep(i,national_poll$days[i]))),]
national_poll$date <- date_temp
national_poll$sample_size <- sample_size_temp

# save
write.csv(national_poll[,-(6:8)], "0_data/national_poll.csv", row.names = FALSE)
national_poll <- national_poll[,-(6:8)]

################################################################################
cluster2 <- list("SEO" = "Seoul",
                 "SDG" = c("Gyeonggi-do","Incheon"),
                 "CCG" = c("Daejeon","Chungcheongbuk-do","Chungcheongnam-do"), # Sejong-si
                 "HNG" = c("Gwangju","Jeollabuk-do","Jeollanam-do"),
                 "DGG" = c("Daegu","Gyeongsangbuk-do"),
                 "BUG" = c("Busan","Gyeongsangnam-do"), # "Ulsan"
                 "GJJ" = c("Gangwon-do","Jeju-do") )

################################################################################
poll_max_method_index <- apply(poll[,c("method1_ratio","method2_ratio","method3_ratio")],1,which.max)
poll_method <- sapply(seq_along(poll_max_method_index),
                      \(x) poll[x,c("method1","method2","method3")[poll_max_method_index[x]]])

poll$survey_q |> table()

contingency_table <- data.frame(
  q=poll[sapply(1:325, \(i) which.min(poll$reg_num != unique(poll$reg_num)[i])),]$survey_q,
  m=poll_method[sapply(1:325, \(i) which.min(poll$reg_num != unique(poll$reg_num)[i]))]
)
table(contingency_table)
(poll$end-poll$start+1)[sapply(1:325, \(i) which.min(poll$reg_num != unique(poll$reg_num)[i]))] |> table()


poll_multiple_day <- poll[,c("subregion","start","end","survey_q",
                             "lee","yoon","sim","ahn")]
poll_multiple_day$method <- poll_method
poll_multiple_day$survey_q[poll_multiple_day$survey_q=="support/appropriate"] <- "support"

poll_multiple_day[,c("lee","yoon","sim","ahn")] <- round(poll[,c("lee","yoon","sim","ahn")] * poll$sample_size*(100-poll$etc)/100)
poll_multiple_day$days <- as.numeric(as.Date(poll_multiple_day$end) - as.Date(poll_multiple_day$start) + 1)
poll_n_four <- poll_multiple_day[,c("lee","yoon","sim","ahn","days")]

poll_multiple_day <- poll_multiple_day[unlist(sapply(seq_len(nrow(poll_multiple_day)), function(x) rep(x,each=poll_multiple_day$days[x]))),]
poll_multiple_day$date <-
  do.call("c",sapply(1:nrow(poll),function(x) with(poll,seq(as.Date(start[x]),as.Date(end[x]),length.out=as.numeric(as.Date(end[x])-as.Date(start[x])+1)))))

mod <- poll_n_four[,c("lee","yoon","sim","ahn")] %/% poll_n_four[,"days"] 
residue <- poll_n_four[,c("lee","yoon","sim","ahn")] %% poll_n_four[,"days"] 
mod_add <- mod[unlist(sapply(seq_len(nrow(poll_n_four)), function(x) rep(x,each=poll_n_four$days[x]))),]
residue_add <- mod_add * 0
residue_add[,1] <- unlist(sapply(1:nrow(residue),function(x) c(rep(1,residue[x,"lee"]),rep(0,poll_n_four[x,"days"]-residue[x,"lee"]))))
residue_add[,2] <- unlist(sapply(1:nrow(residue),function(x) c(rep(1,residue[x,"yoon"]),rep(0,poll_n_four[x,"days"]-residue[x,"yoon"]))))
residue_add[,3] <- unlist(sapply(1:nrow(residue),function(x) c(rep(1,residue[x,"sim"]),rep(0,poll_n_four[x,"days"]-residue[x,"sim"]))))
residue_add[,4] <- unlist(sapply(1:nrow(residue),function(x) c(rep(1,residue[x,"ahn"]),rep(0,poll_n_four[x,"days"]-residue[x,"ahn"]))))

poll_multiple_day[,c("lee","yoon","sim","ahn")] <- residue_add + mod_add

poll_multiple_day <- poll_multiple_day[,c("subregion","survey_q","method","date",
                                          "lee","yoon","sim","ahn")]

head(poll_multiple_day)
