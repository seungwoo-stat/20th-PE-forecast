## run `data_gen.R`, `design_mat_gen.R` and `poll_data_gen.R` file and save it to 
## `pe.RData` file

source("1a_data_generate/data_gen.R")
source("1a_data_generate/design_mat_gen.R")
source("1a_data_generate/poll_data_gen.R")

## save image file
rm(list=ls()[!(ls() %in% 
                 c("cluster", "pe", "pe_twoparty_vote_share", "pe20", "pe_sum",
                   "X","Z",
                   "poll","national_poll","cluster2","poll_multiple_day"))])

save.image(file="pe.RData")
