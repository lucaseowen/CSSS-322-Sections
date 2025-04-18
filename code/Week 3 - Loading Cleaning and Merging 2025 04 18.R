#### PREAMBLE ####
library(dplyr)
library(tidyr)
library(ggplot2)

#### LOAD DATA ####

gpp <- read.csv("data/322/gpp 2025 04 16.csv")

lit <- read.csv("data/322/lit 2025 04 16.csv")

#### CLEAN DATA ####

#expand litigation so that each row is just a single patent-court case
lit_expanded <- lit %>%
  separate_rows(patents, sep = "\\|")

#count number of cases per patent
lit_sum <- lit_expanded %>% 
  group_by(patents) %>% 
  summarize(count = n())

#check distribution of court cases
lit_sum <- lit_sum[lit_sum$patents!="",]
ggplot(data=lit_sum, aes(x=count))+
  geom_histogram()

#create binary variable for if patent is standard essential or not
unique(gpp$X5g_v2a_ft_c.pools)
class(gpp$X5g_v2a_ft_c.pools)

gpp$SEP <- ifelse(gpp$X5g_v2a_ft_c.pools="", 0, 1)


gpp$X5g_v2a_ft_c.pools[gpp$X5g_v2a_ft_c.pools==""] <- NA_character_
gpp$SEP <- ifelse(is.na(gpp$X5g_v2a_ft_c.pools), 0, 1)


#### JOIN DATA ####

gpp_joined <- left_join(gpp, lit_sum, by=c("grant_number"="patents"))
colnames(gpp_joined)[colnames(gpp_joined)=="count"] <- "court_cases"

#clean up environment
rm(gpp, lit, lit_sum, lit_expanded)


gpp_final <- gpp_joined[,c("grant_number", "SEP", "court_cases")]

gpp_final$court_cases[is.na(gpp_final$court_cases)] <- 0

write.csv(gpp_final, "data/322/patent_litigation_analysis_data 2025 04 18.csv", row.names = F)
