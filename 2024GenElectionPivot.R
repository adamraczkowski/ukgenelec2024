library(tidyverse)
library(readr)

#Load data
electdata_2024_2 <- read_csv("Training and Development/UK Gen/electdata_2024.csv")

#calculate total vote for each consitutency for 2024 election
electdata_2024_turnout_2 <- electdata_2024_2 %>%
  mutate(TotalVote = CON + LAB + LIB + Reform + Green + NAT + MIN + OTH)

#calculate turnout for each constituency for 2024 election
electdata_2024_turnout_2 <- electdata_2024_turnout_2 %>%
  mutate(Turnout = TotalVote / Electorate)

#Put SNP, Plaid Cymru, and Irish nationalits into separate columns to allow for adjustment based on YouGov survey results (which track the two nationalist parties separately)
electdata_2024_turnout_2 <- electdata_2024_turnout_2 %>%
  mutate(SNP = case_when(Area == 2 ~ NAT, TRUE ~ 0),
         PC = case_when(Area == 6 ~ NAT, TRUE ~ 0),
         UUP = case_when(Area == 1 ~ CON, TRUE ~ 0),
         SDLP = case_when(Area == 1 ~ LAB, TRUE ~ 0),
         DUP = case_when(Area == 1 ~ LIB, TRUE ~ 0),
         Alliance = case_when(Area == 1 ~ Reform, TRUE ~ 0),
         SF = case_when(Area == 1 ~ NAT, TRUE ~ 0),
         CON = case_when(Area == 1 ~ 0, TRUE ~ CON),
         LAB = case_when(Area == 1 ~ 0, TRUE ~ LAB),
         LIB = case_when(Area == 1 ~ 0, TRUE ~ LIB),
         Reform = case_when(Area == 1 ~ 0, TRUE ~ Reform),
         NAT = case_when(Area == 1 ~ 0, TRUE ~ NAT))

#calculate the adjusted party votes using the proportion of voters who told YouGov's poll of 42,119 GB Adults conducted 7/5-8/2024 that they voted for one party in 2019 and Reform in 2024
#Assign remainder of Reform UK votes to Other
#Adjusted Party Vote = (((Reform UK Vote in 2024 x % of Reform UK Voters Who Voted for Party in 2019) + 2024 Party Votes
electdata_2024_adjusted_2 <- electdata_2024_turnout_2 %>%
  mutate(CONadj = ((electdata_2024_turnout_2$Reform * .788) + electdata_2024_turnout_2$CON),
         LABadj = ((electdata_2024_turnout_2$Reform * .069) + electdata_2024_turnout_2$LAB),
         LIBadj = ((electdata_2024_turnout_2$Reform * .016) + electdata_2024_turnout_2$LIB),
         Greenadj = ((electdata_2024_turnout_2$Reform * .014) + electdata_2024_turnout_2$Green),
         PCadj = ((electdata_2024_turnout_2$Reform * .002) + electdata_2024_turnout_2$PC),
         SNPadj = ((electdata_2024_turnout_2$Reform * .008) + electdata_2024_turnout_2$SNP),
         OTHadj = ((electdata_2024_turnout_2$Reform * .101) + electdata_2024_turnout_2$OTH))

#round CONadj to nearest integer
electdata_2024_adjusted_2 <- electdata_2024_adjusted_2 %>%
  mutate(CONadj = round(CONadj, 0),
         LABadj = round(LABadj, 0),
         LIBadj = round(LIBadj, 0),
         Greenadj = round(Greenadj, 0),
         PCadj = round(PCadj, 0),
         SNPadj = round(SNPadj, 0),
         OTHadj = round(OTHadj, 0))

#create new data frame with adjusted votes
electdata_2024_NEW_2 <- electdata_2024_adjusted_2 %>%
  select(Name, MP, Area, County, Electorate, CONadj, LABadj, LIBadj, Greenadj, SNPadj, PCadj, UUP, SDLP, DUP, Alliance, SF, MIN, OTHadj, TotalVote, Turnout)

#identify the winning party in each constituency with the adjusted votes
electdata_2024_NEW_2 <- electdata_2024_NEW_2 %>%
  mutate(winner = pmap_chr(select(., c("CONadj", "LABadj", "LIBadj", "Greenadj", "SNPadj", "PCadj", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTHadj")), function(...) {
    row_values <- c(...)
    selected_columns <- c("CONadj", "LABadj", "LIBadj", "Greenadj", "SNPadj", "PCadj", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTHadj")
    selected_columns[which.max(row_values)]
  }))

#identify the actual winning party in each constituency
electdata_2024_turnout_2 <- electdata_2024_turnout_2 %>%
  mutate(winner = pmap_chr(select(., c("CON", "LAB", "LIB", "Green", "Reform", "SNP", "PC", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTH")), function(...) {
    row_values <- c(...)
    selected_columns <- c("CON", "LAB", "LIB", "Green", "Reform", "SNP", "PC", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTH")
    selected_columns[which.max(row_values)]
  }))

#Display actual results of 2024 election
table(electdata_2024_turnout_2$winner)

#Display adjusted results of 2024 election
table(electdata_2024_NEW_2$winner)
