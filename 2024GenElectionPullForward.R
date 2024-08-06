library(tidyverse)
library(readr)

#Load data
electdata_2024 <- read_csv("Training and Development/UK Gen/electdata_2024.csv")
electdata_2019implied <- read_csv("Training and Development/UK Gen/electdata_2019implied.csv")

#calculate total vote for each consitutency for 2024 election
electdata_2024_turnout <- electdata_2024 %>%
  mutate(TotalVote = CON + LAB + LIB + Reform + Green + NAT + MIN + OTH)

#calculate turnout for each constituency for 2024 election
electdata_2024_turnout <- electdata_2024_turnout %>%
  mutate(Turnout = TotalVote / Electorate)

#calculate total vote for each consitutency for 2019 election
electdata_2019_turnout <- electdata_2019implied %>%
  mutate(TotalVote = CON + LAB + LIB + Brexit + Green + NAT + MIN + OTH)

#calculate turnout for each constituency for 2019 election
electdata_2019_turnout <- electdata_2019_turnout %>%
  mutate(Turnout = TotalVote / Electorate)

#calculate turnout adjustment factor between 2024 and 2019 election and add to 2019 election data frame
electdata_2019_turnout_adjust <- electdata_2019_turnout %>%
  mutate(TurnAdjFactor = electdata_2024_turnout$TotalVote / electdata_2019_turnout$TotalVote)

#Put SNP, Plaid Cymru, and Irish nationalits into separate columns to allow for adjustment based on YouGov survey results (which track the two nationalist parties separately)
electdata_2019_turnout_adjust <- electdata_2019_turnout_adjust %>%
  mutate(SNP = case_when(Area == 2 ~ NAT, TRUE ~ 0),
         PC = case_when(Area == 6 ~ NAT, TRUE ~ 0),
         UUP = case_when(Area == 1 ~ CON, TRUE ~ 0),
         SDLP = case_when(Area == 1 ~ LAB, TRUE ~ 0),
         DUP = case_when(Area == 1 ~ LIB, TRUE ~ 0),
         Alliance = case_when(Area == 1 ~ Brexit, TRUE ~ 0),
         SF = case_when(Area == 1 ~ NAT, TRUE ~ 0),
         CON = case_when(Area == 1 ~ 0, TRUE ~ CON),
         LAB = case_when(Area == 1 ~ 0, TRUE ~ LAB),
         LIB = case_when(Area == 1 ~ 0, TRUE ~ LIB),
         Brexit = case_when(Area == 1 ~ 0, TRUE ~ Brexit),
         NAT = case_when(Area == 1 ~ 0, Area == 2 ~ 0, Area == 6 ~ 0, TRUE ~ NAT))

electdata_2024_turnout <- electdata_2024_turnout %>%
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
#Adjusted Conservative vote = (((Party Votes in 2019 x Turnout Adjustment Factor) x % of 2019 Party Voters Who Voted Reform in 2024) + 2024 Party Votes
electdata_2024_adjusted <- electdata_2024_turnout %>%
  mutate(CONadj = (((electdata_2019_turnout_adjust$CON * electdata_2019_turnout_adjust$TurnAdjFactor) * .25) + electdata_2024_turnout$CON),
  LABadj = (((electdata_2019_turnout_adjust$LAB * electdata_2019_turnout_adjust$TurnAdjFactor) * .03) + electdata_2024_turnout$LAB),
  LIBadj = (((electdata_2019_turnout_adjust$LIB * electdata_2019_turnout_adjust$TurnAdjFactor) * .02) + electdata_2024_turnout$LIB),
  Greenadj = (((electdata_2019_turnout_adjust$Green * electdata_2019_turnout_adjust$TurnAdjFactor) * .07) + electdata_2024_turnout$Green),
  PCadj = (((electdata_2019_turnout_adjust$PC * electdata_2019_turnout_adjust$TurnAdjFactor) * .06) + electdata_2024_turnout$PC),
  SNPadj = (((electdata_2019_turnout_adjust$SNP * electdata_2019_turnout_adjust$TurnAdjFactor) * .03) + electdata_2024_turnout$SNP),
  OTHadj = (((electdata_2019_turnout_adjust$Brexit * electdata_2019_turnout_adjust$TurnAdjFactor) * .69) + electdata_2024_turnout$OTH))

#round CONadj to nearest integer
electdata_2024_adjusted <- electdata_2024_adjusted %>%
  mutate(CONadj = round(CONadj, 0),
         LABadj = round(LABadj, 0),
         LIBadj = round(LIBadj, 0),
         Greenadj = round(Greenadj, 0),
         PCadj = round(PCadj, 0),
         SNPadj = round(SNPadj, 0),
         OTHadj = round(OTHadj, 0))

#create new data frame with adjusted votes
electdata_2024_NEW <- electdata_2024_adjusted %>%
  select(Name, MP, Area, County, Electorate, CONadj, LABadj, LIBadj, Greenadj, SNPadj, PCadj, UUP, SDLP, DUP, Alliance, SF, MIN, OTHadj, TotalVote, Turnout)

#identify the winning party in each constituency with the adjusted votes
electdata_2024_NEW <- electdata_2024_NEW %>%
  mutate(winner = pmap_chr(select(., c("CONadj", "LABadj", "LIBadj", "Greenadj", "SNPadj", "PCadj", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTHadj")), function(...) {
    row_values <- c(...)
    selected_columns <- c("CONadj", "LABadj", "LIBadj", "Greenadj", "SNPadj", "PCadj", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTHadj")
    selected_columns[which.max(row_values)]
  }))

#identify the actual winning party in each constituency
electdata_2024_turnout <- electdata_2024_turnout %>%
  mutate(winner = pmap_chr(select(., c("CON", "LAB", "LIB", "Green", "Reform", "SNP", "PC", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTH")), function(...) {
    row_values <- c(...)
    selected_columns <- c("CON", "LAB", "LIB", "Green", "Reform", "SNP", "PC", "UUP", "SDLP", "DUP", "Alliance", "SF", "MIN", "OTH")
    selected_columns[which.max(row_values)]
  }))

#Display actual results of 2024 election
table(electdata_2024_turnout$winner)

#Display adjusted results of 2024 election
table(electdata_2024_NEW$winner)
