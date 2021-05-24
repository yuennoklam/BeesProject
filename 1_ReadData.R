library(readxl) # to read in Excel files
library(tidyverse) # for easy data manipulation (includes packages dplyr, tidyr, stringr, ggplot2, purr, tibble, readr, forcats)
library(chron) # for handling dates/times

transect = read_xlsx("RawData/Transect\ data_master_2021-05-04.xlsx",2)

B = subset(transect, select=site:campestris) %>% # extract just the columns relevant to bumblebee analyses
  mutate(
    site = factor(site),
    site = recode(site, "seh" = "waf"),
    visit = factor(visit),
    year = factor(year),
    date = as.Date(paste(day, month, year, sep="-"), "%d-%m-%Y"), # concatenate day, month, year to create date
    monthtxt = month.abb[month],
    yrmth = paste(year, month, sep="-"), # create a new vector of month-year
    jdate = difftime(date, as.Date(paste(01, 03, year, sep="-"),"%d-%m-%Y"), units = c("days")), # calculate days since 1st March.
    landuse = factor(landuse),
    treatment = factor(treatment),
    field = factor(field),
    bound.area = as.numeric(bound.area),
    snh = as.numeric(snh),
    starttime = times(strftime(start,"%H:%M:%S")), # convert to just time (removes date, which was wrong date anyway)
    endtime = times(strftime(end,"%H:%M:%S")), # convert to just time (removes date, which was wrong date anyway)
    windfac = factor(wind),
    wind3 = factor(1+(windfac=="2")+2*(windfac=="3")+2*(windfac=="4")), # get rid of the one value of 4 in windstrength
    HYMENOPTERA = as.numeric(HYMENOPTERA)
  )

# Code to exclude WAF in 2011 & 2012, exclude LHF Mar & Apr visits:
B1 = filter(B, !( (year == '2011' & site == 'waf' ) | (site == 'waf' & visit == '6') | (site == 'waf' & visit == '7') | # remove WAF and SEH in 2011, and visits 6 and 7 from Waf (these are the June and July visits in 2012. In the Aug visit I didn't collect bumblebees)
                    (site == 'wh' & visit == '8') |  #  remove WH visit 8 (July 2012),
                    (site == 'lhf' & monthtxt == 'Mar') | (site == 'lhf' & monthtxt == 'Apr') | (site == 'lhf' & monthtxt == 'Jul') ) # remove LHF visits in March, April and July 2011 #(site == 'lhf' & visit == '1')|(site == 'lhf' & visit == '2')|(site == 'lhf' & visit == '5')
) %>%
  filter(!is.na(HYMENOPTERA)) # remove rows where there is an NA in HYMENOPTERA column


#remove ‘unidbomb’, ‘HYMENOPTERA’, ‘Apidae’, ‘Bombus’, AC to AG columns from B1
B1 <- within(B1, rm("unidbomb", "HYMENOPTERA","Apidae","Bombus","ter.luc.worker","terrestris","lucorum"))

# Bombus.terr.luc (treat it with aggregate species) (exclude AC to AG) (include AI to AT)
#rename column
names(B1)[26] <- "Bombus.terr.luc"

#make B1 species tidy for analysis
library(tidyverse)
B1 <- gather(B1, species, abundance, Bombus.terr.luc:campestris, factor_key=TRUE)
B1 <- B1[!(B1$abundance==0),]
B1$abundance <- as.numeric(B1$abundance)
#see total number of Bombus recorded
sum(B1$abundance)

for (i in 1:nrow(B1)) {
  if (B1$Bombus.terr.luc[i] != 0) {
    B1$species[i] <- "Bombus.terr.luc"
  }
  if (B1$hortorum[i]!= 0) {
    B1$species[i] <- "hortorum"
  }
  if (B1$ruderatus[i]!= 0) {
    B1$species[i] <- "ruderatus"
  }
  if (B1$pascuorum[i]!= 0) {
    B1$species[i] <- "pascuorum"
  }
  if (B1$lapidarius[i]!= 0) {
    B1$species[i] <- "lapidarius"
  }
  if (B1$ruderarius[i]!= 0) {
    B1$species[i] <- "ruderarius"
  }
  if (B1$pratorum[i]!= 0) {
    B1$species[i] <- "pratorum"
  }
  if (B1$hypnorum[i]!= 0) {
    B1$species[i] <- "hypnorum"
  }
  if (B1$barbutellus[i]!= 0) {
    B1$species[i] <- "barbutellus"
  }
  if (B1$bohemicus[i]!= 0) {
    B1$species[i] <- "bohemicus"
  }
  if (B1$rupestris[i]!= 0) {
    B1$species[i] <- "rupestris"
  }
  if (B1$vestalis[i]!= 0) {
    B1$species[i] <- "vestalis"
  }
  if (B1$campestris[i]!= 0) {
    B1$species[i] <- "campestris"
  }
}

#add a row of species abundance
B1$abundance <- NA

for (i in 1:nrow(B1)) {
  if (B1$Bombus.terr.luc[i] != 0) {
    B1$abundance[i] <- B1$Bombus.terr.luc[i]
  }
  if (B1$hortorum[i]!= 0) {
    B1$abundance[i] <- B1$hortorum[i]
  }
  if (B1$ruderatus[i]!= 0) {
    B1$abundance[i] <- B1$ruderatus[i]
  }
  if (B1$pascuorum[i]!= 0) {
    B1$abundance[i] <- B1$pascuorum[i]
  }
  if (B1$lapidarius[i]!= 0) {
    B1$abundance[i] <- B1$lapidarius[i]
  }
  if (B1$ruderarius[i]!= 0) {
    B1$abundance[i] <-B1$ruderarius[i]
  }
  if (B1$pratorum[i]!= 0) {
    B1$abundance[i] <- B1$pratorum[i]
  }
  if (B1$hypnorum[i]!= 0) {
    B1$abundance[i] <- B1$hypnorum[i]
  }
  if (B1$barbutellus[i]!= 0) {
    B1$abundance[i] <- B1$barbutellus[i]
  }
  if (B1$bohemicus[i]!= 0) {
    B1$abundance[i] <- B1$bohemicus[i]
  }
  if (B1$rupestris[i]!= 0) {
    B1$abundance[i] <- B1$rupestris[i]
  }
  if (B1$vestalis[i]!= 0) {
    B1$abundance[i] <- B1$vestalis[i]
  }
  if (B1$campestris[i]!= 0) {
    B1$abundance[i] <- B1$campestris[i]
  }
}


#subset B1 to rows containing a bumblebee species
bee <- data.frame()
bee <- B1[which(is.na(B1$species) != TRUE),]

#make full name
for (i in 1:nrow(bee)) {
  if (bee$species[i] != "Bombus.terr.luc") {
    bee$fullname[i] <- paste("Bombus", bee$species[i])
  } else {bee$fullname[i] <- bee$species[i]}
}

#make years together


#add trait type and unit to traits.tbl
traits.tbl$type <- c("functional", "functional", "proxy", "proxy", "functional")
traits.tbl$unit <- c("nominal", "phenological", "continuous", "nominal", "nominal")


#read traits list, exclude the first row and use 2nd as column name
traitlist = read_xlsx("RawData/Traits_VarahUK_20201127.xlsx",1, skip = 1, col_names = TRUE)
names(traitlist)[1] <- "fullname"

#export trait list that contains species within our data B1 only
beetraits <- data_frame()
library(plyr)
beetraits <- match_df(traitlist, bee, on="fullname")
#Treat Bombus.terr.luc as an integrated species of Bombus terrestris and Bombus lucorum
#add the two species traits to beetraits
beetraits[nrow(beetraits) + 1,] = traitlist[traitlist$fullname=="Bombus terrestris",]
beetraits[nrow(beetraits) + 1,] = traitlist[traitlist$fullname=="Bombus lucorum",]

#make species x trait table
beetraits <- beetraits[-c(3:8,11,12,14,15:26)]

#Integrate Bombus terrestris and lucorum (ITD average is calculated; month of more existence is used)
c <- c("Bombus.terr.luc", "5.395", "Primitively eusocial", 
       "Renter: Existing cavities", "Polylectic s.s.", 
       "1","1","1","1","1","1","1","1","1","1","1",NA)

beetraits <- rbind(beetraits, c)

#turn month to no. of active month in beetraits
beetraits <- as.data.frame(beetraits)
for (i in 6:17) {
  beetraits[,i] <- as.numeric(beetraits[,i])
}

library(dplyr)
beetraits <- beetraits %>% 
  rowwise() %>% 
  mutate(ActiveMonth = apply(beetraits[,6:17], 1, sum, na.rm=TRUE))

#now make the beetraits data into defined cetegories for analysis (beetraitsA)
beetraitsA <- beetraits

for (i in 1:nrow(beetraitsA)) {
  if (beetraitsA$Sociality[i] == "Social parasite" | beetraitsA$Sociality[i] == "Primitively eusocial") {
    beetraitsA$Sociality[i] <- "Not obligately solitary"
  }
  if (beetraitsA$`Lecty (Mueller & Kuhlmann, 2008)`[i] == "Polylectic s.s.") {
    beetraitsA$`Lecty (Mueller & Kuhlmann, 2008)`[i] <- "Polylectic/flexible"
  }
}








