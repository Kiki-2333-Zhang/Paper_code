library(readstata13)
library(sjlabelled)
datapath <- ("F:\\OneDrive\\graduate\\data\\CHARLS\\")

##### 1 Process 2018 Data #####

## Merge 2018 individual data and select variables needed.
data_list_18 <- list.files(paste0(datapath, "2018 Wave4\\data 20200913\\"))
# read all 2018 sub-data
for(i in data_list_18){
  temp <- read.dta13(paste0(datapath,"2018 Wave4\\data 20200913\\", i))
  temp <- set_label(temp, varlabel(temp))
  assign(paste0(gsub(".dta", "", i), "_18"), temp)
  print(paste0(i, " has been imported"))
}
merge_list_18 <- c("Sample_Infor_18", "Demographic_Background_18", "Family_Information_18",
                   "Family_Transfer_18","Health_Status_and_Functioning_18", "Cognition_18", 
                   "Insider_18", "Health_Care_and_Insurance_18", "Work_Retirement_18", "Pension_18",
                   "Household_Income_18", "Individual_Income_18", "Housing_18", "Weights_18")
# merge data in the order in questionnaire.
# versionID maybe different in sub-data, so it need to be moved before merging.
# Merging Family_Information and Family_Transfer all would result in increased obs, so use all.x = T
# 1. check the intersection in datalist names. Found duplicated xrtype, xrgender, and ziwtime.
for(i in 1:(length(merge_list_18) - 1)){
  for(j in (i+1):length(merge_list_18)){
    print(c(merge_list_18[i], merge_list_18[j],
            intersect(names(get(merge_list_18[i])), names(get(merge_list_18[j])))))
  }
}
# merge the data.
Indi_18 <- subset(Sample_Infor_18, select = -c(versionID))
for(i in c(2:10, 12, 14)){
  Indi_18 <- merge(Indi_18, subset(get(merge_list_18[i]), select = -c(versionID)), 
                   by = c("ID", "householdID", "communityID"), all.x = T)
  print(paste0(i, " has been merged!"))
}
# Delete duplicated xrtype, xrgender, and ziwtime
dup_xrtype <- grep("xrtype", names(Indi_18))
dup_xrgender <- grep("xrgender", names(Indi_18))
dup_ziwtime <- grep("ziwtime", names(Indi_18))
Indi_18$xrtype <- apply(Indi_18[,dup_xrtype], 1, max, na.rm = T)
Indi_18$xrgender <- apply(Indi_18[,dup_xrgender], 1, max, na.rm = T)
Indi_18$ziwtime <- apply(Indi_18[,dup_ziwtime], 1, max, na.rm = T)
Indi_18 <- Indi_18[,-c(dup_xrtype, dup_xrgender, dup_ziwtime)]
rm(list = merge_list_18, i, j, temp, merge_list_18, data_list_18, dup_xrgender, dup_xrtype, dup_ziwtime)                                                            
# select related variabes in 2018
sp.list <- grep("da05[1-7]{1}", names(Indi_18), value = T) # social participation related variables
mh.list <- grep("dc0[0,1,2]{1}[0-9]{1}", names(Indi_18), value = T)  # mental health related variables, MMSE and CESD
de.list <- grep("ba|bb00[0,1]{1}", names(Indi_18), value = T) # other demographic variables, age, gender, living status, now living place, if_local
sample.list <- c("ID", "householdID", "communityID", "xrtype", "xrgender", "died", "imonth")# Individual, household and community ID
data <- Indi_18[,c(sample.list, de.list, sp.list, mh.list)]


## 1-1-2 Delete those no longer living in this community ##
# find those who no longer living in this community
table(data$bb001_w3, exclude = NULL) # live in the same place as last survey
# find those who changed the address in 2015
data.15 <- read.dta13(paste0(datapath,"2015 Wave3\\data 20190620\\Demographic_Background.dta"))
data.15 <- set_label(data.15, varlabel(data.15))
table(is.na(data.15$bb001_w3)) # 34 missing value, almost missing the whole Demographic Background section
data <- merge(data, data.15[,c("ID", "bb001_w3", "xrtype")], by = "ID", all.x = TRUE, suffixes = c(".18", ".15"))
table(is.na(data$bb001_w3.15)) # weird, there is much more lost than died.18 + new.18
# find those who changed the address in 2013
data.13 <- read.dta13(paste0(datapath,"2013 Wave2\\data 20151118\\Demographic_Background.dta"))
data.13 <- set_label(data.13, varlabel(data.13))
table(data.13$bb000_w2_1)
data <- merge(data, data.13[,c("ID", "bb000_w2_1", "xrtype")], by = "ID", all.x = TRUE)
# delete those who died in 2018
table(data$died)
data <- subset(data, died == "0 Alive") # remove 997 died, 19816 case remain
data.backup <- data #!!!!!!!!!!!!!!First backup Here!!!!!!!!!!!!
# delete those who do not live in this community in 2018
table(data$bb001_w3.18, data$xrtype.18, exclude = NULL) # No NA. Why New Interview all moved away?
data <- subset(data, bb001_w3.18 == "1 Address during Last Survey")  # 19816 -> 17510
table(data$bb001_w3.15, data$xrtype.15, exclude = NULL)
# delete those who do not live in this community in 2015
data <- subset(data, bb001_w3.15 == 1)  # 17510 -> 15517
# delete those who do not live in this community in 2013
table(data$bb000_w2_1, data$xrtype, exclude = NULL) # So many NA in xrtype and current address
table(is.na(data$bb000_w2_1), data$xrtype.15) # many NA, only 202 is new sample in 2015, so delete all
data <- subset(data, bb000_w2_1 == "1 The Same Address")  # 15517 -> 11529
# delete those who do not live in family housing
table(data$bb001_w3_1, exclude = NULL)
data <- subset(data, bb001_w3_1 == "1 Family Housing")  # 11529 -> 11418
# delete those who accept interview at 9 or 11
table(data$imonth, exclude = NULL)
data <- subset(data, imonth != "09" & imonth != "11")  # 11418 -> 11394
rm(data.13, data.15)
# delete those live in Chaohu as it has been abolished
psu11 <- read.dta13(paste0(datapath, "2011 Wave1/data 20130312/psu.dta"))
data <- data[!(data$communityID %in% psu11[psu11$city == "巢湖市", "communityID"]), ] # 11394 -> 11352
rm(psu11)
data.backup2 <- data #!!!!!!! BACKUP2 HERE!!!!!

## 1-1-3 Process Individual Data ##
## Control Variables
# sex, female as reference
table(data$ba000_w2_3)
anyNA(data$ba000_w2_3)
data$sex <- data$ba000_w2_3
data$sex <- relevel(data$sex, ref = "1 Male")
table(data$sex, exclude = NULL)
# age
table(data$ba004_w3_1, exclude = NULL) # ID birth data
table(data$ba005_w4, exclude = NULL) # true birth date identical with ID birth date?
table(data$ba002_1, exclude = NULL) # true birth date
# VIM::aggr(data[,c("ba004_w3_1", "ba005_w4", "ba002_1")])
data$age <- ifelse(data$ba005_w4 == "2 Different" & is.na(data$ba002_1) == T, data$ba002_1, data$ba004_w3_1)
table(Indi_18$zfrbirth, exclude = NULL) # Recorded Brith
data$age <- 2018 - data$age
summary(data$age)
# edu
data <- merge(data, subset(Indi_18, select = c(ID, bd001_w2_4)), by = "ID", all.x = TRUE)
table(data$bd001_w2_4, exclude = NULL)
data$edu <- data$bd001_w2_4
anyNA(data$edu)
data$edu.fac <- 2
data$edu.fac[data$edu == "1 No Formal Education (Illiterate)" | data$edu == "2 Did not Finish Primary School"] <- 0
data$edu.fac[data$edu == "3 Sishu/Home School" | data$edu == "4 Elementary School"] <- 1
data$edu.fac <- as.factor(data$edu.fac)
summary(data$edu.fac)
# household capacity
household <- read.dta13((paste0(datapath,"2018 Wave4\\data 20200913\\Family_Information.dta")))
house.cap <- household[, grep("householdID|cv009|^a001_w4|^a005_w3", names(household))]
house.cap$cap <- rowSums(!is.na(house.cap[,2:66]))
house.cap$spouse <- !is.na(house.cap$cv009)
data <- merge(data, house.cap[,c("householdID", "cap", "spouse")], by = "householdID", all.x = TRUE)
rm(house.cap, household)
table(data$cap, data$spouse, exclude = NULL) 
# Air conditioner use
house <- read.dta13("F:\\OneDrive\\graduate\\data\\CHARLS\\2018 Wave4\\data 20200913\\Household_Income.dta")
table(house$ha065_s11, exclude = NULL)
house$AC <- house$ha065_s11
data <- merge(data, house[c("householdID", "AC")], by = "householdID", all.x = TRUE)
summary(data$AC)
rm(house)
# self rated health, too many NA
data <- merge(data, Indi_18[,c("ID", "da002", "db032")], by = "ID", all.x = TRUE)
table(data$da002, data$db032, exclude = NULL)
data$self_health <- data$da002
data$self_health_T <- forcats::fct_rev(data$self_health)
table(data$self_health_T, exclude = NULL)
# indoor interviewer feel temperature
house.18 <- read.dta13(paste0(datapath,"2018 Wave4\\data 20200913\\Housing.dta"))
data <- merge(data, subset(house.18, select = c("householdID", "i026")), by = "householdID", all.x = TRUE)
rm(house.18)
data$indoor_temp <- data$i026
summary(data$indoor_temp)
data$indoor_temp[is.na(data$indoor_temp)] <- "6 Not Applicable"
data$indoor_temp_T <- relevel(data$indoor_temp, ref = "3 Bearable")
# community_location, this should be the place that most of resident agreed on.
table(data$bb001_w3_2, exclude = NULL)
data$urban <- ifelse(data$bb001_w3_2 == "1 Central of City/Town" | data$bb001_w3_2 == "2 Urban-Rural Integration Zone", 
                     TRUE, FALSE)
temp <- aggregate(urban ~ communityID, data, mean)
hist(temp$urban)
temp[temp$urban >=0.5, "urban"] <- 1
temp[temp$urban < 0.5, "urban"] <- 0
data <- subset(data, select = -c(urban))
data <- merge(data, temp, by = "communityID", all.x = TRUE)
table(data$urban)
rm(temp)
# community_type
psu11 <- read.dta13(paste0(datapath, "2011 Wave1/data 20130312/psu.dta"))
psu13 <- read.dta13(paste0(datapath, "2013 Wave2/data 20151118/PSU.dta"))
names(psu11)
names(psu13)
psu <- merge(psu11[,c(1,4)], psu13[,c(1,4)], by = "communityID")
names(psu)[2:3] <- c("psu11", "psu13")
mean(psu$psu11 == psu$psu13) # Same for psu 11 and 13
mean(data$communityID %in% psu$communityID)
data <- merge(data, psu[,c(1,2)], by = "communityID", all.x = TRUE)
rm(psu, psu11, psu13)
table(data$urban, data$psu11)
table(data$bb001_w3_2, data$psu11) # Weird result.
# Working Status
data <- merge(data, Indi_18[c("ID", "fc008", "fc001", "fa002_w4", "fa003")], by = "ID")
summary(data$fa002_w4)
data$agri_work <- data$fc008 == "1 Yes" | data$fc001 == "1 Yes"
data$non_agri <- data$fa002_w4 == "1 Yes" | data$fa003 == "1 Yes"
table(is.na(data$agri_work), is.na(data$non_agri)) # shared missing value
table(data$agri_work, data$non_agri)
# Chronic Diseases
data <- merge(data, Indi_18[, names(Indi_18) %in% grep("^ID|zdiagnosed|^da007", names(Indi_18), value = T)], 
              all.x = TRUE, by = "ID")
table(data$da007_1_, data$zdiagnosed_1_, exclude = NULL)
data$cd1 <- ifelse(data$zdiagnosed_1_ == "1 Yes" | data$da007_1_ == "1 Yes", 1, 0)
data$cd2 <- ifelse(data$zdiagnosed_2_ == "1 Yes" | data$da007_2_ == "1 Yes", 1, 0)
data$cd3 <- ifelse(data$zdiagnosed_3_ == "1 Yes" | data$da007_3_ == "1 Yes", 1, 0)
data$cd4 <- ifelse(data$zdiagnosed_4_ == "1 Yes" | data$da007_4_ == "1 Yes", 1, 0)
data$cd5 <- ifelse(data$zdiagnosed_5_ == "1 Yes" | data$da007_5_ == "1 Yes", 1, 0)
data$cd6 <- ifelse(data$zdiagnosed_6_ == "1 Yes" | data$da007_6_ == "1 Yes", 1, 0)
data$cd7 <- ifelse(data$zdiagnosed_7_ == "1 Yes" | data$da007_7_ == "1 Yes", 1, 0)
data$cd8 <- ifelse(data$zdiagnosed_8_ == "1 Yes" | data$da007_8_ == "1 Yes", 1, 0)
data$cd9 <- ifelse(data$zdiagnosed_9_ == "1 Yes" | data$da007_9_ == "1 Yes", 1, 0)
data$cd10 <- ifelse(data$zdiagnosed_10_ == "1 Yes" | data$da007_10_ == "1 Yes", 1, 0)
data$cd11 <- ifelse(data$zdiagnosed_11_ == "1 Yes" | data$da007_11_ == "1 Yes", 1, 0)
data$cd12 <- ifelse(data$zdiagnosed_12_ == "1 Yes" | data$da007_12_ == "1 Yes", 1, 0)
data$cd13 <- ifelse(data$zdiagnosed_13_ == "1 Yes" | data$da007_13_ == "1 Yes", 1, 0)
data$cd14 <- ifelse(data$zdiagnosed_14_ == "1 Yes" | data$da007_14_ == "1 Yes", 1, 0)
data$NoCD <- rowSums(data[,names(data) %in% grep("^cd\\d{1,2}$", names(data), value = T)], na.rm = TRUE)
table(data$NoCD, exclude = NULL)
data$CD <- data$NoCD != 0
# Medication Use
data <- merge(data, Indi_18[,c("ID", "ef001_w4", "da020_w4_s2", "da020_w4_s3")], by = "ID", all.x = TRUE)
table(data$ef001_w4, exclude =  NULL)
table(data$da020_w4_s2, exclude = NULL)
table(data$da020_w4_s3, exclude = NULL)
data$pill <- data$ef001_w4 == "1 Yes"
data$pill_psy <- data$da020_w4_s2 == "2 Taking Anti Depressants" | 
  data$da020_w4_s3 == "3 Taking Tranquilizers or Sleeping Pills"
data[is.na(data$pill), "pill"] <- 0
data[is.na(data$pill_psy), "pill_psy"] <- 0
table(data$pill, data$pill_psy, exclude = NULL)
# House Condition
house <- read.dta13("F:\\OneDrive\\graduate\\data\\CHARLS\\2018 Wave4\\data 20200913\\Housing.dta")
table(house$i004, exclude = NULL)
house$house_material <- house$i004 == "1 Concrete and Steel/Bricks and Wood"
table(house$i006, exclude = NULL)
house$house_type <- house$i006 == "2 Common Multi-story Building" 
table(house$i008, exclude = NULL)
house$house_level <- house$i008
table(house$house_level)
house$house_level <- floor(house$house_level)
table(house$i010_w4)
house[house$i010_w4 %in% c("1 Yes", "3 Do Not Need") & is.na(house$house_level),]$house_level <- 1
table(house$house_level, exclude = NULL)
table(house$i011)
house[house$i011 %in% c("1 0 Step", "2 1-5 Steps") & is.na(house$house_level),]$house_level <- 1
house[house$i011 %in% "3 6-15 Steps" & is.na(house$house_level),]$house_level <- 2
house[house$i011 %in% "4 16-25 Steps" & is.na(house$house_level),]$house_level <- 3
house[house$i011 %in% "5 More Than 25 Steps" & is.na(house$house_level),]$house_level <- 4
house[house$house_level %in% 0,]$house_level <- 1
table(house$house_level, exclude = NULL)
table(house$i009, exclude = NULL)
mean(data$householdID %in% house$householdID)
data <- merge(data, subset(house, select = c(householdID, house_material, house_type, house_level)), 
                           by = "householdID", all.x = TRUE)
rm(house)
# Smoke and Drink
table(Indi_18$zsmoke)
table(Indi_18$da059)
table(Indi_18$da061)
table(Indi_18$da067, exclude = NULL)
data <- merge(data, subset(Indi_18, select = c(ID, zsmoke, da059, da061, da067)), by = "ID",
              all.x = TRUE)
VIM::aggr(data[,c("zsmoke", "da059", "da061")])
data$smoke <- data$da059
data[is.na(data$smoke) & !is.na(data$zsmoke),]$smoke <- "1 Yes"
data[data$da061 %in% "2 Quit",]$smoke <- "2 No"
data$smoke <- data$smoke == "1 Yes"
table(data$smoke, exclude = NULL)
table(data$da067, exclude = NULL)
data$drink <- data$da067 == "1 Drink More than Once a Month"

# Dependent Variables: depression
# VIM::aggr(data[paste0("dc",formatC(seq(9,18), width = 3, flag = 0))]) # Same NA for all item.
data.dep <- data[c("ID",paste0("dc",formatC(seq(9,18), width = 3, flag = 0)))]
data.dep <- cbind(data.dep, as.data.frame(lapply(data.dep[2:11], as.numeric)))
data <- merge(data, data.dep[, c(1,12:21)], by = "ID", all.x = TRUE)
table(data$dc009.x)
table(data$dc009.y)  # Should delete value = 5 or value = 6. Delete below.
data$depression <- rowSums(data[c("dc009.y", "dc010.y", "dc011.y", "dc012.y", "dc014.y", "dc015.y", "dc017.y", "dc018.y")])
data$depression <- data$dep - data$dc013.y - data$dc016.y
summary(data$depression)
rm(data.dep)
# Mediating Variables， social participation
VIM::aggr(data[,c("da056_s1", "da056_s2", "da056_s3", "da056_s4", "da056_s5", "da056_s6", 
                  "da056_s7", "da056_s8", "da056_s9", "da056_s10", "da056_s11", "da056_s12")], 
     prop = FALSE, numbers = TRUE, labels = TRUE) # 26 NA for every item
data.temp <- data[c("ID", grep("da05[6,7]{1}_[^w]", names(data), value = TRUE))]
data.temp <- subset(data.temp, is.na(da056_s1) == FALSE)
summary(data.temp$da056_s11)
for (i in 1:8){
  data.temp[ncol(data.temp)+1] <- (as.integer(data.temp[[paste0("da056_s",i)]]) - i) *
    (4 - as.integer(data.temp[[paste0("da057_",i,"_")]]))
  data.temp[is.na(data.temp[[ncol(data.temp)]]) & data.temp[[paste0("da056_s",i)]] != "0 No", ncol(data.temp)] <- 1
}
lapply(data.temp[paste0("V",seq(25,32))],table)
data.temp$participation <- rowSums(data.temp[paste0("V",seq(25,32))], na.rm = TRUE)
data <- merge(data, data.temp[,c("ID","participation")], by = "ID", all.x = TRUE)
rm(data.temp)
summary(data$participation)
attach(data)
data$count_parti <- (da056_s1 != "0 No") + (da056_s2 != "0 No") + (da056_s3 != "0 No") + (da056_s4 != "0 No") +
  (da056_s5 != "0 No") + (da056_s6 != "0 No") + (da056_s7 != "0 No") + (da056_s8 != "0 No")
detach(data)
summary(data$count_parti)
data$parti_charity <- (data$da056_s3 != "0 No") + (data$da056_s6 != "0 No") + (data$da056_s7 != "0 No")
data$parti_together <- (data$da056_s1 != "0 No") + (data$da056_s2 != "0 No") + (data$da056_s4 != "0 No") + 
  (data$da056_s5 != "0 No") + (data$da056_s8 != "0 No")
data$parti_alone <- (data$da056_s9 != "0 No") + (data$da056_s10 != "0 No")  
table(data$parti_charity)
table(data$parti_together)
table(data$parti_alone)
data$parti_charity_d <- data$parti_charity >= 1
data$parti_together_d <- data$parti_together >= 1
data$parti_alone_d <- data$parti_alone >= 1
data.backup3 <- data   ########*****########### BACKUP HERE!!!!
rm(i, de.list, mh.list, sample.list, sp.list)

data <- subset(data, select = c(ID, householdID, communityID, sex, age, edu, edu.fac, cap, spouse, AC, self_health_T, indoor_temp_T, 
                                urban, psu11, agri_work, non_agri, NoCD, CD, pill, pill_psy, house_material, 
                                house_type, house_level, smoke, drink, 
                                depression, participation, count_parti, parti_charity, parti_together, parti_alone, 
                                parti_charity_d, parti_together_d, parti_alone_d,
                                dc009.y, dc010.y, dc011.y, dc012.y, dc013.y, dc014.y, dc015.y, dc016.y, dc017.y, dc018.y,
                                db032))

## Delete NA
# delete those repsond by others, db032 == 3 or 4 first
summary(data$db032)
data <- subset(data, db032 == "1 Never" | db032 == "2 A Few Times")  # 11352-771=10581

# delete missing dependent variables and 8 or 9  # 10581 -> 9009
VIM::aggr(data[paste0("dc",formatC(seq(9,18), width = 3, flag = 0), ".y")]) # 264 NA for every item
data <- subset(data, is.na(dc009.y) == FALSE) # 
lapply(data[paste0("dc",formatC(seq(9,18), width = 3, flag = 0), ".y")], summary)
data <- subset(data, dc009.y <= 4 & dc010.y <= 4 & dc011.y <= 4 & dc012.y <= 4 & dc013.y <= 4 &
                 dc014.y <= 4 & dc015.y <= 4 & dc016.y <= 4 & dc017.y <= 4 & dc018.y <= 4)
table(data$depression)

# delete missing control variables
summary(data$age) # 107NA
summary(data$sex)
summary(data$cap)
summary(data$edu.fac)
summary(data$self_health_T) # 2 NA
summary(data$AC)  # 7 NA 
summary(data$indoor_temp)
summary(data$urban)
summary(data$psu11)
summary(data$agri_work) 
summary(data$non_agri)
summary(data$NoCD)
summary(data$pill)
summary(data$pill_psy)
summary(data$participation) 
summary(data$smoke)
summary(data$drink)

data$indep_na <- is.na(data$age) | is.na(data$self_health_T) | is.na(data$AC)
table(data$indep_na)
independent_delete <- t.test(depression ~ indep_na, data)
independent_delete
data <- subset(data, is.na(age) != TRUE & is.na(self_health_T) != TRUE & is.na(AC) != TRUE)  # 9009 -> 8893

##### Add 2015 depression as #####
data.15 <- readstata13::read.dta13(paste0(datapath,"2015 Wave3\\data 20190620\\Health_Status_and_Functioning.dta"))
data.15 <- data.15[c("ID",paste0("dc",formatC(seq(9,18), width = 3, flag = 0)))]
data.15 <- cbind(data.15, as.data.frame(lapply(data.15[2:11], as.numeric)))
data.15 <- data.15[,-c(2:11)]
data.15$depression <- rowSums(data.15[c("dc009", "dc010", "dc011", "dc012", "dc014", "dc015", "dc017", "dc018")])
data.15$depression <- data.15$depression - data.15$dc013 - data.15$dc016
summary(data$depression)
data.15 <- subset(data.15, dc009 <= 4 & dc010 <= 4 & dc011 <= 4 & dc012 <= 4 & dc013 <= 4 &
                    dc014 <= 4 & dc015 <= 4 & dc016 <= 4 & dc017 <= 4 & dc018 <= 4)
data.15 <- subset(data.15, is.na(dc009) == FALSE)
data.15 <- data.15[,-c(2:11)]
colnames(data.15)[2] <- "depression15"
table(data.15$depression15, exclude = NULL)
data <- merge(data, data.15, by = "ID", all.x = TRUE)
table(data$depression15, exclude = NULL)
rm(data.15)

saveRDS(data, "2018Clean.rds")
