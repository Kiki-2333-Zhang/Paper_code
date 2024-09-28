##### Match Data #####
data.sample <- readstata13::read.dta13("F:/OneDrive/graduate/data/CHARLS/2018 Wave4/data 20200913/Sample_Infor.dta")
data <- readRDS("F:/OneDrive/graduate/my_research/In_progress/heatwave/mental health/file/Version 9/2 Process Individual data/2018Clean.rds")
climate <- readRDS("F:/OneDrive/graduate/my_research/In_progress/heatwave/mental health/file/Version 9/1 Process weather data/CHARLS_temp.rds")

names(data.sample)
data.sample <- subset(data.sample, select = c(ID, communityID, imonth))
data <- merge(data, data.sample, by = c("ID", "communityID"), all.x = TRUE)
climate$loc <- as.integer(climate$loc == "urban")
data <- merge(data, climate, by.x = c("communityID", "urban", "imonth"), by.y = c("communityID", "loc", "month"), all.x = TRUE)
rm(climate, data.sample)

table(data$age >= 55)
data <- data[data$age >= 55, ]

##### Descriptive Statistics #####
library(ltm)
cronbach.alpha(data[paste0("dc",formatC(seq(9,18), width = 3, flag = 0),".y")]) # 0.685 for depression scale, acceptable
hist(data$depression)
qqnorm(data$depression)
qqline(data$depression)

library(vtable)
st(data, vars = CV)  # CV is defined below
table(as.logical(data$pill))
summary(data$depression)
sd(data$depression)
mean(data$Dura3_35)
sd(data$Dura3_35)
table(data$parti_charity_d)
table(data$parti_together_d)
mean(data$cap)
sd(data$cap)
mean(data$age)
sd(data$age)

##### Regression Model #####
library(lmerTest)
# library(Matrix)
data$city <- substr(data$communityID,1,4)
data$city <- as.factor(data$city)
data$parti_alone_d <- as.numeric(data$parti_alone_d)
data$parti_charity_d <- as.numeric(data$parti_charity_d)
data$parti_together_d <- as.numeric(data$parti_together_d)

## Main Model
ml1.0 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + (1 | city), data)
ml1.1 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + Dura3_35 + (1 | city), data)
ml1.2 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + parti_together_d + parti_charity_d + (1 | city), data)
ml1.3 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + Dura3_35 * parti_together_d + Dura3_35 * parti_charity_d + (1 | city), data)
summary(ml1.0)
confint(ml1.0)
summary(ml1.1)
confint(ml1.1)
summary(ml1.2)
confint(ml1.2)
summary(ml1.3)
confint(ml1.3)
car::vif(ml1.1)
performance::icc(ml1.0)
performance::icc(ml1.1)
performance::icc(ml1.2)
performance::icc(ml1.3)

## Sensitive Model
# Add 2015 depression
ss1.1 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
               CD + pill + depression15 + Dura3_35 + (1 | city), data)
ss1.2 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + depression15 + parti_together_d + parti_charity_d + (1 | city), data)
ss1.3 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + depression15 + Dura3_35 * parti_together_d + Dura3_35 * parti_charity_d + (1 | city), data)
summary(ss1.1)
summary(ss1.2)
summary(ss1.3)
confint(ss1.3)

# Change Heatwave to compound hot
ss2.1 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + CHW_35 + (1 | city), data)
ss2.2 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + parti_together_d + parti_charity_d + (1 | city), data)
ss2.3 <- lmer(depression ~ sex + age + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + CHW_35 * parti_together_d + CHW_35 * parti_charity_d + (1 | city), data)
summary(ss2.1)
summary(ss2.2)
summary(ss2.3)
confint(ss2.3)

# Change depression to dummy, cannot converge, abandon.
data$depression.d <- data$depression >= 12
ss3.1 <- glmer(depression.d ~ sex + age + edu.fac + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + Dura3_35 + (1 | city), data, family = binomial(link = "logit"))
ss3.2 <- glmer(depression.d ~ sex + age + edu.fac + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + parti_together_d + parti_charity_d + (1 | city), data, family = binomial(link = "logit"))
ss3.3 <- glmer(depression.d ~ sex + age + edu.fac + cap + AC + self_health_T + indoor_temp_T + psu11 + agri_work + non_agri + 
                CD + pill + Dura3_35 * parti_together_d + Dura3_35 * parti_charity_d + (1 | city), data, family = binomial(link = "logit"))
summary(ss3.1)
summary(ss3.2)
summary(ss3.3)
