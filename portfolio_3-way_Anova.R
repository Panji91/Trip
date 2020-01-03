install.packages("rio")
library(foreign)
library(rio)
library(xlsx)
library(dplyr)
library(ztable)

#############################데이터 불러오기#####################################
tripdata <- read.csv("C:\\Users\\vksrl\\Documents\\Panji R file\\프로젝트\\외래관광객실태조사\\2013 ~ 2017 최종 데이터 파일\\2013 ~ 2017_F.csv", header = T)
names(tripdata)[1] <- "C2"

###############################################################진짜배기#########################
####China and Japan three-way
###데이터 정리###
####연령데이터정리
trip_Thr <- tripdata
trip_Thr$age[tripdata$age == 0] <- "young"
trip_Thr$age[tripdata$age == 1] <- "young"
trip_Thr$age[tripdata$age == 2] <- "young"
trip_Thr$age[tripdata$age == 3] <- "old"
trip_Thr$age[tripdata$age == 4] <- "old"
trip_Thr$age[tripdata$age == 5] <- "old"
trip_Thr$age[tripdata$age == 9] <- NA

####성별데이터정리
trip_Thr$sex[tripdata$sex == 1] <- "m"
trip_Thr$sex[tripdata$sex == 2] <- "f"

####패키지데이터 정리
trip_Thr$pckge[tripdata$A == 1] <- "solo"
trip_Thr$pckge[tripdata$A == 2] <- "group"
trip_Thr$pckge[tripdata$A == 3] <- "group"




###만족도(전반적만족도 사용)
trip_Thr$stsfctn_all <- trip_Thr$T2

###데이터 재구성
varkeepJC <- c("id","pckge","sex","age","year","nat","T1_1","T1_2","T1_3","T1_4","T1_5","T1_6","T1_7","T1_8","T1_9","T1_10","stsfctn_all")
trip_ThrJC1 <- trip_Thr[,varkeepJC]
trip_ThrJC1 <- na.omit(trip_ThrJC1)

trip_ThrJC <- trip_ThrJC1 %>% filter(nat <= 2)
trip_ThrJC$age <- as.factor(trip_ThrJC$age)
trip_ThrJC$sex <- as.factor(trip_ThrJC$sex)
trip_ThrJC$pckge <- as.factor(trip_ThrJC$pckge)
trip_ThrJC$nat <- as.factor(trip_ThrJC$nat)
trip_ThrJC$stsfctn_all <- as.numeric(trip_ThrJC$stsfctn_all)

###T 테스트
t.test(stsfctn_all~nat, var.equal = F, data=trip_ThrJC) # 영가설을 기각하여 서로 독립적임을 알 수 있음.

###삼원변량

fit_allJC <- aov(stsfctn_all~age*sex*pckge, data = trip_ThrJC)
summary(fit_allJC)

###독립 변수통제
sexJC_M <- trip_ThrJC %>% filter(sex == "m")
sexJC_F <- trip_ThrJC %>% filter(sex == "f")
ageJC_Y <- trip_ThrJC %>% filter(age == "young")
ageJC_O <- trip_ThrJC %>% filter(age == "old")
pckgeJC_S <- trip_ThrJC %>% filter(pckge == "solo")
pckgeJC_G <- trip_ThrJC %>% filter(pckge == "group")

###주효과분석
library(moonBook)
library(dplyr)

##age 주효과
mean_age <- trip_ThrJC %>%
  group_by(age) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_age <- trip_ThrJC %>%
  group_by(age) %>%               
  summarise(sd(stsfctn_all))
table(trip_ThrJC$age) # n값
mean_age # mean 값
sd_age # sd 값

##sex 주효과
mytable(sex~stsfctn_all, data = trip_ThrJC)
mean_sex <- trip_ThrJC %>%
  group_by(sex) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_sex <- trip_ThrJC %>%
  group_by(sex) %>%               
  summarise(sd(stsfctn_all))
table(trip_ThrJC$sex) # n값
mean_sex # mean 값
sd_sex # sd 값

##pckge 주효과
mytable(pckge~stsfctn_all, data = trip_ThrJC)
mean_pckge <- trip_ThrJC %>%
  group_by(pckge) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_pckge <- trip_ThrJC %>%
  group_by(pckge) %>%               
  summarise(sd(stsfctn_all))
table(trip_ThrJC$pckge) # n값
mean_pckge # mean 값
sd_pckge # sd 값

# 하나의 독립변수를 통제한 이원변량분석을 실시한다.
# 주효과가 의미 있게 나올 경우, mytable을 이용하여 평균비교를 하였고
# 상호작용 효과가 의미 있게 나올 경우, contrast를 수행하였다.
# 각 그래프는 ggplot을 이용하였다.



############## 이원변량분석 ##############

#1. sex = m 일때 이원변량분석
sexJC_M_aov <- aov(stsfctn_all ~ age*pckge, data = sexJC_M)
summary(sexJC_M_aov)

##주효과

#age
mytable(age~stsfctn_all, data = sexJC_M)
mean_sexJC_M_age <- sexJC_M %>%
  group_by(age) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_sexJC_M_age <- sexJC_M %>%
  group_by(age) %>%               
  summarise(sd(stsfctn_all))
table(sexJC_M$age) # n값
mean_sexJC_M_age # mean 값
sd_sexJC_M_age # sd 값
#pckge
mytable(age~stsfctn_all, data = sexJC_M)
mean_sexJC_M_pckge <- sexJC_M %>%
  group_by(pckge) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_sexJC_M_pckge <- sexJC_M %>%
  group_by(pckge) %>%               
  summarise(sd(stsfctn_all))
table(sexJC_M$pckge) # n값
mean_sexJC_M_pckge # mean 값
sd_sexJC_M_pckge # sd 값


##그래프
library(lsmeans)
ref_sexJC_M <- lsmeans(sexJC_M_aov, specs = c("age","pckge"))
ref_sexJC_M_df <- as.data.frame(summary(ref_sexJC_M))
library(ggplot2)
pd11 <- position_dodge(0.1)
g411 <- ggplot(ref_sexJC_M_df, aes(x=pckge, y=lsmean, group=age, colour=age))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd11)+geom_line(position=pd11)+geom_point(position=pd11)
print(g411)

##contrast
sexJC_M$age <- as.numeric(sexJC_M$age)
sexJC_M$pckge <- as.numeric(sexJC_M$pckge)
sexJC_M$age_pckge <- as.factor(10*sexJC_M$age + sexJC_M$pckge)
interaction <- aov(stsfctn_all ~ age_pckge, data = sexJC_M)
summary(interaction)
test1 <- c(-1, 0, 1, 0)
test2 <- c(0, -1, 0, 1)
summary(contrast(ref_sexJC_M, list(packageG_age = test1, packageS_age = test2)))



#2. sex = F 일때 이원변량분석
sexJC_F_aov <- aov(stsfctn_all ~ age*pckge, data = sexJC_F)
summary(sexJC_F_aov)
mytable(age~stsfctn_all, data = sexJC_F)

##주효과
#age
mytable(age~stsfctn_all, data = sexJC_F)
mean_sexJC_F_age <- sexJC_F %>%
  group_by(age) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_sexJC_F_age <- sexJC_F %>%
  group_by(age) %>%               
  summarise(sd(stsfctn_all))
table(sexJC_F$pckge) # n값
mean_sexJC_F_age # mean 값
sd_sexJC_F_age # sd 값

##그래프
library(lsmeans)
ref_sexJC_F <- lsmeans(sexJC_F_aov, specs = c("age","pckge"))
ref_sexJC_F_df <- as.data.frame(summary(ref_sexJC_F))
library(ggplot2)
pd12 <- position_dodge(0.1)
g412 <- ggplot(ref_sexJC_F_df, aes(x=pckge, y=lsmean, group=age, colour=age))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd12)+geom_line(position=pd12)+geom_point(position=pd12)
print(g412)


#3. age = Young 일때
ageJC_Y_aov <- aov(stsfctn_all ~ sex*pckge, data = ageJC_Y)
summary(ageJC_Y_aov)

##주효과
#sex
mytable(sex~stsfctn_all, data = ageJC_Y)
mean_ageJC_Y_sex <- ageJC_Y %>%
  group_by(sex) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_ageJC_Y_sex <- ageJC_Y %>%
  group_by(sex) %>%               
  summarise(sd(stsfctn_all))
table(ageJC_Y$sex) # n값
mean_ageJC_Y_sex # mean 값
sd_ageJC_Y_sex # sd 값

##그래프
library(lsmeans)
ref_ageJC_Y <- lsmeans(ageJC_Y_aov, specs = c("sex","pckge"))
ref_ageJC_Y_df <- as.data.frame(summary(ref_ageJC_Y))
library(ggplot2)
pd13 <- position_dodge(0.1)
g413 <- ggplot(ref_ageJC_Y_df, aes(x=pckge, y=lsmean, group=sex, colour=sex))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd13)+geom_line(position=pd13)+geom_point(position=pd13)
print(g413)


#4. age = Old 일때
ageJC_O_aov <- aov(stsfctn_all ~ sex*pckge, data = ageJC_O)
summary(ageJC_O_aov)
mytable(sex~stsfctn_all, data = ageJC_O)
mytable(pckge~stsfctn_all, data = ageJC_O)

##주효과
#sex
mytable(sex~stsfctn_all, data = ageJC_O)
mean_ageJC_O_sex <- ageJC_O %>%
  group_by(sex) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_ageJC_O_sex <- ageJC_O %>%
  group_by(sex) %>%               
  summarise(sd(stsfctn_all))
table(ageJC_O$sex) # n값
mean_ageJC_O_sex # mean 값
sd_ageJC_O_sex # sd 값

#pckge
mytable(pckge~stsfctn_all, data = ageJC_O)
mean_ageJC_O_pckge <- ageJC_O %>%
  group_by(pckge) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_ageJC_O_pckge <- ageJC_O %>%
  group_by(pckge) %>%               
  summarise(sd(stsfctn_all))
table(ageJC_O$pckge) # n값
mean_ageJC_O_pckge # mean 값
sd_ageJC_O_pckge # sd 값


##그래프
library(lsmeans)
ref_ageJC_O <- lsmeans(ageJC_O_aov, specs = c("sex","pckge"))
ref_ageJC_O_df <- as.data.frame(summary(ref_ageJC_O))
library(ggplot2)
pd14 <- position_dodge(0.1)
g414 <- ggplot(ref_ageJC_O_df, aes(x=pckge, y=lsmean, group=sex, colour=sex))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd14)+geom_line(position=pd14)+geom_point(position=pd14)
print(g414)

##contast
ageJC_O$sex <- as.numeric(ageJC_O$sex)
ageJC_O$pckge <- as.numeric(ageJC_O$pckge)
ageJC_O$sex_pckge <- as.factor(10*ageJC_O$sex + ageJC_O$pckge)
interaction <- aov(stsfctn_all ~ sex_pckge, data = ageJC_O)
summary(interaction)
test1 <- c(-1, 0, 1, 0)
test2 <- c(0, -1, 0, 1)
summary(contrast(ref_ageJC_O, list(packageG_sex = test1, packageS_sex = test2)))

#5. pckge = Solo 일때
pckgeJC_S_aov <- aov(stsfctn_all ~ sex*age, data = pckgeJC_S)
summary(pckgeJC_S_aov)
mytable(sex~stsfctn_all, data = pckgeJC_S)
mytable(age~stsfctn_all, data = pckgeJC_S)

##주효과
#sex
mytable(sex~stsfctn_all, data = pckgeJC_S)
mean_pckgeJC_S_sex <- pckgeJC_S %>%
  group_by(sex) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_pckgeJC_S_sex <- pckgeJC_S %>%
  group_by(sex) %>%               
  summarise(sd(stsfctn_all))
table(pckgeJC_S$sex) # n값
mean_pckgeJC_S_sex # mean 값
sd_pckgeJC_S_sex # sd 값

#age
mytable(age~stsfctn_all, data = pckgeJC_S)
mean_pckgeJC_S_age <- pckgeJC_S %>%
  group_by(age) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_pckgeJC_S_age <- pckgeJC_S %>%
  group_by(age) %>%               
  summarise(sd(stsfctn_all))
table(pckgeJC_S$age) # n값
mean_pckgeJC_S_age # mean 값
sd_pckgeJC_S_age # sd 값


##그래프
library(lsmeans)
ref_pckgeJC_S <- lsmeans(pckgeJC_S_aov, specs = c("sex","age"))
ref_pckgeJC_S_df <- as.data.frame(summary(ref_pckgeJC_S))
library(ggplot2)
pd15 <- position_dodge(0.1)
g415 <- ggplot(ref_pckgeJC_S_df, aes(x=age, y=lsmean, group=sex, colour=sex))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd15)+geom_line(position=pd15)+geom_point(position=pd15)
print(g415)

##contrast
pckgeJC_S$sex <- as.numeric(pckgeJC_S$sex)
pckgeJC_S$age <- as.numeric(pckgeJC_S$age)
pckgeJC_S$sex_age <- as.factor(10*pckgeJC_S$sex + pckgeJC_S$age)
interaction <- aov(stsfctn_all ~ sex_age, data = pckgeJC_S)
summary(interaction)
test1 <- c(-1, 0, 1, 0)
test2 <- c(0, -1, 0, 1)
summary(contrast(ref_pckgeJC_S, list(ageY_sex = test1, ageO_sex = test2)))


#6. pckge = Group 일때
pckgeJC_G_aov <- aov(stsfctn_all ~ sex*age, data = pckgeJC_G)
summary(pckgeJC_G_aov)
mytable(sex~stsfctn_all, data = pckgeJC_G)
mytable(age~stsfctn_all, data = pckgeJC_G)

##주효과
#sex
mytable(sex~stsfctn_all, data = pckgeJC_G)
mean_pckgeJC_G_sex <- pckgeJC_G %>%
  group_by(sex) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_pckgeJC_G_sex <- pckgeJC_G %>%
  group_by(sex) %>%               
  summarise(sd(stsfctn_all))
table(pckgeJC_G$sex) # n값
mean_pckgeJC_G_sex # mean 값
sd_pckgeJC_G_sex # sd 값

#age
mytable(age~stsfctn_all, data = pckgeJC_G)
mean_pckgeJC_G_age <- pckgeJC_G %>%
  group_by(age) %>%               
  summarise(mean_satisfaction = mean(stsfctn_all))
sd_pckgeJC_G_age <- pckgeJC_G %>%
  group_by(age) %>%               
  summarise(sd(stsfctn_all))
table(pckgeJC_G$age) # n값
mean_pckgeJC_G_age # mean 값
sd_pckgeJC_G_age # sd 값


##그래프
library(lsmeans)
ref_pckgeJC_G <- lsmeans(pckgeJC_G_aov, specs = c("sex","age"))
ref_pckgeJC_G_df <- as.data.frame(summary(ref_pckgeJC_G))
library(ggplot2)
pd16 <- position_dodge(0.1)
g416 <- ggplot(ref_pckgeJC_G_df, aes(x=age, y=lsmean, group=sex, colour=sex))+geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd16)+geom_line(position=pd16)+geom_point(position=pd16)
print(g416)