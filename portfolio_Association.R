###################################################
### 연관분석
###################################################
library(arules)
tour <- read.csv("2013 ~ 2017_F.csv", header = TRUE)
library(dplyr)

####################################################################
#그룹별 관광지 연관분석
tourist <- tour

tourist$group[tourist$nat == 1 & tourist$sex == 1] <- 1

tourist$group[tourist$nat == 1 & tourist$sex == 2] <- 2

tourist$group[tourist$nat == 2 & tourist$age <= 2] <- 3

tourist$group[tourist$nat == 2 & tourist$age > 2 ] <- 4
tourist_1 <- tourist %>%
  filter(nat==1|nat==2)
tourist_1 <- tourist_1 %>%
  filter(age < 7)#나이


tourist_2 <- tourist_1$N2_1
tourist_2 <- data.frame(tourist_2,tourist_1$group)
colnames(tourist_2)=c("site","group")
for (i in 1:2){
  tourist_2[,i] <- as.factor(tourist_2[,i])
}
tourist_2 <- tourist_2 %>% 
  filter(site==1|site==2|site==3|site==4|site==5|site==6|site==7|site==8|site==9|site==10
         |site==11|site==12|site==13|site==14|site==15|site==16|site==17|site==18|site==19|site==20)
#===============================================================================
tour_141 <- as(tourist_2, "transactions")#Transactions 데이터로 저장

###################################################
### code chunk number 17: summary
###################################################
itemFrequencyPlot(tour_141, support = 0.1, cex.names=0.8)# support =0.1 일때 연관성분석

###################################################
### code chunk number 20: apriori
###################################################
rules <- apriori(tour_141, 
                 parameter = list(support = 0.01, confidence = 0.01, minlen = 1))#minlen option=하나의 상품만 구입하는 경우를 제거/ex)minlen = 1(1개짜리를 제거)
rules

###################################################
### code chunk number 21: summary
###################################################
summary(rules)
inspect(sort(rules, by = "count"))

###################################  관광지별 연관분석  #########################################

rulesSite1 <- subset(rules, subset = rhs %in% "site=1")
inspect(head(rulesSite1, n = 1, by = "confidence"))
rulesSite2 <- subset(rules, subset = rhs %in% "site=2")
inspect(head(rulesSite2, n = 1, by = "confidence"))
rulesSite3 <- subset(rules, subset = rhs %in% "site=3")
inspect(head(rulesSite3, n = 1, by = "confidence"))
rulesSite4 <- subset(rules, subset = rhs %in% "site=4")
inspect(head(rulesSite4, n = 1, by = "confidence"))
rulesSite5 <- subset(rules, subset = rhs %in% "site=5")
inspect(head(rulesSite5, n = 1, by = "confidence"))
rulesSite6 <- subset(rules, subset = rhs %in% "site=6")
inspect(head(rulesSite6, n = 1, by = "confidence"))
rulesSite7 <- subset(rules, subset = rhs %in% "site=7")
inspect(head(rulesSite7, n = 1, by = "confidence"))
rulesSite8 <- subset(rules, subset = rhs %in% "site=8")
inspect(head(rulesSite8, n = 1, by = "confidence"))
rulesSite9 <- subset(rules, subset = rhs %in% "site=9")
inspect(head(rulesSite9, n = 1, by = "confidence"))
rulesSite10 <- subset(rules, subset = rhs %in% "site=10")
inspect(head(rulesSite10, n = 1, by = "confidence"))
rulesSite11 <- subset(rules, subset = rhs %in% "site=11")
inspect(head(rulesSite11, n = 1, by = "confidence"))
rulesSite12 <- subset(rules, subset = rhs %in% "site=12")
inspect(head(rulesSite12, n = 1, by = "confidence"))
rulesSite13 <- subset(rules, subset = rhs %in% "site=13")
inspect(head(rulesSite13, n = 1, by = "confidence"))
rulesSite14 <- subset(rules, subset = rhs %in% "site=14")
inspect(head(rulesSite14, n = 1, by = "confidence"))
rulesSite15 <- subset(rules, subset = rhs %in% "site=15")
inspect(head(rulesSite15, n = 1, by = "confidence"))
rulesSite16 <- subset(rules, subset = rhs %in% "site=16")
inspect(head(rulesSite16, n = 1, by = "confidence"))
rulesSite17 <- subset(rules, subset = rhs %in% "site=17")
inspect(head(rulesSite17, n = 1, by = "confidence"))
rulesSite18 <- subset(rules, subset = rhs %in% "site=18")
inspect(head(rulesSite18, n = 1, by = "confidence"))
rulesSite19 <- subset(rules, subset = rhs %in% "site=19")
inspect(head(rulesSite19, n = 1, by = "confidence"))
rulesSite20 <- subset(rules, subset = rhs %in% "site=20")
inspect(head(rulesSite20, n = 1, by = "confidence"))