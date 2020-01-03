library(dplyr)
library(factoextra)
library(FactoMineR)
#파일 불러오기
tour <- read.csv("2013 ~ 2017_F.csv", header = TRUE)

#그룹지정
tourist <- tour

tourist$group[tourist$nat == 1 & tourist$sex == 1] <- 1

tourist$group[tourist$nat == 1 & tourist$sex == 2] <- 2

tourist$group[tourist$nat == 2 & tourist$age <= 2] <- 3

tourist$group[tourist$nat == 2 & tourist$age > 2 ] <- 4
tourist_1 <- tourist %>%
  filter(nat==1|nat==2)
tourist_1 <- tourist_1 %>%
  filter(age < 7)#나이 결측치 제거


tourist_2 <- tourist_1$N2_1
tourist_2 <- data.frame(tourist_2,tourist_1$group)
colnames(tourist_2)=c("site","group")
#factor처리
for (i in 1:2){
  tourist_2[,i] <- as.factor(tourist_2[,i])
}

str(tourist_2)
tourist_2 <- (as.data.frame(table(tourist_2)))
library(reshape)
tourist_2 <- cast(tourist_2,site~group,drop=FALSE)
tourist_2 <- tourist_2[1:20,]#관광지 20번까지 자르기

res.ca <- CA(tourist_2[,-1], graph = FALSE)
#그래프 그리기
fviz_ca_biplot(res.ca ,ylim=c(-1,1), xlim=c(-1,1), repel=TRUE)
fviz_ca_biplot(res.ca, map ="rowprincipal", arrow = c(TRUE, TRUE))

