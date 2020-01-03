library(dplyr)


#######################################################################
#############################만족도 회귀###############################
#######################################################################


#######################################################################
#13년 데이터 회귀, 일본, 중국 따로 회귀
tour_1317 <- read.csv("2013 ~ 2017_F.csv")
tour_13 <- tour_1317 %>% 
  filter(year == 2013) %>% 
  filter((T2 < 8) & (T1_1 < 8) & (T1_2 < 8) & (T1_3 < 8) & (T1_4 < 8) & (T1_5 < 8) & (T1_6 < 8) & (T1_7 < 8) & (T1_8 < 8) & (T1_9 < 8) & (T1_10 < 8))
tour_satis13 <- tour_13[,132:142]
summary(tour_satis13)
#######################################################################


#######################################################################
#전세계 만족도
lm_satis13 <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_satis13)
summary(lm_satis13)
step(lm_satis13, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_satis13))

#일본관광객 만족도
tour_jap <- tour_13 %>% 
  filter(nat == 1)
tour_jap13 <- tour_jap[,132:142]
summary(tour_jap13)
lm_satis13jap <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap13)
step(lm_satis13jap, direction="backward")
summary(lm(T2~T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap13))

#중국관광객 만족도
tour_chi <- tour_13 %>% 
  filter(nat == 2)
tour_chi13 <- tour_chi[,132:142]
summary(tour_chi13)
lm_satis13chi <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi13)
step(lm_satis13chi, direction="backward")
summary(lm(T2~T1_1+T1_3+T1_4+T1_5+T1_6+T1_7+T1_9+T1_10, data=tour_chi13))
#######################################################################


#######################################################################
#14년 데이터 회귀, 일본, 중국 따로 회귀
tour_14 <- tour_1317 %>% 
  filter(year == 2014) %>% 
  filter((T2 < 8) & (T1_1 < 8) & (T1_2 < 8) & (T1_3 < 8) & (T1_4 < 8) & (T1_5 < 8) & (T1_6 < 8) & (T1_7 < 8) & (T1_8 < 8) & (T1_9 < 8) & (T1_10 < 8))
tour_satis14 <- tour_14[,132:142]
summary(tour_satis14)
#######################################################################
#8번보다 큰 값(모름, 무응답)을 제거하고 분석
#library(dplyr)
#tour_satis14_1 <- tour_satis14%>%
#  filter((q13a01 < 8) & (q13a02 < 8) & (q13a03 < 8) & (q13a04 < 8) & (q13a05 < 8) & (q13a06 < 8) & (q13a07 < 8) & (q13a08 < 8) & (q13a09 < 8)
#         & (q13a10 < 8))
#aaaaa <- lm(q13~q13a01+q13a02+q13a03+q13a04+q13a05+q13a06+q13a07+q13a08+q13a10, data=tour_satis14_1)
#step(aaaaa, direction="backward")
#table(tour_satis14_1$q13)

#######################################################################
#전세계 만족도
lm_satis14 <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_satis14)
summary(lm_satis14)
step(lm_satis14, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_10, data=tour_satis14))

#일본관광객 만족도
tour_jap14 <- tour_14 %>% 
  filter(nat == 1)
tour_satis14jap <- tour_jap14[,132:142]
summary(tour_satis14jap)
lm_satis14jap <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap14)
step(lm_satis14jap, direction="backward")
summary(lm(T2~T1_1+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9, data=tour_jap14))

#중국관광객 만족도
tour_chi14 <- tour_14 %>% 
  filter(nat == 2)
summary(tour_chi14)
lm_satis14chi <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi14)
step(lm_satis14chi, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_10, data=tour_chi14))
#######################################################################


#######################################################################
#15년 데이터 회귀, 일본, 중국 따로 회귀
#table(tour_1317)
#summary(tour_satis1317)
tour_15 <- tour_1317 %>% 
  filter(year == 2015) %>% 
  filter((T2 < 8) & (T1_1 < 8) & (T1_2 < 8) & (T1_3 < 8) & (T1_4 < 8) & (T1_5 < 8) & (T1_6 < 8) & (T1_7 < 8) & (T1_8 < 8) & (T1_9 < 8) & (T1_10 < 8))
tour_satis15 <- tour_15[,132:142]
summary(tour_satis15)
#######################################################################


#######################################################################
#전세계 만족도
lm_satis15 <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_15)
summary(lm_satis15)
step(lm_satis15, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_15))


#일본관광객 만족도
tour_jap15 <- tour_15 %>% 
  filter(nat == 1)
tour_satis15jap <- tour_jap15[,132:142]
summary(tour_satis15jap)
lm_satis15jap <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap15)
step(lm_satis15jap, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_4+T1_5+T1_6+T1_8+T1_9+T1_10, data=tour_jap15))

#중국관광객 만족도
tour_chi15 <- tour_15 %>% 
  filter(nat == 2)
summary(tour_chi15)
lm_satis15chi <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi15)
step(lm_satis15chi, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_9+T1_10, data=tour_chi15))
#######################################################################


#######################################################################
#16년 데이터 회귀, 일본, 중국 따로 회귀
tour_16 <- tour_1317 %>% 
  filter(year == 2016) %>% 
  filter((T2 < 8) & (T1_1 < 8) & (T1_2 < 8) & (T1_3 < 8) & (T1_4 < 8) & (T1_5 < 8) & (T1_6 < 8) & (T1_7 < 8) & (T1_8 < 8) & (T1_9 < 8) & (T1_10 < 8))
tour_satis16 <- tour_16[,132:142]
summary(tour_satis16)
#######################################################################


#######################################################################
#전세계 만족도
lm_satis16 <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_16)
summary(lm_satis16)
step(lm_satis16, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_16))

#일본관광객 만족도
tour_jap16 <- tour_16 %>% 
  filter(nat == 1)
tour_satis16jap <- tour_jap16[,132:142]
summary(tour_satis16jap)
lm_satis16jap <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap16)
step(lm_satis16jap, direction="backward")
summary(lm(T2~T1_1+T1_3+T1_4+T1_5+T1_7+T1_9, data=tour_jap16))

#중국관광객 만족도
tour_chi16 <- tour_16 %>% 
  filter(nat == 2)
summary(tour_chi16)
lm_satis16chi <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi16)
step(lm_satis16chi, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi16))
#######################################################################


#######################################################################
#17년 데이터 회귀, 일본, 중국 따로 회귀
tour_17 <- tour_1317 %>% 
  filter(year == 2017) %>% 
  filter((T2 < 8) & (T1_1 < 8) & (T1_2 < 8) & (T1_3 < 8) & (T1_4 < 8) & (T1_5 < 8) & (T1_6 < 8) & (T1_7 < 8) & (T1_8 < 8) & (T1_9 < 8) & (T1_10 < 8))
tour_satis17 <- tour_17[,132:142]
summary(tour_satis17)
#######################################################################


#######################################################################
#전세계 만족도
lm_satis17 <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_17)
summary(lm_satis17)
step(lm_satis17, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_17))


#일본관광객 만족도
tour_jap17 <- tour_17 %>% 
  filter(nat == 1)
tour_satis17jap <- tour_jap17[,132:142]
summary(tour_satis17jap)
lm_satis17jap <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_jap17)
step(lm_satis17jap, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9, data=tour_jap17))

#중국관광객 만족도
tour_chi17 <- tour_17 %>% 
  filter(nat == 2)
summary(tour_chi17)
lm_satis17chi <- lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_8+T1_9+T1_10, data=tour_chi17)
step(lm_satis17chi, direction="backward")
summary(lm(T2~T1_1+T1_2+T1_3+T1_4+T1_5+T1_6+T1_7+T1_9+T1_10, data=tour_chi17))
#######################################################################