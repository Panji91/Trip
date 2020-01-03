options(java.parameters=c("-Xmx12g"))

library(KoNLP) 
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
library(rJava) 
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(readxl)
library(stringr)
library(tidyverse)
library(tibble)
library(arules) 
library(combinat) 
library(tm) 
library(proxy) 
library(DBI)
library(RMySQL)
library(stringr)
useSejongDic()
useNIADic()

oCon <- dbConnect(MySQL(), user="root", password="autoset",dbname="title", host="127.0.0.1")
dbListTables(oCon)
#먼저실행후 데이터셋불러오기, console
dbGetQuery(oCon,"set names utf8")
food_0615 = dbGetQuery(oCon, "select * from twitter_kfood0615");
dbDisconnect(Con)

#names(dbData_2017)[3] = c("contents")

colnames(food_0615)
food_0615$content <- iconv(food_0615$content, "utf8", "UTF-8")
food_0615$date <- iconv(food_0615$date, "utf8", "UTF-8")
food_0615$content[1:10]
food_0615$date <- as.numeric(food_0615$date, "utf8", "UTF-8")
food_0615 <- na.omit(food_0615)
food_0615$id <- as.factor(food_0615$no)


food_0615$date <- gsub(' 1월','01월',food_0615$date)
food_0615$date <- gsub(' 2월','02월',food_0615$date)
food_0615$date <- gsub(' 3월','03월',food_0615$date)
food_0615$date <- gsub(' 4월','04월',food_0615$date)
food_0615$date <- gsub(' 5월','05월',food_0615$date)
food_0615$date <- gsub(' 6월','06월',food_0615$date)
food_0615$date <- gsub(' 7월','07월',food_0615$date)
food_0615$date <- gsub(' 8월','08월',food_0615$date)
food_0615$date <- gsub(' 9월','09월',food_0615$date)
food_0615$date <- gsub('년','',food_0615$date)
food_0615$date <- gsub('월','',food_0615$date)
food_0615$date <- gsub('일','',food_0615$date)
food_0615$date <- gsub(' ','',food_0615$date)
food_0615$date <- str_trim(food_0615$date, side = c("both"))

food_0615$year <- str_sub(food_0615$date, 1, 4)


food_2017 <- food_0615%>%
  filter(year == '2017')

food_2016 <- food_0615%>%
  filter(year == '2016')

food_2015 <- food_0615%>%
  filter(year == '2015')

food_2014 <- food_0615%>%
  filter(year == '2014')

food_2013 <- food_0615%>%
  filter(year == '2013')

food_2017$contents <- gsub('[~!@#$%&*.()_+=?<>]',' ',food_2017$contents)
food_2017$contents <- gsub("에게"," ",food_2017$contents)
food_2017$contents <- gsub("했기"," ",food_2017$contents)
food_2017$contents <- gsub("합시다"," ",food_2017$contents)
food_2017$contents <- gsub("했습니다"," ",food_2017$contents)
food_2017$contents <- gsub("했다"," ",food_2017$contents)
food_2017$contents <- gsub("합니다"," ",food_2017$contents)
food_2017$contents <- gsub("입니다"," ",food_2017$contents)
food_2017$contents <- gsub("싸고"," ",food_2017$contents)
food_2017$contents <- gsub("으므로"," ",food_2017$contents)
food_2017$contents <- gsub("를"," ",food_2017$contents)
food_2017$contents <- gsub("하세요"," ",food_2017$contents)
food_2017$contents <- gsub("하는"," ",food_2017$contents)
food_2017$contents <- gsub("있는"," ",food_2017$contents)
food_2017$contents <- gsub("하게"," ",food_2017$contents)
food_2017$contents <- gsub("한국의"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("한국"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("일본"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("문제"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("자신"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("그것"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("때문"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("오늘"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("정도"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("사람"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("생각"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("이번"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("하면"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("다음"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("하면"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("무엇"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("우리"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경우"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("어제"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("네요"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("여기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("잔소리"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("장황"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("관계자"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("사이"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("오후"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("결정"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경우"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("최고"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("하게"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("경기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("하나"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("부터"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("브로를"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("캬리"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("파뮤"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("무료"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("다음"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("기다리고"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("라든지"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("있습니다."," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("있으면"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("수없는"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("맛있었습니다"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("히가시"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("오오쿠보에서","오오쿠보", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("개월에"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("않으면"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("맛있게"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("어떻습니까"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("있고,"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("마시세요"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("즐기세요"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("요리도"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("요리는"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("먹으러"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("본고장의"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("개최되는"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("추첨으로"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("신바시"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("요리"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("유명"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("일품"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("세계"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("점심"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("웃음"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("내점"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("자랑"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("연회"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("개최"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("사용"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("쿠폰"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("최대"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("에서"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("맛있었"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("이쪽"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("이용"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("다양한"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("하마"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("마츠"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("웃음"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("최초"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("역근"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("음식"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("합작"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("닭 갈비","닭갈비", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("환 영회 ","환영회 ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("독실"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("안녕"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("이온"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("오랜만"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("당점"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("준비"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("여자"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("김치 찌개","김치찌개", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("김치 찌개 ","김치찌개 ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("전통"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("본고장"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("바시"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("볼륨"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("도보"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("좌석"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("세트"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("도보"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("전통의"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("치즈 닭갈비","치즈닭갈비", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("오미야"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("광장"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("양념"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("인기"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("대문"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("여행"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("도시"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("친구"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("사진"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("사랑"," ", food_2017$contents ,ignore.case = TRUE)
food_2017$contents <- gsub("런치"," ", food_2017$contents ,ignore.case = TRUE)

food_2017$contents <- gsub("[a-zA-Z]"," ",food_2017$contents)



food_2017$contents <- gsub("   "," ", food_2017$contents ,ignore.case = TRUE)

food_2017 <- food_2017 %>% 
  filter(nchar(contents) < 200)

food_2017 <- food_2017 %>% 
  filter(nchar(contents) > 0)

food_2017 <- na.omit(food_2017)


###################################################################################################################
###################################################################################################################
sword <- readLines(".//topic//stopword.txt", encoding="UTF-8")
sword <- unique(sword) # 중복제거
#sword <- iconv(sword, "utf8", "UTF-8")
sword <- gsub("\uFEFF", "", sword)

ko.words <- function(doc){
  d <- as.character(doc)
  #pos <- str_split(d,";")## 띄어쓰기(' ')를 기준으로 한 문장을 여러 단어로 나눔
  pos <- paste(SimplePos22(d))
  #extracted <- str_match(pos, "([가-힣a-zA-Z]+)/(NC|PA|PV)")#명사,동사, 형용사 동시에 가져옴
  extracted <- str_match(pos, "([가-힣a-zA-Z]+)/(NC)")#명사,동사, 형용사 동시에 가져옴
  
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
options(mc.cores=1)
library(tm)
foodCorpus<- VCorpus(VectorSource(food_2017$content))#형태소 분석이 된 데이터를 corpus로 만든다음, tdm 작성
foodCorpus[[1]]$content
tdm <- TermDocumentMatrix(foodCorpus,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 10)))

# tdm <- TermDocumentMatrix(foodCorpus, control = list(tokenize="scan",
#                                                            stopwords=sword,
#                                                            wordLength=c(2,7)))
food_tdm <- tdm
#inspect(yahoo_tdm)
#food_tdm <- removeSparseTerms(tdm, sparse = 0.99)
wordFreq <- slam::row_sums(food_tdm)
wordFreq <- sort(wordFreq,decreasing=TRUE)
wordFreq_count <- head(sort(wordFreq,decreasing=TRUE),200) ## 확인용
wordFreq_count

library(wordcloud)
library(SnowballC)
library(wordcloud2)
dev.new(width = 1000, height = 1000, unit = "px")
display.brewer.all()
color <- brewer.pal(8, "Paired")

w <- names(wordFreq_count)
wordFreq_count_df <- as.data.frame(wordFreq_count)
wordFreq_count_c <- cbind(w,wordFreq_count_df)
colnames(wordFreq_count_c) <- c('word','freq')

windowsFonts(font=windowsFont("맑은고딕"))

wordcloud2(data = wordFreq_count_c, size = 1.5, 
           fontFamily = 'Noto Sans CJK KR Bold', fontWeight = 'bold', shape = 'circle')
##############################################################################################################
###############################################################################################################
################################################################################################################
food_tdm_rm <- removeSparseTerms(food_tdm, sparse = 0.99)
food_tdm_mat <- as.matrix(food_tdm_rm)
food_tdm_mat <- as.matrix(food_tdm)
v <- sort(slam::row_sums(food_tdm_mat), decreasing = T)
data_v <- data.frame(word=names(v),freq=v)
word_weibo <-head(sort(data_v, decreasing = T),100)
#word_weibo <- data.frame(word=names(v),freq=v)
word_weibo
wordcloud2(data = data_v, size = 1.5, 
           fontFamily = 'Noto Sans CJK KR Bold', fontWeight = 'bold', shape = 'circle')
##############################################################################################################
###############################################################################################################
################################################################################################################

