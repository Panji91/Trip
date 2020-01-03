options(java.parameters=c("-Xmx12g"))
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')

library(KoNLP)
library(tidyverse)
library(tibble)
library(tidytext)
library(broom)
library(topicmodels)
library(rJava)
library(arules) 
library(igraph) 
library(combinat) 
library(tm) 
library(proxy) 
library(stringr)
library(readxl)
library(dplyr)
library(RMySQL)
library(readr)
library(slam)
useSejongDic() 
useNIADic()

con <- dbConnect(MySQL(), user="root", password="autoset",dbname="title", host="127.0.0.1")
dbListTables(con)
dbGetQuery(con,"set names utf8") 
yahoojp_2018 = dbGetQuery(con, "select * from yahoojp2018_dd");
colnames(yahoojp_2018)
yahoojp_2018$contents <- iconv(yahoojp_2018$contents, "utf8", "UTF-8")
yahoojp_2018$date <- iconv(yahoojp_2018$date, "utf8", "UTF-8")
yahoojp_2018$user <- iconv(yahoojp_2018$user, "utf8", "UTF-8")


#yahoojp_2018 <- read.csv("D:/r_data/yahoojp_18.csv",stringsAsFactors = F,h=F)


#rm(yahoojp_2015)
dbDisconnect(con)
colnames(yahoojp_2018)
#names(yahoojp_2018) <- c("no","url","user","date","contents")
yahoojp_2018$id <- as.factor(yahoojp_2018[,c(1)])

yahoojp_2018 <- yahoojp_2018[,c(5:6)]
colnames(yahoojp_2018)

rm(yahoopus)
yahoopus <- yahoojp_2018$contents
yahoopus <- gsub("konest","코네스트",yahoopus)
yahoopus <- gsub("코 네스트","코네스트" ,yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("있습니"," " ,yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("여러분"," " ,yahoopus ,ignore.case = TRUE)
#yahoopus <- gsub("[a-zA-Z]"," " ,yahoopus ,ignore.case = TRUE)


yahoopus <- gsub("\\d+"," " , yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("[^가-힣]", " ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub('[~!@#$%&*()_+=?<>■▶]',' ',yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("서인에"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("마지막"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("일본에"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("가운데"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("오랜만"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("일본의"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("한국의"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("한국"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("일본"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("문제"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("자신"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("그것"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("때문"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("오늘"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("정도"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("사람"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("생각"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("이번"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("하면"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("다음"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("하면"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("무엇"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("우리"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경우"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("어제"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("네요"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("여기"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("잔소리"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("장황"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("관계자"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("사이"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("오후"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경기"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("결정"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경기"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경우"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경기"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("최고"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("하게"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("경기"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("하나"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("부터"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("브로를"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("캬리"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("파뮤"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("무료"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("다음"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("삼일전일일이일삼일사일오일육일칠일팔일구일십일십일일십이일십삼일십사일십오일십육일십칠일십팔일십구일이십일이십일일이십이일이십삼일이십사일이십오일이십육일이십칠일이십팔일이십구일삼십일삼십일일하루"," ", yahoopus ,ignore.case = TRUE)
yahoopus <- gsub("년월일"," ", yahoopus ,ignore.case = TRUE)

yahoopus <- gsub("   ","", yahoopus ,ignore.case = TRUE)

yahoopus[1:10]

########################################################################
######################불용어 사전#####################################################
sword <- readLines(".//topic//stopword.txt", encoding="UTF-8")
sword <- unique(sword) # 중복제거
sword <- gsub("\uFEFF", "", sword)


sword <- paste(sword, collapse = "|")

#yahoopus_s[[1]]$content
##################################################################################3
# 주제에 맞는 사용자 사전 입력
##################################################################################33333333

newWord <- readLines(".//topic//conword.txt", encoding="UTF-8")
newWord <- gsub("<U+FEFF>", "", newWord)

newDic <- data.frame(newWord, "ncn")
buildDictionary(ext_dic = c("sejong","woorimalssm","insighter"), user_dic=newDic,replace_usr_dic = T)


for(i in seq_along(sword)) {
  yahoopus <- stringr::str_replace_all(yahoopus, fixed(sword)," ") 
}

##################################################################################3
# # 제목에서 두 칸 이상의 빈 공간, 특수문자, 한자 등을 제거합니다.
# title <- str_replace_all(title, pattern="\r", replacement="") %>%
#   str_replace_all(pattern="\n", replacement=" ") %>%
#   str_replace_all(pattern="[\u3000]", replacement="") %>%
#   str_replace_all(pattern="[  ]{2}", replacement="") %>%
#   str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
#   str_replace_all(pattern="[\u4E00-\u9FD5○]", replacement="") %>%
#   str_trim(side="both")

#########################################################################################
#전처리 끝
######################################################################################



### 문장길이 1000이하만 저장
yahoopus1 <- Filter(function(x) {nchar(x) <= 1000} ,yahoopus)

yahoopus_s[[40]]$content ## 확인용

# Term-Document Matrix는 각 단어가 행을, 각 텍스트가 열을 이루는 구조를 지닙니다
# (transpose하면 Document-Term Matrix가 됨). 
# yahoo_tdm <- TermDocumentMatrix(yahoopus_s, control = list(tokenize="scan",
#                                                            wordLength=c(2,7)))

#########################################################################################
#########################################################################################
## 명사추출함수
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos22(d))
  extracted <- str_match(pos, "([가-힣a-zA-Z]+)/(NC)")#명사 가져옴
  
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
options(mc.cores=1) ## 옵션값, 속도위해 
yahoopus_s <- VCorpus(VectorSource(yahoopus1))#형태소 분석이 된 데이터를 corpus로 만든다음, tdm 작성

## tdm변환
tdm <- TermDocumentMatrix(yahoopus_s,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 10))) 

View(tdm)
####################################################################################################
####################################################################################################

yahoo_tdm <- tdm
yahoo_tdm <- removeSparseTerms(yahoo_tdm, sparse = 0.99) ##단어 제거, 나오는 정도가 1%미만제거

#inspect(yahoo_tdm)

# nTerms(yahoo_tdm)
# nDocs(yahoo_tdm)
# Terms(yahoo_tdm)
# yahoo_tdm <- removeSparseTerms(yahoo_tdm, sparse = 0.99)
# wordFreq <- slam::row_sums(yahoo_tdm)
# wordFreq <- sort(wordFreq,decreasing=TRUE)
# wordFreq
# wordFreq_top <- head(sort(wordFreq,decreasing=TRUE),400)

dtm <- as.DocumentTermMatrix(yahoo_tdm)  ## dtm변환
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]    ## 빈도 1이상만 포함
dtm <- dtm.new
inspect(dtm)
View(inspect(dtm))

library(topicmodels)
## lda실시 k는 토픽갯수
lda <- LDA(dtm, k=15,control=list(seed=12345))

# term <- terms(lda, 20) #토픽별로 핵심단어20개 추출
# terms <- as.data.frame(terms(lda, 20), stringsAsFactors=FALSE)
# terms

summary(lda)


###########################그래프 찍기#############################
x <- posterior(lda)$terms # $terms와 $topics를 인덱싱할 수 있음
y <- data.frame(t(x[,apply(x,2,max)>0.03]))
z <- data.frame(type=paste("Topic", 1),
                keyword=rownames(y), posterior=y[,1])

## 토픽번호저장, 시각화위해, 토픽갯수에 맞게 조절
for (i in 2:14) {
  z <- rbind(z, data.frame(type=paste("Topic",i),
                           keyword=rownames(y), posterior=y[,i]))
  
}

ggplot(z,aes(keyword,posterior, fill=as.factor(keyword)))+
  geom_bar(position = "dodge", stat = "identity")+
  coord_flip()+
  facet_wrap(~type,nrow = 1) +
  theme(legend.position = "none")

#######################################################################################3
##########################################################################################
### 군집분석 
##################################################################
###################################################################
dev.new(width = 1000, height = 1000, unit = "px")
w1 <- names(wordFreq[1:30])
yahooCluste <- yahoo_tdm[Terms(yahoo_tdm) %in% w1,]
distmatrix <- dist(scale(yahooCluste))

distmatrix
yahoo_fit <- hclust(distmatrix, method = "ward.D")
plot(yahoo_fit, xlab = "", sub = "", main = "clustering keywords")
rect.hclust(yahoo_fit, k=5)

str(yahooCluste)
##########################################################################
###tibble dataset을 활용한 textmining package "tidytext"의 활용 
###########################################################################
#
#TF-IDF(Term Frequency - Inverse Document Frequency)
#
ap_td <- tidy(dtm)

ap_td

ap_td <- ap_td %>%
  count(document, term, sort = TRUE)%>%
  ungroup()
ap_td

total_words <- ap_td %>%
  group_by(document) %>%
  summarize(total=sum(n))

total_words

at_td <- left_join(ap_td, total_words, by="document")
at_td

freq_by_rank <-at_td %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

at_td <- at_td %>%
  bind_tf_idf(document, term, n)
at_td

at_td_tf_idf <- at_td %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

at_td_tf_idf
#TF-IDF(Term Frequency - Inverse Document Frequency)


at_td_tf_idf100 <- head(at_td, by = "tf_idf",100)
View(at_td_tf_idf100)


############################################################################
############################################################################
#LDA beta, gamma 구하기
####################################################################
## gamma 토픽자체가 문서에서 나올확율
## beta 단어가 토픽에서 나올 확률
############################################################################
############################################################################
############################################################################

library(tidytext)
#install.packages("broom")
library(broom)
yahoo_topics <- tidy(lda, matrix = "beta") #per-topic-per-word probability
yahoo_topics


## 단어 beta값 기준으로 50개 추출
yahoo_topic_top <- yahoo_topics %>% 
  group_by(topic) %>% 
  top_n(50, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

yahoo_topic_top

#시각화
yahoo_topic_top %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


## topic1 추출
yahoo_topics1 <- yahoo_topics%>%
  filter(topic == 1)%>%
  arrange(desc(beta))

# View(yahoo_topics1)
# View(yahoo_topics2)
# View(yahoo_topics3)
# View(yahoo_topics4)
# View(yahoo_topics5)
# View(yahoo_topics6)
# View(yahoo_topics7)
# View(yahoo_topics8)
# View(yahoo_topics9)
# View(yahoo_topics10)
# View(yahoo_topics11)
# View(yahoo_topics12)
# View(yahoo_topics13)
# View(yahoo_topics14)
# View(yahoo_topics15)

head(yahoo_topics1, by = "beta",100)

## topic2 추출
yahoo_topics2 <- yahoo_topics%>%
  filter(topic == 2)%>%
  arrange(desc(beta))

yahoo_topics2

## topic3 추출
yahoo_topics3 <- yahoo_topics%>%
  filter(topic == 3)%>%
  arrange(desc(beta))


## topic4 추출
yahoo_topics4 <- yahoo_topics%>%
  filter(topic == 4)%>%
  arrange(desc(beta))


## topic5 추출
yahoo_topics5 <- yahoo_topics%>%
  filter(topic == 5)%>%
  arrange(desc(beta))

yahoo_topics5

yahoo_topics6 <- yahoo_topics%>%
  filter(topic == 6)%>%
  arrange(desc(beta))

yahoo_topics7 <- yahoo_topics%>%
  filter(topic == 7)%>%
  arrange(desc(beta))

yahoo_topics8 <- yahoo_topics%>%
  filter(topic == 8)%>%
  arrange(desc(beta))

yahoo_topics9 <- yahoo_topics%>%
  filter(topic == 9)%>%
  arrange(desc(beta))

yahoo_topics10 <- yahoo_topics%>%
  filter(topic == 10)%>%
  arrange(desc(beta))

yahoo_topics11 <- yahoo_topics%>%
  filter(topic == 11)%>%
  arrange(desc(beta))

yahoo_topics12 <- yahoo_topics%>%
  filter(topic == 12)%>%
  arrange(desc(beta))

yahoo_topics13 <- yahoo_topics%>%
  filter(topic == 13)%>%
  arrange(desc(beta))

yahoo_topics14 <- yahoo_topics%>%
  filter(topic == 14)%>%
  arrange(desc(beta))

yahoo_topics15 <- yahoo_topics%>%
  filter(topic == 15)%>%
  arrange(desc(beta))



library(tidyr)


### topic 간에 beta값 비교
beta_spread_b53 <- yahoo_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic3 > .001 | topic5 > .001) %>%
  mutate(log_ratio53 = log2(topic5 / topic3)) ## greatest difference in Beta between topic5 vs topic3

beta_spread_b53

# Diverging Barcharts 토픽간 term비교
ggplot(beta_spread_b53, aes(y=log_ratio12, x=term)) + 
  geom_bar(stat='identity', width=.5)  +
  labs(subtitle="log_ratio12 = log2(topic2 / topic1)", 
       title= "Diverging Bars") + 
  coord_flip()

#####################################################################

#Document-topic probabilities
## gamma 토픽자체가 문서에서 나올확율
## beta 단어가 토픽에서 나올 확률

yahoo_documents_g <- tidy(lda, matrix = "gamma") ## gamma값
yahoo_documents_b <- tidy(lda, matrix = "beta")  ## beta값

yahoo_documents_g
yahoo_documents_b

yahoo_documents_bb <- head(yahoo_documents_b, by = "beta",100)
## 예시 
#해당 topic에서 생성 된 해당 document의 word=term의 예상 비율
# # A tibble: 304,260 x 3
# document topic  gamma
# <chr>    <int>  <dbl>
# 1 1            1 0.0508
# 2 3            1 0.0517 
# 3 10           1 0.0517
# 4 11           1 0.0517
# 5 12           1 0.0494
# 6 13           1 0.0517
# 7 15           1 0.0517
# 8 17           1 0.0508
# 9 19           1 0.0517
# 10 20           1 0.0498
#맨위의 1document의 각 단어는 topic1에서 오는 확률이 5.08%이다.
#

###문서6의 beta값 
yahoo_documents_b6 <- tidy(dtm) %>%
  filter(document == 6) %>%
  arrange(desc(count))

yahoo_documents_b6
## 예시 
##########################################################
# A tibble: 240 x 3
# topic term          beta
# <int> <chr>        <dbl>
#   1     1 가이드 0.00551    
# 2     2 가이드 0.000000615
# 3     3 가이드 0.00134    
# 4     4 가이드 0.136      
# 5     5 가이드 0.246      
# 6     6 가이드 0.00131    
# 7     7 가이드 0.000166   
# 8     8 가이드 0.00311    
# 9     9 가이드 0.000351   
# 10    10 가이드 0.000883  
#
#해당 TOPic에서 생성되는 term=word의 확률을 계산, 5번째topic에서 생성될 확률24.6%로 가장높음


##############################################################################################3

library(gtools)
library(scales)


#### We could use dplyr’s top_n() to find the top 5 terms within each topic.
#### hani_documents <- tidy(lda, matrix = "beta") 
top_terms <- yahoo_documents_b %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

## topic term 시각화
library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
library(ggraph)

yahoo_documents_b %>%
  filter(topic == 3 | topic == 2 | topic == 1 | topic == 4 | topic ==5 | topic ==6 | topic ==7 | topic ==8 |topic ==9  | topic ==10 | topic ==11 | topic ==12 | topic ==13 | topic ==14  | topic ==15 ) %>%
  top_n(150, beta) %>%
  #filter(correlation > .55) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = beta), show.legend = FALSE ) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#     Topic1  Topic2   Topic3  Topic4  Topic5  Topic6  Topic7  Topic8  Topic9  Topic10  Topic11  Topic12  Topic13  Topic14  Topic15
# 1     납치    미국    정부    여행    보도    이름    아침   드라마  블로그     남북   위안부     영화     북한     여성   올림픽
# 2   메구미    세계    연속    가게    기사  년월일  이야기     방송    문화   대통령     웃음     공개     아베     올해     여자
# 3     가족    조선    인정    요리    신문    정보    하늘     사랑    마을     합의     마음     촬영   김정은     지역     평창
# 4     북한    바보    요구    판결    조사    확인    사용     배우    기업     문장     소리     감독     미국     체포     개최
# 5     버전  러시아    사건    공항    깃발    내용    친구     출연    기술   한반도     동상   오사카   트럼프     혐의     대표
# 6   멧세지    의원    해결    호텔  자위대    회사    시작 프로그램    재일     협정     설치     안녕     총리   자동차     참석
# 7     생명    정치    장관    기업  아사히    공연    노래     연기    시장     평화     기억     영상   위원장     이상     동계
# 8     영어  조선인    백성    그룹    사과  이벤트    하루     모습    교육     군사     시간   분위기   대통령     해외     승리
# 9     인도    정부    학교    배상    사용    제공    아이     작품    교류     정부     인간   주지훈     북미     경찰     세계
# 10  피해자    중간  피해자    도착    해상    준비    얼굴     마음    산업     주한     부산     선물   미사일     전체     가압
# 11  납북자    자유    고노    서울    역사    티켓    나무     남자    클릭     연합     소년     기념   비핵화     남성   아시아
# 12    구출    이상    책임    김치  미디어    전화    머리     등장    투자     정상     남편     고객     제재     직원     응원
# 13    서약    시대    상태    음식    비판    부탁    구입     스타    구성     뉴스     시민   이야기     신조     조사     축구
# 14    탈환    무역    요청    청구    언론    예약    감사     한류  인터넷     서울     순간     축제   이야기     회사     우승
# 15  블로그    민족    지원    버스    기계    등록    저녁     활동    금지     공동   브랜드     감사     조선     기록     후원
# 16    유골    미군    관방    메뉴    일보    희망    바람     주연    사업     선언     제품     작품     회담   베트남     남자
# 17    정부    영국    이치    주식    뉴스    도쿄  어머니     뉴스    지원     협력     행복     공원     납치     태국     연습
# 18  까사기  한반도    활동    소송    설명    기간    색상     결혼    협회   문재인     시대     손님     장관     남자     획득
# 19    해결    존재    운동    돼지    주장    질문    다리     채널    통화     외교     자연   선생님     대화     외국     결승
# 20  북한인    백성    교체    주문    적재    연락  아버지     매력    건설     관계     약속     돌파     해결     도시     도쿄
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
library(Matrix)
#library(lda)
library(LDAvis)




##yahoopus <- vcorpus 적용 X
yahoopus1 <- Filter(function(x) {nchar(x) <= 1000} ,yahoopus)

DataContents_2011 <- sapply(yahoopus1, extractNoun, USE.NAMES = F)
head(DataContents_2011)


listdata_2011 <- unlist(DataContents_2011)
#View(listdata)

listdata_2011 <- Filter(function(x){nchar(x)>=2}, listdata_2011)
listdata_2011 <- Filter(function(x){nchar(x)<=6}, listdata_2011)
wordcount <- table(listdata_2011)
wordcount_sort <- sort(wordcount, decreasing = T)

vocab <- names(wordcount_sort)


get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(DataContents_2011, get.terms)


D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(wordcount_sort)

K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1 



### Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

results <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = vocab,
                term.frequency = term.frequency)

library(servr)
# create the JSON object to feed the visualization:
json <- createJSON(phi = results$phi, 
                   theta = results$theta, 
                   doc.length = results$doc.length, 
                   vocab = results$vocab, 
                   term.frequency = results$term.frequency)

serVis(json, out.dir = './vis')
system("mv index.html results.html")


