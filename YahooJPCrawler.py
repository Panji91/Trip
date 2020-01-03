from bs4 import BeautifulSoup
import requests
import pymysql
import time
import datetime as dt
from googletrans import Translator


def last_day_of_month(any_day): # startDate의 마지막 일수 찾기
    next_month = any_day.replace(day=28) + dt.timedelta(days=4)
    return next_month - dt.timedelta(days=next_month.day)


############## GLOBAL VAR ##############
pulse = 0
headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36'}
translator = Translator(service_urls=[
        'translate.google.com',
        'translate.google.co.kr'
    ])


def initYahoo(search, startDate, whileDate, endDate):
    url = "https://blogs.yahoo.co.jp/SEARCH/index.html?p={query}"\
        .format(query=search)
    req = requests.get(url, headers=headers)
    bs = BeautifulSoup(req.text, 'html.parser')
    time.sleep(1)
    btnTmp = bs.select("#pagenav > ul > li > a");
    getUrls(url, startDate, whileDate, endDate, btnTmp)


def getUrls(url, startDate, whileDate, endDate, btnTmp):
    btn = ""
    for i in btnTmp: btn += i.text
    if "次へ" in btn: pulse = 0 # 다음 버튼이 있으면 pulse 0
    else: pulse = 1             # 없으면 pulse 1
    pages = 1
    while startDate != endDate: # startDate가 12월일때 1월로 넘어가면 발생하는 문제 처리해야함
        if pulse == 1: break # 다음 버튼이 없으면 종료
        print("시작 달 :", startDate.month, type(startDate.month))
        print("끝 달 :", endDate.month, type(endDate.month))
        curUrl = url + "&pt=c&so=gd&tflg=date&datef_y={startYear}&datef_m={startMonth}&datef_d={startDay}"\
                       "&datet_y={whileYear}&datet_m={whileMonth}&datet_d={whileDay}&page={pages}"\
            .format(pages=pages,
                    startYear=startDate.year,
                    startMonth=startDate.month,
                    startDay=startDate.day,
                    whileYear=whileDate.year,
                    whileMonth=whileDate.month,
                    whileDay=whileDate.day)
        req = requests.get(curUrl, headers=headers)
        bs = BeautifulSoup(req.text, "html.parser")
        print(curUrl)

        #tmpUrls = bs.select("#artlst > ul > li > div > div.textContentsWrap > div.articleTitle > a"); urlList = []
        tmpUrls = bs.select(".textContentsWrap .articleTitle a"); urlList = []
        for i in tmpUrls:
            urlList.append(i.get("href"))

        getYHPosts(urlList)
        pages += 1
        startDate = whileDate
        startDate += dt.timedelta(days=1)
        print("다음달로 넘어간 startDate :", startDate)
        whileDate = last_day_of_month(startDate)
        print("다음 달의 마지막 일수 whileDate :", whileDate)

        btnTmp = bs.select("#pagenav > ul > li > a"); btn = ""
        for i in btnTmp: btn += i.text
        if "次へ" in btn: pulse = 0  # 다음 버튼이 있으면 pulse 0
        else: pulse = 1  # 없으면 pulse 1
        print(btnTmp)
        print(pulse)


def getYHPosts(urlList):
    for post in urlList:
        req = requests.get(post, headers=headers)
        bs = BeautifulSoup(req.text, "html.parser")
        time.sleep(1)

        url = post; print("주소 :", url)
        try:
            title = bs.select_one("#atcllst > div > div.clearFix.entryTitle > h2 > a > span").text; print("제목 :", title)
        except:
            title = bs.select_one("#mainContentsArea > div.blogMainContents.userDefEntry > div.entry > div.entryHeader.cf.entryTitle > h2 > a > span").text
            print("제목 :", title)
        try:
            user = bs.select_one("#modules > dl > dt > a").text; print("유저명 :", user)
        except:
            user = bs.select_one("#blogSideColumnArea > dl > dt > a").text; print("유저명 :", user)
        date = bs.select_one("li.date > a > span").text
        stPoint = date.rfind("(")
        date = date[:stPoint]
        date = date.replace("/", "-"); print("게시일 :", date)
        try:
            body = translator.translate(bs.select_one("div.entryBody > table > tr > td").text, dest="ko").text
        except:
            print("본문 에러. . .")
            continue
        print("본문 :", body)
        print()

        dbData = [[url, title, user, date, body]]
        connectDB(dbData)


def connectDB(dbData):
    DB_HOST = '127.0.0.1'
    DB_USER = 'root'
    DB_PASSWD = 'autoset'
    DB_NAME = 'pyc'

    conn = pymysql.connect(host=DB_HOST, user=DB_USER, password=DB_PASSWD,
                           db=DB_NAME, charset='utf8')
    curs = conn.cursor()

    sql = """insert into yahooJP2010(url, title, user, date, body)
         values (%s, %s, %s, %s, %s)"""

    curs.executemany(sql, dbData)
    conn.commit()
    conn.close()


if __name__ == "__main__":
    y = 2010; m = 1; d = 1
    startDate = dt.date(year=y, month=m, day=d)
    whileDate = last_day_of_month(startDate)
    endDate = dt.date(year=y, month=1, day=31)
    # %B4%DA%B9%F1%CE%B9%B9%D4 : 한국여행
    search = "%B4%DA%B9%F1%CE%B9%B9%D4"
    initYahoo(search, startDate, whileDate, endDate)