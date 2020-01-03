from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import requests
from bs4 import BeautifulSoup
import pymysql
import time
import datetime as dt
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver import ActionChains
from googletrans import Translator

def connectDB(dbData):
    DB_HOST = '127.0.0.1'
    DB_USER = 'root'
    DB_PASSWD = 'autoset'
    DB_NAME = 'pyc'

    conn = pymysql.connect(host=DB_HOST, user=DB_USER, password=DB_PASSWD,
                           db=DB_NAME, charset='utf8')

    sql = """insert into twitter2006(userName, date, body) values (%s, %s, %s)"""
    curs = conn.cursor()
    curs.executemany(sql, dbData)
    conn.commit()
    conn.close()

def getDriver(keyword):
    options = webdriver.ChromeOptions()
    options.add_argument('headless')
    options.add_argument('disable-gpu')
    driver = webdriver.Chrome("C:/chromedriver.exe", chrome_options=options)
    y = 2006; m = 1; d = 1
    startDate = dt.date(year=y, month=m, day=d)
    untilDate = dt.date(year=y, month=m, day=d+1)
    endDate   = dt.date(year=y, month=12, day=31)
    print("Crawling. . . 현재 날짜 : ", startDate)

    while not startDate == endDate:
        url = "https://twitter.com/search?f=tweets&vertical=default&q={query} since:{startDate} until:{untilDate}"\
            .format(query=keyword,
                    startDate=startDate,
                    untilDate=untilDate)
        driver.get(url)
        driver.implicitly_wait(10)

        # actionChains = ActionChains(driver) # 마우스 액션 변수 생성
        # actionChains.context_click().send_keys(Keys.ARROW_DOWN).perform() # 우클릭 후 키보드 아래 키 누름. 마지막에 perform() 안하면 수행 안됨
        # time.sleep(1)

        startDate = untilDate
        untilDate += dt.timedelta(days=1)

        last_height = driver.execute_script("return document.body.scrollHeight")
        # for i in range(0, 2):
        while True:
            # if 다시시도 == True: 다시시도 클릭

            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(1)

            new_height = driver.execute_script("return document.body.scrollHeight")
            if new_height == last_height:
                break
            last_height = new_height

        getTweet(driver)


def getTweet(driver):
    html = driver.page_source
    bs = BeautifulSoup(html, "html.parser")
    translator = Translator(service_urls=[
        'translate.google.com',
        'translate.google.co.kr'
    ])

    eachUrl = bs.select("#stream-items-id li")
    for ea in eachUrl:
        try:
            driver.implicitly_wait(10)
            userName = ea.select_one("div div.content div.stream-item-header a").text.replace("\n","").strip(); print("유저명 :", userName)
            date = ea.select_one("div div.content div.stream-item-header small a span").text.replace("\n","").strip(); print("게시일 :", date)
            body = ea.select_one("div div.content div.js-tweet-text-container p").text.replace("\n","").strip()

            body = translator.translate(body, dest="ko").text
            print("본문 :", body)

            dbData = [[userName, date, body]]
            connectDB(dbData)
        except:
            continue

    #driver.close()


if __name__ == "__main__":
    # keyword = input("검색 쿼리 입력 : ")
    #韓国旅行 한국여행
    keyword = "韓国旅行"
    getDriver(keyword)