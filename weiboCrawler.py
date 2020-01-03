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
    DB_HOST = '192.168.0.20'
    DB_USER = 'root'
    DB_PASSWD = 'autoset'
    DB_NAME = 'seoul'

    conn = pymysql.connect(host=DB_HOST, user=DB_USER, password=DB_PASSWD,
                           db=DB_NAME, charset='utf8')

    sql = """insert into weibo2012(userName, date, body) values (%s, %s, %s)"""
    curs = conn.cursor()
    curs.executemany(sql, dbData)
    conn.commit()
    conn.close()

def scrollDown(driver, startDate, endDate, keyword, pulse):
    if pulse == 1:
        url = "https://s.weibo.com/weibo?q={query}&typeall=1&suball=1&timescope=custom:{endDate}:{endDate}&Refer=g" \
            .format(query=keyword,
                    endDate=endDate)
        driver.get(url)
        driver.implicitly_wait(10)

        last_height = driver.execute_script("return document.body.scrollHeight")
        while True:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(2)

            new_height = driver.execute_script("return document.body.scrollHeight")
            if new_height == last_height:
                break
            last_height = new_height

        getWeibo(driver)
        exit()

    url = "https://s.weibo.com/weibo?q={query}&typeall=1&suball=1&timescope=custom:{startDate}:{startDate}&Refer=g" \
        .format(query=keyword,
                startDate=startDate)
    driver.get(url)
    driver.implicitly_wait(10)
    # startDate += dt.timedelta(days=1)

    last_height = driver.execute_script("return document.body.scrollHeight")
    while True:
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(2)

        new_height = driver.execute_script("return document.body.scrollHeight")
        if new_height == last_height:
            break
        last_height = new_height

    getWeibo(driver)

def getDriver(keyword):
    options = webdriver.ChromeOptions()
    options.add_argument('headless')
    options.add_argument('disable-gpu')
    driver = webdriver.Chrome("./chromedriver.exe", chrome_options=options)
    y = 2011; m = 1; d = 1
    startDate = dt.date(year=y, month=m, day=d)
    endDate = dt.date(year=y, month=12, day=31)
    print("Crawling. . .")
    pulse = 0
    while True:
        if startDate == endDate:
            pulse = 1
            scrollDown(driver, startDate, endDate, keyword, pulse)
        scrollDown(driver, startDate, endDate, keyword, pulse)
        startDate += dt.timedelta(days=1)


def getWeibo(driver):
    html = driver.page_source
    bs = BeautifulSoup(html, "html.parser")
    translator = Translator(service_urls=[
        'translate.google.com',
        'translate.google.co.kr',
    ])

    master = bs.select("#pl_feedlist_index > div")[1].select("div")
    tmp = ""
    for ea in master:
        try:
            driver.implicitly_wait(10)
            userName = ea.select(".card-feed .content .info div")[1].select("a")[0].text.replace("\n","").strip();
            date = ea.select_one(".from").text.replace("\n","")\
                .replace("年", "-").replace("月", "-").replace("日", "").strip()
            date = date[:10]
            body = ea.select_one(".txt").text.replace("\n","").strip()
            if body == tmp: continue
            tmp = body

            body = translator.translate(body, dest="ko").text
            print("유저명 :", userName)
            print("게시일 :", date)
            print("본문 :", body)
            print("\n")

            dbData = [[userName, date, body]]
            connectDB(dbData)
        except:
            continue


if __name__ == "__main__":
    # keyword = input("검색 쿼리 입력 : ")
    #韩国旅游 한국여행(중국어 간체)
    keyword = "韩国旅游"
    getDriver(keyword)
