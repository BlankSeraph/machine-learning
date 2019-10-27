from selenium import webdriver
from selenium.webdriver import ActionChains
from selenium.webdriver.common.keys import Keys
import pymysql
import time
def getDriver():
    driver = webdriver.Chrome("/Users/wangweiguo/Documents/workspace/expedia/lib/chromedriver")
    return driver
getDriver()
driver.get('https://fwliuhaibo.haodf.com/zixun/list.htm')
name = driver.find_element_by_tag_name('h3').text
print(name)
keshi = driver.find_element_by_css_selector('.fl.pr').find_elements_by_tag_name('a')
print(keshi[0].text)
list_tr = driver.find_elements_by_tag_name('tr')
if keshi.__len__()==2:
    tmp1 = name +'   '+ keshi[0].text +'   '+  keshi[1].text
if keshi.__len__()==1:
    tmp1 = name +'   '+ keshi[0].text +'   '+ '0'
if keshi.__len__()==0:
    tmp1 = name + '   ' + '0' + '   ' + '0'
tmp = tmp1
tmp2 = ""
for i in range(1,list_tr.__len__()):

    tmp = tmp+list_tr[i].find_elements_by_tag_name('td')[1].find_element_by_tag_name('p').text
    print(list_tr[i].find_elements_by_tag_name('td')[1].find_element_by_tag_name('p').text)
    tmp = tmp +"    " +list_tr[i].find_elements_by_tag_name('td')[2].find_element_by_tag_name('p').text
    print(list_tr[i].find_elements_by_tag_name('td')[2].find_element_by_tag_name('p').text)
    list_img = list_tr[i].find_elements_by_tag_name('td')[2].find_element_by_tag_name('p').find_elements_by_tag_name('img')
    tmp = tmp + "   "
    for x in range(0,list_img.__len__()):
        tmp2 = tmp2 +list_img[x].get_attribute('title')+'/'
        print(list_img[x].get_attribute('title'))
    if tmp2=="":
        tmp2 = '0'
    tmp = tmp+tmp2
    tmp = tmp + "   " +list_tr[i].find_elements_by_tag_name('td')[3].find_element_by_tag_name('a').text
    print(list_tr[i].find_elements_by_tag_name('td')[3].find_element_by_tag_name('a').text)
    tmp = tmp + "   " +list_tr[i].find_elements_by_tag_name('td')[4].text
    print(list_tr[i].find_elements_by_tag_name('td')[4].text)
    tmp = tmp + "   " + list_tr[i].find_elements_by_tag_name('td')[5].text.replace("\n","  ")
    print(list_tr[i].find_elements_by_tag_name('td')[5].text)
    print(tmp)
    #f_temp.write(tmp+'\n')
    tmp = tmp1
    tmp2 = ""
    print('$$$$$$$$$$$$$$$$$')

if __name__ == '__main__':
    driver = getDriver()
    get_info(driver,'https://fwliuhaibo.haodf.com/','xixi')