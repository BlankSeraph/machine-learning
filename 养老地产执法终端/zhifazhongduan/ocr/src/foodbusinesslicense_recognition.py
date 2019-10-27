"""
使用Tesseract-OCR，识别食品经营许可证
需要Python 3.6; opencv 4.0 ;tessoract_ocr; pytesseract; zxing
"""

import pytesseract
import cv2
import pandas as pd
import numpy as np
import random
import re
import os
import zxing
import time
import platform


def rotation(img):
    '''
    该函数用于对输入图片的文字倾斜角度进行纠正
    :param img: 原彩色图片
    :return: 角度纠正后的图片，像素大小与原图一致
    '''

    B, G, R = cv2.split(img)  # 拆分三原色通道
    _, RedThresh = cv2.threshold(R, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)  # 在红色通道上进行反向二值化处理
    edges = cv2.Canny(RedThresh, 50, 150, apertureSize=3)  # 图片中文字的边缘检测
    lines = cv2.HoughLines(edges, 1, np.pi / 180, 120)  # 获取霍夫变换后的图像中的部分直线
    sum = 0
    for line in lines[0]:
        sum = sum + line[1]
    aver = sum / len(lines[0])  # 计算平均角度

    (h, w) = img.shape[:2]
    center = (w // 2, h // 2)
    M = cv2.getRotationMatrix2D(center, -aver, 1.0)  # 旋转图片
    rotated = cv2.warpAffine(img, M, (w, h))

    return rotated


def cut_pic(img):
    '''
    该函数用于将输入的证件照片根据所需识别信息位置进行切分
    :param img:原彩色图片
    :return:切割后的包含识别信息的左右两侧图片
    '''
    height = img.shape[0]
    width = img.shape[1]
    img_left = img[int(height * 0.3):int(height * 0.8), int(width * 0.3):int(width * 0.52)]  # 切割后的左侧图片
    img_right = img[int(height * 0.3):int(height * 0.535), int(width * 0.75):int(width * 0.94)]  # 切割后的右侧图片

    return img_left, img_right


def Threshold(img):
    '''
    该函数用于对原彩色图片进行二值化处理，获得用于划分文本区域的二值化图片及用于识别的二值化图片
    :param img:原彩色图片
    :return:用于划分文本区域的二值化图片、用于文字识别的二值化图片
    '''
    B, G, R = cv2.split(img)  # 拆分三原色通道
    w = int(round(img.shape[1]*0.1,0))
    if w % 2 ==0: element = w-1
    else:  element = w
    _, RedThresh = cv2.threshold(R, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)  # 将红色通道二值化处理成字白底黑的图片
    thresh = cv2.adaptiveThreshold(RedThresh, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, element, 5)   # 自适应阈值，找到字的范围

    _, RedThresh1 = cv2.threshold(R, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)  # 将红色通道二值化处理成字黑底白的图片，用于切割，识别
    element = cv2.getStructuringElement(cv2.MORPH_RECT, (2, 2))
    thresh_erode = cv2.erode(RedThresh1, element)  # 对中的文字进行腐蚀操作，使文字更加清晰

    return thresh, thresh_erode


def findTextRegion(thresh):
    '''
    该函数用于对二值化后的图片进行文本区域框识别
    :param thresh:用于划分文本区域的二值化图片
    :return:文本区域的坐标位置
    '''
    element1 = cv2.getStructuringElement(cv2.MORPH_RECT, (8, 4))  # 膨胀元素
    dilation = cv2.dilate(thresh, element1, iterations=1)  # 一次膨胀操作
    contours, hierarchy = cv2.findContours(dilation, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)  # 区域识别

    return contours


def remove(text):
    '''
    该函数用于对识别后的文本进行清洗
    :param text:识别出的文本，str类型
    :return:清洗后的文本，str类型
    '''
    remove = '【】“”《》‘’:{}？！⑦（）%^>℃.”“^-——=&#@￥[]<>""''*!`^……+_~$ ：'
    if len(text) <= 1:
        text = ""
    else:
        for i in text:
            if i in remove:
                text = re.sub(i, "", text)  # 根据正则表达式对文本进行清洗

    return text


# 各文本框文字识别
def img_ocr_word(thresh, thresh_erode):
    '''
    该算法用于对二值化图片进行切割并分别识别文字，并根据切割图片的坐标位置整理图片，并对识别后的文本进行清洗
    :param thresh: 用于划分文本区域的二值化图片
    :param thresh_erode:用于文字识别的二值化图片
    :return: 经过清洗后的最终ocr识别文字，dateframe类型
    '''
    text_string = []
    x_all = []
    y_all = []
    contours = findTextRegion(thresh)  # 获得文本区域的坐标位置
    for cnt in contours:
        x, y, w, h = cv2.boundingRect(cnt)
        if w > thresh.shape[1] * 0.03 and h > thresh.shape[0] * 0.03 and h < thresh.shape[0] * 0.7:  # 过滤过小或过大的区域
            newimage = thresh_erode[y:y + h, x:x + w]  # 切割二值化图片
            vcode = pytesseract.image_to_string(newimage, lang='chi_sim', config='--psm 11')  # 利用Tesseract-orc 进行文本识别
            if vcode == '' or vcode == ' ':
                continue
            else:
                text_string.append(vcode)
                x_all.append(x)
                y_all.append(y)

    # 整理识别后的文字
    text_string = pd.DataFrame(text_string)
    x_all = pd.DataFrame(x_all)
    y_all = pd.DataFrame(y_all)
    text_string = pd.concat([text_string, x_all, y_all], axis=1)
    text_string_sorted = pd.DataFrame()

    if text_string.empty == False:
        text_string.columns = ['text', 'x', 'y']
        text_string_sorted = text_string.sort_values('y')  # 根据坐标位置进行排序
        text_string_sorted['text'] = text_string_sorted['text'].str.replace("\n", "")
        text_string_sorted['text'] = text_string_sorted['text'].str.replace(")", "）")  # 用于解决正则化处理时无法识别英文)的问题
        text_string_sorted['text'] = text_string_sorted['text'].apply(lambda x: remove(x))  # 移除符号
        text_string_sorted = text_string_sorted[text_string_sorted['text'] != ""]  # 选取有价值的文本信息
    else:
        print('recognition_error: Such image can not be recognized.')

    return text_string_sorted


def img_ocr_date(img):
    '''
    该函数用于识别简单的数字或单一文字
    :param img: 原彩色图片
    :return: 识别后的文字，str类型
    '''
    B_channel, G_channel, R_channel = cv2.split(img)  # 分割通道
    _, RedThresh = cv2.threshold(R_channel, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)  # 字黑底白
    vcode = pytesseract.image_to_string(RedThresh, lang='chi_sim', config='--psm 7')

    return vcode


def final_ocr_time(img):
    '''
    该函数用于识别并获得完整的有效期
    :param img:原彩色图片
    :return:完整的有效期，str类型
    '''
    height = img.shape[0]
    width = img.shape[1]
    year = img[int(height * 0.8):int(height * 0.95), int(width * 0.22):int(width * 0.28)]
    month = img[int(height * 0.8):int(height * 0.95), int(width * 0.311):int(width * 0.355)]
    day = img[int(height * 0.8):int(height * 0.95), int(width * 0.39):int(width * 0.43)]
    text_year = img_ocr_date(year)
    text_month = img_ocr_date(month)
    text_day = img_ocr_date(day)
    final_time = text_year + '-' + text_month + '-' + text_day

    return final_time


def final_ocr_result(text_string_left, text_string_right, validity_date):
    '''
    该函数用于将识别后的左侧，右侧以及有效期处的文本整合成最终的完整ocr识别内容
    :param text_string_left:许可证左侧区域识别出的文字，dataframe类型
    :param text_string_right:许可证右侧区域识别出的文字，dataframe类型
    :param validity_date:许可证有效期区域识别出的文字，str类型
    :return:包括标题的最终完整ocr识别内容
    '''
    left_title = ['经营者名称','社会信用代码（身份证号码）','法定代表人（负责人）',
            '住所','经营场所','主体业态','经营项目']
    right_title= ['许可证编号','日常监督管理机构', '日常监督管理人员']
    period_title = ['有效期至']

    left_df = pd.DataFrame()
    right_df = pd.DataFrame()
    final_df = pd.DataFrame()

    if len(text_string_left) >= 7 :
        left_df['title'] = left_title + [''] * (len(text_string_left) - 7)
        left_df['text'] = list(text_string_left['text'])
    elif len(text_string_left) < 7 and text_string_left.empty == False:
        left_df['title'] = left_title
        left_df['text'] = list(text_string_left['text']) + [''] * (7 - len(text_string_left))
    else:
        left_df['title'] = left_title
        left_df['text'] = [''] * (7-len(text_string_left))

    if len(text_string_right) >= 3:
        right_df['title'] = right_title + [''] * (len(text_string_right) - 3)
        right_df['text'] = list(text_string_right['text'])
    elif len(text_string_right) < 3 and text_string_right.empty == False:
        right_df['title'] = right_title
        right_df['text'] = list(text_string_right['text']) + [''] * (3 - len(text_string_right))
    else:
        right_df['title'] = right_title
        right_df['text'] = [''] * (3 - len(text_string_right))


    final_text = list(left_df['text']) + list(right_df['text'])
    final_text.append(validity_date)
    final_df['text'] = final_text
    final_df['title'] = list(left_df['title']) + list(right_df['title']) + period_title

    return final_df


def two_dimensional_recgonize(filePath,img):
    """
    该函数用于定位证件中的二维码，并获得二维码识别结果
    :param filePath: 图片路径
    :param img: 原彩色图片（包含二维码部分）
    :return:二维码识别结果，str类型
    """
    two_dimensional_text = ''
    zx = zxing.BarCodeReader()  # zxing 解析，需注意zxing的文件路径问题
    system = platform.system()
    if system == 'Darwin':
        print('macOS')
        zxdata = zx.decode(filePath)
        two_dimensional_text = zxdata.parsed.encode('ISO8859-1').decode('GBK')
    elif system == 'Linux':
        print('Linux')
    else:
        B, G, R = cv2.split(img)  # 拆分通道
        _, RedThresh = cv2.threshold(R, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)  # 字白底黑
        contours = findTextRegion(RedThresh)
        for cnt in contours:
            x, y, w, h = cv2.boundingRect(cnt)
            if w > img.shape[1] * 0.08 and h > img.shape[0] * 0.08 and h < img.shape[0] * 0.15 and w < img.shape[1] * 0.15:  # 根据区域大小定位二维码
                newimage = img[y:y + h, x:x + w]
                newimage = cv2.cvtColor(newimage, cv2.COLOR_BGR2GRAY)
                ran = int(random.random() * 100000)
                path = '%s.jpg' % ran
                cv2.imwrite(path, newimage)
                zx = zxing.BarCodeReader()  # zxing 解析，需注意zxing的文件路径问题
                zxdata = zx.decode(path, charset='gbk')
                temp = zxdata.parsed
                os.remove(path)  # 删除临时文件
                if temp == '' or temp == ' ':
                    continue
                else:
                    two_dimensional_text = temp

    return two_dimensional_text


def final_result(final_ocr_res, two_dimensional_test):
    '''
    该函数用于利用二维码识别结果校验ocr文本识别结果，并返回最终结果
    :param final_ocr_res: orc最终识别结果，dataframe类型
    :param two_dimensional_test: 二维码识别结果
    :return:最终整理后的结果，dataframe类型
    '''
    if two_dimensional_test!='':
        List = two_dimensional_test.split('\n')
        for i in List:
            if i.startswith('许可证编号：'):
                final_ocr_res.loc[(final_ocr_res.title=='许可证编号'),'text'] = i[6:]
            if i.startswith('经营者名称：'):
                final_ocr_res.loc[(final_ocr_res.title == '经营者名称'), 'text'] = i[6:]
            if i.startswith('法定代表人(负责人)：'):
                final_ocr_res.loc[(final_ocr_res.title == '法定代表人（负责人）'), 'text'] = i[11:]
            if i.startswith('主体业态：'):
                final_ocr_res.loc[(final_ocr_res.title == '主体业态'), 'text'] = i[5:]
            if i.startswith('经营场所：'):
                final_ocr_res.loc[(final_ocr_res.title == '经营场所'), 'text'] = i[5:]
                final_ocr_res.loc[(final_ocr_res.title == '住所'), 'text'] = i[5:]
            if i.startswith('有效期至：'):
                final_ocr_res.loc[(final_ocr_res.title == '有效期至'), 'text'] = i[5:]
    else:
        print('recognition_error: There is no two_dimensional code in the picture, or the parsing of the two-dimensional code fails.')

    return final_ocr_res


def licence_recognition(filePath, type=None, version=None):
    """
    该函数用于http调用，获得证件识别结果
    :param filePath: 图片路径
    :param type: 许可证类型，1为正本，2为副本
    :param version: 许可证版本，如1903表示2019年3月版
    :return: 证件识别数据列表
    """
    final_res = dict()
    if os.path.exists(filePath):
        img = cv2.imread(filePath, cv2.IMREAD_COLOR)  # 原图
        two_dimensional_text = two_dimensional_recgonize(filePath,img)  # 二维码识别结果

        img = rotation(img)  # 旋转处理
        img_left, img_right = cut_pic(img)

        thresh_left, thresh_erode_left = Threshold(img_left)  # 二值化处理后的左侧黑白图片
        text_stringz_left = img_ocr_word(thresh_left, thresh_erode_left)  # 识别结果

        thresh_right, thresh_erode_right = Threshold(img_right)  # 二值化处理后的右侧黑白图片
        text_stringz_right = img_ocr_word(thresh_right, thresh_erode_right)  # 识别结果

        validity_date = final_ocr_time(img)  # 有效期识别
        final_ocr_res = final_ocr_result(text_stringz_left, text_stringz_right, validity_date)  # 整理后结果（待改进）
        final_res = final_result(final_ocr_res, two_dimensional_text)  # 利用二维码校验结果
        final_res = final_res.set_index('title').T.to_dict('records')
    else:
        print('file_error: No such file!')

    return final_res


if __name__ == '__main__':
    print('ocr_data')
    start_time = time.time()
    result = licence_recognition(os.path.abspath(os.path.join(os.getcwd(), '../image/demo.png')))
    print(result)
    print(round(time.time() - start_time, 3), 'seconds')
