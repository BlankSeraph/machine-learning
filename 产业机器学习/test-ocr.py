import pytesseract
from PIL import Image
import cv2
import pandas as pd
import pyzbar

__author__ = 'admin'

#二值化处理
def Threshold(img):
    im_dst = cv2.fastNlMeansDenoisingColored(img,None,8,8,7,21)        # 降噪处理
    #B_channel, G_channel, R_channel = cv2.split(im_dst)  # 注意cv2.split()返回通道顺序
    Grayimg = cv2.cvtColor(im_dst, cv2.COLOR_BGR2GRAY)        #灰度化处理
    thresh = cv2.adaptiveThreshold(Grayimg,255,cv2.ADAPTIVE_THRESH_MEAN_C,cv2.THRESH_BINARY_INV,25,6)      #二值化处理成黑白图片
    #cv2.imwrite(r'C:\Users\DELL\Desktop\test1.jpg', thresh)
    return thresh

#识别文字框
def findTextRegion(img):
    element1 = cv2.getStructuringElement(cv2.MORPH_RECT, (24, 6))        # 膨胀元素
    element2 = cv2.getStructuringElement(cv2.MORPH_RECT, (30, 9))        # 腐蚀元素
    # 一次膨胀操作
    dilation = cv2.dilate(img, element1, iterations=1)
    # 一次腐蚀操作
    erosion = cv2.erode(dilation, element2, iterations = 1)
    #根据图片像素进行再次膨胀，使轮廓更清晰
    width = img.shape[1]
    if width>=3000:
        times=4
    elif width<3000 and width>=1500:
        times=3
    else:
        times=2
    dilation = cv2.dilate(erosion, element1, iterations=times)
    #cv2.imwrite(r'C:\Users\DELL\Desktop\test2.jpg', dilation)
    contours, hierarchy = cv2.findContours(dilation, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    return contours

#各文本框文字识别
def img_ocr(img,thresh,contours):
    text_string=[]
    two_dimensional_string=[]
    for cnt in contours:
        x, y, w, h = cv2.boundingRect(cnt)
        if w > 0 and h > 0 and w<img.shape[1]*0.8 and h<img.shape[0]*0.8 :
            newimage = thresh[y :y + h , x :x + w ]
            # cv2.imshow("",newimage)
            # cv2.waitKey(0)
            vcode = pytesseract.image_to_string(newimage, lang='chi_sim')
            if vcode == '' or vcode == ' ':
                # color_image = img[y :y + h , x :x + w ]           #可在这里做二维码扫描步骤
                continue
            else:
                text_string.append(vcode)
    text_string = pd.DataFrame(text_string)
    return text_string

if __name__=="__main__":
    img = cv2.imread(r'C:\Users\DELL\Desktop\eHealth\1_Industrial_Machine_Learning_task\OCR_tsak\sample\2.jpg',cv2.IMREAD_COLOR)
    thresh = Threshold(img)       #二值化处理后的黑白图片
    contours = findTextRegion(thresh)       #返回各文本框的轮廓坐标
    final_string = img_ocr(img,thresh,contours)        #返回识别结果
    final_string.to_csv('C:/Users/DELL/Desktop/result.csv', index=0, encoding="gb2312")


