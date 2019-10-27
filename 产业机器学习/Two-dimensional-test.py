import requests
from PIL import Image,ImageEnhance
import random
import zxing
import pyzbar.pyzbar as pyzbar
import os


filename=r'C:\Users\DELL\Desktop\eHealth\1_Industrial_Machine_Learning_task\OCR_tsak\sample\4.jpg'
img = Image.open(filename)
img = ImageEnhance.Contrast(img).enhance(4.0)      #增加对比度
img = img.convert('L')              #灰度化


# zxing
# ran = int(random.random() * 100000)
# img.save('%s%s.jpg' % (os.path.basename(filename).split('.')[0], ran))
#
# zx = zxing.BarCodeReader()
# zxdata = zx.decode('%s%s.jpg' %(os.path.basename(filename).split('.')[0],ran),'gbk')
# print(zxdata.parsed)

#pyzbar
barcode = pyzbar.decode(img)
barcodeData = barcode[0].data.decode("gbk")
print(barcodeData)
