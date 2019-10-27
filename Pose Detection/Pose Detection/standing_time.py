import csv
import os
import pandas as pd
import numpy as np
# Left=[]
# Right=[]
#
# n=0
def get_csv_path(Main_path):              #批量取csv文件
    i = 0
    folder = []
    for a, _, _ in os.walk(Main_path):
        if i % 2 == 1:
            folder.append(a)
        i += 1
    csv_name = 'alphapose-results.csv'
    csv_path = [each + '\\' + csv_name for each in folder]

    return csv_path

def get_label(reader):                   #打标签
    tab = []
    for index,row in reader.iterrows():
        left_feet_y =  float(row['左脚'].split(',')[1].replace(")", ""))
        right_feet_y = float(row['右脚'].split(',')[1].replace(")", ""))
        height = abs(left_feet_y - right_feet_y )
        if height > 12.0:            # 做差比较并标注
            tab.append(1)
        else:
            tab.append(0)
    tab = pd.DataFrame(tab)
    tab.columns = ['站立标记']
    res_label=pd.concat([reader, tab], axis=1)

    # 选出从0变为1或从1变为0的索引（包容前后3帧内为0的情况）
    index=[]
    for m in range(0, len(res_label)):
        if m <3 :
            continue
        elif  res_label['站立标记'][m] == 1 and ((res_label['站立标记'][m-3:m] == 0).all() or (res_label['站立标记'][m+1:m+4] == 0).all()):
            index.append(m)

    #选出连续为1的最大间隔
    max_interval=0
    for m in range(0, len(index)-1):
        interval = index[m+1]-index[m]
        if interval > max_interval:
            min_index=index[m]
            max_index=index[m+1]
            max_interval=interval

    # 重新标记
    res_label1=res_label.copy()
    res_label1.loc[0:min_index-1,'站立标记'] = 0
    res_label1.loc[min_index:max_index,'站立标记'] = 1
    res_label1.loc[max_index+1:len(res_label1),'站立标记'] = 0

    return res_label1

def count_time(res_label):            #计算时间
    total_frame= res_label[res_label['站立标记']==1]
    time = round(len(total_frame)/ 12,2)

    return time


# if __name__ == '__main__':
def run():
    Main_path = 'D:\\zhengmianqiege'  # 前序步骤的结果数据存放文件夹

    time_list = [] # 站立时间序列
    for each_csv in get_csv_path(Main_path):
        reader = pd.read_csv(each_csv,encoding='utf-8')
        result_label=get_label(reader)
        time=count_time(result_label)
        time_list.append(time)


        save_folder = r'C:\\Users\\DELL\\Desktop\\eHealth\\1_Industrial_Machine_Learning_task\\Stand_task\\About_algorithm\\count_time\\'
        res_path = save_folder + each_csv.split('\\')[2] + '.csv'
        result_label.to_csv(res_path,index=0,encoding="UTF-8")
    # 此代码块的作用：处理后保留过程文件给Frequency，同时输出一个最终结果：time_list
    return time_list




