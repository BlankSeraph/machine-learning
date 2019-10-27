# -*- coding: utf-8 -*-
# @Time     : 2019/5/6 16:55
# @Author   : Junee
# @FileName: frequency.py
# @Software  : PyCharm
# Observing PEP 8 coding style
import pandas as pd
import os

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

# if __name__ == '__main__':
def run():
    Main_path = r'D:\ML_数据集\姿态检测'
    swing_count_list = []
    for each_csv in get_csv_path(Main_path):
        angle_list = [ ]
        standing_flag_list = []
        reader = pd.read_csv(each_csv, encoding='utf-8')
        for index, row in reader.iterrows():
            angle = float(row['角度'])
            standing_flag = float(row['站立标记'])
            angle_list.append(angle)
            standing_flag_list.append(standing_flag)

        # 方案一：差分法
        # ---------------------------------------
        # angle_list序列进行一阶差分，差分后序列长度会减一，但这个不影响计算逻辑
        angle_change = [angle_list[i+1] - angle_list[i] for i in range(len(angle_list)-1)]
        count_list = [angle_change[i+1] * angle_change[i] for i in range(len(angle_change)-1)]
        # 同号为正，异号为负，异号记为一次摆动
        # --------------------------------------

        # 方案二：带有阈值的差分法
        # --------------------------------------
        # threshold = 1 # 阈值为1°
        # angle_change = []
        # for j, each in enumerate(angle_list):
        #     if abs(angle_list[j+1] - angle_list[j]) > threshold:  # 只记录大于阈值范围的角度变化
        #         angle_change.append(angle_list[j+1] - angle_list[j])
        #     else:
        #         angle_change.append(0)
        # count_list = [angle_change[i+1] * angle_change[i] for i in range(len(angle_change)-1)]
        # --------------------------------------

        count = 0
        for index_, each in enumerate(count_list):
            # 如果异号且属于单脚站立期间，则累计
            if (each < 0) and (standing_flag_list[index_] == 1):
                count += 1
        swing_count_list.append(count)
    # 返回的是一个摆动次数的列表，其跟单脚站立时间的列表进行除法运算即可得到频率列表
    return swing_count_list