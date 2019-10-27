# -*- coding: utf-8 -*-
# @Time     : 2019/4/21 21:00
# @Author   : Junee
# @FileName: angle_cal.py
# @Software  : PyCharm
# Observing PEP 8 coding style
import os
import numpy as np
import csv


def get_csv_path(Main_path):
    i = 0
    folder = []
    for a, _, _ in os.walk(Main_path):
        if i % 2 == 1:
            folder.append(a)
        i += 1
    # print(folder)
    csv_name = 'alphapose-results.csv'
    csv_path = [each + '\\' + csv_name for each in folder]
    return csv_path


def csv_reader(csv_path):
    # 索引：左肩：6；右肩：7
    left_shoulder = []
    right_shoulder = []
    i = 0
    with open(csv_path, 'r', encoding='utf-8') as infile:
        reader = csv.reader(infile)
        for each in reader:
            if i == 0:
                pass
            else:
                left_temp_ = each[6].strip("(").strip(")").split(',')
                left_temp = list(map(float, left_temp_))
                # left_temp = list(map(round,left_temp))  # 精度减少,只保留整数
                left_shoulder.append(left_temp)

                right_temp_ = each[7].strip("(").strip(")").split(',')
                right_temp = list(map(float, right_temp_))
                # right_temp = list(map(round,right_temp))  # 精度减少
                right_shoulder.append(right_temp)

            i += 1
        print('该视频切片数：%d' % i)
    # with open(csv_path, 'r', encoding='utf-8') as infile:
    #     reader = csv.reader(infile)
    #     j = 0  # 取最后五帧图片的左右肩作为基线
    #     for each in reader:
    #         if j >= i - 5:
    #             left_temp_ = each[ 6 ].strip("(").strip(")").split(',')
    #             left_temp = list(map(float, left_temp_))
    #             # left_temp = list(map(round,left_temp))  # 精度减少
    #             baseline_left_shoulder.append(left_temp)
    #
    #             right_temp_ = each[ 7 ].strip("(").strip(")").split(',')
    #             right_temp = list(map(float, right_temp_))
    #             # right_temp = list(map(round,right_temp))  # 精度减少
    #             baseline_right_shoulder.append(right_temp)
    #
    #         j += 1
    # # print(baseline_left_shoulder)
    # baseline_left_x = sum([ each[ 0 ] for each in baseline_left_shoulder ]) / len(baseline_left_shoulder)
    # baseline_left_y = sum([ each[ 1 ] for each in baseline_left_shoulder ]) / len(baseline_left_shoulder)
    # baseline_right_x = sum([ each[ 0 ] for each in baseline_right_shoulder ]) / len(baseline_right_shoulder)
    # baseline_right_y = sum([ each[ 1 ] for each in baseline_right_shoulder ]) / len(baseline_right_shoulder)
    #
    # baseline_left = [baseline_left_x,baseline_left_y]
    # baseline_right = [baseline_right_x,baseline_right_y]

    # 由于最后五帧的效果不理想，直接定义一个baseline
    baseline_left = [0.0, 0.0]
    baseline_right = [1.0, 0.0]
    # 测试
    # print(baseline_left)
    # print(baseline_right)

    return baseline_left, baseline_right, left_shoulder, right_shoulder


def get_shake_angle(
        baseline_left,
        baseline_right,
        cur_line_left,
        cur_line_right):

    # 规定左边减去右边为正方向
    baseline_vector = np.array(
        [(baseline_left[0] - baseline_right[0]), (baseline_left[1] - baseline_right[1])])
    cur_line_vector = np.array(
        [(cur_line_left[0] - cur_line_right[0]), (cur_line_left[1] - cur_line_right[1])])
    cos_value = (float(baseline_vector.dot(cur_line_vector)) /
                 (np.sqrt(baseline_vector.dot(baseline_vector)) *
                  np.sqrt(cur_line_vector.dot(cur_line_vector))))   # 注意转成浮点数运算
    return np.arccos(cos_value) * (180 / np.pi)  # 返回角度制


# if __name__ == '__main__':
def run():
    Main_path = r'D:\ML_数据集\姿态检测'  # 前序步骤的结果数据存放文件夹
    index = 0
    for each_csv in get_csv_path(Main_path):
        index += 1
        res = []
        print('关键点路径：%s' % each_csv)
        baseline_left, baseline_right, left_shoulder, right_shoulder = csv_reader(
            each_csv)
        for each_left, each_right in zip(left_shoulder, right_shoulder):
            angle = get_shake_angle(
                baseline_left,
                baseline_right,
                each_left,
                each_right)
            angle = abs(180 - angle)
            if angle >= 45:
                angle = 0.0012345  # 设定一个异常值替换方案，换成0.000012345°
            if each_left[1] < each_right[1]:  # 左肩低于右肩，规定为负角度
                angle *= -1
            res.append(angle)

        res = list(map(str, res))  # 解决python浮点数的存储精度问题

        # 三种保存方式
        # 分布到各个文件夹下
        # res_path = each_csv.replace('alphapose-results', 'angle')
        # with open(res_path, 'w', newline='') as outfile:
        #     writer = csv.writer(outfile)
        #     for each in res:
        #         writer.writerow([each])
        #     print('写入成功：%s' % index)

        # 集中到单一文件夹下
        # save_folder = 'D:\\ML_数据集\\angle_detection\\'
        # res_path = save_folder + each_csv.split('\\')[3] + '.csv'
        # # print(res_path)
        # with open(res_path, 'w', newline='') as outfile:
        #     writer = csv.writer(outfile)
        #     for each in res:
        #         writer.writerow([each])
        #     print('第%d次写入成功' % index)
        #     print('-'*20)

        # 整合到前序步骤的csv文件中
        save_folder = 'D:\\ML_数据集\\angle_detection_with_original_data\\'
        res_path = save_folder + each_csv.split('\\')[ 3 ] + '.csv'
        csv_temp = []
        with open(each_csv,'r',encoding='utf-8') as infile:
            reader = csv.reader(infile)
            for each in reader:
                csv_temp.append(each)
        res.insert(0,'角度')

        for old, new in zip(csv_temp,res):
            old.append(new)

        with open(res_path, 'w', newline='') as outfile:
            writer = csv.writer(outfile)
            for each in csv_temp:
                writer.writerow(each)
            print('第%d次写入成功' % index)
            print('-'*20)
