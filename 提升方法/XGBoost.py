# -*- coding: utf-8 -*-
# @Time     : 2018/12/15 22:01
# @Author   : Junee
# @FileName: XGBoosting.py
# @Software  : PyCharm
# Observing PEP 8 coding style
from xgboost import XGBClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
import pandas as pd
# 载入数据集
train = pd.read_csv('train_modified.csv')
target = 'Disbursed' # Disbursed的值就是二元分类的输出
IDcol = 'ID'
# split data into X and y
x_columns = [x for x in train.columns if x not in [target, IDcol]]
X = train[x_columns]
Y = train['Disbursed']

# 把数据集拆分成训练集和测试集
seed = 7
test_size = 0.33
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size=test_size, random_state=seed)

# 拟合XGBoost模型
model = XGBClassifier()
model.fit(X_train, y_train)

# 对测试集做预测
y_pred = model.predict(X_test)
predictions = [ round(value) for value in y_pred ]

# 评估预测结果
accuracy = accuracy_score(y_test, predictions)
print("Accuracy: %.2f%%" % (accuracy * 100.0))