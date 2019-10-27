# -*- coding: utf-8 -*-
# @Time     : 2018/12/15 20:11
# @Author   : Junee
# @FileName: GBDT.py
# @Software  : PyCharm
# Observing PEP 8 coding style
import pandas as pd
import numpy as np
from sklearn.ensemble import GradientBoostingClassifier
import sklearn.model_selection as ms
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score,accuracy_score
import matplotlib.pyplot as plt
import time
import warnings
warnings.filterwarnings('ignore')

train = pd.read_csv('train_modified.csv')
target = 'Disbursed' # Disbursed的值就是二元分类的输出
IDcol = 'ID'
# print(train['Disbursed'].value_counts())

x_columns = [x for x in train.columns if x not in [target, IDcol]]
X = train[x_columns]
y = train['Disbursed']

gbm0 = GradientBoostingClassifier(random_state=10)
gbm0.fit(X,y)
y_pred = gbm0.predict(X)
y_predprob = gbm0.predict_proba(X)[:,1]
# print("Accuracy : %.4g" % ms.cross_val_score(y.values, y_pred))
# print("AUC Score (Train): %f" % ms.cross_val_score(y, y_predprob))
correct = ['roc_auc','recall',None,'precision','f1']
# 准确度计算
# for i in correct:
#     estimate = ms.cross_val_score(gbm0, X, y=y, scoring=i, cv=5)
#     print(str(i)+':' + str(np.mean(estimate)))
# 网格寻优
# 对决策树的数量进行寻优
# param_test1 = {'n_estimators': range(20,81,10)}
# gsearch1 = GridSearchCV(estimator=GradientBoostingClassifier(learning_rate=0.1, min_samples_split=300,
#                                   min_samples_leaf=20,max_depth=8,max_features='sqrt', subsample=0.8,random_state=10),
#                        param_grid = param_test1, scoring='roc_auc',iid=False,cv=5)
# gsearch1.fit(X,y)
# print(gsearch1.best_estimator_, gsearch1.best_params_, gsearch1.best_score_)
# 对决策树的深度、内部结点的最小样本数进行寻优（剪枝）
# t0 = time.clock()
# param_test2 = {'max_depth': range(3,10,2), 'min_samples_split': range(100,601,200)}
# gsearch2 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=60, min_samples_leaf=20,
#       max_features='sqrt', subsample=0.8, random_state=10),
#    param_grid = param_test2, scoring='roc_auc',iid=False, cv=5)
# gsearch2.fit(X,y)
# t1 = time.clock()
# print(gsearch2.best_estimator_,'\n', gsearch2.best_params_, gsearch2.best_score_)
# print('用时：',t1-t0,'秒')

# 对决策树的内部结点最小样本数，叶子结点的最小样本数进行寻优
param_test3 = {'min_samples_split':range(800,1900,200), 'min_samples_leaf':range(60,101,10)}
t2 = time.clock()
gsearch3 = GridSearchCV(estimator = GradientBoostingClassifier(learning_rate=0.1, n_estimators=60,max_depth=7,
                                     max_features='sqrt', subsample=0.8, random_state=10),
                       param_grid = param_test3, scoring='roc_auc',iid=False, cv=5)
gsearch3.fit(X,y)
t3 = time.clock()
print(gsearch3.best_estimator_, gsearch3.best_params_, gsearch3.best_score_)
print('用时：',t3-t2,'秒')  # 81秒，leaf=60;split=1200,score=0.82

# 最优参数模型
gbm1 = GradientBoostingClassifier(learning_rate=0.1, n_estimators=60,max_depth=7, min_samples_leaf =60,
               min_samples_split =1200, max_features='sqrt', subsample=0.8, random_state=10)
gbm1.fit(X,y)
y_pred_1 = gbm1.predict(X)
y_predprob_1 = gbm1.predict_proba(X)[:,1]
print("Accuracy : %.4g" % accuracy_score(y.values, y_pred_1))
print("AUC Score (Train): %f" % roc_auc_score(y, y_predprob_1))

