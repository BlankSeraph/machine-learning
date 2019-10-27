# -*- coding: utf-8 -*-

import numpy as np
import matplotlib
matplotlib.use('TkAgg')

import pandas as pd

from sklearn import preprocessing

from sklearn.svm import SVC
from sklearn.model_selection import StratifiedKFold
from sklearn.feature_selection import RFECV
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
import numpy as np






"""
数据读取
"""
excelFile = r'/Users/cohe/Documents/珠海项目/筛选5.xlsx'
df = pd.DataFrame(pd.read_excel(excelFile))
df = df.fillna(0)
df1= df[['体温','脉搏','呼吸频率',\
         '身高',\
         '平均高压','平均低压','平均压差','身高',
         '体重','腰围','BMI','锻炼频率',\
         '荤素','嗜盐','嗜糖','嗜油','吸烟状况','饮酒频率',\
         '心率','心律','杂音',\
         '空腹血糖','心电图',\
         'B超腹部','是否脑血管疾病','是否肾脏疾病','是否心脏疾病','是否血管疾病',\
         '神经系统疾病',
         '血常规-白细胞',\
         '性别',\
         '年龄','血型',\
         '高血压遗传病','糖尿病遗传病','高血压','高血压高危人群','糖尿病高危人群']]
df2 = df['糖尿病']



df11 =df1.values
df11= preprocessing.scale(df11)
df22 =df2.values



# 使用特征SVM-RFE特征选择方法

# Create the RFE object and compute a cross-valid ated score.
svc = SVC(kernel="linear")
# The "accuracy" scoring is proportional to the number of correct
# classifications
rfecv = RFECV(estimator=svc, step=1, cv=StratifiedKFold(5),
              scoring='accuracy')
rfecv.fit(df11, df22)
print("Optimal number of features : %d" % rfecv.n_features_)
# 绘图 Plot number of features VS. cross-validation scores
plt.figure()
plt.xlabel("Number of features selected")
plt.ylabel("Cross validation score (nb of correct classifications)")
plt.plot(range(1, len(rfecv.grid_scores_) + 1), rfecv.grid_scores_)
plt.show()
names = df1.columns.values.tolist()
print(sorted(zip(map(lambda x: round(x, 4), rfecv.ranking_), names)))


#打乱样本
df = df.sample(frac = 1)
# 提取选择出的特征
df_1 = df[['平均低压','平均压差','平均高压',\
         '是否心脏疾病','是否脑血管疾病','是否血管疾病']]
df_2 = df['高血压']


#朴素贝叶斯
import sklearn.model_selection as sk_model_selection
import sklearn.naive_bayes as sk_bayes
model = sk_bayes.MultinomialNB(alpha=1.0,fit_prior=True,class_prior=None) #多项式分布的朴素贝叶斯
model = sk_bayes.BernoulliNB(alpha=1.0,binarize=0.0,fit_prior=True,class_prior=None) #伯努利分布的朴素贝叶斯
model = sk_bayes.GaussianNB()#高斯分布的朴素贝叶斯

correct = ['roc_auc','recall',None,'precision','f1']
for i in correct:
    ix = sk_model_selection.cross_val_score(model, df_1, y=df_2, scoring=i, cv=10)
    print(str(i) + str(np.mean(ix)))

#查看十次AUC
roc_auc=sk_model_selection.cross_val_score(model, df_1, y=df_2, scoring='roc_auc',cv=10)
print('交叉验证结果: ',roc_auc)



#逻辑回归
import sklearn.linear_model as sk_linear
model = sk_linear.LogisticRegression(penalty='l2',dual=False,C=1.0,n_jobs=1,random_state=20,fit_intercept=True)
for i in correct:
    ix = sk_model_selection.cross_val_score(model, df_1, y=df_2, scoring=i, cv=10)
    print(str(i) +' '+ str(np.mean(ix)))
#打印AUC值
ROC_AUC=sk_model_selection.cross_val_score(model, df_1, y=df_2, scoring='roc_auc',cv=10)


#决策树算法
from sklearn.tree import DecisionTreeClassifier
#选择分类器
decision_tree_classifier = DecisionTreeClassifier()
#设定参数网格
parameter_grid = {
                  'criterion': ['gini', 'entropy'],
                   'max_depth': [1, 2, 3, 4, 5,6]

                  }
#采用5折交叉验证 寻找最优参数
grid_search = GridSearchCV(decision_tree_classifier,
                           param_grid=parameter_grid,
                           cv=5)
grid_search.fit(df_1, df_2)
print('Best score: {}'.format(grid_search.best_score_))
print('Best parameters: {}'.format(grid_search.best_params_))

#使用最优参数构建模型
decision_tree_classifier = DecisionTreeClassifier(criterion='entropy', max_depth=6)
accuracy=sk_model_selection.cross_val_score(decision_tree_classifier, df_1, y=df_2, scoring=None,cv=10)

#打印各种评估结果
for i in correct:
    ix = sk_model_selection.cross_val_score(decision_tree_classifier, df_1, y=df_2, scoring=i, cv=10)
    print(str(i) + str(np.mean(ix)))


#画ROC曲线
from scipy import interp
df_1= np.array(df_1)
df_2= np.array(df_2)


mean_tpr = 0.0
mean_fpr = np.linspace(0, 1, 100)
all_tpr = []
i=0
adjustthresholds = 0
gedeng = 0


# 画ROC曲线  画5次
for train, test in StratifiedKFold(n_splits=5).split(df_1, df_2):

    # 通过训练数据，使用svm线性核建立模型，并对测试集进行测试，求出预测得分
    probas = model.fit(df_1[train], df_2[train]).predict_proba(df_1[test])

    # 计算 ROC curve and area the curve
    # 通过roc_curve()函数，求出fpr和tpr，以及阈值
    fpr, tpr, thresholds = roc_curve(df_2[test], probas[:, 1])

    # print(thresholds[np.argwhere((tpr - fpr) == max(tpr-fpr))])
    print(thresholds[np.argwhere((tpr - fpr) == max(tpr-fpr))])
    #戈登系数
    gedeng += np.mean(thresholds[np.argwhere((tpr - fpr) == max(tpr-fpr))])
    adjustthresholds += max(thresholds[np.argwhere(tpr >0.90)])
    print("**********")
    mean_tpr += interp(mean_fpr, fpr, tpr)  # 对mean_tpr在mean_fpr处进行插值，通过scipy包调用interp()函数

    mean_tpr[0] = 0.0  # 初始处为0
    roc_auc = auc(fpr, tpr)
    # 画图，只需要plt.plot(fpr,tpr),变量roc_auc只是记录auc的值，通过auc()函数能计算出来
    plt.plot(fpr, tpr, lw=1, label='ROC fold %d (area = %0.2f)' % (i, roc_auc))
    i = i+1

print("gendeng:"+str(gedeng/5))
print("adjust:"+str(adjustthresholds/5))
plt.plot([0, 1], [0, 1], '--', color=(0.6, 0.6, 0.6), label='Luck')


mean_tpr /= 5 # 在mean_fpr100个点，每个点处插值插值多次取平均
mean_tpr[-1] = 1.0  # 坐标最后一个点为（1,1）
mean_auc = auc(mean_fpr, mean_tpr)  # 计算平均AUC值
# 画平均ROC曲线
# print mean_fpr,len(mean_fpr)
# print mean_tpr
plt.plot(mean_fpr, mean_tpr, 'k--',
         label='Mean ROC (area = %0.2f)' % mean_auc, lw=2)

plt.xlim([-0.05, 1.05])
plt.ylim([-0.05, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC')
plt.legend(loc="lower right")
plt.show()
