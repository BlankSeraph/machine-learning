# -*- coding: utf-8 -*-
# @Time     : 2018/12/13 22:43
# @Author   : Junee
# @FileName: Bagging.py
# @Software  : PyCharm
# Observing PEP 8 coding style
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.linear_model import LogisticRegression
from  sklearn.tree import DecisionTreeClassifier
from sklearn.svm import SVC
from sklearn.model_selection import train_test_split
from sklearn.datasets import make_moons
# from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')

# 导入数据
from sklearn.datasets import load_breast_cancer
cancer = load_breast_cancer()
X = cancer.data
y = cancer.target
# X, y = make_moons(n_samples=500, noise=0.30, random_state=42)
X_train, X_test, y_train, y_test = train_test_split(X, y,test_size=0.2,random_state=42)

# plt.plot(X_train,y_train)
# plt.scatter(X[:,0],X[:,1],marker='o',c=y)
# plt.show()

# 基学习器
log_clf_1 = LogisticRegression(penalty='l2',random_state=42,)
log_clf_2 = LogisticRegression(penalty='l1',random_state=10)
log_clf_3 = LogisticRegression(penalty='l1',random_state=20)
rf_clf = RandomForestClassifier(n_estimators=10,criterion='entropy')
dec_tree = DecisionTreeClassifier()
svm_clf = SVC()
# 投票分类器
voting_clf = VotingClassifier(estimators=[("lr", log_clf_1),("svc", svm_clf),("tree",dec_tree)])  # ("rf", rf_clf),
# voting_clf = VotingClassifier(estimators=[("Model_1", log_clf_1), ("Model_2", log_clf_2), ("Model_3", log_clf_3)])
# voting_clf.fit( X_train, y_train )
for clf in ( log_clf_1,svm_clf,dec_tree,voting_clf ):  # rf_clf,
# for clf in ( log_clf_1, log_clf_2, log_clf_3, voting_clf ):
    clf.fit( X_train, y_train )
    y_pred = clf.predict( X_test )
    train_score = clf.score(X_train,y_train)
    test_score = clf.score(X_test,y_test)
    print( clf.__class__.__name__, train_score,test_score)