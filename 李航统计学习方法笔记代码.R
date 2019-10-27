####  第一章 多项式回归的实现 过拟合现象 ####

polyfit<-function(y,x,maxdeg){
  pwrs<-powers(x,maxdeg)#生成不同阶数的x
  lmout<-list()
  class(lmout)<-"polyreg"#创建一个新类
  for(i in 1:maxdeg){
    lmo<-lm(y~pwrs[,1:i])
    lmo$fitted.cvvalues<-lvoneout(y,pwrs[,1:i,drop=FALSE])
    lmout[[i]]<-lmo
  }
  lmout$x<-x
  lmout$y<-y
  return(lmout)
}

print.polyreg<-function(fits){
  maxdeg<-length(fits)-2#计算拟合的模型数
  n<-length(fits$y)
  tbl<-matrix(nrow = maxdeg,ncol=2)
  cat("mean squared prediction errors,by degree\n")
  colnames(tbl)<-c("MSPE","TRAIN")
  for(i in 1:maxdeg){
    fi<-fits[[i]]
    errs<-fits$y-fi$fitted.cvvalues
    spe<-sum(errs^2)
    tbl[i,1]<-spe/n
    tbl[i,2]<-sum(fi$residuals^2)/n
  }
  print(tbl)
  return(tbl)
}

plot.polyreg<-function(fits){
  plot(fits$x,fits$y,xlab="X",ylab="Y")
  maxdg<-length(fits)-2
  cols<-c("red","green","blue")
  dg<-curvecount<-1
  while(dg<maxdg){
    prompt<-paste("RETURN for CV fit for degree",dg,"or type degree",
                  "or q for quit:")
    rl<-readline(prompt)
    dg<-if(rl=="") dg else if(rl!="q") as.integer(rl) else break
    lines(fits$x,fits[[dg]]$fitted.values,col=cols[curvecount%%3+1])
    dg<-dg+1
    curvecount<-curvecount+1
  }
}


powers<-function(x,dg){
  pw<-matrix(x,nrow = length(x))
  prod<-x
  for(i in 2:dg){
    prod<-prod*x
    pw<-cbind(pw,prod)
  }
  return(pw)
}

lvoneout<-function(y,xmat){
  n<-length(y)
  predy<-vector(length = n)
  for(i in 1:n){
    lmo<-lm(y[-i]~xmat[-i,])
    betahat<-as.vector(lmo$coef)
    predy[i]<-betahat%*%c(1,xmat[i,])#交叉验证中y的预测值
  }
  return(predy)
}

#### 例1.1多项式模拟 ####
n<-60
x<-(1:n)/n
y<-vector(length = n)
for(i in 1:n) y[i]<-sin((3*pi/2)*x[i])+x[i]^2+rnorm(1,0,.5)
dg<-12
lmo<-polyfit(y,x,dg);lmo
error<-print(lmo)
plot(lmo)
#### 绘制MSPE/TRAIN图 ####
plot(error[,"TRAIN"],ylab="error",xlab="complex",
     ylim=c(min(error),max(error)),col="blue",type="b")
points(error[,"MSPE"],type="b",col="red")
abline(v=4,lty=3)
text(locator(),"MSPE",col="red")
text(locator(),"TRAIN",col="blue")


#### 第二章 感知机 ####
####  线性可分感知机原始形式的实现 ####
####  兼容线性不可分的情形，通过设置容忍度endure>0即可 ####
#### 学习函数linePercept() ####
linePercept<-function(cls="y",atr=c("x1","x2"),data=NULL,aita=1,
                      endure=0,maxiter=1000,w0=rep(0,length(atr)),b0=0){
  datause<-data;datause$xb<-1#建立扩充后的数据框，主要是为了将b归入扩充权重向量
  wmat<-matrix(c(w0,b0),nrow=length(atr)+1,ncol=1)#先用矩阵按列储存初始扩充权重
  iterk<-0
  misssample<-vector()
  while(iterk>=0){
    sign_mat<-as.matrix(datause[,c(atr,"xb"),drop=F])%*%wmat[,iterk+1,drop=F]%*%
      t(as.matrix(datause[,cls,drop=F]))#计算时注意将data.frame转换为matrix
    sign_vec<-diag(sign_mat)
    minlab<-which.min(sign_vec)#误分情况最严重的点
    if(endure==0){
       if(sign_vec[minlab]>endure){
          cat("The Final sign_min is : ",sign_vec[minlab],"\n") 
          break
         } 
    } else if(endure>0){#abs(sign_vec[minlab])表示最大误差距离
       if(all(w0==0)&&b0==0) stop("w0 and b0 must not all be 0 when endure>0.")
       if(all(sign_vec>0)) break 
       if(abs(sign_vec[minlab])<endure){
          cat("The Final sign_min is : ",abs(sign_vec[minlab]),"\n") 
          break
         } 
    } else stop("The endure must not be smaller than 0. ")
    if(iterk>maxiter) break #当迭代次数大于maxiter时停止
    wchange<-wmat[,iterk+1,drop=F]+
      aita*datause[,cls][minlab]*t(as.matrix(datause[minlab,c(atr,"xb"),drop=F]))
    wmat<-cbind(wmat,wchange)
    misssample[iterk+1]<-minlab
    iterk<-iterk+1
  }
  rownames(wmat)<-c(atr,"b");colnames(wmat)<-paste0("iter",0:iterk)
  Percept<-list(Finalweight=t(wmat[,ncol(wmat)]),weight=wmat,
                iteration=iterk,miss=misssample,origindata=data,
                atrdata=data[,atr,drop=F],clsdata=data[,cls,drop=F],
                endure=endure,aita=aita,w0=w0,b0=b0)
  class(Percept)<-"linePercept"
  return(Percept)
}
#### 绘图函数 ####
plot.linePercept<-function(obj){#只对二维数据有用
  plot(obj$atrdata[,1],obj$atrdata[,2],
       xlim=c(min(obj$atrdata[,1])-+abs(max(obj$atrdata[,1]))/3,
              max(obj$atrdata[,1])+abs(max(obj$atrdata[,1]))/3),
       ylim=c(min(obj$atrdata[,2])-abs(max(obj$atrdata[,2]))/3,
              max(obj$atrdata[,2])+abs(max(obj$atrdata[,2]))/3),
       col=2*abs(obj$clsdata[,1])+obj$clsdata[,1],pch=19,
       xlab=colnames(obj$atrdata)[1],ylab=colnames(obj$atrdata)[2])
  abline(b=-obj$Finalweight[1,1]/obj$Finalweight[1,2],
         a=-obj$Finalweight[1,3]/obj$Finalweight[1,2],
         col="red",lwd=1.25)
  text(obj$atrdata[,1],obj$atrdata[,2],obj$clsdata[,1])
}
#### 打印函数 ####
print.linePercept<-function(obj){
  print.default(obj[c(1,3)])
}
#### 预测函数 ####
#### preClinePercept()函数只能预测一个实例 ####
preClinePercept<-function(lPobj,cls="y",atr=c("x1","x2"),atr_value=c(0,1)){
  latr<-length(atr)#特征个数
  levelcls<-unique(lPobj$clsdata[,1])
  numcls<-as.numeric(levelcls)
  # chrcls<-as.character(levelcls)
  atrmat<-matrix(c(atr_value,1),nrow=latr+1,ncol=1)
  sgn<-ifelse(sign(lPobj$Finalweight%*%atrmat)>0,max(numcls),min(numcls))
  return(as.vector(sgn))
}
#### predic.linePercept()函数可以一次预测多个点 ####
## 如果是二维特征，会绘制对应的预测图 ####
predict.linePercept<-function(lPobj,cls="y",atr=c("x1","x2"),atr_value=NULL){
  predvalue<-apply(atr_value,1,preClinePercept,lPobj=lPobj,atr=atr,cls=cls)
  out_pre<-atr_value
  out_pre[,cls]<-predvalue
  if(length(atr)==2){
    plot(lPobj);points(out_pre[,atr[1]],out_pre[,atr[2]],pch=23,col="red",cex=2.5,lwd=2)
    text(out_pre[,atr[1]],out_pre[,atr[2]],predvalue,col="red")
  }
  return(out_pre)
}

#### 例2.1 线性可分，默认w0=0,b0=0,endure=0,aita=1  aita为学习率 ####
## 学习 ##
percept<-linePercept(data=dataB2.1,cls="y",atr=c("x1","x2"))
is(percept$Finalweight)
plot(percept)
## 预测 ##
data_atr<-data.frame(x1=c(0,2,1,3),x2=c(1,1,3,2))
predict(percept,cls = "y",atr = c("x1","x2"),atr_value = data_atr)

#### 线性感知机对偶算法的实现 ####
####  DualPercept()函数只能用于线性可分集 ####
DualPercept<-function(cls="y",atr=c("x1","x2"),data=NULL,aita=1,
                      maxiter=1000,alpha0=rep(0,nrow(data)),b0=0){
  datause<-as.matrix(data)#转换成矩阵，方便运算
  sample_num<-nrow(datause)#样本个数
  clsdata<-datause[,cls,drop=F];atrdata<-datause[,atr,drop=F]
  Gram<-atrdata%*%t(atrdata)#先计算Gram矩阵
  alphaMat<-matrix(c(alpha0,b0),nrow=sample_num+1,ncol=1)#先建立参数扩充矩阵
  iterk<-0
  misssample<-vector()
  while(iterk>=0){
    alpha_vec<-alphaMat[1:sample_num,iterk+1]#vector
    b<-alphaMat[sample_num+1,iterk+1]#一个数
    alpha_cls<-matrix(alpha_vec*clsdata[,1],nrow = sample_num,ncol=1)
    signMat<-(Gram%*%alpha_cls+b)%*%t(clsdata)#计算判断矩阵
    sign_vec<-diag(signMat)#得到不同点的计算结果
    minlab<-which.min(sign_vec)#挑出误分最严重的点
    if(sign_vec[minlab]>0) break
    alphaChange<-alpha_vec
    alphaChange[minlab]<-alphaChange[minlab]+aita
    bChange<-b+aita*clsdata[,1][minlab]
    AllChange<-matrix(c(alphaChange,bChange),sample_num+1,1)
    alphaMat<-cbind(alphaMat,AllChange)
    misssample[iterk+1]<-minlab
    iterk<-iterk+1
  }
  rownames(alphaMat)<-c(paste0("alpha",1:sample_num),"b")
  colnames(alphaMat)<-paste0("iter",0:iterk)
  Finalalpha<-t(alphaMat[,ncol(alphaMat)])#vector
  Finalweight<-rep(0,length(atr))
  for(i in 1:sample_num){#计算weight
    weight<-clsdata[,1][i]*Finalalpha[i]*atrdata[i,]
    Finalweight<-Finalweight+weight
  }
  Finalweight<-c(Finalweight,Finalalpha[sample_num+1])
  Finalweight<-matrix(Finalweight,nrow=1);colnames(Finalweight)<-c(atr,"b")
  PerceptDual<-list(Finalweight=Finalweight,Finalalpha=Finalalpha,
                    iteration=iterk,Alpha=alphaMat,miss=misssample,
                    atrdata=atrdata,clsdata=clsdata,aita=aita,
                    alpha0=alpha0,b0=b0)
  class(PerceptDual)<-c("DualPercept","linePercept")
  return(PerceptDual)
}
### 例2.2 的R实现 ###
###  使用了S3类的继承性质 ###
perpectdual<-DualPercept(cls="y",atr = c("x1","x2"),data=dataB2.1)
plot(perpectdual)
perpectdual
class(perpectdual)
names(perpectdual)
perpectdual[1:5]
predict(perpectdual,cls="y",atr = c("x1","x2"),atr_value = data_atr)



#### 第三章  K近邻 ####
#### 2维平衡kd树的R实现 ####
### kd_tie()函数用于计算二维kd树的不同次迭代的所有节点 ###
### tielist值指节点，奇数的list对应x1（第一维）的结点，偶数的list对应x2的结点，list的顺序对应迭代的顺序###
### x_1，x_2的值指各次迭代中对应的子样本集，x_1，x_2一一对应 ###
kd_tie<-function(x1,x2){
  x_1<-list(list(x1));x_2<-list(list(x2));l<-length(x1)
  timelab<-1;tielist<-list()
  while(timelab<=l){
    if(!timelab%%2==0){#奇数为第一维的数据及结点，偶数对应的是第二维的数据及结点
      x<-x_1[[timelab]]#获取一个list
    } else {x<-x_2[[timelab]]}
    tie<-sapply(x,median)
    tie<-round(tie)
    tielist[[timelab]]<-tie
    x_1_new<-list()
    x_2_new<-list()
    lstx<-length(x)
    for(j in 1:lstx){
      xj<-x[[j]]
      x_left<-which(xj<round(median(xj)))
      x_right<-which(xj>round(median(xj)))
      x_1_new[[2*j-1]]<-x_1[[timelab]][[j]][x_left]
      x_1_new[[2*j]]<-x_1[[timelab]][[j]][x_right]
      x_2_new[[2*j-1]]<-x_2[[timelab]][[j]][x_left]
      x_2_new[[2*j]]<-x_2[[timelab]][[j]][x_right]
    }
    x_1[[timelab+1]]<-x_1_new
    x_2[[timelab+1]]<-x_2_new
    lbreak<-sapply(x_1_new,length)
    if(any(lbreak<=1)){
      end_timetab<-timelab+1
      if(!end_timetab%%2==0){
        end_tie<-sapply(x_1_new,median)
        tielist[[end_timetab]]<-round(end_tie)
      } else {
        end_tie<-sapply(x_2_new,median)
        tielist[[end_timetab]]<-round(end_tie)
      }
      break
    }
    timelab<-timelab+1
  }
  list(tielist=tielist,x_1=x_1,x_2=x_2)
}
x1<-c(2,4,5,7,8,9,12,13,11,1,3,14);x2<-c(3,7,4,2,1,6,9,5,12,11,13,15)
x1<-c(2,4,5,7,8,9);x2<-c(3,7,4,2,1,6)
kd_tie(x1,x2)

kd_plot<-function(x1,x2){
  m1<-max(x1);m2<-max(x2);s1<-min(x1);s2<-min(x2);l<-length(x1)
  kd2_out<-kd_tie(x1=x1,x2=x2)
  tie_kd2<-kd2_out$tielist#提取包含结点信息的list
  ltie<-length(tie_kd2)
  plot(x1,x2,xlim = c(s1,m1),ylim = c(s2,m2),type = "n",xlab ="x(1)",ylab="x(2)",
       main="Balance kd2 tree plot")
  points(x1,x2,pch=19)
  xkd<-tie_kd2[[1]][1]
  abline(v=xkd,col="red",lty=3)
  for(i in 2:ltie){
    plt<-tie_kd2[[i]]
    lplt<-length(plt)
    lsep<-seq(1,lplt,by=2)
    if(i%%2==0){
      for(j in lsep){
        lines(c(s1-1,xkd[(j+1)/2]),c(plt[j],plt[j]),col="red",lty=3)
        lines(c(xkd[(j+1)/2],m1+1),c(plt[j+1],plt[j+1]),col="red",lty=3)
      } 
    }else{
      for(j in lsep){
        lines(c(plt[j],plt[j]),c(s2-1,xkd[(j+1)/2]),col="red",lty=3)
        lines(c(plt[j+1],plt[j+1]),c(xkd[(j+1)/2],m2+1),col="red",lty=3)
      } 
    }
    xkd<-tie_kd2[[i]]
  }
  return(tiekd2=kd2_out)
}
x1<-c(-2,4,5,7,8,9,-5);x2<-c(3,7,-4,2,1,6,8)
kd_plot(x1=x1,x2=x2)
####  Lp范数递减性模拟 ####
LpSim.Plot<-function(number,maxp=1){
  if(any(number<0)) stop("The number must not smaller than 0.")
  max_num<-max(number)
  LpVec<-vector(length = maxp)
  for(i in 1:maxp) LpVec[i]<-(sum(number^i))^(1/i)
  tye<-ifelse(maxp<=20,"b","l")
  cols<-ifelse(maxp<=20,"blue","red")
  plot(1:maxp,LpVec,type = tye,ylab="LpValue",xlab="p",col=cols,
       main="Simulate Plot of Lp")
  list(maxnumber=max_num,minLp=min(LpVec),LpValue=LpVec)
}
LpSim.Plot(number = 1:5,maxp=15)
LpSim.Plot(number = 1:5,maxp=150)
LpSim.Plot(number = sample(1:50,10),maxp=15)
#### 基于线性扫描的KNN实现 lineKnn()####
#计算一个实例与所有训练样本的Lp距离，data仅含特征信息
LpCalculate<-function(dataTest,atr=c("x1","x2","x3"),dataTrain=NULL,p=2){
  datause<-as.matrix(dataTrain);n<-nrow(datause)
  LpVec<-vector(length = n)
  for(i in 1:n) LpVec[i]<-(sum(abs(dataTest-datause[i,atr])^p))^(1/p)
  return(LpVec)
}

lineKnn<-function(cls="y",atr=c("x1","x2","x3"),dataTrain=NULL,
                  dataTest=NULL,k=3,p=2){
  data_use<-dataTrain#注意数据框中有字符时所有被转成字符
  atrdata<-data_use[,atr,drop=F];clsdata<-data_use[,cls]#vector
  dataTest<-dataTest[,atr]
  LpMat<-t(apply(dataTest,1,LpCalculate,atr=atr,dataTrain=atrdata,p=p))
  options(warn=-1)
  library(dprep)
  if(k==1){
    clsMatk<-apply(LpMat,1,function(x) clsdata[order(x)[1:k]])
    kPredict<-clsMatk#vector
  } else{
    clsMatk<-t(apply(LpMat,1,function(x) clsdata[order(x)[1:k]]))#只需要知道最近k个的类别
    kPredict<-apply(clsMatk,1,function(x) sample(moda(x),1))
  }#moda适用于字符向量
  detach("package:dprep")
  outPredict<-dataTest;outPredict[,cls]<-kPredict
  df<-list(FinalPredict=kPredict,PredictMat=outPredict,clsMatk=clsMatk,
           LpMat=LpMat,dataTrain=dataTrain,dataTest=dataTest,atr=atr,cls=cls,k=k,p=p)
  class(df)<-"lineKnn"
  return(df)
}

print.lineKnn<-function(Knnobj){
  print(Knnobj[1])
}

plot.lineKnn<-function(Knnobj){
  Train<-Knnobj$dataTrain
  Test<-Knnobj$dataTest
  atr<-Knnobj$atr;cls<-Knnobj$cls
  latr<-length(atr)
  if(latr==2){
    plot(Train[,atr[1]],Train[,atr[2]],xlab=atr[1],ylab=atr[2],
         col=as.numeric(as.factor(Train[,cls])),pch=abs(as.numeric(as.factor(Train[,cls]))),
         main="Predict Plot of Knn")
    points(Test[,atr[1]],Test[,atr[2]],col="blue",
           pch=abs(as.numeric(as.factor(Knnobj$FinalPredict))),cex=2)
  }
}
#### 示例iris数据集  ####
lab<-sample(1:150,130)
iris[,1:2]
dataKnn_iris<-iris[lab,]
dataKnn_iris_test<-iris[-lab,]
dataKnn_iris_atr<-iris[-lab,-5]
Knn_iris<-lineKnn(cls="Species",atr=c("Sepal.Length","Sepal.Width"),
        dataTrain = dataKnn_iris,dataTest = dataKnn_iris_atr,k=3,p=10)
names(Knn_iris)
sapply(Knn_iris,is)
cbind(as.character(dataKnn_iris_test[,5]),as.character(Knn_iris$FinalPredict))
Knn_iris$PredictMat
Knn_iris$LpMat
Knn_iris$clsMatk

#### 第四章 朴素贝叶斯法的实现 ####
#### navieBayes() 基于极大似然估计及贝叶斯估计的朴素贝叶斯法实现（离散特征情形）####
navieBayes<-function(cls="Y",atr=c("X1","X2"),data=NULL,lmada=0){
  if(!is.data.frame(data)) stop("Please enter a data.frame.")
  if(lmada<0) stop("lmada must be greater than or equal to ZERO.")
  d<-as.data.frame(apply(data,2,as.factor))
  n<-nrow(d)
  prodvar_lst<-list()#用来装计算出来的概率
  prec_var<-d[cls][,1];levelprec<-levels(prec_var);lprec<-length(levelprec)
  prec_p<-data.frame(level=levelprec,prob=NA)
  for(i in 1:lprec){
    prec_p[i,2]<-(sum(prec_var==levelprec[i])+lmada)/(n+lprec*lmada)#类Y的先验概率
  }
  prodvar_lst[[cls]]<-prec_p
  lvar=length(atr)#特征个数
  for(i in 1:lvar){#特征的条件先验概率
    xvar<-d[atr[i]][,1]
    txy<-table(xvar,prec_var)+lmada
    ptxy<-prop.table(txy,2)
    prodvar_lst[[atr[i]]]<-ptxy
  }
  prodvar_lst$lmada<-lmada
  prodvar_lst$response<-cls
  prodvar_lst$variables<-atr
  class(prodvar_lst)<-"navieBayes"  #指定输出对象的类为"navieBayes"，以便编写S3类泛函
  return(prodvar_lst)
}
navieBayes(cls="Y",atr=c("X1","X2"),data=dataB4.1,lmada = 1)

####  编写打印函数：print.navieBayes() ####
print.navieBayes<-function(obj){
  cat("response = prec_var: ",obj$response,";","lmada = ",obj$lmada,"\n","\n")
  cat("The variables are : ",obj$variables,"\n","\n")
  lobj<-length(c(obj$response,obj$variables))
  print.default(obj[1:lobj])
}

####  编写预测函数: predict.navieBayes ####
#### preCnavieBayes() 只能进行一个实例的预测 ####
preCnavieBayes<-function(NBobj,cls=NULL,atr=NULL,atr_value=NULL){
  level<-NBobj[[cls]][,1];ncls<-length(level)
  latr<-length(NBobj)-4#特征的个数
  start_atr<-2
  end_atr<-latr+1
  predict_df<-data.frame(matrix(NA,ncls,latr+2))#先建立一个数据框储存结果
  colnames(predict_df)<-c(atr,"level","post_p")
  for(l in 1:latr){
    predict_df[1:ncls,l]<-atr_value[l]
  }
  predict_df$level<-level
  for(i in 1:ncls){
    xvec<-NULL
    for(j in start_atr:end_atr){
      xwhich<-which(rownames(NBobj[[atr[j-1]]])==as.character(atr_value[j-1]))
      ywhich<-which(colnames(NBobj[[atr[j-1]]])==as.character(predict_df$level[i]))
      px<-NBobj[[atr[j-1]]][xwhich,ywhich]
      xvec<-c(xvec,px)
    }
    ypre<-NBobj[[1]][,2][i]
    predict_df[i,4]<-ypre*prod(xvec)
  }
  return(predict_df)
}

#### 泛函predict.navieBayes()针对类“navieBayes”，可一次进行多个样本实例的预测 ####
predict.navieBayes<-function(NBobj,cls=NULL,atr=NULL,atr_value=NULL){
  if(!is.data.frame(atr_value)) stop("atr_value must be a data.frame!")
  post_lst<-apply(atr_value,1,preCnavieBayes,NBobj=NBobj,atr=atr,cls=cls)
  lst<-length(post_lst)
  post_df<-NULL
  for(i in 1:lst){
    prc_df<-post_lst[[i]]
    post_df<-rbind(post_df,prc_df)
  }
  cat("The response : ",cls,"\n")
  return(post_df)
}

####  例4.1 ####
X1<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
X2<-c("S","M","M","S","S","S","M","M","L","L","L","M","M","L","L")
Y<-c(-1,-1,1,1,-1,-1,-1,1,1,1,1,1,1,1,-1)
dataB4.1<-data.frame(X1=X1,X2=X2,Y=Y)
## lmada=0 极大似然估计 ##
plist<-navieBayes(cls="Y",atr=c("X1","X2"),data=dataB4.1,lmada = 0)
pred_var<-data.frame(X1=c(2,1,1,3,3),X2=c("S","L","S","M","L"))
predict(plist,cls="Y",atr=c("X1","X2"),atr_value =pred_var)

## 例4.2 lmada=1  贝叶斯估计 拉普拉斯平滑 ##
plist1<-navieBayes(cls="Y",atr=c("X1","X2"),data=dataB4.1,lmada = 1)
pred_var<-data.frame(X1=c(2,1),X2=c("S","L"))
predict(plist1,cls="Y",atr=c("X1","X2"),atr_value =pred_var)

## lmada=3 ###
plist3<-navieBayes(cls="Y",atr=c("X1","X2"),data=dataB4.1,lmada = 3)
pred_var<-data.frame(X1=c(2,1),X2=c("S","L"))
predict(plist3,cls="Y",atr=c("X1","X2"),atr_value =pred_var)

print.default(plist3)

plist
class(plist)
names(plist)
plist$lmada
plist$variables
sapply(plist,class)
?str
1/15
1/45

####  第五章 决策树 ####
#### 0-1分布的H(p)曲线 ####
p<-pretty(c(0.01,0.99),100)
HpVec<-vector(length = length(p))
for(i in 1:length(p)) HpVec[i]<--p[i]*log(p[i],2)-(1-p[i])*log(1-p[i],2)
plot(p,HpVec,type="l",col="red");abline(v=0.5,lty=3)

#### 编写函数计算信息增益及信息增益比 InfoGain() ####
InfoGain<-function(cls=NULL,atr=NULL,method=c("info","inforate"),data=NULL){
  HDfunc<-function(atrcls){#atrcls为向量
    l<-length(atrcls)
    tatrcls<-table(atrcls)
    atrclspvec<-as.vector(tatrcls)/l
    logatrclspvec<-ifelse(atrclspvec==0,0,log(atrclspvec,2))
    HD<--as.vector(atrclspvec%*%logatrclspvec)
    return(HD)
  }
  HDcls<-HDfunc(atrcls = data[,cls])
  HDatr<-apply(data[,atr],2,HDfunc)
  HatrVec<-apply(data[,atr],2,Hatr,clsvec=data[,cls])
  if(method=="info"){
    infogain<-HDcls-HatrVec
  } else if(method=="inforate"){
    infogain<-(HDcls-HatrVec)/HDatr
  } else stop("Please choose a useable method.")
  names(infogain)<-atr
  list(infogain=infogain,HDcls=HDcls,HatrVec=HatrVec,HDatr=HDatr)
}

Hatr<-function(atrvec=NULL,clsvec=NULL){#输入为特征向量及类别向量，计算经验条件熵
  n<-length(atrvec)
  tatr<-table(atrvec)
  atrpvec<-as.vector(tatr)/n
  txy<-table(atrvec,clsvec)
  ptxy<-prop.table(txy,1)
  logptxy<-ifelse(ptxy==0,0,log(ptxy,2))
  loctab<-ptxy*logptxy#对应位置元素相乘,table
  atr_clspvec<-apply(loctab,1,sum)#vector
  hatr<--as.vector(atrpvec%*%atr_clspvec)
  return(hatr)
}
#### 例5.2的R实现 ####
A1<-rep(c("青年","中年","老年"),each=5)
A2<-c("否","否","是","是","否","否","否","是","否","否","否","否","是","是","否")
A3<-c("否","否","否","是","否","否","否","是","是","是","是","是","否","否","否")
A4<-c("一般","好","好","一般","一般","一般","好","好","非常好","非常好","非常好","好",
      "好","非常好","一般")
Y<-c("否","否","是","是","否","否","否","是","是","是","是","是","是","是","否")
dataB5.1<-data.frame(A1,A2,A3,A4,Y);dataB5.1
Hatr(atrvec = dataB5.1$A1,clsvec = dataB5.1$Y)
InfoGain(cls="Y",atr=c("A1","A2","A3","A4"),method="info",data=dataB5.1)
InfoGain(cls="Y",atr=c("A1","A2","A3","A4"),method="inforate",data=dataB5.1)
#### 改变后的样本数据计算信息增益 ####
A1<-rep(c("少年","青年","中年","老年","晚年"),each=3)#改为5类
A2<-c("否","否","是","是","否","否","否","是","否","否","否","否","是","是","否")
A3<-c("否","否","否","是","否","否","否","是","是","是","是","是","否","否","否")
A4<-c("坏","好","好","坏","一般","一般","好","好","非常好","非常好","非常好","好",
      "好","极好","一般")#改为5类
Y<-c("否","否","是","是","否","否","否","是","是","是","是","是","是","是","否")
dataB5.1<-data.frame(A1,A2,A3,A4,Y);dataB5.1
InfoGain(cls="Y",atr=c("A1","A2","A3","A4"),method="info",data=dataB5.1)
InfoGain(cls="Y",atr=c("A1","A2","A3","A4"),method="inforate",data=dataB5.1)

#### 例5.3的R实现 ####
library(dprep)
subTree<-function(cls="Y",atr=c("A1","A2","A3","A4"),method=c("info","inforate"),
                  data=NULL,ept=0.1){
  atrcl<-atr;clscl<-cls;datacl<-data
  clsclvalue<-unique(datacl[,clscl])
  infoCalcul<-InfoGain(cls=clscl,atr=atrcl,data=datacl,method = method)#首次迭代结果
  subtree<-list()
  if(length(clsclvalue)==1){
    subtree[["origindata"]]<-datacl
    subtree[["single"]]<-clsclvalue
    subtree[["infoatr"]]<-"None"
    return(subtree)
  } else if(length(atrcl)==0||max(infoCalcul$infogain)<ept){
    lab<-moda(datacl[,clscl])
    subtree[["origindata"]]<-datacl
    if(length(lab)==1) subtree[["single"]]<-lab
    if(length(lab)>=2) subtree[["single"]]<-sample(lab,1)
    subtree[["infoatr"]]<-"None"
    return(subtree)
  }
  atrlab<-which.max(infoCalcul$infogain);
  atrchs<-datacl[,atrcl[atrlab]]#挑选信息增益最大的特征
  unqatrchs<-unique(atrchs);lunq<-length(unqatrchs)
  for(i in 1:lunq){
    subtree[[i]]<-datacl[which(atrchs==unqatrchs[i]),-atrlab]#每一个组件都是data.frame
  }
  names(subtree)<-paste0(atrcl[atrlab],"=",unqatrchs)
  subtree[["newatr"]]<-atrcl[-atrlab]
  subtree[["infoatr"]]<-atrcl[atrlab]
  return(subtree)
}
####  例5.3求解 ID3 信息增益 ####
stree<-subTree(cls="Y",atr = c("A1","A2","A3","A4"),method="info",data=dataB5.2);stree
stree2<-lapply(stree[1:2],subTree,cls="Y",atr=c("A1","A2","A4"),method="info")
####  例5.3求解 C4.5 信息增益比 ####
stree<-subTree(cls="Y",atr = c("A1","A2","A3","A4"),method="inforate",data=dataB5.2)
stree2<-lapply(stree[1:2],subTree,cls="Y",atr=c("A1","A2","A4"),method="inforate")

####  决策树剪枝cutTree()  ####
Extree<-function(obj){#用于提取叶节点的子集,obj必须是两层的list
  lobj<-length(obj)
  lvec<-sapply(obj,length)-2
  newlst<-list()
  st<-0
  for(i in 1:lobj){
    for(j in 1:lvec[i]){
      newlst[[st+j]]<-obj[[i]][[j]]
    }
    st<-st+lvec[i]
  }
  return(newlst)
}

####  编写函数计算经验熵 ####
HDfunc<-function(atrcls){#atrcls为向量
  l<-length(atrcls)
  tatrcls<-table(atrcls)
  atrclspvec<-as.vector(tatrcls)/l
  logatrclspvec<-ifelse(atrclspvec==0,0,log(atrclspvec,2))
  HD<--as.vector(atrclspvec%*%logatrclspvec)
  return(HD)
}

#### 计算损失函数 ####
cutTree<-function(cls="Y",data=NULL,alpha=1){#data为Extree()的输出结果
  ldata<-length(data)
  clslst<-list()
  for(i in 1:ldata){
    clslst[[i]]<-data[[i]][,cls]
  }
  hdvec<-sapply(clslst,HDfunc)#每个叶节点的经验熵,vector
  ldvec<-sapply(clslst,length)#每个叶结点的样本量,vector
  Cfunc<-hdvec%*%ldvec+alpha*ldata
  return(Cfunc)
}

stree1<-subTree(cls="Y",atr = c("A1","A2","A3","A4"),method="info",data=dataB5.2)
stree2<-lapply(stree1[1:2],subTree,cls="Y",atr=c("A1","A2","A4"),method="info")
le1<-Extree(list(stree1))
le2<-Extree(stree2)
alp=1
cutTree(cls="Y",data=le1,alpha=alp)
cutTree(cls="Y",data=le2,alpha=alp)

#### 模拟alpha的变动对剪枝的影响 ####
ysimple<-vector(length = 20);ycomplex<-vector(length = 20)
for(i in 1:20){
  ysimple[i]<-as.vector(cutTree(cls="Y",data=le1,alpha=i))
  ycomplex[i]<-as.vector(cutTree(cls="Y",data=le2,alpha=i))
}
plot(1:20,ycomplex,type="b",col="red",xlab="alpha",ylab = "loss",
     main="Loss Plot of Alpha")
points(1:20,ysimple,type="b",col="blue")
text(locator(),"SimpleTree",col="blue")
text(locator(),"ComplexTree",col="red")

####  Gini(p)与1/2H(p)关系模拟 ####
#p是一个概率向量，及满足所有元素的和为1
HpSim<-function(p){#熵
  lp<-ifelse(p==0,0,log(p,2))
  hp<--p%*%lp
  return(hp)
}
GpSim<-function(p){#基尼系数
  p2<-1-p
  gp<-p%*%p2
  return(gp)
}
#随机生成概率分布p#
pCreate<-function(l=10,chs=1000){
  num<-sample(0:chs,l)
  p<-num/sum(num)
  return(p)
}

plist<-list()
lst<-vector(length = 100)
chlst<-vector(length = 100)
for(i in 1:100){
  l<-sample(2:20,1)
  chs<-sample(30:1000,1)
  lst[i]<-l
  chlst[i]<-chs
  plist[[i]]<-pCreate(l=l,chs=chs)
}
all(sapply(plist,sum)==1)#检查是否和都为1
hpvec<-sapply(plist,HpSim)
gpvec<-sapply(plist,GpSim)
dataHG<-data.frame(K=lst,halfHp=hpvec/2,
                   Gp=gpvec,Hp=hpvec)
datahg<-dataHG[order(dataHG[,1]),]
plot(datahg$K,datahg$Hp,type="b",col="black",xlab = "K",
     ylab="uncertainty",main="The Plot of Hp and Gp")
points(datahg$K,datahg$halfHp,type = "b",col="red")
points(datahg$K,datahg$Gp,type = "b",col="blue")
text(locator(),"Gp",col="blue")
text(locator(),"halfHp",col="red")
text(locator(),"Hp",col="black")

####  图5.7的R实现 ####
p<-pretty(c(0.01,0.99),100)
HpVec<-vector(length = length(p))
GpVec<-vector(length = length(p))
for(i in 1:length(p)){
  HpVec[i]<--p[i]*log(p[i],2)-(1-p[i])*log(1-p[i],2)
  GpVec[i]<-2*p[i]*(1-p[i])
} 
error<-ifelse(p<.5,p,1-p)
plot(p,HpVec/2,type="l",col="red",xlab="p",ylab="value")
lines(p,GpVec,type="l",col="blue")
lines(p,error,type = "l",col="black")
abline(v=0.5,lty=3)
text(locator(),"Gp",col="blue")
text(locator(),"halfHp",col="red")
text(locator(),"error",col="black")

####  基尼系数的R实现、例5.4的程序求解  ####
GpSim<-function(p){#基尼系数
  p2<-1-p
  gp<-p%*%p2
  return(gp)
}

GiniSingle<-function(atrvec=NULL,clsvec=NULL){#输入的是特征向量与属性向量
  D<-length(clsvec)
  txy<-table(atrvec,clsvec)
  nam<-rownames(txy)
  unqatr<-unique(atrvec)
  lunq<-length(unqatr)
  giniatr<-vector(length = lunq)
  for(i in 1:lunq){
    t1<-txy[i,];st1<-sum(t1)
    t2<-txy[-i,,drop=F];st2<-sum(t2)
    p1<-t1/st1;p2<-apply(t2,2,sum)/st2
    giniatr[i]<-(st1/D)*GpSim(p1)+(st2/D)*GpSim(p2)
  }
  names(giniatr)<-nam
  return(giniatr)
}
GiniSingle(A1,Y)
GiniSingle(A2,Y)
GiniSingle(A3,Y)
GiniSingle(A4,Y)

GiniCART<-function(cls=NULL,atr=NULL,data=NULL){
  if(length(unique(data[,cls]))==1) return(list(Finalabel="None",D=data))
  ginilst<-apply(data[,atr],2,GiniSingle,clsvec=data[,cls])#list
  nlst<-names(ginilst)
  outgini<-sapply(ginilst,function(x) rbind(which.min(x),min(x)))
  nvec<-vector(length = length(atr))
  for(i in 1:length(atr)){
    ns<-names(ginilst[[i]])
    nvec[i]<-ns[outgini[1,i]]
  }
  minlab<-which.min(outgini[2,])
  atrlab<-outgini[1,minlab];atrchs<-names(ginilst[[minlab]])[atrlab]
  lab<-which(data[,nlst[minlab]]==atrchs)
  list(Finalabel=c(nlst[minlab],atrchs),FinalGini=outgini[2,minlab],
       GiniMat=outgini,Ginilst=ginilst,data[lab,-minlab],data[-lab,-minlab])
}

cart1<-GiniCART(cls="Y",atr=c("A1","A2","A3","A4"),data=dataB5.2);cart1[1:4]
cart2<-lapply(cart1[5:6],GiniCART,cls="Y",atr=c("A1","A2","A4"));cart2[[1]]
da1<-cart1[[5]]
table(da1$A2,da1$Y)


#### 第六章 逻辑斯蒂回归于最大熵模型 ####
gradLogistic<-function(cls=NULL,atr=NULL,data=NULL,scale=TRUE,
                       w0=rep(0,length(atr)+1),aita=1,ept=1e-5,maxiter=100000){
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  datause<-data;datause$xb<-1#扩充矩阵
  atrdata<-datause[,c(atr,"xb"),drop=F];atrdata<-as.matrix(atrdata)
  if(scale){#自变量数据标准化
    for(i in 1:length(atr)){
      atrdata[,i]<-scale(atrdata[,i])#0,1标准化
    }
  }
  clsdata<-datause[,cls,drop=F];clsdata<-as.matrix(clsdata)
  N<-nrow(datause)
  MinusLog<-function(wuse,y=clsdata[,1],x=atrdata){
    n<-nrow(atrdata)
    MLog<-vector(length = n)
    for(i in 1:n){
      ep<-as.vector(wuse%*%x[i,])
      epe<-exp(ep)
      if(is.infinite(epe)){
        MLog[i]<-ep-y[i]*ep
      } else{
        MLog[i]<-log(1+epe)-y[i]*ep
      }
    }
    return(sum(MLog))
  }
  calpi<-function(x){
    ex<-exp(w%*%x)
    if(is.infinite(ex)){
      px<-1
    } else{
      px<-ex/(1+ex)
    }
    return(px)
  }
  w<-w0#指定w0,vector
  iterk<-1
  while(iterk>=1){
    pi<-apply(atrdata,1,calpi)#指定pi(k),vector
    piMinusy<-matrix(pi-clsdata[,1],nrow = N,ncol=1)#N*1矩阵
    gradf<-t(atrdata)%*%piMinusy#利用矩阵乘法,N*1矩阵
    gradfvec<-gradf[,1]
    #print(sqrt(sum(gradfvec^2)))
    if(sqrt(sum(gradfvec^2))<=ept){
      stoprule<-'sqrt(sum(gradfvec^2))<=ept'
      break
    }
    wbefore<-w
    #print(w)
    w<-w-aita*gradfvec
    MinusLogBtw<-MinusLog(wuse=w)-MinusLog(wuse=wbefore)
    wBtw<-w-wbefore
    if(abs(MinusLogBtw)<ept||sqrt(sum(wBtw^2))<ept){
      stoprule<-'abs(MinusLogBtw)<ept||sqrt(sum(wBtw^2))<ept' 
      break
    } 
    if(iterk>=maxiter){
      stoprule<-'iterk>=maxiter'
      break
    } 
    iterk<-iterk+1
    #print(iterk)
  }
  names(w)<-c(atr,"b")
  outlst<-list(weight=w,minusLogkplus1=MinusLog(wuse=w),
               minusLogk=MinusLog(wuse=wbefore),variable=atr,
               response=cls,origindata=data,iteration=iterk,
               formula=paste(cls,"~",paste(atr,collapse = "+")),
               stoprule=stoprule)
  class(outlst)<-"gradLogistic"
  return(outlst)
}

print.gradLogistic<-function(obj){
  cat("The stoprule is : ",obj$stoprule,"\n")
  cat("iteration : ",obj$iteration,"\n")
  cat("formula : ",obj$formula,"\n")
  oldlst<-options()
  options(digits = 9)
  print(obj[1:3])
  options(oldlst)
}

predict.gradLogistic<-function(obj,atr=NULL,atr_value=NULL){
  weight<-obj$weight
  atr_value$b<-1
  for(i in 1:length(atr)){
    atr_value[,i]<-scale(atr_value[,i])
  }
  predone<-function(x,w){#x,w均是向量
    ep1<-exp(w%*%x)
    if(is.infinite(ep1)){
      p1<-1-0.001
    } else{
      p1<-ep1/(1+ep1)
    }
    return(p1)
  }
  P1<-apply(atr_value,1,predone,w=weight)
  P0<-1-P1
  predvec<-ifelse(P1>=0.5,1,0)
  pMatdf<-data.frame(P1=P1,P0=P0,predict=predvec)
  list(FinalPredict=predvec,PredictMat=pMatdf)
}

#### 利用mtcars数据集进行测试 ####
####  测试模型1 ####
dataB6.1<-mtcars#训练集
dataB6.1_pred<-mtcars[,c("mpg","cyl","disp","hp")]#回测自变量数据集
gradlog1<-gradLogistic(data=dataB6.1,cls="am",
                       atr=c("mpg","cyl","disp","hp"))#训练模型
predLog1<-predict(gradlog1,atr=c("mpg","cyl","disp","hp"),
                  atr_value = dataB6.1_pred)#模型回测
miss<-data.frame(newG=predLog1$FinalPredict,G=mtcars$am)
tbl1<-table(miss$newG,miss$G);tbl1
sum(diag(tbl1))/sum(tbl1)#正确率


dataB6.1_pred2<-dataB6.1[,c("mpg","cyl","disp","hp","drat","wt","qsec")]
gradlog2<-gradLogistic(data=dataB6.1,cls="vs",
                       atr=c("mpg","cyl","disp","hp","drat","wt","qsec"))
predLog2<-predict(gradlog2,atr=c("mpg","cyl","disp","hp","drat","wt","qsec"),
                  atr_value = dataB6.1_pred2)

miss2<-data.frame(newG=predLog2$FinalPredict,G=mtcars$vs)
tbl2<-table(miss2$newG,miss2$G);tbl2
sum(diag(tbl2))/sum(tbl2)#正确率


dataB6.1_pred3<-dataB6.1[,c("mpg","cyl","drat","wt")]
gradlog3<-gradLogistic(data=dataB6.1,cls="vs",
                       atr=c("mpg","cyl","drat","wt"))

predLog3<-predict(gradlog3,atr=c("mpg","cyl","drat","wt"),
                  atr_value = dataB6.1_pred3)
miss3<-data.frame(newG=predLog3$FinalPredict,G=mtcars$vs)
tbl3<-table(miss3$newG,miss3$G);tbl3
sum(diag(tbl3))/sum(tbl3)#正确率
####  迭代效果模拟：迭代50/100/1000/1万/10万/100万次 ####
gradLogistic(data=dataB6.1,cls="am",
             atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 50)

gradLogistic(data=dataB6.1,cls="am",
             atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 100)

gradLogistic(data=dataB6.1,cls="am",
             atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 1000)

gradLogistic(data=dataB6.1,cls="am",
            atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 10000)

gradLogistic(data=dataB6.1,cls="am",
            atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 100000)

gradLogistic(data=dataB6.1,cls="am",
            atr=c("mpg","cyl","disp","hp"),ept = 1e-10,maxiter = 1000000)

#### DFP算法的实现 ####
DFPLogistic<-function(cls=NULL,atr=NULL,data=NULL,scale=TRUE,ept=1e-5,
                      G0=diag(rep(1,length(atr)+1)),MoreStopRule=FALSE,
                      w0=rep(0,length(atr)+1),aita=.1,maxiter=100000,
                      SearchAita=F,maxsearch=1000){
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  datause<-data;datause$xb<-1#扩充矩阵
  atrdata<-datause[,c(atr,"xb"),drop=F];atrdata<-as.matrix(atrdata)
  if(scale){#自变量数据标准化
    for(i in 1:length(atr)){
      atrdata[,i]<-scale(atrdata[,i])#0,1标准化
    }
  }
  clsdata<-datause[,cls,drop=F];clsdata<-as.matrix(clsdata)
  N<-nrow(datause)
  MinusLog<-function(wuse,y=clsdata[,1],x=atrdata){#计算g(w)
    n<-nrow(atrdata)
    MLog<-vector(length = n)
    for(i in 1:n){
      ep<-as.vector(wuse%*%x[i,])
      epe<-exp(ep)
      if(is.infinite(epe)){
        MLog[i]<-ep-y[i]*ep
      } else{
        MLog[i]<-log(1+epe)-y[i]*ep
      }
    }
    return(sum(MLog))
  }
  calpi<-function(x,wx){#计算pi
    oldex<-wx%*%x
    ex<-exp(oldex)
    if(is.infinite(ex)){
      px<-1+sample(c(-ept*10/3,-ept*6,-ept*5/4,-ept/2),1)
    } else if(ex==0){
      px<-sample(c(ept*10/3,ept*15,ept*9/4,ept*17/2),1)
    } else{
      px<-ex/(1+ex)
    }
    return(px)
  }
  calgrad<-function(dfatr,dfcls,Nl,wc){#计算梯度
    pi<-apply(dfatr,1,calpi,wx=wc)#指定pi(k),vector
    piMinusy<-matrix(pi-dfcls[,1],nrow = Nl,ncol=1)#N*1矩阵
    gradfCal<-t(dfatr)%*%piMinusy#利用矩阵乘法,（n+1）*1矩阵，计算梯度
    return(gradfCal)
  }
  findAita<-function(dataatr,datacls,wkk,dtakk,ata_ept=1e-1,#一维搜索函数
                   ata0=1,maxatak=maxsearch){#wk,dtak为vector
    expata1<-function(wk,dtak,ati,x){
    exaita1<-as.vector((wk-ati*dtak)%*%x)
    expcal1<-exp(exaita1)
    if(is.infinite(expcal1)){
      pi1<-1+sample(c(-ata_ept/3,-ata_ept,-ata_ept/4,-ata_ept/2),1)
    } else if(expcal1==0){
      pi1<-sample(c(ata_ept/3,ata_ept,ata_ept/4,ata_ept/2),1)
    } else{
      pi1<-expcal1/(1+expcal1)
    }
    pi1
  }
    expata2<-function(wk,dtak,ati,x){
    exaita2<-as.vector((wk-ati*dtak)%*%x)
    expcal2<-exp(exaita2)
    if(is.infinite(expcal2)){
      pi2<-sample(c(ata_ept/3,ata_ept,ata_ept/4,ata_ept/2),1)
    } else if(expcal2==0){
      pi2<-sample(c(ata_ept/3,ata_ept,ata_ept/4,ata_ept/2),1)
    } else{
      pi2<-expcal2/(1+expcal2)^2
    }
    pi2
  }
    ata<-ata0
    iteratak<-1
    while(iteratak>=1){
      p1<-apply(dataatr,1,expata1,wk=wkk,dtak=dtakk,ati=ata)
      p2<-apply(dataatr,1,expata2,wk=wkk,dtak=dtakk,ati=ata)
      ppi<-p1-datacls[,1]
      dtkM<-matrix(dtakk,nrow=length(dtakk),ncol=1)
      dtkx<-as.vector(dataatr%*%dtkM)
      H1<-as.vector(ppi%*%dtkx)
      H2<-as.vector(p2%*%(dtkx^2))
      ataold<-ata
      atanew<-ata-H1/H2
      ata<-atanew
      if(abs(atanew-ataold)<ata_ept) break
      if(iteratak>=maxatak) break
      iteratak<-iteratak+1
      }
  return(ata)
  }
  w<-w0#指定w0,vector
  G<-G0#指定初始正定矩阵
  changeG<-0
  changeW<-0
  changeAita<-0
  iterk<-1
  while(iterk>=1){
    #if(iterk>=4) browser()
    if(any(is.infinite(w))||any(is.nan(w))){
       w<-sample(seq(-2,2,length=200),length(w0))
       changeW<-changeW+1
    }
    #如果计算出Inf或-Inf或NaN，则重新选择w继续迭代
    gradf<-calgrad(dfatr=atrdata,dfcls=clsdata,Nl=N,wc=w)#（n+1）*1矩阵，计算梯度
    gradfvec<-gradf[,1]#获得梯度向量
    #print(sqrt(sum(gradfvec^2)))
    if(sqrt(sum(gradfvec^2))<=ept){
      stoprule<-'sqrt(sum(gradfvec^2))<=ept'
      break
    }
    wbefore<-w
    #print(w)
    if(any(is.infinite(G))||any(is.nan(G))){
     G<-G0*sample(seq(-1,1,length=1000),1)
     w<-sample(seq(-10,10,length=200),length(w0))
     aita<-sample(seq(0.01,2,length=100),1)
     changeW<-changeW+1
     changeG<-changeG+1
     changeAita<-changeAita+1
    } 
    #如果计算出Inf或-Inf或NaN，则重新选择正定的G/w/aita，重新迭代
    ## 进入一维搜索，寻找最优步长 ##
    olddelta<-G%*%gradf
    if(SearchAita){
      aita<-findAita(dataatr = atrdata,datacls = clsdata,wkk = wbefore,
                     dtakk = as.vector(olddelta),ata0 = aita)
      aita<-max(0.1,aita)
    }
    deltak<--aita*olddelta#(n+1)*1矩阵
    wnew<-wbefore+as.vector(deltak)#更新w
    w<-wnew
    gradfnew<-calgrad(dfatr=atrdata,dfcls=clsdata,Nl=N,wc=wnew)#w已经改变
    yk<-gradfnew-gradf#(n+1)*1矩阵
    G<-G+(deltak%*%t(deltak))/as.vector(t(deltak)%*%yk)-
      (G%*%yk%*%t(yk)%*%G)/as.vector(t(yk)%*%G%*%yk)#更新G
    if(MoreStopRule){
      MinusLogBtw<-MinusLog(wuse=w)-MinusLog(wuse=wbefore)
      wBtw<-w-wbefore
      if(abs(MinusLogBtw)<ept||sqrt(sum(wBtw^2))<ept){
      stoprule<-'abs(MinusLogBtw)<ept||sqrt(sum(wBtw^2))<ept' 
        break
      } 
    }
    if(iterk>=maxiter){
      stoprule<-'iterk>=maxiter'
      break
    } 
    iterk<-iterk+1
    #print(iterk)
  }
  names(w)<-c(atr,"b")
  outlst<-list(weight=w,minusLogkplus1=MinusLog(wuse=w),
               minusLogk=MinusLog(wuse=wbefore),LpGradf=sqrt(sum(gradfvec^2)),
               changW=changeW,changeG=changeG,changeAita=changeAita,
               variable=atr,response=cls,
               origindata=data,iteration=iterk,
               formula=paste(cls,"~",paste(atr,collapse = "+")),
               stoprule=stoprule)
  class(outlst)<-"DFPLogistic"
  return(outlst)
}

print.DFPLogistic<-function(obj){
  cat("The stoprule is : ",obj$stoprule,"\n")
  cat("iteration : ",obj$iteration,"\n")
  cat("formula : ",obj$formula,"\n")
  oldlst<-options()
  options(digits = 9)
  print(obj[1:7])
  options(oldlst)
}

predict.DFPLogistic<-function(obj,atr=NULL,atr_value=NULL){
  weight<-obj$weight
  atr_value$b<-1
  for(i in 1:length(atr)){
    atr_value[,i]<-scale(atr_value[,i])
  }
  predone<-function(x,w){#x,w均是向量
    ep1<-exp(w%*%x)
    if(is.infinite(ep1)){
      p1<-1-0.001
    } else{
      p1<-ep1/(1+ep1)
    }
    return(p1)
  }
  P1<-apply(atr_value,1,predone,w=weight)
  P0<-1-P1
  predvec<-ifelse(P1>=0.5,1,0)
  pMatdf<-data.frame(P1=P1,P0=P0,predict=predvec)
  list(FinalPredict=predvec,PredictMat=pMatdf)
}


####  DFP算法测试  ####

#### mtcars数据集 模型1：多停止条件 ####
dataDFPLog<-mtcars
dataDFPLog_pred<-mtcars[,c("mpg","cyl","disp","hp")]#回测自变量数据集
DFPLog1<-DFPLogistic(data=dataDFPLog,cls="am",
                    atr=c("mpg","cyl","disp","hp"),ept=1e-3,
                    maxiter = 10000,MoreStopRule = T);DFPLog1#训练模型
predDFPLog1<-predict(DFPLog1,atr=c("mpg","cyl","disp","hp"),
                  atr_value = dataDFPLog_pred)#模型回测
miss<-data.frame(newG=predDFPLog1$FinalPredict,G=mtcars$am)
tbl1<-table(miss$newG,miss$G);tbl1
sum(diag(tbl1))/sum(tbl1)#正确率

#### mtcars数据集 模型2：更改分类变量，单停止条件，进行一维搜索 ####
DFPLog2<-DFPLogistic(data=dataDFPLog,cls="vs",
                    atr=c("mpg","cyl","disp","hp"),ept=1e-3,
                    maxiter = 100000,MoreStopRule = F,SearchAita = T);DFPLog2
#### 对比：不进行一维搜索 ####
DFPLogistic(data=dataDFPLog,cls="vs",
            atr=c("mpg","cyl","disp","hp"),ept=1e-3,
            maxiter = 100000,MoreStopRule = F,SearchAita = F)#不进行一维搜索

#### mtcars数据集 模型3：更改分类变量及自变量，单停止条件 ####
DFPLog3<-DFPLogistic(data=dataDFPLog,cls="vs",
                    atr=c("mpg","hp","wt","qsec"),ept=1e-3,
                    maxiter = 10000,MoreStopRule = F);DFPLog3


#### iris数据集 数据准备 ####
dataDFPLog_iris<-iris[1:100,]
dataDFPLog_iris$Species<-ifelse(dataDFPLog_iris$Species=="setosa",1,0)
trainlab<-sample(100,50)
dataDFPiris_train<-dataDFPLog_iris[trainlab,]
dataDFPiris_test<-dataDFPLog_iris[-trainlab,]
dataDFPiris_test_atr<-dataDFPiris_test[,-5]#测试特征集

#### iris数据集  模型1  ####
DFPLogiris1<-DFPLogistic(cls="Species",data=dataDFPiris_train,
                         atr=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                         ept=1e-3,MoreStopRule = F,maxiter = 10000);DFPLogiris1
predDFPLogiris1<-predict(DFPLogiris1,atr_value = dataDFPiris_test_atr,#模型预测
                      atr=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"))
miss<-data.frame(newG=predDFPLogiris1$FinalPredict,G=dataDFPiris_test$Species)
tbl<-table(miss$newG,miss$G);tbl
sum(diag(tbl))/sum(tbl)#预测正确率


#### iris数据集  模型2  ####
DFPLogiris2<-DFPLogistic(cls="Species",data=dataDFPiris_train,
                         atr=c("Petal.Length","Petal.Width"),
                         ept=1e-3,MoreStopRule = F,maxiter = 10000);DFPLogiris2

predDFPLogiris2<-predict(DFPLogiris2,atr=c("Petal.Length","Petal.Width"),
                  atr_value = dataDFPiris_test_atr[,-c(1,2)])#模型回测
miss<-data.frame(newG=predDFPLogiris2$FinalPredict,G=dataDFPiris_test$Species)
tbl1<-table(miss$newG,miss$G);tbl1
sum(diag(tbl1))/sum(tbl1)#正确率


#### iris数据集  模型3： 另外两类 ####
dataDFPLog_iris2<-iris[51:150,]
dataDFPLog_iris2$Species<-ifelse(dataDFPLog_iris2$Species=="versicolor",1,0)
trainlab2<-sample(1:100,50)
dataDFPiris_train2<-dataDFPLog_iris2[trainlab2,]
dataDFPiris_test2<-dataDFPLog_iris2[-trainlab2,]
dataDFPiris_test_atr2<-dataDFPiris_test2[,-5]#测试特征集


DFPLogiris2_1<-DFPLogistic(cls="Species",data=dataDFPiris_train2,
                         atr=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                         ept=1e-3,MoreStopRule = F,maxiter = 10000);DFPLogiris2_1

predDFPLogiris2_1<-predict(DFPLogiris2_1,
                           atr=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                           atr_value = dataDFPiris_test_atr2)#模型回测
miss<-data.frame(newG=predDFPLogiris2_1$FinalPredict,G=dataDFPiris_test2$Species)
tbl1<-table(miss$newG,miss$G);tbl1
sum(diag(tbl1))/sum(tbl1)#正确率0.92



DFPLogiris2_2<-DFPLogistic(cls="Species",data=dataDFPiris_train,
                         atr=c("Petal.Length","Petal.Width"),
                         ept=1e-3,MoreStopRule = F,maxiter = 10000);DFPLogiris2_2

predDFPLogiris2_2<-predict(DFPLogiris2_2,atr=c("Petal.Length","Petal.Width"),
                  atr_value = dataDFPiris_test_atr2[,-c(1,2)])#模型回测
miss<-data.frame(newG=predDFPLogiris2_2$FinalPredict,G=dataDFPiris_test2$Species)
tbl1<-table(miss$newG,miss$G);tbl1
sum(diag(tbl1))/sum(tbl1)#正确率0.88

#### 第七章 支持向量机 ####
####  基于SMO算法的SVM实现 线性及非线性 ####
## 编写核函数，输入为一个非扩充的特征矩阵，行数为样本id，列为特征；输出为核函数的Gram矩阵 ####
lineKernel<-function(data){#data为数据框
  dMat<-as.matrix(data)
  return(dMat%*%t(dMat))#返回Gram矩阵
}

polyKernel<-function(data,p=2){
  dMat<-as.matrix(data)
  lineGram<-dMat%*%t(dMat)
  polyGram<-(lineGram+1)^p
  return(polyGram)#返回Gram矩阵
}

gaussiKernel<-function(data,lmada=sqrt(ncol(data))/sqrt(2),Lp=2){
  #lmada指定高斯核函数的标准差，默认为特征个数的平方根，基于Lp范
  dMat<-as.matrix(data)
  ln<-nrow(data)
  Gram1<-matrix(NA,nrow=ln,ncol=ln)
  for(i in 1:ln){#i行
    for(j in 1:ln){#j列
      dij<-dMat[i,]-dMat[j,]
      absdij<-abs(dij)
      Lpdij<-(sum(absdij^Lp))^(1/Lp)
      Gram1[i,j]<-Lpdij^2
    }
  }
  Gram2<-Gram1/(-2*(lmada^2))
  gaussiGram<-exp(Gram2)
  return(gaussiGram)
}

#计算g(x)
gfunc<-function(clsvec,gram,alphak,bk){#clsvec为向量,Gram为矩阵,alphak为alpha向量
  ncls<-length(clsvec)
  gvec<-vector(length = ncls)
  for(i in 1:ncls){
    gonevec<-vector(length = ncls)
    for(j in 1:ncls){
      gonevec[j]<-alphak[j]*clsvec[j]*gram[i,j]
    }
    gvec[i]<-sum(gonevec)+bk
  }
  return(gvec)#返回一个vector
}

is(gfunc(clsvec = c(1,-1,1),Gram=testx,alphak = c(1,2,3),bk=3))
is(lineKernel(data=testx))
is(polyKernel(data=testx))
is(gaussiKernel(data=testx))


####  选择工作工作集的方法 ####
findAlpha<-function(alphak,gveck,ept,C,clsvec){#alphak,gveck为向量
  ygk<-clsvec*gveck
  lab1<-which(alphak==0)
  lab2<-which(alphak==C)
  lab3<-which(alphak>0&alphak<C)
  alllab<-NULL
  if(length(lab1)>=1){
    ygkright<-ygk[lab1]
    yuselab1<-which(ygkright<(1-ept))#违反KKT
    alllab<-c(alllab,lab1[yuselab1])
  } 
  if(length(lab2)>=1){
    ygkerror<-ygk[lab2]
    yuselab2<-which(ygkerror>(1+ept))#违反KKT
    alllab<-c(alllab,lab2[yuselab2])
  } 
  if(length(lab3)>=1){
    ygksupport<-ygk[lab3]
    ygkuse<-abs(ygksupport-1)
    yuselab3<-which(ygkuse>ept)#违反KKT
    alllab<-c(alllab,lab3[yuselab3])
  } 
  ##先检查支持向量
  if(exists("yuselab3")&&length(yuselab3)>=1){
    outlab<-lab3[yuselab3]
  } else{#再检查其他的样本
    outlab<-alllab
  }
  if(!is.null(outlab)){
    ygkMinus1<-abs(ygk-1)[outlab]
    maxerrlab<-outlab[which.max(ygkMinus1)]
    labmax<-maxerrlab
  } else{
    labmax<-NULL
  }
  outlst<-list(alllab=alllab,outlab=outlab,labmax=labmax)
  return(outlst)
}
debug(findAlpha)
findAlpha(alphak = c(0,0,0,0,0),gveck = c(0,0,0,0,0),ept=1e-2,C=4,clsvec = c(1,1,-1,-1,1))
undebug(findAlpha)

#### 另一种工作集选取算法 ####

SelectFunc<-function(gram,clsdata,C,alphak){
  clsv<-clsdata[,1]
  la<-length(alphak)
  e<-rep(1,la)
  clsMat<-clsdata%*%t(clsdata)
  hMat<-clsMat*gram
  gradk<-as.vector(hMat%*%matrix(alphak,nrow=la,ncol=1))-e
  chsvec<--clsv*gradk
  Iup<-which((alphak<C&clsv==1)|(alphak>0&clsv==-1))
  Ilow<-which((alphak<C&clsv==-1)|(alphak>0&clsv==1))
  chsIup<-Iup[which.max(chsvec[Iup])];chsm<-chsvec[chsIup]
  chsIlow<-Ilow[which.min(chsvec[Ilow])];chsM<-chsvec[chsIlow]
  list(chsIup=chsIup,chsIlow=chsIlow,chsm=chsm,chsM=chsM)
}


smoSVM<-function(cls,atr,data,Kernel=c("line","poly","gaussi"),scale=T,
                 C=10,ept=1e-2,alpha0=rep(0,nrow(data)),b0=0,p=2,Lp=2,
                 lmada=sqrt(ncol(data))/sqrt(2),maxiter=10000,Change=T){
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  datause<-data
  N<-nrow(datause)
  lN<-1:N
  atrdata<-datause[,atr,drop=F];atrdata<-as.matrix(atrdata)
  clsdata<-datause[,cls,drop=F];clsdata<-as.matrix(clsdata)
  clsVec<-clsdata[,1]
  if(scale){#自变量数据标准化
    for(i in 1:length(atr)){
      atrdata[,i]<-scale(atrdata[,i])#0,1标准化
    }
  }
  ## 计算Gram矩阵
  if(Kernel=="line") Gram<-lineKernel(data=atrdata)#matrix
  if(Kernel=="poly") Gram<-polyKernel(data=atrdata,p=p)#matrix
  if(Kernel=="gaussi") Gram<-gaussiKernel(data=atrdata,lmada = lmada,Lp=Lp)#matrix
  alpha<-alpha0
  b<-b0
  iterk<-1
  while(iterk>=1){
    #if(iterk>=9) browser()
    gk<-gfunc(clsvec = clsVec,gram=Gram,alphak = alpha,bk=b)#vector
    Ek<-gk-clsVec#vector
    #获取alphastar1的标签，对应的位置是第几
    if(Change){
      Clst<-SelectFunc(gram=Gram,clsdata=clsdata,C=C,alphak = alpha)
      alp1<-Clst$chsIup;Ekalp1<-Ek[alp1]
      alp2<-Clst$chsIlow;Ekalp2<-Ek[alp2]
      malp<-Clst$chsm;Malp<-Clst$chsM
      y1k<-clsVec[alp1];y2k<-clsVec[alp2]
      #停止条件
      if((malp-Malp)<=ept){
        stoprule<-"(malp-Malp)<=ept"
        break
      }
    } else{
      lst<-findAlpha(alphak = alpha,gveck = gk,ept=ept,C=C,clsvec = clsVec)
      alp1<-lst$labmax;alllab<-lst$alllab;outlab<-lst$outlab
      ## 停止条件
      if(is.null(alp1)||(length(alllab)/N)<ept||(length(outlab)/N)<ept){
        stoprule<-"is.null(alp1)||(length(alllab)/N)<ept||(length(outlab)/N)<ept"
        break
      } 
      Ekalp1<-Ek[alp1]
      y1k<-clsVec[alp1]
      chooselN<-lN[-alp1];chooseEk<-Ek[-alp1]
      alp2<-ifelse(Ekalp1>0,chooselN[which.min(chooseEk)],chooselN[which.max(chooseEk)])
      y2k<-clsVec[alp2]
      Ekalp2<-Ek[alp2]
    }
    alp2old<-alpha[alp2];alp1old<-alpha[alp1]
    #计算上下界，nk
    Hk<-ifelse(y1k==y2k,min(C,alp2old+alp1old),min(C,C+alp2old-alp1old))
    Lk<-ifelse(y1k==y2k,max(0,alp2old+alp1old-C),max(0,alp2old-alp1old))
    k11<-Gram[alp1,alp1];k22<-Gram[alp2,alp2];k12<-Gram[alp1,alp2];k21<-k12
    nk<-k11+k22-2*k12
    #更新选出的alpha
    alp2kplus1_unc<-alp2old+y2k*(Ekalp1-Ekalp2)/nk
    if(alp2kplus1_unc>Hk){
      alpha[alp2]<-Hk
    } else if(alp2kplus1_unc<Lk){
      if(Lk!=0){
        alpha[alp2]<-Lk
      } else if(Lk==0&&alp2old!=0){
        alpha[alp2]<-Lk
      } else{#只有在
        alpha[alp2]<-sample(c(.15,.1,ept/6,ept*10,ept),1)
      }
    } else{
      alpha[alp2]<-alp2kplus1_unc
    }
    alpha[alp1]<-alp1old+y1k*y2k*(alp2old-alpha[alp2])
    ## alpha已经更新
    alp1new<-alpha[alp1];alp2new<-alpha[alp2]
    ##检查alp2new与alp2old是否不一样，特殊情况：alp2kplus1_unc<Lk，Lk=0时alp2new==alp2old
    #这是alpha就没有完成更新，那么接下来的迭代也不会更新；极端情况下Hk也会==0
    ## 接着更新阈值bk为bkplus1
    b_old<-b
    b1kplus1<--Ekalp1-y1k*k11*(alp1new-alp1old)-y2k*k21*(alp2new-alp2old)+b_old
    b2kplus1<--Ekalp2-y1k*k12*(alp1new-alp1old)-y2k*k22*(alp2new-alp2old)+b_old
    if(alp1new>0&&alp1new<C){
      b<-b1kplus1
    } else if(alp2new>0&&alp2new<C){
      b<-b2kplus1
    } else if((alp1new==0||alp1new==C)&&(alp2new==0||alp2new==C)){
      b<-(b2kplus1+b1kplus1)/2
    } else if((alp1new>0&&alp1new<C)&&(alp2new>0&&alp2new<C)){
      b<-b1kplus1
    }
    #b已经更新
    if(iterk>=maxiter){
      stoprule<-"iterk>=maxiter"
      break
    }
    iterk<-iterk+1
  }
  nonzero<-which(alpha!=0);lnz<-length(nonzero)
  nonZeroAlpha<-alpha[nonzero]
  names(nonZeroAlpha)<-nonzero
  alpy<-alpha[nonzero]*clsVec[nonzero]
  if(Kernel=="line"){
    w<-rep(0,length(atr))
    for(i in 1:lnz){
      w<-w+alpy[i]*atrdata[nonzero[i],]
    } 
  } else{
    w<-NULL
  }
  bvec<-vector(length = lnz)
  for(j in 1:lnz){
    gramvec<-as.vector(Gram[nonzero,j])
    bvec[j]<-clsVec[nonzero[j]]-as.vector(alpy%*%gramvec)
  }
  outlst<-list(nonZeroAlpha=nonZeroAlpha,bMean=mean(bvec),support=nonzero,w=w,
               stoprule=stoprule,formula=paste(cls,"~",paste0(atr,collapse = "+")),
               variables=atr,response=cls,iteration=iterk,clsvec=clsVec,
               ScaleAtr=atrdata,Gram=Gram,Kernel=Kernel,data=data,p=p,Lp=Lp,lmada=lmada)
  class(outlst)<-"smoSVM"
  return(outlst)
}

print.smoSVM<-function(obj){
  Kernel<-obj$Kernel
  cat("The stoprule is : ",obj$stoprule,"\n")
  cat("iteration : ",obj$iteration,"\n")
  cat("formula : ",obj$formula,"\n")
  oldlst<-options()
  if(Kernel=="line"){
    print(obj[1:4])
  } else{
    print(obj[1:3])
  }
}

predict.smoSVM<-function(SVMobj,cls,atr,atr_value,scale=T){
  testdata<-as.matrix(atr_value)
  if(scale){#归一化
    for(i in 1:length(atr)){
      testdata[,i]<-scale(testdata[,i])
    }
  }
  usealpha<-SVMobj$nonZeroAlpha
  support<-SVMobj$support
  b<-SVMobj$bMean
  #Gram<-SVMobj$Gram
  Kernel<-SVMobj$Kernel
  traincls<-SVMobj$clsvec
  if(scale){
    trainatr<-SVMobj$ScaleAtr
  } else{
    trainatr<-SVMobj$data[,atr]
    trainatr<-as.matrix(trainatr)
  }
  ## 编写预测
  usecls<-traincls[support]
  usetrainAtr<-trainatr[support,,drop=F]
  alpy<-usealpha*usecls
  predoneSVM<-function(x){
    newAtr<-rbind(x,usetrainAtr)
    if(Kernel=="line") gram<-lineKernel(data=newAtr)
    if(Kernel=="poly") gram<-polyKernel(data=newAtr,p=SVMobj$p)
    if(Kernel=="gaussi") gram<-gaussiKernel(data=newAtr,lmada=SVMobj$lmada,Lp=SVMobj$Lp)
    kvec<-gram[1,2:(length(support)+1)]
    sgn<-sign(alpy%*%kvec+b)
    sgn<-ifelse(sgn>0,1,-1)
    return(sgn)
  }
  
  predcls<-apply(testdata,1,predoneSVM)
  predcls
}

####  函数测试  ####
####  mtcars数据集测试  ####
datasvm_mtcars<-mtcars[,c(1,3,4,5,6,7,9)]#训练集
rownames(datasvm_mtcars)<-NULL
datasvm_mtcars$am<-ifelse(datasvm_mtcars$am==0,-1,1)
datasvm_mtcars_pred<-datasvm_mtcars[,c("mpg","disp","hp","qsec","drat")]

#### mtcars模型1 线性核函数 ####
svm1<-smoSVM(cls="am",atr=c("mpg","disp","hp","qsec","drat"),data=datasvm_mtcars,
             Kernel = "line",maxiter = 100000,ept=1e-2,C=10,
             scale = T,Change = T);svm1#训练模型
pmt1<-predict(svm1,atr=c("mpg","disp","hp","qsec","drat"),cls="am",
        atr_value =datasvm_mtcars_pred,scale = TRUE );pmt1#模型回测
tblmt1<-table(datasvm_mtcars$am,pmt1);tblmt1
sum(diag(tblmt1))/sum(tblmt1)#回测正确率,0.875

#### mtcars模型2 高斯径向基核函数 ####
svm2<-smoSVM(cls="am",atr=c("mpg","disp","hp","qsec","drat"),data=datasvm_mtcars,
             Kernel = "gaussi",maxiter = 100000,ept=1e-2,C=10,
             scale = T,Change = T);svm2#训练模型
pmt2<-predict(svm2,atr=c("mpg","disp","hp","qsec","drat"),cls="am",
              atr_value =datasvm_mtcars_pred,scale = TRUE );pmt2#模型回测
tblmt2<-table(datasvm_mtcars$am,pmt2);tblmt2
sum(diag(tblmt2))/sum(tblmt2)#回测正确率,0.96875

#### mtcars模型3 多项式核函数 ####
svm3<-smoSVM(cls="am",atr=c("mpg","disp","hp","qsec","drat"),data=datasvm_mtcars,
             Kernel = "poly",maxiter = 100000,ept=1e-2,C=10,p=3,
             scale = T,Change = T);svm3#训练模型
pmt3<-predict(svm3,atr=c("mpg","disp","hp","qsec","drat"),cls="am",
              atr_value =datasvm_mtcars_pred,scale = TRUE );pmt3#模型回测
tblmt3<-table(datasvm_mtcars$am,pmt3);tblmt3
sum(diag(tblmt3))/sum(tblmt3)#回测正确率,0.90625


####  iris数据集测试  ####
datasvmiris<-iris[1:100,]
datasvmiris$Species<-ifelse(datasvmiris$Species=="setosa",1,-1)
trainlab<-sample(1:100,70)
datasvmiris_train<-datasvmiris[trainlab,]
datasvmiris_test<-datasvmiris[-trainlab,]
datasvmiris_test_atr<-datasvmiris_test[,-5]

#### iris模型1 线性核函数 ####
svmiris1<-smoSVM(cls="Species",atr=c("Sepal.Length","Sepal.Width"),
             data=datasvmiris,Kernel = "line",maxiter = 10000,
             ept=1e-2,C=5,scale = T,Change = TRUE);svmiris1
piris1<-predict(svmiris1,atr=c("Sepal.Length","Sepal.Width"),cls="Species",
              atr_value =datasvmiris_test_atr[,1:2],scale = TRUE );piris1#模型预测
tbliris1<-table(datasvmiris_test$Species,piris1);tbliris1
sum(diag(tbliris1))/sum(tbliris1)#预测正确率,0.9333333

#### iris模型2 高斯径向基核函数 ####
svmiris2<-smoSVM(cls="Species",atr=c("Sepal.Length","Sepal.Width"),
                 data=datasvmiris,Kernel = "gaussi",maxiter = 10000,
                 ept=1e-2,C=5,scale = T,Change = TRUE,Lp=3);svmiris2
piris2<-predict(svmiris2,atr=c("Sepal.Length","Sepal.Width"),cls="Species",
                atr_value =datasvmiris_test_atr[,1:2],scale = TRUE );piris2#模型预测
tbliris2<-table(datasvmiris_test$Species,piris2);tbliris2
sum(diag(tbliris2))/sum(tbliris2)#预测正确率,0.8666667

#### iris模型3 多项式核函数 ####
svmiris3<-smoSVM(cls="Species",atr=c("Sepal.Length","Sepal.Width"),
                 data=datasvmiris,Kernel = "poly",maxiter = 10000,
                 ept=1e-2,C=5,scale = T,Change = TRUE,p=3);svmiris3
piris3<-predict(svmiris3,atr=c("Sepal.Length","Sepal.Width"),cls="Species",
                atr_value =datasvmiris_test_atr[,1:2],scale = TRUE );piris3#模型预测
tbliris3<-table(datasvmiris_test$Species,piris3);tbliris3
sum(diag(tbliris3))/sum(tbliris3)#预测正确率,0.9333333




####  第八章 提升方法 ####
#### 例8.1的R实现 ####
## 计算模型Gm的系数alpham 
AdaboostAlpha<-function(em){#em是错误率
  if(em==0) alpham<-Inf
  if(em==1) alpham<--Inf
  if(em>0&em<1){
    alp<-(1-em)/em
    alplog<-(1/2)*log(alp)
    alpham<-alplog
  }
  alpham
}

## 计算样本权重m+1次迭代，Dm+1
AdaboostWeight<-function(weightm,alpham,clsvec,preclsvec){
  #alpham不能是Inf,-Inf,NaN
  calog<--alpham*clsvec*preclsvec
  expwm<-weightm*exp(calog)
  wmplus1<-expwm/sum(expwm)
  wmplus1
}

## 计算带权重的em
AdaboostError<-function(clsvec,preclsvec,weightm){#输入都是向量
  sum(weightm[which(clsvec!=preclsvec)])
}


## 编写简单的树桩决策树：一次搜索
SearchOneTree<-function(atr,cls,weightm,data,sep=0.5){
  atrvec<-data[,atr];clsvec<-data[,cls]
  latr<-length(atrvec)
  searchx<-atrvec+sep
  searchx<-searchx[-latr]
  emveclow<-vector(length = latr-1)
  emvecup<-vector(length = latr-1)
  for(i in 1:(latr-1)){
    sch<-searchx[i]
    clslow<-ifelse(atrvec<=sch,1,-1)
    clsup<-ifelse(atrvec<=sch,-1,1)
    emveclow[i]<-AdaboostError(weightm = weightm,clsvec=clsvec,preclsvec = clslow)
    emvecup[i]<-AdaboostError(weightm = weightm,clsvec=clsvec,preclsvec = clsup)
  }
  lowmin<-which.min(emveclow);upmin<-which.min(emvecup)
  if(emveclow[lowmin]!=emvecup[upmin]){
    error<-min(emveclow[lowmin],emvecup[upmin])
    finalab<-ifelse(emveclow[lowmin]<emvecup[upmin],lowmin,upmin)
  } else{
    error<-emveclow[lowmin]
    finalab<-lowmin
  }
  if(finalab==lowmin){
    ModelFinal<-paste("Model:: ",atr,"<=",searchx[lowmin]," is ","1"," else"," -1.",sep = "")
    preclsvec<-ifelse(atrvec<=searchx[lowmin],1,-1)
  } else{
    ModelFinal<-paste("Model:: ",atr,">",searchx[upmin]," is ","1"," else"," -1.",sep = "")
    preclsvec<-ifelse(atrvec<=searchx[upmin],-1,1)
  }
  list(error=error,ModelFinal=ModelFinal,preclsvec=preclsvec)
}

####   例8.1 一步到位 ####
AdaboostTreeStool<-function(atr,cls,data,weight0=rep(1/length(clsvec),length(clsvec)),
                            ept=0,maxiter=10000,sep=.5){
  atrvec<-data[,atr];clsvec<-data[,cls]
  weight<-weight0
  f<-rep(0,length(clsvec))
  Gmodel<-NULL
  Galpha<-NULL
  Gerror<-NULL
  iterk<-1
  while(iterk>=1){
    G<-SearchOneTree(atr=atr,cls=cls,data=data,weightm = weight,sep = sep)
    err<-G$error;pcls<-G$preclsvec
    if(err==0||err==1){
      stoprule<-"err==0||err==1"
      outlst<-list(stoprule=stoprule,Model=G$ModelFinal,error=err)
      break
    }
    ModelG<-G$ModelFinal
    Gmodel<-c(Gmodel,ModelG)
    Gerror<-c(Gerror,err)
    alpha<-AdaboostAlpha(err)
    Galpha<-c(Galpha,alpha)
    D<-AdaboostWeight(weightm = weight,alpham = alpha,clsvec = clsvec,preclsvec = pcls)
    weight<-D
    f<-f+alpha*pcls;sgnf<-sign(f);sgnf<-ifelse(sgnf==1,1,-1)
    errf<-1-sum(sgnf==clsvec)/length(clsvec)#f的误分率
    if(errf<=ept){
      stoprule<-"errf<=ept"
      outlst<-list(stoprule=stoprule,errf=errf,iteration=iterk,AdaboostModel=Gmodel,
                   AdaboostAlpha=Galpha,AdaboostError=Gerror,AdaboostPredict=sgnf)
      break
    }
    if(iterk>=maxiter){
      stoprule<-"iterk>=maxiter"
      outlst<-list(stoprule=stoprule,errf=errf,iteration=iterk,AdaboostModel=Gmodel,
                   AdaboostAlpha=Galpha,AdaboostError=Gerror,AdaboostPredict=sgnf)
      break
    }
    iterk<-iterk+1
  }
  return(outlst)
}


x<-0:9
y<-c(1,1,1,-1,-1,-1,1,1,1,-1)
dataxy<-data.frame(x=x,y=y);dataxy
##  求解第一个模型
D1<-rep(1/10,10)
G1<-SearchOneTree(atr="x",cls = "y",data=dataxy,weightm =D1);G1
##  求模型G1的系数alpha1及f1
alpha1<-AdaboostAlpha(G1$error);alpha1
f1<-alpha1*G1$preclsvec;f1
## 更新训练数据集的权值分布D1为D2
D2<-AdaboostWeight(weightm = D1,alpham = alpha1,clsvec = y,preclsvec = G1$preclsvec);D2


## 求解第二个模型
G2<-SearchOneTree(atr="x",cls = "y",data=dataxy,weightm = D2);G2
##  求模型G2的系数alpha2及f2
alpha2<-AdaboostAlpha(G2$error);alpha2
f2<-alpha2*G2$preclsvec+f1;f2
## 更新训练数据集的权值分布D2为D3
D3<-AdaboostWeight(weightm = D2,alpham = alpha2,clsvec = y,preclsvec = G2$preclsvec);D3


## 求解第三个模型
G3<-SearchOneTree(atr="x",cls = "y",data=dataxy,weightm = D3);G3
##  求模型G3的系数alpha3及f3
alpha3<-AdaboostAlpha(G3$error);alpha3
f3<-alpha3*G3$preclsvec+f2;f3
## 更新训练数据集的权值分布D3为D4
D4<-AdaboostWeight(weightm = D3,alpham = alpha3,clsvec = y,preclsvec = G3$preclsvec);D4

##  计算组合模型f3的误分率
tblf3<-table(sign(f3),y);tblf3
1-sum(diag(tblf3))/sum(tblf3)


##  使用AdaboostTreeStool()函数一步到位 
AdaboostTreeStool(atr="x",cls = "y",data=dataxy)

##  使用mtcars/iris数据集测试函数AdaboostTreeStool() ##
datamtcars1<-mtcars[,c("mpg","am")]
datamtcars1$am<-ifelse(datamtcars1$am==1,1,-1)
AdaboostTreeStool(atr="mpg",cls = "am",data=datamtcars1,ept=.1,sep=0)

datamtcars2<-mtcars[,c("mpg","vs")]
datamtcars2$vs<-ifelse(datamtcars2$vs==1,1,-1)
AdaboostTreeStool(atr="mpg",cls = "vs",data=datamtcars2,ept=.1,sep=0)


datairis<-iris[1:100,c(3,5)]
datairis$Species<-ifelse(datairis$Species=="setosa",1,-1)
AdaboostTreeStool(atr="Petal.Length",cls = "Species",data=datairis)


####   第九章 EM算法 ####
####  高斯混合模型参数估计的EM算法 ####
gaussiEM<-function(clsvec,K=2,mean0=rep(0,K),var0=rep(1,K),alpha0=rep(1/K,K),
                   ept=1e-1,maxiter=10000,Lp=2){
  lN<-length(clsvec)
  mean<-mean0
  var<-var0
  alpha<-alpha0
  iterk<-1
  while(iterk>=1){
    parameterold<-c(mean,var)
    rjkMat<-gaussiResponse(clsvec = clsvec,K=K,Ml=mean,Vl=var,Alpl=alpha)
    paralst<-gaussiParameter(rMat = rjkMat,clsvec = clsvec,K=K)
    mean<-paralst$M
    var<-paralst$V
    alpha<-paralst$A
    parameternew<-c(mean,var)
    pnewMinusoldLp<-(parameternew-parameterold)^Lp
    Lpvalue<-sum(pnewMinusoldLp)^(1/Lp)
    if(Lpvalue<=ept){
      stoprule<-"Lpvalue<=ept"
      break
    }
    if(iterk>=maxiter){
      stoprule<-"iterk>=maxiter"
      break
    }
    #print(mean);print(var);print(alpha)
    iterk<-iterk+1
  }
  outlst<-list(stoprule=stoprule,iteration=iterk,Mean=mean,
               Var=var,Alpha=alpha,K=K)
  class(outlst)<-"gaussiEM"
  return(outlst)
}

## 计算当前分模型对观测数据yj的响应度
gaussiResponse<-function(clsvec,K=2,Ml=rep(0,K),Vl=rep(1,K),Alpl=rep(1/K,K)){
  lj<-length(clsvec);lk<-K
  rjkMat<-matrix(0,nrow=lj,ncol=lk)
  for(j in 1:lj){
    rvec<-vector(length = lk)
    for(k in 1:lk){
      rjk<-Alpl[k]*dnorm(clsvec[j],mean=Ml[k],sd=sqrt(Vl[k]))
      if(rjk==0) rjk<-sample(c(1e-20,1e-30,1e-40,1e-100,1e-65),1)
      rvec[k]<-rjk
    }
    rjkMat[j,]<-rvec/sum(rvec)
  }
  return(rjkMat)
}

##  更新计算各个参数
gaussiParameter<-function(rMat,clsvec,K=2){
  N<-length(clsvec)
  Mplus<-vector(length = K)
  Vplus<-vector(length = K)
  Alplus<-vector(length = K)
  for(i in 1:K){
    rk<-rMat[,i];srk<-sum(rk)
    Mk<-as.vector(rk%*%clsvec)/srk
    ymvec<-(clsvec-Mk)^2
    Vk<-as.vector(rk%*%ymvec)/srk
    Alpk<-srk/N
    Mplus[i]<-Mk;Vplus[i]<-Vk;Alplus[i]<-Alpk
  }
  list(M=Mplus,V=Vplus,A=Alplus)
}

##  编写打印函数
print.gaussiEM<-function(obj){
  cat("Stoprule : ",obj$stoprule,"\n")
  cat("iteration : ",obj$iteration,"\n")
  cat("the number of gaussi is : ",obj$K,"\n")
  print(obj[3:5])
}

##  测试1：生成混合正态分布数据 ##
comp <- sample(c(0, 1), size = 10000, prob = c(0.7, 0.3), replace = T)
sim1<-rnorm(10000, mean = ifelse(comp == 0, 0, 1), sd = ifelse(comp == 0, 1, 2))
g1<-gaussiEM(clsvec = sim1,ept=1e-3,mean0 = c(0,.5),K=2);g1

##  测试2 ##
comp2 <- sample(c(0,1,2), size = 100000, prob = c(0.5,0.3,0.2), replace = T)
sim2<-rnorm(100000, mean =comp2, sd = ifelse(comp == 0, 1,ifelse(comp2==1,4,2)))
g2<-gaussiEM(clsvec = sim2,ept=1e-3,mean0 = c(0,.8,.9),var0 = c(.6,7.8,2.3),K=3);g2


y<-c(-67,-48,6,8,14,16,23,24,28,29,41,49,56,60,75)
gaussiEM(y)

####  第十章 隐马尔可夫模型 ####
####  隐马尔可夫模型观测序列的生成 ####
#A是转移概率矩阵，B是观测概率矩阵，PI是初始概率向量，Lth是输出的观测序列长度
#size为想要生成的观测序列的组数，size，Lth可以是vector
ObjectHMM<-function(size=1,Lth=5,A,B,PI,StateLabel=1:nrow(A),
                    ObjectLabel=1:ncol(B),seed=NULL){
  stlab<-StateLabel#各个状态值采用的标记
  objlab<-ObjectLabel#各个观测值采用的标记
  lsi<-size
  if(length(Lth)==1) Lth<-rep(Lth,size)
  stlst<-list()
  objlst<-list()
  if(!is.null(seed)) set.seed(seed=seed)
  for(i in 1:lsi){
    lt<-Lth[i]
    stvec<-vector(length = lt)
    objvec<-vector(length = lt)
    stvec[1]<-sample(stlab,1,prob = PI)#确定初始状态
    st1<-which(stlab==stvec[1])#在B中对应的行数，即状态
    objvec[1]<-sample(objlab,1,prob = B[st1,])#确定初始观测
    for(j in 2:lt){
      st<-which(stlab==stvec[j-1])#确定当前状态
      stvec[j]<-sample(stlab,1,prob =A[st,])#确定下一个状态
      stnew<-which(stlab==stvec[j])#下一个状态在B中对应的行数
      objvec[j]<-sample(objlab,1,prob = B[stnew,])#确定下一个观测
    }
    stlst[[i]]<-stvec
    objlst[[i]]<-objvec
  }
  outlst<-list(obs=objlst,state=stlst)
  return(outlst)
}

## 测试 例10.1 
a10.1<-matrix(c(0,1,0,0,
                .4,0,.6,0,
                0,.4,0,.6,
                0,0,.5,.5),nrow = 4,byrow = T);a10.1
pi10.1<-rep(.25,4);pi10.1
b10.1<-matrix(c(.5,.5,
                .3,.7,
                .6,.4,
                .8,.2),nrow = 4,byrow = T);b10.1
ObjectHMM(size=2,Lth = 5,A=a10.1,B=b10.1,PI=pi10.1,
          ObjectLabel = c("红","白"))
ObjectHMM(size=2,Lth = 5,A=a10.1,B=b10.1,PI=pi10.1,
          ObjectLabel = c("红","白"),seed=66)
ObjectHMM(size=3,Lth = c(3,4,5),A=a10.1,B=b10.1,
          PI=pi10.1,ObjectLabel = c("红","白"))



#### HMM前向算法实现 ####
forwardHMM<-function(obs,A,B,PI,StateLabel=as.character(1:nrow(A)),
                     ObjectLabel=as.character(1:ncol(B))){
  #obs为观测值序列
  lT<-length(obs)#观测序列的长度
  Bmat<-B;colnames(Bmat)<-ObjectLabel;rownames(Bmat)<-StateLabel
  Amat<-A;colnames(Amat)<-StateLabel;rownames(Amat)<-StateLabel
  fA<-function(alpcal,Acol) alpcal%*%Acol
  #计算前向概率初值
  AlpMat<-matrix(nrow=lT,ncol = nrow(Amat))
  colnames(AlpMat)<-paste0("st:",StateLabel)
  rownames(AlpMat)<-paste0("T:",1:lT)
  alpha<-PI*Bmat[,which(ObjectLabel==obs[1])]#vector
  AlpMat[1,]<-alpha
  iterk<-2
  while(iterk<=lT){
    #更新迭代结果
    alp<-apply(Amat,2,fA,alpcal=alpha)
    alpha<-alp*Bmat[,which(ObjectLabel==obs[iterk])]
    AlpMat[iterk,]<-alpha
    iterk<-iterk+1
  }
  list(FinalProb=sum(alpha),AlpMat=AlpMat)
}

## 例10.2的程序求解
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2

forwardHMM(obs=c("红","白","红"),A=A10.2,
           B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"))


## 结合函数ObjectHMM()来做个尝试试
hmm1<-ObjectHMM(size=3,Lth = c(3,4,5),A=a10.1,B=b10.1,
                PI=pi10.1,ObjectLabel = c("红","白"));hmm1
hmm1_obs<-hmm1$obs;hmm1_obs
lapply(hmm1_obs,forwardHMM,A=a10.1,B=b10.1,
       PI=pi10.1,ObjectLabel=c("红","白"))



#### HMM模型后向算法的实现 ####
backwardHMM<-function(obs,A,B,PI,StateLabel=as.character(1:nrow(A)),
                      ObjectLabel=as.character(1:ncol(B))){
  #obs为观测值序列
  lT<-length(obs)#观测序列的长度
  lst<-nrow(A)#状态的取值个数
  Bmat<-B;colnames(Bmat)<-ObjectLabel;rownames(Bmat)<-StateLabel
  Amat<-A;colnames(Amat)<-StateLabel;rownames(Amat)<-StateLabel
  fB<-function(Arow,Bcol,btcal) sum(Arow*Bcol*btcal)
  beta<-rep(1,lst)
  BtMat<-matrix(nrow=lT,ncol = lst)
  colnames(BtMat)<-paste0("st:",StateLabel)
  rownames(BtMat)<-paste0("T:",lT:1)
  BtMat[1,]<-beta
  iterk<-1
  while(iterk<=(lT-1)){#迭代是从最后一个观测开始的
    bcol<-Bmat[,which(ObjectLabel==obs[lT-iterk+1])]
    beta<-apply(Amat,1,fB,Bcol=bcol,btcal=beta)
    BtMat[iterk+1,]<-beta
    iterk<-iterk+1
  }
  bo1<-Bmat[,which(ObjectLabel==obs[1])]
  finalprob<-sum(PI*bo1*beta)
  list(FinalProb=finalprob,BtMat=BtMat)
}


## 例10.2的程序求解 后向算法
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2
backwardHMM(obs=c("红","白","红"),A=A10.2,
            B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"))

hmm<-ObjectHMM(size=2,Lth = c(3,4),A=A10.2,B=B10.2,
               PI=pi10.2,ObjectLabel = c("红","白"))
hmm_obs<-hmm$obs;hmm_obs
lapply(hmm_obs,forwardHMM,A=A10.2,B=B10.2,PI=pi10.2,ObjectLabel=c("红","白"))
lapply(hmm_obs,backwardHMM,A=A10.2,B=B10.2, PI=pi10.2,ObjectLabel=c("红","白"))


####  一些概率及期望的计算 ####
stijHMM<-function(obs,sti=NULL,stij=NULL,time=NULL,A,B,PI,
                  StateLabel=as.character(1:nrow(A)),
                  ObjectLabel=as.character(1:ncol(B)),
                  if.sti=F,if.stij=F){
  #sti输入指定的状态，obs为当前观测序列,stijz指定转移状态,time指定时刻
  #sti,time均可以是长度相等的向量;stij只能是一个二维向量
  lT<-length(obs)
  Alplst<-forwardHMM(obs=obs,A=A,B=B,PI=PI,StateLabel = StateLabel,
                     ObjectLabel = ObjectLabel)
  Btlst<-backwardHMM(obs=obs,A=A,B=B,PI=PI,StateLabel = StateLabel,
                     ObjectLabel = ObjectLabel)
  AlpMat<-Alplst$AlpMat
  BtMat<-Btlst$BtMat
  btmat<-BtMat[lT:1,,drop=F]
  Probs<-Alplst$FinalProb
  if(!if.stij){
    PstiMat<-AlpMat*btmat/Probs#t时刻状态为i的概率矩阵
    rownames(PstiMat)<-rownames(AlpMat);colnames(PstiMat)<-colnames(AlpMat)
    si<-which(StateLabel==sti)
    if(!is.null(sti)&&!is.null(time)) psti<-PstiMat[time,si] else psti<-NULL
  }
  if(!if.sti){
    fbj<-function(j,x,BM) BM[j,which(ObjectLabel==x)]
    wj<-which(StateLabel==stij[2]);wi<-which(StateLabel==stij[1])
    bjDf<-data.frame(jobs=obs[-1])
    bjvec<-apply(bjDf,1,fbj,j=wj,BM=B)
    Pstijvec<-A[wi,wj]*AlpMat[1:(lT-1),wi]*bjvec*btmat[-1,wj]/Probs#长度为lT-1
    if(!is.null(time)&&!is.null(stij)) pstij<-Pstijvec[time] else pstij<-NULL
  }
  if(!if.sti&&!if.stij){
    outlst<-list(Probs=Probs,psti=psti,pstij=pstij,PstiMat=PstiMat,Pstijvec=Pstijvec,
                 AlpMat=AlpMat,BtMat=btmat,sti=sti,stij=stij,time=time)
  } else if(!if.stij&&if.sti){
    outlst<-list(Probs=Probs,psti=psti,PstiMat=PstiMat,AlpMat=AlpMat,
                 BtMat=btmat,sti=sti,stij=stij,time=time)
  } else if(if.stij&&!if.sti){
    outlst<-list(Probs=Probs,pstij=pstij,Pstijvec=Pstijvec,AlpMat=AlpMat,
                 BtMat=btmat,sti=sti,stij=stij,time=time)
  } else{
    stop("if.sti and if.stij can not both TRUE.")
  }
  return(outlst)
}

##  检查函数的正确性
stij1<-stijHMM(obs=c("红","白","红"),sti="1",stij=c("1","1"),time=1,A=A10.2,
               B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"))
stij2<-stijHMM(obs=c("红","白","红"),sti="1",stij=c("1","2"),time=1,A=A10.2,
               B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"))
stij3<-stijHMM(obs=c("红","白","红"),sti="1",stij=c("1","3"),time=1,A=A10.2,
               B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"))
apply(stij1$PstiMat,1,sum)
apply(stij1$PstiMat,2,sum)
sum(c(stij1$Pstijvec,stij2$Pstijvec,stij3$Pstijvec))
sum(stij1$PstiMat[1:2,1])

names(stij1)

stijHMM(obs=c("红","白","红"),sti=NULL,
    stij=NULL,time=NULL,A=A10.2,
    B=B10.2,PI=pi10.2,ObjectLabel=c("红","白")
    ,if.sti = TRUE)

stijHMM(obs=c("红","白","红"),sti="1",
  stij=c("1","1"),time=NULL,A=A10.2,
  B=B10.2,PI=pi10.2,ObjectLabel=c("红","白"),
  if.stij=TRUE)

stijHMM(obs=c("红","白","红"),sti="1",stij=c("1","1"),time=NULL,A=A10.2,
        B=B10.2,PI=pi10.2,ObjectLabel = c("红","白"), if.sti = TRUE ,if.stij=TRUE)




####  HMM监督学习方法的R实现 ####
####  HMM监督学习方法的R实现 ####
superviseHMM<-function(obsMat,stMat,StateLabel=NULL,ObjectLabel=NULL){
  #obsdf为观测序列组成的矩阵T*S，每一列为一次观测序列，行数代表时刻t=1,2,...,T.
  #stdf为状态序列组成的矩阵T*S，每一列为一次状态序列，行数代表时刻t=1,2,...,T.
  #StateLabel输入状态的字符标签，ObjectLabel输入观测的字符标签
  lT<-nrow(obsMat)#时间长度
  S<-ncol(obsMat)#样本量
  stMatchs<-stMat[-lT,]
  lst<-length(StateLabel)#状态集合长度
  lobs<-length(ObjectLabel)#观测集合长度
  obsvec<-as.vector(obsMat);stvec<-as.vector(stMat)
  aijMat<-matrix(nrow=lst,ncol=lst)##转移概率矩阵
  colnames(aijMat)<-StateLabel;rownames(aijMat)<-StateLabel
  bjkMat<-matrix(nrow = lst,ncol=lobs)##观测概率矩阵
  colnames(bjkMat)<-ObjectLabel;rownames(bjkMat)<-StateLabel
  pivec<-vector(length=lst)##初始概率向量
  findaij<-function(ichr,jchr){#计算一个转移概率
    #在stdf进行搜索，从时间t=1开始至t=lT-1
    SAij<-length(which(stMatchs==ichr))
    Aij<-0
    for(t in 1:(lT-1)){
      tvec<-stMat[t,];tplus1vec<-stMat[t+1,]
      tlab<-which(tvec==ichr);tplus1st<-tplus1vec[tlab]
      sj<-sum(tplus1st==jchr)
      Aij<-Aij+sj
    }
    return(Aij/SAij)
  }
  findbjk<-function(jchr,kchr){#计算一个观测概率
    jlab<-which(stvec==jchr)
    kvec<-obsvec[jlab]
    sum(kvec==kchr)/length(jlab)
  }
  #计算转移概率矩阵
  for(i in 1:lst){
    for(j in 1:lst){
      aijMat[i,j]<-findaij(ichr=StateLabel[i],jchr = StateLabel[j])
    }
  }
  #计算观测概率矩阵
  for(j in 1:lst){
    for(k in 1:lobs){
      bjkMat[j,k]<-findbjk(jchr=StateLabel[j],kchr = ObjectLabel[k])
    }
  }
  #计算初始概率向量
  first<-stMat[1,]#初始状态
  for(i in 1:lst){
    pi[i]<-length(which(first==StateLabel[i]))/S
  }
  outlst<-list(pi=pi,aijMat=aijMat,bjkMat=bjkMat,
               StateLabel=StateLabel,ObjectLabel=ObjectLabel)
  class(outlst)<-c("superviseHMM","HMM")
  return(outlst)
}

print.superviseHMM<-function(obj){
  cat("State::",obj$StateLabel,"; ","Observation::",obj$ObjectLabel,"\n")
  print(obj[1:3])
}

## 测试1  利用ObjectHMM()函数生成数据
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2
## 生成隐马尔可夫观测序列及状态序列
test1<-ObjectHMM(size=2000,Lth = 5,A=A10.2,B=B10.2,PI=pi10.2,
                 StateLabel = as.character(1:3),ObjectLabel = c("红","白"))
obs1<-test1$obs
st1<-test1$state
obsmat<-do.call(cbind,obs1)#观测矩阵
stmat<-do.call(cbind,st1)#状态矩阵

shmm1<-superviseHMM(obsMat = obsmat,stMat = stmat,StateLabel =as.character(1:3),
             ObjectLabel =c("红","白") )#训练模型
shmm1

pi10.2
A10.2
B10.2

## 测试2
A1<-matrix(c(0.5,0.3,0.2,
                0.4,0.4,0.2,
                0.1,0.2,0.7),nrow=3,byrow = T);A10.2
B1<-matrix(c(0.6,0.4,
                0.3,0.7,
                0.4,0.6),nrow = 3,byrow = T);B10.2
pi1<-c(0.3,0.3,0.4);pi1
## 生成隐马尔可夫观测序列及状态序列
test2<-ObjectHMM(size=2000,Lth = 5,A=A10,B=B10,PI=pi10,
                 StateLabel = as.character(1:3),ObjectLabel = c("红","白"))
obs2<-test2$obs
st2<-test2$state
obsmat2<-do.call(cbind,obs2)#观测矩阵
stmat2<-do.call(cbind,st2)#状态矩阵

shmm2<-superviseHMM(obsMat = obsmat2,stMat = stmat2,
                    StateLabel =as.character(1:3),ObjectLabel =c("红","白") )#训练模型
shmm2

pi10
A10
B10

#### HMM模型Buam-Welch无监督学习算法的R实现 ####
BuamWelchHMM<-function(obs,A0,B0,PI0,StateLabel=as.character(1:nrow(A0)),
                       ObjectLabel=as.character(1:ncol(B0)),ept=1e-2,
                       maxiter=10000,Lp=2){
  #obs输入一个观测序列，vector
  lT<-length(obs);lst<-length(StateLabel);lobs<-length(ObjectLabel)
  Abw<-A0;Bbw<-B0;PIbw<-PI0
  iterk<-1
  while(iterk>=1){
    #保存旧参数
    Abwold<-Abw;Bbwold<-Bbw;PIbwold<-PIbw
    #更新初始概率向量
    pilst<-stijHMM(obs=obs,A=Abw,B=Bbw,PI=PIbw,if.sti = TRUE,
                   StateLabel = StateLabel,ObjectLabel = ObjectLabel)
    PIbw<-pilst$PstiMat[1,]
    #更新转移概率矩阵、观测概率矩阵
    for(i in 1:lst){#i行
      ir<-StateLabel[i]
      #更新转移概率矩阵
      for(j in 1:lst){
        jr<-StateLabel[j]#获取状态标签
        calij<-stijHMM(obs=obs,stij=c(ir,jr),A=Abw,B=Bbw,PI=PIbw,
                       StateLabel = StateLabel,ObjectLabel=ObjectLabel)
        pstiMat<-calij$PstiMat;pstijvec<-calij$Pstijvec
        Abw[i,j]<-sum(pstijvec)/sum(pstiMat[1:(lT-1),i])
      }
      #更细观测概率矩阵
      for(k in 1:lobs){
        klab<-which(obs==ObjectLabel[k])
        cali<-stijHMM(obs=obs,A=Abw,B=Bbw,PI=PIbw,StateLabel = StateLabel,
                      ObjectLabel=ObjectLabel,if.sti=TRUE)
        piMat<-cali$PstiMat
        pstivec<-piMat[,i];pkvec<-pstivec[klab]
        Bbw[i,k]<-sum(pkvec)/sum(pstivec)
      }
    }
    # 停止条件
    Abwoldvec<-as.vector(Abwold);Abwvec<-as.vector(Abw)
    Bbwoldvec<-as.vector(Bbwold);Bbwvec<-as.vector(Bbw)
    abw<-Abwvec-Abwoldvec;Lpabw<-sum(abs(abw)^Lp)*(1/Lp)
    bbw<-Bbwvec-Bbwoldvec;Lpbbw<-sum(abs(bbw)^Lp)*(1/Lp)
    pibw<-PIbw-PIbwold;Lppibw<-sum(abs(pibw)^Lp)*(1/Lp)
    allbw<-c(abw,bbw,pibw);Lpallbw<-sum(abs(allbw)^Lp)*(1/Lp)
    if(Lpabw<=ept&&Lpbbw<=ept&&Lppibw<=ept){
      stoprule<-"Lpabw<=ept&&Lpbbw<=ept&&Lppibw<=ept"
      break
    }
    if(Lpallbw<=ept){
      stoprule<-"Lpallbw<=ept"
      break
    }
    if(iterk>=maxiter){
      stoprule<-"iterk>=maxiter"
      break
    }
    iterk<-iterk+1
  }
  outlst<-list(pi=PIbw,aijMat=Abw,bjkMat=Bbw,iteration=iterk,
               stoprule=stoprule,StateLabel=StateLabel,ObjectLabel=ObjectLabel)
  class(outlst)<-c("BuamWelchHMM","HMM")
  return(outlst)
}

MoreBuamWelchHMM<-function(obsMat,A0,B0,PI0,StateLabel=as.character(1:nrow(A0)),
                           ObjectLabel=as.character(1:ncol(B0)),ept=1e-2,
                           maxiter=10000,Lp=2){
  lMat<-ncol(obsMat)
  lst<-length(StateLabel)
  lobs<-length(ObjectLabel)
  lT<-nrow(obsMat)
  A<-matrix(0,nrow=lst,ncol=lst)
  B<-matrix(0,nrow = lst,ncol=lobs)
  PI<-rep(0,lst)
  for(i in 1:lMat){
    obsuse<-obsMat[,i]
    calst<-BuamWelchHMM(obs=obsuse,A0=A0,B0=B0,PI0=PI0,StateLabel = StateLabel,
                        ObjectLabel = ObjectLabel,maxiter = maxiter,ept = ept,Lp=Lp)
    Arev<-calst$aijMat;Brev<-calst$bjkMat;pirev<-calst$pi
    A<-A+Arev
    B<-B+Brev
    PI<-PI+pirev
  }
  A<-A/lMat;B<-B/lMat;PI<-PI/lMat
  outlst<-list(pi=PI,aijMat=A,bjkMat=B,
               StateLabel=StateLabel,ObjectLabel=ObjectLabel)
  class(outlst)<-c("BuamWelchHMM","HMM")
  return(outlst)
}

print.BuamWelchHMM<-function(obj){
  cat("State::",obj$StateLabel,"; ","Observation::",obj$ObjectLabel,"\n")
  print(obj[1:3])
}



#### 测试
## 初始化参数
A1<-matrix(c(0.5,0.3,0.2,
             0.4,0.4,0.2,
             0.1,0.2,0.7),nrow=3,byrow = T);A10.2
B1<-matrix(c(0.6,0.4,
             0.3,0.7,
             0.4,0.6),nrow = 3,byrow = T);B10.2
pi1<-c(0.3,0.3,0.4);pi1
## 生成隐马尔可夫观测序列及状态序列
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2
## 生成隐马尔可夫观测序列及状态序列
test1<-ObjectHMM(size=2000,Lth = 5,A=A10.2,B=B10.2,PI=pi10.2,
                 StateLabel = as.character(1:3),ObjectLabel = c("红","白"))
obs1<-test1$obs
st1<-test1$state
obsmat<-do.call(cbind,obs1)#观测矩阵
stmat<-do.call(cbind,st1)#状态矩阵

BuamWelchHMM(obs=obsmat[,1],A0=A1,B0=B1,PI0=pi1,
             ObjectLabel =  c("红","白"),ept = 1e-2)
BuamWelchHMM(obs=obsmat[,10],A0=A1,B0=B1,PI0=pi1,
             ObjectLabel =  c("红","白"),ept = 1e-2)
MoreBuamWelchHMM(obsMat = obsmat,A0=A1,B0=B1,PI0=pi1,
                 ObjectLabel =  c("红","白"),ept = 1e-2)



####  预测算法：近似算法的R实现 ####
approxHMM<-function(obsMat,A,B,PI,StateLabel=as.character(1:nrow(A)),
                    ObjectLabel=as.character(1:ncol(B))){
  approxone<-function(obs){
    calst<-stijHMM(obs = obs,A=A,B=B,PI=PI,StateLabel = StateLabel,
                   ObjectLabel = ObjectLabel,if.sti = TRUE)
    #只计算状态概率矩阵，不计算转移概率
    pstiMat<-calst$PstiMat#状态概率矩阵
    #对每一行搜索概率最大的状态
    stlab<-apply(pstiMat,1,which.max)
    StateLabel[stlab]
  }
  if(is.vector(obsMat)) obsMat<-matrix(obsMat,nrow=length(obsMat))
  apply(obsMat,2,approxone)
}


#### 测试1
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2
## 生成隐马尔可夫观测序列及状态序列
test1<-ObjectHMM(size=100,Lth = 100,A=A10.2,B=B10.2,PI=pi10.2,
                 StateLabel = as.character(1:3),ObjectLabel = c("红","白"),seed=100)
obs1<-test1$obs
st1<-test1$state
obsmat<-do.call(cbind,obs1)#观测矩阵
stmat<-do.call(cbind,st1)#状态矩阵

t1<-approxHMM(obsMat = obsmat,A=A10.2,B=B10.2,
              PI=pi10.2,ObjectLabel = c("红","白"))
sum(t1==stmat)/10000#准确率

#### 测试2
A10<-matrix(c(0.3,0.2,0.2,0,0.3,
              0.1,0.2,0.3,0.3,0.1,
              0.2,0.2,0.3,0.15,0.15,
              0.2,0.1,0.1,0.3,0.4,
              0.1,0.2,0.3,0.3,0.1),nrow=5,byrow = T);A10
B10<-matrix(c(0.5,0.5,
              0.4,0.6,
              0.7,0.3,
              0.4,0.6,
              0.45,0.55),nrow = 5,byrow = T);B10
pi10<-c(0.2,0.1,0.25,0.2,0.25);pi10
## 生成隐马尔可夫观测序列及状态序列
test2<-ObjectHMM(size=100,Lth = 100,A=A10,B=B10,PI=pi10,
                 StateLabel = as.character(1:5),ObjectLabel = c("红","白"),seed=888)
obs2<-test2$obs
st2<-test2$state
obsmat2<-do.call(cbind,obs2)#观测矩阵
stmat2<-do.call(cbind,st2)#状态矩阵

t2<-approxHMM(obsMat = obsmat2,A=A10,B=B10,
              PI=pi10,ObjectLabel = c("红","白"))
sum(t2==stmat2)/10000#预测准确率


#### 测试3
a10<-matrix(c(0.4,0.6,
              0.7,0.3),nrow = 2,byrow = T);a10
b10<-matrix(c(0.5,0.5,
              0.3,0.7),nrow = 2,byrow = T);b10
p10<-c(0.3,0.7);p10
## 生成隐马尔可夫观测序列及状态序列
test3<-ObjectHMM(size=100,Lth = 100,A=a10,B=a10,PI=p10,
                 StateLabel = as.character(1:2),ObjectLabel = c("红","白"),seed=666)
obs3<-test3$obs
st3<-test3$state
obsmat3<-do.call(cbind,obs3)#观测矩阵
stmat3<-do.call(cbind,st3)#状态矩阵

t3<-approxHMM(obsMat = obsmat3,A=a10,B=a10,
              PI=p10,ObjectLabel = c("红","白"))
sum(t3==stmat3)/10000#准确率


#### 预测算法 维特比算法的R实现 ####
ViterbiHMM<-function(obs,A,B,PI,StateLabel=as.character(1:nrow(A)),
                     ObjectLabel=as.character(1:ncol(B)),if.show=TRUE){
  lst<-length(StateLabel)
  lT<-length(obs)
  lobs<-length(ObjectLabel)
  obs1<-which(ObjectLabel==obs[1])
  delta<-PI*B[,obs1]#初始化delta
  pasi<-rep(0,lst)#初始化pasi
  deltaMat<-matrix(nrow=lT,ncol = lst)#delta矩阵，行为时刻，列为状态
  pasiMat<-matrix(nrow=lT,ncol = lst)#pasi矩阵，行为时刻，列为状态
  deltaMat[1,]<-delta;pasiMat[1,]<-pasi
  #进入递推
  iterk<-2
  while(iterk<=lT){
    obslab<-which(ObjectLabel==obs[iterk])
    bitvec<-B[,obslab]
    #利用R矩阵乘法特点
    delm<-matrix(rep(delta,lst),nrow = lst,byrow = TRUE)
    useMat<-delm*t(A)#对应位置相乘
    pasi<-apply(useMat,1,which.max)#每行取最大,vector
    caldelta<-vector(length = lst)
    for(i in 1:lst){
      caldelta[i]<-useMat[i,pasi[i]]
    }
    delta<-caldelta*bitvec
    deltaMat[iterk,]<-delta
    pasiMat[iterk,]<-pasi
    iterk<-iterk+1
  }
  #先找到最优路径的概率及终点
  statelab<-vector(length = lT)
  finalstatelab<-which.max(delta)#获取位置
  finalprob<-delta[finalstatelab]
  statelab[lT]<-finalstatelab
  #回溯
  for(j in (lT-1):1){
    statelab[j]<-pasiMat[j+1,statelab[j+1]]
  }
  rownames(deltaMat)<-paste0("T:",1:lT)
  colnames(deltaMat)<-paste0("st:",StateLabel)
  rownames(pasiMat)<-paste0("T:",1:lT)
  colnames(pasiMat)<-paste0("st:",StateLabel)
  predvec<-StateLabel[statelab];names(predvec)<-paste0("T:",1:lT)
  if(if.show){
    out<-list(FinalState=predvec,deltaMat=deltaMat,pasiMat=pasiMat)
  } else{
    out<-predvec
  }
  return(out)
}

####  批量预测
MoreViterbiHMM<-function(obsMat,A,B,PI,StateLabel=as.character(1:nrow(A)),
                         ObjectLabel=as.character(1:ncol(B))){
  if(is.vector(obsMat)) obsMat<-matrix(obsMat,nrow=length(obsMat))
  apply(obsMat,2,ViterbiHMM,A=A,B=B,PI=PI,
        StateLabel=StateLabel,ObjectLabel=ObjectLabel,if.show=FALSE)
}


####  求解例10.3
A10.3<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.3
B10.3<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.3
pi10.3<-c(0.2,0.4,0.4);pi10.3
ViterbiHMM(obs=c("红","白","红"),A=A10.3,B=B10.3,PI=pi10.3,
           ObjectLabel =c("红","白") )

approxHMM(obs=c("红","白","红"),A=A10.3,B=B10.3,PI=pi10.3,
           ObjectLabel =c("红","白") )



#### 测试1
A10.2<-matrix(c(0.5,0.2,0.3,
                0.3,0.5,0.2,
                0.2,0.3,0.5),nrow=3,byrow = T);A10.2
B10.2<-matrix(c(0.5,0.5,
                0.4,0.6,
                0.7,0.3),nrow = 3,byrow = T);B10.2
pi10.2<-c(0.2,0.4,0.4);pi10.2
## 生成隐马尔可夫观测序列及状态序列
test1<-ObjectHMM(size=100,Lth = 5,A=A10.2,B=B10.2,PI=pi10.2,
                 StateLabel = as.character(1:3),ObjectLabel = c("红","白"),seed=666)
obs1<-test1$obs
st1<-test1$state
obsmat<-do.call(cbind,obs1)#观测矩阵
stmat<-do.call(cbind,st1)#状态矩阵

t1<-MoreViterbiHMM(obsMat = obsmat,A=A10.2,B=B10.2,
              PI=pi10.2,ObjectLabel = c("红","白"))
sum(t1==stmat)/500#准确率



#### 测试2
A10<-matrix(c(0.3,0.2,0.2,0,0.3,
              0.1,0.2,0.3,0.3,0.1,
              0.2,0.2,0.3,0.15,0.15,
              0.2,0.1,0.1,0.3,0.4,
              0.1,0.2,0.3,0.3,0.1),nrow=5,byrow = T);A10
B10<-matrix(c(0.5,0.5,
              0.4,0.6,
              0.7,0.3,
              0.4,0.6,
              0.45,0.55),nrow = 5,byrow = T);B10
pi10<-c(0.2,0.1,0.25,0.2,0.25);pi10
## 生成隐马尔可夫观测序列及状态序列
test2<-ObjectHMM(size=100,Lth = 5,A=A10,B=B10,PI=pi10,
                 StateLabel = as.character(1:5),ObjectLabel = c("红","白"),seed=888)
obs2<-test2$obs
st2<-test2$state
obsmat2<-do.call(cbind,obs2)#观测矩阵
stmat2<-do.call(cbind,st2)#状态矩阵

t2<-MoreViterbiHMM(obsMat = obsmat2,A=A10,B=B10,
              PI=pi10,ObjectLabel = c("红","白"))
sum(t2==stmat2)/500#预测准确率

#### 测试3
a10<-matrix(c(0.4,0.6,
              0.7,0.3),nrow = 2,byrow = T);a10
b10<-matrix(c(0.5,0.5,
              0.3,0.7),nrow = 2,byrow = T);b10
p10<-c(0.3,0.7);p10
## 生成隐马尔可夫观测序列及状态序列
test3<-ObjectHMM(size=100,Lth = 5,A=a10,B=a10,PI=p10,
                 StateLabel = as.character(1:2),ObjectLabel = c("红","白"),seed=666)
obs3<-test3$obs
st3<-test3$state
obsmat3<-do.call(cbind,obs3)#观测矩阵
stmat3<-do.call(cbind,st3)#状态矩阵

t3<-MoreViterbiHMM(obsMat = obsmat3,A=a10,B=a10,
              PI=p10,ObjectLabel = c("红","白"))
sum(t3==stmat3)/500#准确率




















