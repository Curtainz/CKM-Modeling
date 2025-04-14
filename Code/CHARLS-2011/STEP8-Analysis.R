library(dplyr)
# 加载数据
data <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/7-final_cleaned_data.csv")

# 去除无关变量(X.1, X, ID, householdID, communityID)
data <- data %>% select(-X.1, -X, -ID, -householdID, -communityID)

# 去除直接相关变量(heartProblem, waist, kidneyDisease, weight, hypertension, systolic, diastolic, height, stroke, diabetes)
data <- data %>% select(-heartProblem, -waist, -kidneyDisease, -weight, -hypertension, -systolic, -diastolic, -height, -stroke, -diabetesOrHighBloodSugar)

##### 热图分析 #####
library(corrplot)

# 重置绘图设备
dev.off()  
plot.new() 

# 计算相关性矩阵
cor_matrix <- cor(data)

# 检查是否有 NA 值
any(is.na(cor_matrix))

# 将 NA 替换为 0
cor_matrix[is.na(cor_matrix)] <- 0  

# 可视化相关性矩阵（通过热图）
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.5)

##### 随机森林分析 #####
library(randomForest)
library(ggplot2)
library(caret)

# 重置绘图设备
dev.off()  
plot.new() 

# 分割数据集
trainlist <- createDataPartition(data$stage,p=0.7, list = FALSE)
trainset <- data[trainlist,]
testset <- data[-trainlist,]

# 训练集
rf.train <- randomForest(as.factor(stage)~.,data=trainset,importance=TRUE,na.action=na.pass)
rf.train

# 测试集
rf.test <- predict(rf.train,newdata=testset,type="class")

# 矩阵
rf.cf <- caret::confusionMatrix(as.factor(rf.test),as.factor(testset$stage))
rf.cf
rf.test2 <- predict(rf.train,newdata=testset,type="prob")
head(rf.test2)

#绘制变量重要性图
# 3.1 提取每个变量对样本分类的重要性
##RF1$importance #包含分类数+2列数据，
##每个自变量对每个分类的平均正确性降低值(mean descrease in accuracy),
##后两列分别为变量对所有分类的MeanDecreaseAccuracy和MeanDecreaseGini(节点不纯度减少值)。
##两个值越大，变量的重要性越大。
#RF.best$importanceSD # 变量重要值的置换检验的标准误，最后一列为MeanDecreaseAccuracy置换检验的p值。
imp = data.frame(importance(rf.train),MDA.p = rf.train$importanceSD[4])
head(imp) # 提取变量重要性值及置换检验p值.

# 3.2 将变量按MeanDecreaseGini重要性降序排列
library(dplyr)
imp = arrange(imp,desc(MeanDecreaseGini)) 
head(imp) 

## 输出重要性排序结果到本地
write.csv(imp,"importance.csv",quote = FALSE)

# 3.3 提取重要性top30变量绘制条形图
## top10
imp = imp[1:41,]
imp

## 随机森林变量重要性条形图
##变量按MeanDecreaseGini降序排列：reorder(rownames(imp),MeanDecreaseGini)
library(ggplot2)
p1= ggplot(imp,aes(x=MeanDecreaseGini,y=reorder(rownames(imp),MeanDecreaseGini)))+
  geom_bar(position = position_dodge(),
           width = 0.5,
           stat = "identity",
           fill="steelblue")+ # 柱子的宽度与位置要保持一致，拼图时左/右才能时柱子对齐。
  theme_minimal() +
  xlab("Mean Decrease in Gini Index")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  theme(axis.text.y = element_text(size = 16,colour = "black"),
        axis.text.x = element_text(size=14,color="black"),
        axis.title.x.bottom = element_text(size=16,color="black"),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #panel.grid = element_blank() # 去除网格线
  )
p1
