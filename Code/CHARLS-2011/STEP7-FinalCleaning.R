# 加载程序包
library(haven)
library(dplyr)
library(table1)

# 读取数据
data <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/6-merged_data.csv")

# 定义性别，将ID的最后一位数覆盖到gender
data <- data %>%
  mutate(gender = substr(ID, nchar(ID), nchar(ID)))

# 剔除异常值
data <- data %>%
  filter(!if_any(everything(), ~ grepl("outlier", .)))

# 删除异常值定性列
data <- data %>%
  select(-height_def, -weight_def, -waist_def, -bp_def)

# 剔除空缺值
data <- data %>%
  filter(complete.cases(height)) %>%
  filter(complete.cases(weight)) %>%
  filter(complete.cases(systolic)) %>%
  filter(complete.cases(diastolic)) %>%
  filter(complete.cases(stage)) %>%
  filter(complete.cases(region)) %>%
  filter(complete.cases(age)) %>%
  filter(complete.cases(education)) %>%
  filter(complete.cases(marital)) %>%
  filter(complete.cases(selfratedhealth)) %>%
  filter(complete.cases(waist))
