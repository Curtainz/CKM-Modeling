# 加载程序包
library(haven)
library(dplyr)

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/4-combined_variable_data.dta")

##### 整理自我健康评估列 #####
data <- data %>% mutate(da002 = ifelse(is.na(da002), da002, da002 + 1)) # 把da002的所有值+1，不包括空值
data <- data %>% mutate(da001 = ifelse(is.na(da001), da002, da001))     # 把da001的空值替换为da002的值
data <- data %>% select(-da002)                                         # 删除da002

##### 整理慢性病列 #####
data <- data %>% 
  mutate(da007_1_ = ifelse(is.na(da008_1_), da007_1_, da008_1_)) %>% # 用自报数据代替高血压
  mutate(da007_5_ = ifelse(is.na(da008_5_), da007_5_, da008_5_)) %>% # 用自报数据代替肺部慢性病
  mutate(da007_11_ = ifelse(is.na(da008_11_), da007_11_, da008_11_)) # 用自报数据代替情感或精神问题
data <- data %>% select(-da008_11_, -da008_5_, -da008_1_)            # 删除da008_11_ da008_5_ da008_1_

##### 社交活动分数计算 #####
# 社交活动参与情况
data <- data %>% # 将da056s1-11的空值替换为0，非空值替换为1，12除外
  mutate(da056s1 = ifelse(is.na(da056s1), 0, 1)) %>% 
  mutate(da056s2 = ifelse(is.na(da056s2), 0, 1)) %>% 
  mutate(da056s3 = ifelse(is.na(da056s3), 0, 1)) %>% 
  mutate(da056s4 = ifelse(is.na(da056s4), 0, 1)) %>% 
  mutate(da056s5 = ifelse(is.na(da056s5), 0, 1)) %>% 
  mutate(da056s6 = ifelse(is.na(da056s6), 0, 1)) %>% 
  mutate(da056s7 = ifelse(is.na(da056s7), 0, 1)) %>% 
  mutate(da056s8 = ifelse(is.na(da056s8), 0, 1)) %>% 
  mutate(da056s9 = ifelse(is.na(da056s9), 0, 1)) %>% 
  mutate(da056s10 = ifelse(is.na(da056s10), 0, 1)) %>% 
  mutate(da056s11 = ifelse(is.na(da056s11), 0, 1))
data <- data %>% # 创建社交活动参与分数列，为da056s1-11的和
  mutate(social_attend_score = da056s1 + da056s2 + da056s3 + da056s4 + da056s5 + da056s6 + da056s7 + da056s8 + da056s9 + da056s10 + da056s11)
data <- data %>% # 判断社交活动参与状态，0-3分为低分，4-7分为中等分，8分以上为高分
  mutate(social_attend_status = case_when(
    social_activity_score <= 3 ~ "low",
    social_activity_score <= 7 ~ "medium",
    TRUE ~ "high"
  ))
# 社交活动参与频繁度情况
data <- data %>% # 将da057_1_-_11_的空值替换为0
  mutate(da057_1_ = ifelse(is.na(da057_1_), 0, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(is.na(da057_2_), 0, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(is.na(da057_3_), 0, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(is.na(da057_4_), 0, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(is.na(da057_5_), 0, da057_5_)) %>% 
  mutate(da057_6_ = ifelse(is.na(da057_6_), 0, da057_6_)) %>% 
  mutate(da057_7_ = ifelse(is.na(da057_7_), 0, da057_7_)) %>% 
  mutate(da057_8_ = ifelse(is.na(da057_8_), 0, da057_8_)) %>% 
  mutate(da057_9_ = ifelse(is.na(da057_9_), 0, da057_9_)) %>% 
  mutate(da057_10_ = ifelse(is.na(da057_10_), 0, da057_10_)) %>% 
  mutate(da057_11_ = ifelse(is.na(da057_11_), 0, da057_11_))
# 将da057_1_-_11_列里的1变成3，3变成1
data <- data %>% 
  mutate(da057_1_ = ifelse(da057_1_ == 1, 3, da057_1_)) %>% 
  mutate(da057_1_ = ifelse(da057_1_ == 3, 1, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 1, 3, da057_2_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 3, 1, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 1, 3, da057_3_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 3, 1, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 1, 3, da057_4_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 3, 1, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 1, 3, da057_5_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 3, 1, da057_5_)) %>% 
  mutate(da057_6_ = ifelse(da057_6_ == 1, 3, da057_6_)) %>% 
  mutate(da057_6_ = ifelse(da057_6_ == 3, 1, da057_6_)) %>% 
  mutate(da057_7_ = ifelse(da057_7_ == 1, 3, da057_7_)) %>% 
  mutate(da057_7_ = ifelse(da057_7_ == 3, 1, da057_7_)) %>% 
  mutate(da057_8_ = ifelse(da057_8_ == 1, 3, da057_8_)) %>% 
  mutate(da057_8_ = ifelse(da057_8_ == 3, 1, da057_8_)) %>%
  mutate(da057_9_ = ifelse(da057_9_ == 1, 3, da057_9_)) %>%
  mutate(da057_9_ = ifelse(da057_9_ == 3, 1, da057_9_)) %>%
  mutate(da057_10_ = ifelse(da057_10_ == 1, 3, da057_10_)) %>%
  mutate(da057_10_ = ifelse(da057_10_ == 3, 1, da057_10_)) %>%
  mutate(da057_11_ = ifelse(da057_11_ == 1, 3, da057_11_)) %>%
  mutate(da057_11_ = ifelse(da057_11_ == 3, 1, da057_11_))
# 创建社交活动参与频繁度分数列，为da057_1_-_11_的和
data <- data %>% 
  mutate(social_freq_score = da057_1_ + da057_2_ + da057_3_ + da057_4_ + da057_5_ + da057_6_ + da057_7_ + da057_8_ + da057_9_ + da057_10_ + da057_11_)
data <- data %>% # 判断社交活动参与频繁度状态，0-9分为低分，10-19分为中等分，20-33分为高分
  mutate(social_freq_status = case_when(
    social_freq_score <= 9 ~ "low",
    social_freq_score <= 19 ~ "medium",
    TRUE ~ "high"
  ))

##### ADL分数计算 #####
data <- data %>% # 把db001-db009的所有值-1，不包括空值
  mutate(db001 = ifelse(is.na(db001), db001, db001 - 1)) %>% 
  mutate(db002 = ifelse(is.na(db002), db002, db002 - 1)) %>% 
  mutate(db003 = ifelse(is.na(db003), db003, db003 - 1)) %>% 
  mutate(db004 = ifelse(is.na(db004), db004, db004 - 1)) %>% 
  mutate(db005 = ifelse(is.na(db005), db005, db005 - 1)) %>% 
  mutate(db006 = ifelse(is.na(db006), db006, db006 - 1)) %>% 
  mutate(db007 = ifelse(is.na(db007), db007, db007 - 1)) %>% 
  mutate(db008 = ifelse(is.na(db008), db008, db008 - 1)) %>% 
  mutate(db009 = ifelse(is.na(db009), db009, db009 - 1))
data <- data %>% # 若该样本在da001-da009的空值不大于9个，则把da001-da009的所有空值赋值为0
  mutate(db001 = ifelse(is.na(db001), 0, db001)) %>% 
  mutate(db002 = ifelse(is.na(db002), 0, db002)) %>% 
  mutate(db003 = ifelse(is.na(db003), 0, db003)) %>% 
  mutate(db004 = ifelse(is.na(db004), 0, db004)) %>% 
  mutate(db005 = ifelse(is.na(db005), 0, db005)) %>% 
  mutate(db006 = ifelse(is.na(db006), 0, db006)) %>% 
  mutate(db007 = ifelse(is.na(db007), 0, db007)) %>% 
  mutate(db008 = ifelse(is.na(db008), 0, db008)) %>% 
  mutate(db009 = ifelse(is.na(db009), 0, db009))
data <- data %>% 
  mutate(adl_score = db001 + db002 + db003 + db004 + db005 + db006 + db007 + db008 + db009) # 计算ADL分数
data <- data %>% # 判断ADL状态，0-3分为良好，4-6分为中等，7分以上为差
  mutate(adl_status = case_when(
    adl_score <= 3 ~ "good",
    adl_score <= 6 ~ "mild",
    TRUE ~ "severe"
  ))


