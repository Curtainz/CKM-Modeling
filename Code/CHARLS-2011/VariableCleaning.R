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
##########################
#将1和3对调以便于分数计算#
##########################
# 将da057_1_-da057_11_列里的1变成10，3变成30
data <- data %>% 
  mutate(da057_1_ = ifelse(da057_1_ == 1, 10, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 1, 10, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 1, 10, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 1, 10, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 1, 10, da057_5_)) %>% 
  mutate(da057_6_ = ifelse(da057_6_ == 1, 10, da057_6_)) %>% 
  mutate(da057_7_ = ifelse(da057_7_ == 1, 10, da057_7_)) %>% 
  mutate(da057_8_ = ifelse(da057_8_ == 1, 10, da057_8_)) %>% 
  mutate(da057_9_ = ifelse(da057_9_ == 1, 10, da057_9_)) %>% 
  mutate(da057_10_ = ifelse(da057_10_ == 1, 10, da057_10_)) %>% 
  mutate(da057_11_ = ifelse(da057_11_ == 1, 10, da057_11_)) %>% 
  mutate(da057_1_ = ifelse(da057_1_ == 3, 30, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 3, 30, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 3, 30, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 3, 30, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 3, 30, da057_5_)) %>%
  mutate(da057_6_ = ifelse(da057_6_ == 3, 30, da057_6_)) %>%
  mutate(da057_7_ = ifelse(da057_7_ == 3, 30, da057_7_)) %>%
  mutate(da057_8_ = ifelse(da057_8_ == 3, 30, da057_8_)) %>%
  mutate(da057_9_ = ifelse(da057_9_ == 3, 30, da057_9_)) %>%
  mutate(da057_10_ = ifelse(da057_10_ == 3, 30, da057_10_)) %>%
  mutate(da057_11_ = ifelse(da057_11_ == 3, 30, da057_11_))
# 将da057_1_-da057_11_列里的10变成3，30变成1
data <- data %>% 
  mutate(da057_1_ = ifelse(da057_1_ == 10, 3, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 10, 3, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 10, 3, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 10, 3, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 10, 3, da057_5_)) %>%
  mutate(da057_6_ = ifelse(da057_6_ == 10, 3, da057_6_)) %>%
  mutate(da057_7_ = ifelse(da057_7_ == 10, 3, da057_7_)) %>%
  mutate(da057_8_ = ifelse(da057_8_ == 10, 3, da057_8_)) %>%
  mutate(da057_9_ = ifelse(da057_9_ == 10, 3, da057_9_)) %>%
  mutate(da057_10_ = ifelse(da057_10_ == 10, 3, da057_10_)) %>%
  mutate(da057_11_ = ifelse(da057_11_ == 10, 3, da057_11_)) %>%
  mutate(da057_1_ = ifelse(da057_1_ == 30, 1, da057_1_)) %>% 
  mutate(da057_2_ = ifelse(da057_2_ == 30, 1, da057_2_)) %>% 
  mutate(da057_3_ = ifelse(da057_3_ == 30, 1, da057_3_)) %>% 
  mutate(da057_4_ = ifelse(da057_4_ == 30, 1, da057_4_)) %>% 
  mutate(da057_5_ = ifelse(da057_5_ == 30, 1, da057_5_)) %>%
  mutate(da057_6_ = ifelse(da057_6_ == 30, 1, da057_6_)) %>%
  mutate(da057_7_ = ifelse(da057_7_ == 30, 1, da057_7_)) %>%
  mutate(da057_8_ = ifelse(da057_8_ == 30, 1, da057_8_)) %>%
  mutate(da057_9_ = ifelse(da057_9_ == 30, 1, da057_9_)) %>%
  mutate(da057_10_ = ifelse(da057_10_ == 30, 1, da057_10_)) %>%
  mutate(da057_11_ = ifelse(da057_11_ == 30, 1, da057_11_))
##########################
data <- data %>% # 创建社交活动参与频繁度分数列，为da057_1_-_11_的和
  mutate(social_freq_score = da057_1_ + da057_2_ + da057_3_ + da057_4_ + da057_5_ + da057_6_ + da057_7_ + da057_8_ + da057_9_ + da057_10_ + da057_11_)
# 如果da056s12的值不为空，则将该样本的社交活动参与分数和社交活动参与频繁度分数赋值为0
data <- data %>% 
  mutate(social_attend_score = ifelse(is.na(da056s12), social_attend_score, 0)) %>% 
  mutate(social_freq_score = ifelse(is.na(da056s12), social_freq_score, 0))
#data <- data %>% # 判断社交活动参与状态，0-3分为低分，4-7分为中等分，8分以上为高分
#  mutate(social_attend_status = case_when(
#    social_activity_score <= 3 ~ "low",
#    social_activity_score <= 7 ~ "medium",
#    TRUE ~ "high"
#  ))
#data <- data %>% # 判断社交活动参与频繁度状态，0-9分为低分，10-19分为中等分，20-33分为高分
#  mutate(social_freq_status = case_when(
#    social_freq_score <= 9 ~ "low",
#    social_freq_score <= 19 ~ "medium",
#    TRUE ~ "high"
#  ))

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

##### BADL分数计算 #####
data <- data %>% # 创建BADL分数列，为db010-db015的和
  mutate(badl_score = db010 + db011 + db012 + db013 + db014 + db015)
data <- data %>% # 判断BADL状态，小于等于12分为独立，大于12分为依赖
  mutate(badl_status = case_when(
    badl_score <= 12 ~ "indep",
    TRUE ~ "dep"
  ))

##### IADL分数计算 #####
data <- data %>% # 创建IADL分数列，为db016-db020的和
  mutate(iadl_score = db016 + db017 + db018 + db019 + db020)
data <- data %>% # 判断IADL状态，小于等于10分为独立，大于10分为依赖
  mutate(iadl_status = case_when(
    iadl_score <= 10 ~ "indep",
    TRUE ~ "dep"
  ))

##### 认知数据整理计算 #####
# 时间认知数据整理
data <- data %>% # dc001s1-dc001s3中不为空值的赋值1，为空值的赋值0
  mutate(dc001s1 = ifelse(is.na(dc001s1), 0, 1)) %>% 
  mutate(dc001s2 = ifelse(is.na(dc001s2), 0, 1)) %>% 
  mutate(dc001s3 = ifelse(is.na(dc001s3), 0, 1))
data <- data %>% #dc002-dc003 dc025中的2和空值赋值0
  mutate(dc002 = ifelse(dc002 == 2, 0, dc002)) %>% 
  mutate(dc002 = ifelse(is.na(dc002), 0, dc002)) %>%
  mutate(dc003 = ifelse(dc003 == 2, 0, dc003)) %>%
  mutate(dc003 = ifelse(is.na(dc003), 0, dc003)) %>%
  mutate(dc025 = ifelse(dc025 == 2, 0, dc025)) %>%
  mutate(dc025 = ifelse(is.na(dc025), 0, dc025))
data <- data %>% # 将dc006s1-s11中最大的数值转写到wordrecall_1 列
  mutate(wordrecall_1 = pmax(dc006s1, dc006s2, dc006s3, dc006s4, dc006s5, dc006s6, dc006s7, dc006s8, dc006s9, dc006s10, dc006s11, na.rm = TRUE))
data <- data %>% # 将dc027s1-s11中最大的数值转写到wordrecall_2 列
  mutate(wordrecall_2 = pmax(dc027s1, dc027s2, dc027s3, dc027s4, dc027s5, dc027s6, dc027s7, dc027s8, dc027s9, dc027s10, dc027s11, na.rm = TRUE))
# 删除dc006s1-dc006s11 dc027s1-dc027s11
data <- data %>% select(-dc006s1, -dc006s2, -dc006s3, -dc006s4, -dc006s5, -dc006s6, -dc006s7, -dc006s8, -dc006s9, -dc006s10, -dc006s11, -dc027s1, -dc027s2, -dc027s3, -dc027s4, -dc027s5, -dc027s6, -dc027s7, -dc027s8, -dc027s9, -dc027s10, -dc027s11)
data <- data %>% #将wordrecall两列中的11赋值为0
  mutate(wordrecall_1 = ifelse(wordrecall_1 == 11, 0, wordrecall_1)) %>% 
  mutate(wordrecall_2 = ifelse(wordrecall_2 == 11, 0, wordrecall_2))
data <- data %>% # dc019-dc023中空值赋值0
  mutate(dc019 = ifelse(is.na(dc019), 0, dc019)) %>% 
  mutate(dc020 = ifelse(is.na(dc020), 0, dc020)) %>% 
  mutate(dc021 = ifelse(is.na(dc021), 0, dc021)) %>% 
  mutate(dc022 = ifelse(is.na(dc022), 0, dc022)) %>% 
  mutate(dc023 = ifelse(is.na(dc023), 0, dc023))
data <- data %>% # 判断dc019-dc023是否正确，正确的赋值1，不正确的赋值0
  mutate(dc019 = ifelse(dc019 == 93, 1, 0)) %>% 
  mutate(dc020 = ifelse(dc020 == 86, 1, 0)) %>% 
  mutate(dc021 = ifelse(dc021 == 79, 1, 0)) %>% 
  mutate(dc022 = ifelse(dc022 == 72, 1, 0)) %>% 
  mutate(dc023 = ifelse(dc023 == 65, 1, 0))
data <- data %>% # 计算认知得分
  mutate(cognitive_score = dc001s1 + dc001s2 + dc001s3 + dc002 + dc003 + dc025 + wordrecall_1 + dc019 + dc020 + dc021 + dc022 + dc023)
data <- data %>% # bd001空值赋值0
  mutate(bd001 = ifelse(is.na(bd001), 0, bd001))
data <- data %>% # 判断认知状态：若bd001=1，大于等于10分为正常；bd001=2~4，大于等于12分为正常；bd001>4，大于等于15分为正常
  mutate(cognitive_status = case_when(
    bd001 == 0 ~ NA,
    bd001 == 1 & cognitive_score >= 10 ~ "normal",
    bd001 %in% 2:4 & cognitive_score >= 12 ~ "normal",
    bd001 > 4 & cognitive_score >= 15 ~ "normal",
    TRUE ~ "abnormal"
  ))

##### 计算抑郁症状 #####
data <- data %>% # dc009-dc018每一列都减1
  mutate(dc009 = ifelse(is.na(dc009), dc009, dc009 - 1)) %>% 
  mutate(dc010 = ifelse(is.na(dc010), dc010, dc010 - 1)) %>% 
  mutate(dc011 = ifelse(is.na(dc011), dc011, dc011 - 1)) %>% 
  mutate(dc012 = ifelse(is.na(dc012), dc012, dc012 - 1)) %>% 
  mutate(dc013 = ifelse(is.na(dc013), dc013, dc013 - 1)) %>% 
  mutate(dc014 = ifelse(is.na(dc014), dc014, dc014 - 1)) %>% 
  mutate(dc015 = ifelse(is.na(dc015), dc015, dc015 - 1)) %>% 
  mutate(dc016 = ifelse(is.na(dc016), dc016, dc016 - 1)) %>% 
  mutate(dc017 = ifelse(is.na(dc017), dc017, dc017 - 1)) %>% 
  mutate(dc018 = ifelse(is.na(dc018), dc018, dc018 - 1))
data <- data %>% # 计算抑郁症状得分
  mutate(
    na_count = rowSums(is.na(select(., dc009:dc018))),
    depression_score = if_else(na_count <= 2, 
                               dc009 + dc010 + dc011 + dc012 - dc013 + dc014 + dc015 - dc016 + dc017 + dc018, 
                               NA_real_)) %>%   select(-na_count)  # 移除临时列 na_count
data <- data %>% # 判断抑郁症状状态，大于10分代表存在抑郁问题
  mutate(depression_status = case_when(
    is.na(depression_score) ~ NA,
    depression_score > 10 ~ "yes",
    TRUE ~ "no"
  ))

##### 查询并导出数据 #####
table(data$adl_status)
table(data$badl_status)
table(data$iadl_status)
table(data$cognitive_status)
table(data$depression_status)
write_dta(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/5-cleaned_variable_data.dta")
