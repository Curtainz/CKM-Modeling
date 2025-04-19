library(dplyr)
library(haven)
library(readxl)
library(mice)

# 读取数据
data <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/AIO_Output.csv")

# 选择需要的列
data_2011 <- data %>% select(ID, gender, age)
rm(data)

# ██████╗  ██████╗  ██╗███████╗    ██████╗  █████╗ ████████╗ █████╗ 
# ╚════██╗██╔═████╗███║██╔════╝    ██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗
#  █████╔╝██║██╔██║╚██║███████╗    ██║  ██║███████║   ██║   ███████║
# ██╔═══╝ ████╔╝██║ ██║╚════██║    ██║  ██║██╔══██║   ██║   ██╔══██║
# ███████╗╚██████╔╝ ██║███████║    ██████╔╝██║  ██║   ██║   ██║  ██║

##### B 基本信息 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/demographic_background.dta")
# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# be001婚姻状态，bb001_w3_2现在居住地区的类型
household_roster <- data %>% select(ID, householdID, communityID, be001, bb001_w3_2)

##### D 健康状况和功能 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/health_status_and_functioning.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# da001个人健康状况评估
# da002个人健康状况评估
# zda005残疾问题 1-5, da005 1-5
# da049晚上真正睡着的时间有几小时
# da056s1-12是否进行这些社交活动（12为无）
# da057_1_-_11_社交活动频率
# da067过去一年中是否喝酒，频率如何
# db001-db020行为困难
# dc001-dc003日期、星期、季节认知
# dc004自我记忆力评估
# dc006s1-11词语回忆测试
# dc027s1-11延时词语回忆测试
# dc009-dc018抑郁症状
# dc019-dc023计算能力测试，dc024计算中是否使用辅助工具（1是 2否）
# dc025画图测试
health_status_and_functioning <- data %>% 
  select(ID, householdID, communityID, da001, da002, zda005_1_, zda005_2_, zda005_3_, zda005_4_, zda005_5_,
         da005_1_, da005_2_, da005_3_, da005_4_, da005_5_, da049, da056s1, da056s2, da056s3, da056s4, da056s5,
         da056s6, da056s7, da056s8, da056s9, da056s10, da056s11, da056s12, da057_1_, da057_2_, da057_3_,
         da057_4_, da057_5_, da057_6_, da057_7_, da057_8_, da057_9_, da057_10_, da057_11_,
         da067, db001, db002, db003, db004,
         db005, db006, db007, db008, db009, db010, db011, db012, db013, db014, db015, db016, db017, db018,
         db019, db020, dc001s1, dc001s2, dc001s3, dc002, dc003, dc004, dc006s1, dc006s2, dc006s3, dc006s4, dc006s5, dc006s6,
         dc006s7, dc006s8, dc006s9, dc006s10, dc006s11, dc027s1, dc027s2, dc027s3, dc027s4, dc027s5,
         dc027s6, dc027s7, dc027s8, dc027s9, dc027s10, dc027s11, dc009, dc010, dc011, dc012, dc013,
         dc014, dc015, dc016, dc017, dc018, dc019, dc020, dc021, dc022, dc023, dc024, dc025)

##### F 工作、退休和养老金 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/work_retirement_and_pension.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# fn002_w3是否在领取养老金
work_retirement_and_pension <- data %>% select(ID, householdID, communityID, fn002_w3)
rm(data)

##### 体检数据处理 #####
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/Biomarker.dta") # 读取体检数据
# 从biodata中提取<个人ID, 家庭ID, 社区ID, 身高, 体重是否≥150KG, 体重, 腰围, 收缩压1-3, 舒张压1-3>
data <- data %>% select(ID, householdID, communityID, qi002, pl001, ql002, qm002, qa003, qa007, qa011, qa004, qa008, qa012) 
# 重命名列
data <- data %>% 
  rename(
    height = qi002,
    weight = ql002,
    waist = qm002,
    systolic = qa003,
    diastolic = qa004
  )
data <- data %>% # 身高异常值标记，规则：小于80或大于200
  mutate(height_def = case_when(
    is.na(height) ~ "outlier",
    height < 80 ~ "outlier",
    height > 200 ~ "outlier",
    TRUE ~ "normal"))
data <- data %>% select(ID, householdID, communityID, height, height_def, everything()) # 移动列
data <- data %>% # 更新体重值，如果体重判断为5，则赋值为150
  mutate(weight = case_when(
    pl001 == 5 ~ 150,
    TRUE ~ weight))
data <- data %>% select(-pl001) # 剔除体重判断列
data <- data %>% # 体重异常值标记，规则：小于20或大于150
  mutate(
    weight_def = case_when(
      is.na(weight) ~ "outlier",
      weight < 20 ~ "outlier",
      weight > 150 ~ "outlier",
      TRUE ~ "normal"))
data <- data %>% # 腰围异常值标记，规则：小于40或大于200
  mutate(
    waist_def = case_when(
      is.na(waist) ~ "outlier",
      waist < 40 ~ "outlier",
      waist > 200 ~ "outlier",
      TRUE ~ "normal"))
data <- data %>% # 计算每个人的低压平均值，不包括异常值和缺失值
  rowwise() %>%
  mutate(
    systolic = mean(c(systolic, qa007, qa011)[c(systolic, qa007, qa011) <= 900], na.rm = TRUE)
  ) %>%
  ungroup()
data <- data %>% # 计算每个人的高压平均值，不包括异常值和缺失值
  rowwise() %>%
  mutate(
    diastolic = mean(c(diastolic, qa008, qa012)[c(systolic, qa008, qa012) <= 900], na.rm = TRUE)
  ) %>%
  ungroup()
data <- data %>% select(-qa007, -qa008, -qa011, -qa012) # 剔除血压
data <- data %>% # 血压异常值标记，规则：收缩压小于舒张压、收缩压小于50或大于300、舒张压小于30或大于180
  mutate(
    bp_def = case_when(
      is.na(systolic) ~ "outlier",
      is.na(diastolic) ~ "outlier",
      systolic < 50 ~ "outlier",
      systolic > 300 ~ "outlier",
      diastolic < 30 ~ "outlier",
      diastolic > 180 ~ "outlier",
      systolic < diastolic ~ "outlier",
      TRUE ~ "normal"))
data <- data %>% # 定义并排序BMI异常值
  mutate(
    BMI_def = ifelse(height_def == "normal" & weight_def == "normal", "normal", "outlier"))
# 去除身高体重腰围血压都缺失的样本
biodata <- data %>% filter(!is.na(height) & !is.na(weight) & !is.na(waist) & !is.na(systolic) & !is.na(diastolic))

##### 数据整合 #####

merged_data_1 <- merge(household_roster, health_status_and_functioning, by="ID")
merged_data_1 <- merged_data_1 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data_1 <- merged_data_1 %>%
  select(-c(householdID.y, communityID.y))

merged_data_2 <- merge(merged_data_1, work_retirement_and_pension, by="ID")
merged_data_2 <- merged_data_2 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data_2 <- merged_data_2 %>%
  select(-c(householdID.y, communityID.y))
merged_data_3 <- merge(merged_data_2, biodata, by="ID")
merged_data_3 <- merged_data_3 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
data_merged <- merged_data_3 %>%
  select(-c(householdID.y, communityID.y))
# 清理变量
rm(merged_data_1, merged_data_2, household_roster, health_status_and_functioning, work_retirement_and_pension, data, merged_data_3, biodata)

##### 整理自我健康评估列 #####
data <- data_merged
data <- data %>% mutate(da002 = ifelse(is.na(da002), da002, da002 + 1)) # 把da002的所有值+1，不包括空值
data <- data %>% mutate(da001 = ifelse(is.na(da001), da002, da001))     # 把da001的空值替换为da002的值
data <- data %>% select(-da002) 

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
# 创建社交活动参与频繁度分数列，为da057_1_-_11_的和
data <- data %>%
  mutate(social_freq_score = da057_1_ + da057_2_ + da057_3_ + da057_4_ + da057_5_ + da057_6_ + da057_7_ + da057_8_ + da057_9_ + da057_10_ + da057_11_)
# 如果da056s12的值不为空，则将该样本的社交活动参与分数和社交活动参与频繁度分数赋值为0
data <- data %>% 
  mutate(social_attend_score = ifelse(is.na(da056s12), social_attend_score, 0)) %>% 
  mutate(social_freq_score = ifelse(is.na(da056s12), social_freq_score, 0))

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

##### BADL分数计算 #####
data <- data %>% # db010至db015的所有值-1，不包括空值
  mutate(db010 = ifelse(is.na(db010), db010, db010 - 1)) %>% 
  mutate(db011 = ifelse(is.na(db011), db011, db011 - 1)) %>% 
  mutate(db012 = ifelse(is.na(db012), db012, db012 - 1)) %>% 
  mutate(db013 = ifelse(is.na(db013), db013, db013 - 1)) %>% 
  mutate(db014 = ifelse(is.na(db014), db014, db014 - 1)) %>%
  mutate(db015 = ifelse(is.na(db015), db015, db015 - 1))
data <- data %>% # 创建BADL分数列，为db010-db015的和
  mutate(badl_score = db010 + db011 + db012 + db013 + db014 + db015)
data <- data %>% # 判断BADL状态，小于等于12分为独立，大于12分为依赖
  mutate(badl_status = case_when(
    badl_score <= 10 ~ "0",
    TRUE ~ "1"
  ))

##### IADL分数计算 #####
data <- data %>% # db016至db020的所有值-1，不包括空值
  mutate(db016 = ifelse(is.na(db016), db016, db016 - 1)) %>% 
  mutate(db017 = ifelse(is.na(db017), db017, db017 - 1)) %>% 
  mutate(db018 = ifelse(is.na(db018), db018, db018 - 1)) %>% 
  mutate(db019 = ifelse(is.na(db019), db019, db019 - 1)) %>% 
  mutate(db020 = ifelse(is.na(db020), db020, db020 - 1))
data <- data %>% # 创建IADL分数列，为db016-db020的和
  mutate(iadl_score = db016 + db017 + db018 + db019 + db020)
data <- data %>% # 判断IADL状态，小于等于10分为独立，大于10分为依赖
  mutate(iadl_status = case_when(
    iadl_score <= 10 ~ "0",
    TRUE ~ "1"
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

#data <- data_2011 %>% # 判断认知状态：若bd001=1，大于等于10分为正常；bd001=2~4，大于等于12分为正常；bd001>4，大于等于15分为正常
#  mutate(cognitive_status = case_when(
#    education == 1 & data$cognitive_score >= 10 ~ "0",
#    education == 2 & data$cognitive_score >= 12 ~ "0",
#    education == 3 & data$cognitive_score >= 15 ~ "0",
#    TRUE ~ "1"
#  ))

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
data_merged <- data %>% # 判断抑郁症状状态，大于10分代表存在抑郁问题
  mutate(depression_status = case_when(
    is.na(depression_score) ~ NA,
    depression_score > 10 ~ "1",
    TRUE ~ "0"
  ))
rm(data)

# 改名
data_var <- data_merged %>%
  rename(
    region = bb001_w3_2,
    marital = be001,
    selfratedhealth = da001,
    zphysicalDisability = zda005_1_,
    zmentalDisability = zda005_2_,
    zvisionProblem = zda005_3_,
    zhearingProblem = zda005_4_,
    zspeechImpediment = zda005_5_,
    physicalDisability = da005_1_,
    mentalDisability = da005_2_,
    visionProblem = da005_3_,
    hearingProblem = da005_4_,
    speechImpediment = da005_5_,
    avgSleepTime = da049,
    tooluseInCalculation = dc024,
    drawing = dc025,
    physDis_score = adl_score
  )
# 选择变量
data_var <- data_var %>%
  select(
    ID, householdID, communityID, height, height_def, weight, weight_def, waist, waist_def, systolic, diastolic, bp_def, BMI_def,
    region, marital, selfratedhealth, 
    zphysicalDisability, zmentalDisability, zvisionProblem, zhearingProblem, zspeechImpediment, 
    physicalDisability, mentalDisability, visionProblem, hearingProblem, speechImpediment, 
    avgSleepTime, 
    drawing, 
    social_freq_score,
    physDis_score,
    badl_score,
    iadl_score,
    cognitive_score,
    depression_status
  )
# 定义性别，将ID的最后一位数覆盖到gender
data <- data_var %>%
  mutate(gender = substr(ID, nchar(ID), nchar(ID)))

# 剔除异常值
data <- data %>%
  filter(!if_any(everything(), ~ grepl("outlier", .)))

# 删除异常值定性列
data <- data %>%
  select(-height_def, -weight_def, -waist_def, -bp_def)

# 剔除<身高, 体重, 腰围, 低压, 高压, 阶段>空缺值
data <- data %>%
  filter(!is.na(height), !is.na(weight), !is.na(waist), !is.na(systolic), !is.na(diastolic))

# 精简marital数据，1 2 7=1，3 4 5 6=2
data <- data %>%
  mutate(marital = case_when(
    marital %in% c(1, 2, 7) ~ 1,
    marital %in% c(3, 4, 5, 6) ~ 2,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))

# 精简region数据，1 2 3 4 5=1, 6 7=0
data <- data %>%
  mutate(region = case_when(
    region %in% c(1, 2, 3, 4, 5) ~ 1,
    region %in% c(6, 7) ~ 0,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))

# 若zphysicalDisability不为空，则用其值替代physicalDisability的值
data <- data %>%
  mutate(physicalDisability = ifelse(!is.na(zphysicalDisability), zphysicalDisability, physicalDisability)) %>%
  select(-zphysicalDisability) # 删除zphysicalDisability列
# 若zmentalDisability不为空，则用其值替代mentalDisability的值
data <- data %>%
  mutate(mentalDisability = ifelse(!is.na(zmentalDisability), zmentalDisability, mentalDisability)) %>%
  select(-zmentalDisability) # 删除zmentalDisability列
# 若zvisionProblem不为空，则用其值替代visionProblem的值
data <- data %>%
  mutate(visionProblem = ifelse(!is.na(zvisionProblem), zvisionProblem, visionProblem)) %>%
  select(-zvisionProblem) # 删除zvisionProblem列
# 若zhearingProblem不为空，则用其值替代hearingProblem的值
data <- data %>%
  mutate(hearingProblem = ifelse(!is.na(zhearingProblem), zhearingProblem, hearingProblem)) %>%
  select(-zhearingProblem) # 删除zhearingProblem列
# 若zspeechImpediment不为空，则用其值替代speechImpediment的值
data <- data %>%
  mutate(speechImpediment = ifelse(!is.na(zspeechImpediment), zspeechImpediment, speechImpediment)) %>%
  select(-zspeechImpediment) # 删除zspeechImpediment列
data <- data %>% select(-drawing, -BMI_def) 
# 更改age列，age=age+4
data_2011 <- data_2011 %>% mutate(age = age + 4)
# data和data_2011根据ID列取交集
rm(data_merged)
data_merged <- merge(data, data_2011, by = "ID")
data_merged <- data_merged %>%
  rename(
    gender = gender.x,
  )
data_merged <- data_merged %>%
  select(-gender.y)
rm(data, data_2011, data_var)
data_combine <- data_merged
# 读取11年健康状况数据
healthdata_11 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/health_status_and_functioning.dta")

# 选择变量
healthdata_11 <- healthdata_11 %>% select(ID, householdID, da007_1_, da008_1_, da007_2_, 
                                          da007_3_, da007_4_, da007_5_, da008_5_, da007_6_, da007_7_, 
                                          da007_8_, da007_9_, da007_10_, da007_11_, da008_11_, 
                                          da007_12_, da007_13_, da007_13_, da007_14_)

# 将 "0" 添加到 householdID 的末尾
healthdata_11$householdID <- paste0(healthdata_11$householdID, "0")

# 将 ID 更新为 householdID 加上 ID 的最后两个字符
healthdata_11$ID <- paste0(healthdata_11$householdID, substr(healthdata_11$ID, nchar(healthdata_11$ID)-1, nchar(healthdata_11$ID)))

# 更新慢性病确认，如果确认为1，则赋值为1
healthdata_11 <- healthdata_11 %>% mutate(da007_1_ = case_when(da008_1_ == 1 ~ 1, TRUE ~ da007_1_))
healthdata_11 <- healthdata_11 %>% select(-da008_1_)
healthdata_11 <- healthdata_11 %>% mutate(da007_5_ = case_when(da008_5_ == 1 ~ 1, TRUE ~ da007_5_))
healthdata_11 <- healthdata_11 %>% select(-da008_5_)
healthdata_11 <- healthdata_11 %>% mutate(da007_11_ = case_when(da008_11_ == 1 ~ 1, TRUE ~ da007_11_))
healthdata_11 <- healthdata_11 %>% select(-da008_11_)

# 重命名
healthdata_11 <- healthdata_11 %>% rename(
  ID = ID,
  hypertension_11 = da007_1_,
  dyslipidemia_11 = da007_2_,
  diabetesOrHB_11 = da007_3_,
  cancerOrMalignantTumor_11 = da007_4_,
  chronicLungDisease_11 = da007_5_,
  liverDisease_11 = da007_6_,
  heartProblem_11 = da007_7_,
  stoke_11 = da007_8_,
  kidneyDisease_11 = da007_9_,
  stomachOrOtherDisgestiveDisease_11 = da007_10_,
  emotionalOrPsychiatricDisease_11 = da007_11_,
  memoryRelatedDisease_11 = da007_12_,
  arthritisOrRheumatism_11 = da007_13_,
  asthma_11 = da007_14_
  )

# 移除householdID
healthdata_11 <- healthdata_11 %>% select(-householdID)

# 读取13年健康状况数据
healthdata_13 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2013/RawData/health_status_and_functioning.dta")

# 选择变量
healthdata_13 <- healthdata_13 %>% select(ID, da007_w2_1_1_, da007_w2_1_2_, da007_w2_1_3_, 
                                          da007_w2_1_4_, da007_w2_1_5_, da007_w2_1_6_, 
                                          da007_w2_1_7_, da007_w2_1_8_, da007_w2_1_9_, 
                                          da007_w2_1_10_, da007_w2_1_11_, da007_w2_1_12_, 
                                          da007_w2_1_13_, da007_w2_1_14_)

# 重命名
healthdata_13 <- healthdata_13 %>% rename(
  ID = ID,
  verify_hypertension_13 = da007_w2_1_1_,
  verify_dyslipidemia_13 = da007_w2_1_2_,
  verify_diabetesOrHB_13 = da007_w2_1_3_,
  verify_cancerOrMalignantTumor_13 = da007_w2_1_4_,
  verify_chronicLungDisease_13 = da007_w2_1_5_,
  verify_liverDisease_13 = da007_w2_1_6_,
  verify_heartProblem_13 = da007_w2_1_7_,
  verify_stoke_13 = da007_w2_1_8_,
  verify_kidneyDisease_13 = da007_w2_1_9_,
  verify_stomachOrOtherDisgestiveDisease_13 = da007_w2_1_10_,
  verify_emotionalOrPsychiatricDisease_13 = da007_w2_1_11_,
  verify_memoryRelatedDisease_13 = da007_w2_1_12_,
  verify_arthritisOrRheumatism_13 = da007_w2_1_13_,
  verify_asthma_13 = da007_w2_1_14_
)

# 读取15年健康状况数据
healthdata_15 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/health_status_and_functioning.dta")

# 选择变量
healthdata_15 <- healthdata_15 %>% select(ID, da007_w2_1_1_, da007_w2_1_2_, da007_w2_1_3_, 
                                          da007_w2_1_4_, da007_w2_1_5_, da007_w2_1_6_, 
                                          da007_w2_1_7_, da007_w2_1_8_, da007_w2_1_9_, 
                                          da007_w2_1_10_, da007_w2_1_11_, da007_w2_1_12_, 
                                          da007_w2_1_13_, da007_w2_1_14_)

# 重命名
healthdata_15 <- healthdata_15 %>% rename(
  ID = ID,
  verify_hypertension_15 = da007_w2_1_1_,
  verify_dyslipidemia_15 = da007_w2_1_2_,
  verify_diabetesOrHB_15 = da007_w2_1_3_,
  verify_cancerOrMalignantTumor_15 = da007_w2_1_4_,
  verify_chronicLungDisease_15 = da007_w2_1_5_,
  verify_liverDisease_15 = da007_w2_1_6_,
  verify_heartProblem_15 = da007_w2_1_7_,
  verify_stoke_15 = da007_w2_1_8_,
  verify_kidneyDisease_15 = da007_w2_1_9_,
  verify_stomachOrOtherDisgestiveDisease_15 = da007_w2_1_10_,
  verify_emotionalOrPsychiatricDisease_15 = da007_w2_1_11_,
  verify_memoryRelatedDisease_15 = da007_w2_1_12_,
  verify_arthritisOrRheumatism_15 = da007_w2_1_13_,
  verify_asthma_15 = da007_w2_1_14_
)

# 将11 13 15年数据的ID转换为字符型
healthdata_11$ID <- as.character(healthdata_11$ID)
healthdata_13$ID <- as.character(healthdata_13$ID)
healthdata_15$ID <- as.character(healthdata_15$ID)

# 使用merge函数组合11 13 15年数据
healthdata_combine <- merge(healthdata_11, healthdata_13, by = "ID", all.x = TRUE)
healthdata_combine <- merge(healthdata_combine, healthdata_15, by = "ID", all.x = TRUE)

# 将healthdata_combine与data_combine取交集
combine_3rd <- merge(healthdata_combine, data_combine, by = "ID")

# 定义需要处理的健康状态列名
health_conditions <- c("hypertension", "dyslipidemia", "diabetesOrHB", "cancerOrMalignantTumor", 
                       "chronicLungDisease", "liverDisease", "heartProblem", 
                       "stoke", "kidneyDisease", "stomachOrOtherDisgestiveDisease", 
                       "emotionalOrPsychiatricDisease", "memoryRelatedDisease", 
                       "arthritisOrRheumatism", "asthma")

# 遍历每种健康状态
for (condition in health_conditions) {
  # 拼接列名
  condition_2011 <- paste0(condition, "_11")
  verify_2013 <- paste0("verify_", condition, "_13")
  verify_2015 <- paste0("verify_", condition, "_15")
  
  # 检查 2013 年的验证数据
  combine_3rd[[condition_2011]] <- ifelse(
    combine_3rd[[verify_2013]] == 2, # 如果 2013 年验证数据为 2
    ifelse(combine_3rd[[condition_2011]] == 1, 2, 
           ifelse(combine_3rd[[condition_2011]] == 2, 1, NA)), # 翻转数据
    combine_3rd[[condition_2011]] # 否则保持原值
  )
  
  # 检查 2015 年的验证数据
  combine_3rd[[condition_2011]] <- ifelse(
    combine_3rd[[verify_2015]] == 2, # 如果 2015 年验证数据为 2
    ifelse(combine_3rd[[condition_2011]] == 1, 2, 
           ifelse(combine_3rd[[condition_2011]] == 2, 1, NA)), # 翻转数据
    combine_3rd[[condition_2011]] # 否则保持原值
  )
}

# 选择变量
data <- combine_3rd %>% select(ID, gender, age, region, marital, height, weight, waist, 
                               systolic, diastolic, 
                               hypertension_11, dyslipidemia_11, diabetesOrHB_11, 
                               cancerOrMalignantTumor_11, chronicLungDisease_11, 
                               liverDisease_11, heartProblem_11, stoke_11, 
                               kidneyDisease_11, stomachOrOtherDisgestiveDisease_11, 
                               emotionalOrPsychiatricDisease_11, memoryRelatedDisease_11, 
                               arthritisOrRheumatism_11, asthma_11, selfratedhealth,
                               physicalDisability, mentalDisability, visionProblem,
                               hearingProblem, speechImpediment, avgSleepTime, 
                               social_freq_score, physDis_score, badl_score, iadl_score,
                               cognitive_score, depression_status)

# 重命名
data <- data %>% rename(
  ID = ID,
  hypertension = hypertension_11,
  dyslipidemia = dyslipidemia_11,
  diabetesOrHB = diabetesOrHB_11,
  cancerOrMalignantTumor = cancerOrMalignantTumor_11,
  chronicLungDisease = chronicLungDisease_11,
  liverDisease = liverDisease_11,
  heartProblem = heartProblem_11,
  stoke = stoke_11,
  kidneyDisease = kidneyDisease_11,
  stomachOrOtherDisgestiveDisease = stomachOrOtherDisgestiveDisease_11,
  emotionalOrPsychiatricDisease = emotionalOrPsychiatricDisease_11,
  memoryRelatedDisease = memoryRelatedDisease_11,
  arthritisOrRheumatism = arthritisOrRheumatism_11,
  asthma = asthma_11
)
data$depression_status <- as.factor(data$depression_status)
# 使用 mice 进行插补，默认 method 会根据变量类型自动选择
temp_data <- mice(data[, c("region", "hypertension", "dyslipidemia", 
                           "diabetesOrHB", "cancerOrMalignantTumor", 
                           "chronicLungDisease", "liverDisease", "heartProblem", 
                           "stoke", "kidneyDisease", "stomachOrOtherDisgestiveDisease", 
                           "emotionalOrPsychiatricDisease", "memoryRelatedDisease", 
                           "arthritisOrRheumatism", "asthma", "selfratedhealth", 
                           "physicalDisability", "mentalDisability", "visionProblem", "hearingProblem", "speechImpediment",  
                           "avgSleepTime", "badl_score", "iadl_score", "cognitive_score", "depression_status")], m = 1, seed = 123)
temp_data_ <- complete(temp_data)

# 提取填补完成的数据集
data$region <- temp_data_$region
data$hypertension <- temp_data_$hypertension
data$dyslipidemia <- temp_data_$dyslipidemia
data$diabetesOrHB <- temp_data_$diabetesOrHB
data$cancerOrMalignantTumor <- temp_data_$cancerOrMalignantTumor
data$chronicLungDisease <- temp_data_$chronicLungDisease
data$liverDisease <- temp_data_$liverDisease
data$heartProblem <- temp_data_$heartProblem
data$stoke <- temp_data_$stoke
data$kidneyDisease <- temp_data_$kidneyDisease
data$stomachOrOtherDisgestiveDisease <- temp_data_$stomachOrOtherDisgestiveDisease
data$emotionalOrPsychiatricDisease <- temp_data_$emotionalOrPsychiatricDisease
data$memoryRelatedDisease <- temp_data_$memoryRelatedDisease
data$arthritisOrRheumatism <- temp_data_$arthritisOrRheumatism
data$asthma <- temp_data_$asthma
data$selfratedhealth <- temp_data_$selfratedhealth
data$physicalDisability <- temp_data_$physicalDisability
data$mentalDisability <- temp_data_$mentalDisability
data$visionProblem <- temp_data_$visionProblem
data$hearingProblem <- temp_data_$hearingProblem
data$speechImpediment <- temp_data_$speechImpediment
data$avgSleepTime <- temp_data_$avgSleepTime
data$badl_score <- temp_data_$badl_score
data$iadl_score <- temp_data_$iadl_score
data$cognitive_score <- temp_data_$cognitive_score
data$depression_status <- temp_data_$depression_status

# 写出数据
write.csv(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/AIO_Output.csv", row.names = FALSE)
