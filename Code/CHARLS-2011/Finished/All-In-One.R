library(haven)
library(dplyr)
library(mice)

# ███████╗████████╗███████╗██████╗  ██╗
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗███║
# ███████╗   ██║   █████╗  ██████╔╝╚██║
# ╚════██║   ██║   ██╔══╝  ██╔═══╝  ██║
# ███████║   ██║   ███████╗██║      ██║
# ╚══════╝   ╚═╝   ╚══════╝╚═╝      ╚═╝

##### 体检数据处理 #####
biodata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/biomarkers.dta") # 读取体检数据
# 从biodata中提取<个人ID, 家庭ID, 社区ID, 身高, 体重是否≥150KG, 体重, 腰围, 收缩压1-3, 舒张压1-3>
extracted_biodata <- biodata %>% select(ID, householdID, communityID, qi002, pl001, ql002, qm002, qa003, qa007, qa011, qa004, qa008, qa012) 
rm(biodata) #清理变量

##### 健康状况数据处理 #####
healthdata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/health_status_and_functioning.dta") # 读取健康状况数据
# 从healthdata中提取<个人ID, 家庭ID, 社区ID, 高血压(1=yes, 2=no, 后同), 高血压再确认, 糖尿病, 心脏病, 中风, 肾病, 降压药情况(中), 降压药情况(西)
extracted_healthdata <- healthdata %>% select(ID, householdID, communityID, da007_1_, da008_1_, da007_3_, da007_7_, da007_8_, da007_9_, da011s1, da011s2) 
rm(healthdata) #清理变量

##### 血检数据处理 #####
blooddata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/Blood_20140429.dta") # 读取血检数据
# 从blooddata中提取<个人ID, HbA1c, 高密度脂蛋白胆固醇, 甘油三酯, 葡萄糖>
extracted_blooddata <- blooddata %>% select(ID, newhba1c, newhdl, newtg, newglu)
rm(blooddata) #清理变量

##### 数据整合 #####
merged_data_1 <- merge(extracted_biodata,extracted_blooddata,by="ID")
merged_data_2 <- merge(merged_data_1,extracted_healthdata,by="ID")
merged_data_2 <- merged_data_2 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
data_step1 <- merged_data_2 %>%
  select(-c(householdID.y, communityID.y))

##### 清理变量 #####
rm(extracted_biodata, extracted_blooddata, extracted_healthdata, merged_data_1, merged_data_2)

# ███████╗████████╗███████╗██████╗ ██████╗ 
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗╚════██╗
# ███████╗   ██║   █████╗  ██████╔╝ █████╔╝
# ╚════██║   ██║   ██╔══╝  ██╔═══╝ ██╔═══╝ 
# ███████║   ██║   ███████╗██║     ███████╗
# ╚══════╝   ╚═╝   ╚══════╝╚═╝     ╚══════╝

# 重命名列
data <- data_step1 %>% 
  rename(
    height = qi002,
    weight = ql002,
    waist = qm002,
    hypert = da007_1_,
    diabetes_hbs = da007_3_,
    heart_disease = da007_7_,
    stroke = da007_8_,
    kidney_disease = da007_9_,
    systolic = qa003,
    diastolic = qa004
  )

#################
##身高/体重/BMI##
#################
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
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, everything()) # 移动列
data$BMI <- data$weight / (data$height / 100)^2 # 计算 BMI 值，BMI = 体重(kg) / (身高(m))^2
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, everything()) # 移动列

######
#腰围#
######
data <- data %>% # 腰围异常值标记，规则：小于40或大于200
  mutate(
    waist_def = case_when(
      is.na(waist) ~ "outlier",
      waist < 40 ~ "outlier",
      waist > 200 ~ "outlier",
      TRUE ~ "normal"))
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, everything()) # 移动列

######
#血压#
######
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
# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, systolic, diastolic, bp_def, everything())
data <- data %>% # 更新血压，如果血压确认为1，则赋值为1
  mutate(hypert = case_when(
    da008_1_ == 1 ~ 1,
    TRUE ~ hypert))
data <- data %>% select(-da008_1_) # 剔除血压确认列
data <- data %>% # 检查用药情况
  mutate(
    hyp_med = ifelse(da011s1 == 1 | da011s2 == 1, 1, NA))
data <- data %>% select(-da011s1, -da011s2) # 剔除中西药列
# 移动列
data_step2 <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, systolic, diastolic, bp_def, hypert, hyp_med, diabetes_hbs, heart_disease, stroke, kidney_disease, everything())
rm(data) # 清理变量

# ███████╗████████╗███████╗██████╗ ██████╗ 
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗╚════██╗
# ███████╗   ██║   █████╗  ██████╔╝ █████╔╝
# ╚════██║   ██║   ██╔══╝  ██╔═══╝  ╚═══██╗
# ███████║   ██║   ███████╗██║     ██████╔╝
# ╚══════╝   ╚═╝   ╚══════╝╚═╝     ╚═════╝ 

data <- data_step2 %>% # 定义并排序BMI异常值
  mutate(
    BMI_def = ifelse(height_def == "normal" & weight_def == "normal", "normal", "outlier"))
data <- data %>%
  select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, BMI_def, everything())
# 去除4个血检数据都没有的样本
data <- data %>% filter(!is.na(newhba1c) & !is.na(newhdl) & !is.na(newtg) & !is.na(newglu))
# 去除身高体重腰围血压都缺失的样本
data <- data %>% filter(!is.na(height) & !is.na(weight) & !is.na(waist) & !is.na(systolic) & !is.na(diastolic))
# 定义MetS判断函数
data <- data %>%
  rowwise() %>%
  mutate(
    gender = as.numeric(substr(ID, nchar(ID), nchar(ID))),
    abnormal_count = sum(
      !is.na(ifelse(waist_def == "normal", waist, NA)) & ((gender == 1 & waist >= 90) | (gender == 2 & waist >= 80)),
      !is.na(ifelse(newhdl == "normal", newhdl, NA)) & ((gender == 1 & newhdl < 40) | (gender == 2 & newhdl < 50)),
      !is.na(newtg) & newtg >= 150,
      ((!is.na(ifelse(bp_def == "normal", systolic, NA)) & ifelse(bp_def == "normal", systolic, NA) >= 130) |
         (!is.na(ifelse(bp_def == "normal", diastolic, NA)) & ifelse(bp_def == "normal", diastolic, NA) >= 80) |
         !is.na(hyp_med)),
      !is.na(newglu) & newglu >= 100
    ),
    MetS = ifelse(abnormal_count >= 3, "1", "2")
  ) %>%
  ungroup() %>%
  select(-gender, -abnormal_count)
# 定义性别
data <- data %>%
  mutate(gender = as.numeric(substr(data$ID, nchar(data$ID), nchar(data$ID))))
# 给符合stage0的样本进行分类
data <- data %>%
  mutate(stage_0 = ifelse((!is.na(data$BMI) & data$BMI < 23) &
                            (!is.na(data$MetS) & data$MetS == 2) &
                            (!is.na(data$newtg) & data$newtg < 150) &
                            (!is.na(data$hypert) & data$hypert == 2) &
                            (!is.na(data$diabetes_hbs) & data$diabetes_hbs == 2) &
                            (!is.na(data$heart_disease) & data$heart_disease == 2) &
                            (!is.na(data$kidney_disease) & data$kidney_disease == 2),
                          0, NA))
# 给符合stage1的样本进行分类
data <- data %>%
  mutate(stage_1 = ifelse(
    (
      (!is.na(data$BMI) & data$BMI >= 23) |
        (
          ((!is.na(waist) & ((gender == 1 & waist >= 90) | (gender == 2 & waist >= 80)))) |
            ((!is.na(data$newglu) & data$newglu >= 124)) |
            ((!is.na(data$newhba1c) & (data$newhba1c <= 6.4) & (data$newhba1c >= 5.7)))
        )
    ) &
      (!is.na(data$kidney_disease) & data$kidney_disease != 1),
    1, NA))
# 给符合stage2的样本进行分类
data <- data %>%
  mutate(stage_2 = ifelse((!is.na(data$newtg) & data$newtg >= 135) |
                            (!is.na(data$hypert) & data$hypert == 1) |
                            (!is.na(data$MetS) & data$MetS == 1) |
                            (!is.na(data$diabetes_hbs) & data$diabetes_hbs == 1) |
                            (!is.na(data$kidney_disease) & data$kidney_disease == 1),
                          2, NA))
# 给符合stage3的样本进行分类
data <- data %>%
  mutate(stage_3 = ifelse((
    (!is.na(data$stage_1) & data$stage_1 == 1) |
      (!is.na(data$stage_2) & data$stage_2 == 2)
  ) &
    (
      (!is.na(data$stroke) & data$stroke == 1) |
        (!is.na(data$heart_disease) & data$heart_disease == 1) |
        (!is.na(data$kidney_disease) & data$kidney_disease == 1)
    ),
  3, NA))
# stage套
data_step3 <- data %>%
  mutate(stage = pmax(stage_0, stage_1, stage_2, stage_3, na.rm = TRUE))
rm(data) # 清理变量

# ███████╗████████╗███████╗██████╗ ██╗  ██╗
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗██║  ██║
# ███████╗   ██║   █████╗  ██████╔╝███████║
# ╚════██║   ██║   ██╔══╝  ██╔═══╝ ╚════██║
# ███████║   ██║   ███████╗██║          ██║
# ╚══════╝   ╚═╝   ╚══════╝╚═╝          ╚═╝

##### A 家户登记表 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/household_and_community_questionnaire_data/household_roster.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# a001地区类型(1.农村 2.城镇社区)
household_roster <- data %>% select(ID, householdID, communityID, a001)

##### B 基本信息 #####
# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/household_and_community_questionnaire_data/demographic_background.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# ba002_1出生年份
# bd001最高学历
# be001婚姻状态 
demographic_background <- data %>% select(ID, householdID, communityID, ba002_1, bd001, be001)

##### D 健康状况和功能 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/household_and_community_questionnaire_data/health_status_and_functioning.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# da001个人健康状况评估
# da002个人健康状况评估
# da005残疾问题 1-5
# da007慢性病 1-14，da008慢性病自我认识 1 5 11
# da048受访者15岁前的身体状况如何
# da049晚上真正睡着的时间有几小时
# da056s1-12是否进行这些社交活动（12为无）
# da057_1_-_12_社交活动频率
# da059是否吸过烟
# da067过去一年中是否喝酒，频率如何
# db001-db020行为困难
# dc001-dc003日期、星期、季节认知
# dc004自我记忆力评估
# dc006s1-11词语回忆测试
# dc027s1-11延时词语回忆测试
# dc009-dc018抑郁症状
# dc019-dc023计算能力测试，dc024计算中是否使用辅助工具（1是 2否）
# dc025画图测试
# de001-de006自我健康情景选择题
health_status_and_functioning <- data %>% 
  select(ID, householdID, communityID, da001, da002, da005_1_, da005_2_, da005_3_, da005_4_, da005_5_, 
         da007_1_, da008_1_, da007_2_, da007_3_, da007_4_, da007_5_, da008_5_, da007_6_, da007_7_, da007_8_, da007_9_, da007_10_, da007_11_, da008_11_, da007_12_, da007_13_, da007_14_,
         da048, da049, 
         da056s1, da057_1_, da056s2, da057_2_, da056s3, da057_3_, da056s4, da057_4_, da056s5, da057_5_, da056s6, da057_6_, da056s7, da057_7_, da056s8, da057_8_, da056s9, da057_9_, da056s10, da057_10_, da056s11, da057_11_, da056s12,
         da059, da067,
         db001, db002, db003, db004, db005, db006, db007, db008, db009, db010, db011, db012, db013, db014, db015, db016, db017, db018, db019, db020, dc001s1, dc001s2, dc001s3, dc002, dc003, dc004,
         dc006s1, dc006s2, dc006s3, dc006s4, dc006s5, dc006s6, dc006s7, dc006s8, dc006s9, dc006s10, dc006s11, 
         dc027s1, dc027s2, dc027s3, dc027s4, dc027s5, dc027s6, dc027s7, dc027s8, dc027s9, dc027s10, dc027s11,
         dc009, dc010, dc011, dc012, dc013, dc014, dc015, dc016, dc017, dc018, dc019, dc020, dc021, dc022, dc023, dc024, dc025,
         de001, de002, de003, de004, de005, de006)

##### E 医疗保健与保险 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/household_and_community_questionnaire_data/health_care_and_insurance.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# ea001s1-10参加的医疗保险项目（10为没有）
health_care_and_insurance <- data %>% select(ID, householdID, communityID, ea001s1, ea001s2, ea001s3, ea001s4, ea001s5, ea001s6, ea001s7, ea001s8, ea001s9, ea001s10)

##### F 工作、退休和养老金 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/household_and_community_questionnaire_data/work_retirement_and_pension.dta")

# 从数据中提取如下变量
# 个人ID，家庭ID，社区ID
# fn001是否在领取养老金
work_retirement_and_pension <- data %>% select(ID, householdID, communityID, fn001)

##### 血检数据 #####

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/Blood_20140429.dta")

# 从数据中提取如下变量
# 个人ID
# qc1_vb009血小板，newbun尿素氮，newglu血糖，qc1_vb002白细胞，newcho总胆固醇，newtg甘油三酯，newhdl高密度脂蛋白，newldl低密度脂蛋白，newcrpC反应蛋白，newhba1c糖化血红蛋白，newua尿酸
blood <- data %>% select(ID, qc1_vb009, newbun, newglu, qc1_vb002, newcho, newtg, newhdl, newldl, newcrp, newhba1c, newua)

##### 数据整合 #####

merged_data_1 <- merge(household_roster, demographic_background, by="ID")
merged_data_1 <- merged_data_1 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data_1 <- merged_data_1 %>%
  select(-c(householdID.y, communityID.y))

merged_data_2 <- merge(merged_data_1,health_status_and_functioning, by="ID")
merged_data_2 <- merged_data_2 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data_2 <- merged_data_2 %>%
  select(-c(householdID.y, communityID.y))

merged_data_3 <- merge(merged_data_2,health_care_and_insurance, by="ID")
merged_data_3 <- merged_data_3 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data_3 <- merged_data_3 %>%
  select(-c(householdID.y, communityID.y))

merged_data_4 <- merge(merged_data_3,work_retirement_and_pension, by="ID")
merged_data_4 <- merged_data_4 %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
data_step4 <- merged_data_4 %>%
  select(-c(householdID.y, communityID.y))
# 清理变量
rm(merged_data_1, merged_data_2, merged_data_3, merged_data_4, household_roster, demographic_background, health_status_and_functioning, health_care_and_insurance, work_retirement_and_pension, blood, data)

# ███████╗████████╗███████╗██████╗ ███████╗
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗██╔════╝
# ███████╗   ██║   █████╗  ██████╔╝███████╗
# ╚════██║   ██║   ██╔══╝  ██╔═══╝ ╚════██║
# ███████║   ██║   ███████╗██║     ███████║
# ╚══════╝   ╚═╝   ╚══════╝╚═╝     ╚══════╝

##### 整理年龄列 #####
# 用2011减ba002_1的值，将所得到的值覆盖回ba002_1
data <- data_step4 %>%
  mutate(ba002_1 = ifelse(is.na(ba002_1), ba002_1, 2011 - ba002_1))

##### 整理地区列 #####
data <- data %>% # 把a001的所有值-1，不包括空值
  mutate(a001 = ifelse(is.na(a001), a001, a001 - 1))

##### 整理自我健康评估列 #####
data <- data %>% mutate(da002 = ifelse(is.na(da002), da002, da002 + 1)) # 把da002的所有值+1，不包括空值
data <- data %>% mutate(da001 = ifelse(is.na(da001), da002, da001))     # 把da001的空值替换为da002的值
data <- data %>% select(-da002)                                         # 删除da002

##### 整理慢性病列 #####
data <- data %>% # 更新血压，如果血压确认为1，则赋值为1
  mutate(da007_1_ = case_when(
    da008_1_ == 1 ~ 1,
    TRUE ~ da007_1_))
data <- data %>% # 更新肺部慢性病，如果肺部慢性病确认为1，则赋值为1
  mutate(da007_5_ = case_when(
    da008_5_ == 1 ~ 1,
    TRUE ~ da007_5_))
data <- data %>% # 更新情感或精神问题，如果情感或精神问题确认为1，则赋值为1
  mutate(da007_11_ = case_when(
    da008_11_ == 1 ~ 1,
    TRUE ~ da007_11_))
data <- data %>% select(-da008_11_, -da008_5_, -da008_1_) # 删除da008_11_ da008_5_ da008_1_
data <- data %>% # 计算da007_1_至da007_14_中1的个数并赋值给新变量，不包括空值
  mutate(chronic_disease_count = rowSums(select(., da007_1_:da007_14_) == 1, na.rm = TRUE))

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
#data <- data %>% # 判断ADL状态，0-3分为良好，4-6分为中等，7分以上为差
#  mutate(adl_status = case_when(
#    adl_score <= 3 ~ "good",
#    adl_score <= 6 ~ "mild",
#    TRUE ~ "severe"
#  ))

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
data <- data %>% # bd001空值赋值0
  mutate(bd001 = ifelse(is.na(bd001), 0, bd001))
data <- data %>% # 判断认知状态：若bd001=1，大于等于10分为正常；bd001=2~4，大于等于12分为正常；bd001>4，大于等于15分为正常
  mutate(cognitive_status = case_when(
    bd001 == 0 ~ NA,
    bd001 == 1 & cognitive_score >= 10 ~ "0",
    bd001 %in% 2:4 & cognitive_score >= 12 ~ "0",
    bd001 > 4 & cognitive_score >= 15 ~ "0",
    TRUE ~ "1"
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
data_step5 <- data %>% # 判断抑郁症状状态，大于10分代表存在抑郁问题
  mutate(depression_status = case_when(
    is.na(depression_score) ~ NA,
    depression_score > 10 ~ "1",
    TRUE ~ "0"
  ))
rm(data)

# ███████╗████████╗███████╗██████╗  ██████╗ 
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗██╔════╝ 
# ███████╗   ██║   █████╗  ██████╔╝███████╗ 
# ╚════██║   ██║   ██╔══╝  ██╔═══╝ ██╔═══██╗
# ███████║   ██║   ███████╗██║     ╚██████╔╝
# ╚══════╝   ╚═╝   ╚══════╝╚═╝      ╚═════╝ 

# 改名
data_var <- data_step5 %>%
  rename(
    region = a001,
    age = ba002_1,
    education = bd001,
    marital = be001,
    selfratedhealth = da001,
    physicalDisability = da005_1_,
    mentalDisability = da005_2_,
    visionProblem = da005_3_,
    hearingProblem = da005_4_,
    speechImpediment = da005_5_,
    hypertension = da007_1_,
    dyslipidemia = da007_2_,
    diabetesOrHighBloodSugar = da007_3_,
    cancerOrMalignantTumor = da007_4_,
    chronicLungDisease = da007_5_,
    liverDisease = da007_6_,
    heartProblem = da007_7_,
    stroke = da007_8_,
    kidneyDisease = da007_9_,
    stomachOrOtherDisgestiveDisease = da007_10_,
    emotionalOrPsychiatricDisease = da007_11_,
    memoryRelatedDisease = da007_12_,
    arthritisOrRheumatism = da007_13_,
    asthma = da007_14_,
    childhoodHealth = da048,
    avgSleepTime = da049,
    tooluseInCalculation = dc024,
    drawing = dc025,
    bodyAcheOrPain = de001,
    sleepingProblem = de002,
    walkingProblem = de003,
    concentrationOrRememberingProblem = de004,
    breathShortness = de005,
    depression = de006,
    urbanEmployeeMI = ea001s1,
    urbanResidentMI = ea001s2,
    newCooperativeMI = ea001s3,
    urbanAndRuralResidentMI = ea001s4,
    governmentMI = ea001s5,
    medicalAid = ea001s6,
    privateMIbyUnion = ea001s7,
    privateMIbyIndividual = ea001s8,
    otherMI = ea001s9,
    noMI = ea001s10,
    physDis_score = adl_score
  )

# 选择变量
data_stage <- data_step3 %>%
  select(
    ID, householdID, communityID, gender,
    height, height_def, weight, weight_def, waist, waist_def,
    systolic, diastolic, bp_def,
    stage
  )

# 选择变量
data_var <- data_var %>%
  select(
    ID, householdID, communityID, 
    region, age, education, marital, selfratedhealth, 
    physicalDisability, mentalDisability, visionProblem, hearingProblem, speechImpediment, 
    hypertension, 
    dyslipidemia, 
    diabetesOrHighBloodSugar, 
    cancerOrMalignantTumor, 
    chronicLungDisease, 
    liverDisease, 
    heartProblem, 
    stroke, 
    kidneyDisease, 
    stomachOrOtherDisgestiveDisease, 
    emotionalOrPsychiatricDisease, 
    memoryRelatedDisease, 
    arthritisOrRheumatism, 
    asthma, 
    childhoodHealth, avgSleepTime, 
    drawing, 
    bodyAcheOrPain, sleepingProblem, walkingProblem, concentrationOrRememberingProblem, breathShortness, depression, 
    urbanEmployeeMI, urbanResidentMI, newCooperativeMI, urbanAndRuralResidentMI, governmentMI, medicalAid, privateMIbyUnion, privateMIbyIndividual, otherMI, noMI,
    chronic_disease_count,
    social_freq_score,
    physDis_score,
    badl_score,
    badl_status,
    iadl_score,
    iadl_status,
    cognitive_score,
    cognitive_status,
    depression_score,
    depression_status
  )

# 将 "0" 添加到 householdID 的末尾
data_var$householdID <- paste0(data_var$householdID, "0")
data_stage$householdID <- paste0(data_stage$householdID, "0")

# 将 ID 更新为 householdID 加上 ID 的最后两个字符
data_var$ID <- paste0(data_var$householdID, substr(data_var$ID, nchar(data_var$ID)-1, nchar(data_var$ID)))
data_stage$ID <- paste0(data_stage$householdID, substr(data_stage$ID, nchar(data_stage$ID)-1, nchar(data_stage$ID)))

# 将ID统一为字符型
data_var$ID <- as.character(data_var$ID)
data_stage$ID <- as.character(data_stage$ID)

# 合并数据
merged_data <- merge(data_stage, data_var, by="ID")
merged_data <- merged_data %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
data_step6 <- merged_data %>%
  select(-c(householdID.y, communityID.y))
rm(data_stage, data_var, merged_data)

# ███████╗████████╗███████╗██████╗ ███████╗
# ██╔════╝╚══██╔══╝██╔════╝██╔══██╗╚════██║
# ███████╗   ██║   █████╗  ██████╔╝    ██╔╝
# ╚════██║   ██║   ██╔══╝  ██╔═══╝    ██╔╝ 
# ███████║   ██║   ███████╗██║        ██║  
# ╚══════╝   ╚═╝   ╚══════╝╚═╝        ╚═╝ 

# 定义性别，将ID的最后一位数覆盖到gender
data <- data_step6 %>%
  mutate(gender = substr(ID, nchar(ID), nchar(ID)))

# 剔除异常值
data <- data %>%
  filter(!if_any(everything(), ~ grepl("outlier", .)))

# 删除异常值定性列
data <- data %>%
  select(-height_def, -weight_def, -waist_def, -bp_def)

# 剔除<身高, 体重, 腰围, 低压, 高压, 阶段>空缺值
data <- data %>%
  filter(!is.na(height), !is.na(weight), !is.na(waist), !is.na(systolic), !is.na(diastolic), !is.na(stage))

# 整理保险数据
data <- data %>%
  mutate(mi = case_when(
    !is.na(urbanEmployeeMI) | !is.na(urbanResidentMI) | !is.na(newCooperativeMI) | !is.na(urbanAndRuralResidentMI) | !is.na(governmentMI) | !is.na(medicalAid) | !is.na(privateMIbyUnion) | !is.na(privateMIbyIndividual) | !is.na(otherMI) ~ 1,
    !is.na(noMI) ~ 0,
    TRUE ~ 0  # 如果没有数值，赋值为 NA
  ))

# 移除其它保险列
data <- data %>%
  select(-urbanEmployeeMI, -urbanResidentMI, -newCooperativeMI, -urbanAndRuralResidentMI, -governmentMI, -medicalAid, -privateMIbyUnion, -privateMIbyIndividual, -otherMI, -noMI)

# 移除bodyAcheOrPain至depression列
data <- data %>%
  select(-bodyAcheOrPain, -sleepingProblem, -walkingProblem, -concentrationOrRememberingProblem, -breathShortness, -depression)

# 移除drawing列，iadl_status, badl_status, cognitive_status, depression_score
data <- data %>%
  select(-drawing, -iadl_status, -badl_status, -cognitive_status, -depression_score)

# 转换所有 haven_labelled 类型的列
data <- data.frame(lapply(data, function(x) {
  if (inherits(x, "haven_labelled")) {
    as.character(x) # 转换为字符型
  } else {
    x
  }
}))
data <- data %>%
  mutate(across(c(physicalDisability, mentalDisability, visionProblem, hearingProblem, 
                  speechImpediment, hypertension, dyslipidemia, diabetesOrHighBloodSugar, 
                  cancerOrMalignantTumor, chronicLungDisease, liverDisease, heartProblem, 
                  stroke, kidneyDisease, stomachOrOtherDisgestiveDisease, 
                  emotionalOrPsychiatricDisease, memoryRelatedDisease, arthritisOrRheumatism, 
                  asthma, childhoodHealth, depression_status), as.factor))

# 使用 mice 进行插补，默认 method 会根据变量类型自动选择
temp_data <- mice(data[, c("region", "age", "education", "selfratedhealth", "physicalDisability", "mentalDisability", "visionProblem", "hearingProblem", "speechImpediment", "hypertension",
                           "dyslipidemia", "diabetesOrHighBloodSugar", "cancerOrMalignantTumor", "chronicLungDisease", "liverDisease", "heartProblem", "stroke", "kidneyDisease",
                           "stomachOrOtherDisgestiveDisease", "emotionalOrPsychiatricDisease", "memoryRelatedDisease", "arthritisOrRheumatism", "asthma", 
                           "childhoodHealth", "avgSleepTime", "badl_score", "iadl_score", "cognitive_score", "depression_status")], m = 1, seed = 123)
temp_data_ <- complete(temp_data)

# 提取填补完成的数据集
data$region <- temp_data_$region
data$age <- temp_data_$age
data$selfratedhealth <- temp_data_$selfratedhealth
data$physicalDisability <- temp_data_$physicalDisability
data$mentalDisability <- temp_data_$mentalDisability
data$visionProblem <- temp_data_$visionProblem
data$hearingProblem <- temp_data_$hearingProblem
data$speechImpediment <- temp_data_$speechImpediment
data$hypertension <- temp_data_$hypertension
data$dyslipidemia <- temp_data_$dyslipidemia
data$diabetesOrHighBloodSugar <- temp_data_$diabetesOrHighBloodSugar
data$cancerOrMalignantTumor <- temp_data_$cancerOrMalignantTumor
data$chronicLungDisease <- temp_data_$chronicLungDisease
data$liverDisease <- temp_data_$liverDisease
data$heartProblem <- temp_data_$heartProblem
data$stroke <- temp_data_$stroke
data$kidneyDisease <- temp_data_$kidneyDisease
data$stomachOrOtherDisgestiveDisease <- temp_data_$stomachOrOtherDisgestiveDisease
data$emotionalOrPsychiatricDisease <- temp_data_$emotionalOrPsychiatricDisease
data$memoryRelatedDisease <- temp_data_$memoryRelatedDisease
data$arthritisOrRheumatism <- temp_data_$arthritisOrRheumatism
data$asthma <- temp_data_$asthma
data$childhoodHealth <- temp_data_$childhoodHealth
data$avgSleepTime <- temp_data_$avgSleepTime
data$badl_score <- temp_data_$badl_score
data$iadl_score <- temp_data_$iadl_score
data$cognitive_score <- temp_data_$cognitive_score
data$depression_status <- temp_data_$depression_status
# 剔除mice插补后的空缺值
data <- data %>%
  filter(!is.na(region), !is.na(age), !is.na(selfratedhealth), !is.na(physicalDisability), !is.na(mentalDisability), !is.na(visionProblem), !is.na(hearingProblem), !is.na(speechImpediment),
         !is.na(hypertension), !is.na(dyslipidemia), !is.na(diabetesOrHighBloodSugar), !is.na(cancerOrMalignantTumor), !is.na(chronicLungDisease), !is.na(liverDisease),
         !is.na(heartProblem), !is.na(stroke), !is.na(kidneyDisease), !is.na(stomachOrOtherDisgestiveDisease), !is.na(emotionalOrPsychiatricDisease),
         !is.na(memoryRelatedDisease), !is.na(arthritisOrRheumatism), !is.na(asthma), !is.na(childhoodHealth), !is.na(avgSleepTime))

# 清理环境变量
rm(temp_data, temp_data_)

# 精简education数据，1=1，2 3 4 5=2，6 7 8 9 10 11 =3
data <- data %>%
  mutate(education = case_when(
    education %in% c(0, 1) ~ 1,
    education %in% c(2, 3, 4, 5) ~ 2,
    education %in% c(6, 7, 8, 9, 10, 11) ~ 3,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))

# 精简marital数据，1 2=1，3 4 5 6=2
data_step7 <- data %>%
  mutate(marital = case_when(
    marital %in% c(1, 2) ~ 1,
    marital %in% c(3, 4, 5, 6) ~ 2,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))
rm(data)

# 导出数据
write.csv(data_step7, file = "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/AIO_Output.csv", row.names = FALSE, na = "")
