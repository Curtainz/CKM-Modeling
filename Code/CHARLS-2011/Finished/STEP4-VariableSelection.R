# 加载程序包
library(haven)
library(dplyr)

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
# ba004年龄
# bd001最高学历
# be001婚姻状态 
demographic_background <- data %>% select(ID, householdID, communityID, ba004, bd001, be001)

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
# da059是否吸过烟
# da067过去一年中是否喝酒，频率如何
# db010-db020行为困难
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
         da048, da049, da056s1, da056s2, da056s3, da056s4, da056s5, da056s6, da056s7, da056s8, da056s9, da056s10, da056s11, da056s12, da059, da067,
         db010, db011, db012, db013, db014, db015, db016, db017, db018, db019, db020, dc001s1, dc001s2, dc001s3, dc002, dc003, dc004,
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

# 选择合并列
merge_cols <- c("ID", "householdID", "communityID")

# 合并数据
merged_data_ <- full_join(household_roster, demographic_background, by = merge_cols) %>%
  full_join(health_status_and_functioning, by = merge_cols) %>%
  full_join(health_care_and_insurance, by = merge_cols) %>%
  full_join(work_retirement_and_pension, by = merge_cols)

# 选择合并列
merge_cols <- "ID"

# 合并数据
merged_data <- full_join(merged_data_, blood, by = merge_cols)

# 导出数据
write_dta(merged_data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/4-combined_variable_data.dta")
