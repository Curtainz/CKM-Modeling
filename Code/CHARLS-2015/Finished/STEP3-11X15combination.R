library(dplyr)
library(haven)
# 加载数据
data_11 <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/7-final_cleaned_data.csv")
data_15 <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/3-stage_data.dta")

# 选择变量
data_11 <- data_11 %>% select(ID)
data_15 <- data_15 %>% select(ID)

# 取交集
data_combine <- merge(data_11, data_15, by = "ID")

# 读取11年健康状况数据
healthdata_11 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/health_status_and_functioning.dta")

# 选择变量
healthdata_11 <- healthdata_11 %>% select(ID, householdID, da007_1_, da008_1_, da007_3_, da007_7_, da007_8_, da007_9_)

# 将 "0" 添加到 householdID 的末尾
healthdata_11$householdID <- paste0(healthdata_11$householdID, "0")

# 将 ID 更新为 householdID 加上 ID 的最后两个字符
healthdata_11$ID <- paste0(healthdata_11$householdID, substr(healthdata_11$ID, nchar(healthdata_11$ID)-1, nchar(healthdata_11$ID)))

# 更新血压，如果血压确认为1，则赋值为1
healthdata_11 <- healthdata_11 %>% mutate(da007_1_ = case_when(da008_1_ == 1 ~ 1, TRUE ~ da007_1_))
healthdata_11 <- healthdata_11 %>% select(-da008_1_)

# 重命名
healthdata_11 <- healthdata_11 %>% rename(
  ID = ID,
  hypertension_11 = da007_1_,
  diabetesOrHB_11 = da007_3_,
  heartProblem_11 = da007_7_,
  stoke_11 = da007_8_,
  kidneyDisease_11 = da007_9_)

# 移除householdID
healthdata_11 <- healthdata_11 %>% select(-householdID)

# 读取13年健康状况数据
healthdata_13 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2013/RawData/health_status_and_functioning.dta")

# 选择变量
healthdata_13 <- healthdata_13 %>% select(ID, da007_w2_1_1_, da007_w2_1_3_, da007_w2_1_7_, da007_w2_1_8_, da007_w2_1_9_)

# 重命名
healthdata_13 <- healthdata_13 %>% rename(
  ID = ID,
  verify_hypertension_13 = da007_w2_1_1_,
  verify_diabetesOrHB_13 = da007_w2_1_3_,
  verify_heartProblem_13 = da007_w2_1_7_,
  verify_stoke_13 = da007_w2_1_8_,
  verify_kidneyDisease_13 = da007_w2_1_9_)

# 读取15年健康状况数据
healthdata_15 <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/health_status_and_functioning.dta")

# 选择变量
healthdata_15 <- healthdata_15 %>% select(ID, da007_w2_1_1_, da007_w2_1_3_, da007_w2_1_7_, da007_w2_1_8_, da007_w2_1_9_)

# 重命名
healthdata_15 <- healthdata_15 %>% rename(
  ID = ID,
  verify_hypertension_15 = da007_w2_1_1_,
  verify_diabetesOrHB_15 = da007_w2_1_3_,
  verify_heartProblem_15 = da007_w2_1_7_,
  verify_stoke_15 = da007_w2_1_8_,
  verify_kidneyDisease_15 = da007_w2_1_9_)

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
health_conditions <- c("hypertension", "diabetesOrHB", "heartProblem", "stoke", "kidneyDisease")

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
data <- combine_3rd %>% select(ID, hypertension_11, diabetesOrHB_11, heartProblem_11, stoke_11, kidneyDisease_11)

# 重命名
data <- data %>% rename(
  ID = ID,
  hypert = hypertension_11,
  diabetes_hbs = diabetesOrHB_11,
  heart_disease = heartProblem_11,
  stoke = stoke_11,
  kidney_disease = kidneyDisease_11
)

# 将数据保存为dta
write_dta(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/STEP9-CD_combined_data.dta")
