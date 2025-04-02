# 加载程序包
library(haven)
library(dplyr)

##### 体检数据处理 #####

# 读取体检数据
biodata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/biomarkers.dta")

# 从biodata中提取<个人ID, 家庭ID, 社区ID, 身高, 体重是否≥150KG, 体重, 腰围, 收缩压1-3, 舒张压1-3>
extracted_biodata <- biodata %>% select(ID, householdID, communityID, qi002, pl001, ql002, qm002, qa003, qa007, qa011, qa004, qa008, qa012)

# 统计行数
#num_rows_biodata <- nrow(extracted_biodata)

# 查找统计缺失值
#num_height_mis <- sum(is.na(extracted_biodata$qi002))
#num_WeightJudge_mis <- sum(is.na(extracted_biodata$pl001))
#num_weight_mis <- sum(is.na(extracted_biodata$ql002))
#num_waist_mis <- sum(is.na(extracted_biodata$qm002))

#清理变量
rm(biodata)

##### 健康状况数据处理 #####

# 读取健康状况数据
healthdata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/health_status_and_functioning.dta")

# 从healthdata中提取<个人ID, 家庭ID, 社区ID, 高血压(1=yes, 2=no, 后同), 高血压再确认, 糖尿病, 心脏病, 中风, 肾病, 降压药情况(中), 降压药情况(西)
extracted_healthdata <- healthdata %>% select(ID, householdID, communityID, da007_1_, da008_1_, da007_3_, da007_7_, da007_8_, da007_9_, da011s1, da011s2)

# 统计行数
#num_rows_healthdata <- nrow(extracted_healthdata)

# 查找统计缺失值
#num_hyperten_mis <- sum(is.na(extracted_healthdata$da007_1_))
#num_hypertenconfirm_mis <- sum(is.na(extracted_healthdata$da008_1_))
#num_sugar_mis <- sum(is.na(extracted_healthdata$da007_3_))
#num_heartatt_mis <- sum(is.na(extracted_healthdata$da007_7_))
#num_kidneydis_mis <- sum(is.na(extracted_healthdata$da007_9_))

#清理变量
rm(healthdata)

##### 血检数据处理 #####

# 读取血检数据
blooddata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2011/RawData/Active/Blood_20140429.dta")

# 从blooddata中提取<个人ID, HbA1c, 高密度脂蛋白胆固醇, 甘油三酯, 葡萄糖>
extracted_blooddata <- blooddata %>% select(ID, newhba1c, newhdl, newtg, newglu)

# 统计行数
#num_rows_blooddata <- nrow(extracted_blooddata)

# 查找统计缺失值
#num_hba1c_mis <- sum(is.na(extracted_blooddata$newhba1c))
#num_hdl_mis <- sum(is.na(extracted_blooddata$newhdl))
#num_tg_mis <- sum(is.na(extracted_blooddata$newtg))
#num_glu_mis <- sum(is.na(extracted_blooddata$newglu))

#清理变量
rm(blooddata)

##### 数据整合 #####

# 选择合并列
merge_cols <- c("ID", "householdID", "communityID")

# 合并体检数据和健康数据
merged_data_1 <- full_join(extracted_biodata, extracted_healthdata, by = merge_cols)

# 合并血检数据
merged_data_2 <- full_join(merged_data_1, extracted_blooddata, by = "ID")

# 导出数据
write_dta(merged_data_2, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/combined_data.dta")
