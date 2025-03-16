# 加载程序包
library(haven)
library(dplyr)

##### 体检数据处理 #####

# 读取体检数据
biodata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/Biomarker.dta")

# 从biodata中提取<个人ID, 家庭ID, 社区ID, 身高, 体重是否≥150KG, 体重, 腰围>
extracted_biodata <- biodata %>% select(ID, householdID, communityID, qi002, pl001, ql002, qm002)

# 查找统计缺失值
num_height_mis <- sum(is.na(extracted_biodata$qi002))
num_WeightJudge_mis <- sum(is.na(extracted_biodata$pl001))
num_weight_mis <- sum(is.na(extracted_biodata$ql002))
num_waist_mis <- sum(is.na(extracted_biodata$qm002))

#清理变量
rm(biodata)

##### 健康状况数据处理 #####

# 读取健康状况数据
healthdata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/health_status_and_functioning.dta")

# 从healthdata中提取<个人ID, 家庭ID, 社区ID, 高血压(1=yes, 2=no, 后同), 高血压再确认, 糖尿病, 心脏病, 肾病>
extracted_healthdata <- healthdata %>% select(ID, householdID, communityID, zda007_1_, zda008_1_, zda007_3_, zda007_7_, zda007_9_)

# 查找统计缺失值
num_hyperten_mis <- sum(is.na(extracted_healthdata$zda007_1_))
num_hypertenconfirm_mis <- sum(is.na(extracted_healthdata$zda008_1_))
num_sugar_mis <- sum(is.na(extracted_healthdata$zda007_3_))
num_heartatt_mis <- sum(is.na(extracted_healthdata$zda007_7_))
num_kidneydis_mis <- sum(is.na(extracted_healthdata$zda007_9_))

#清理变量
rm(healthdata)

##### 血检数据处理 #####

# 读取血检数据
blooddata <- read_dta("D:/GitHub/CKM-Modeling/Datasets/CHARLS-2015/RawData/CHARLS2015r/Blood.dta")

# 从blooddata中提取<个人ID, HbA1c, 高密度脂蛋白胆固醇, 甘油三酯, 葡萄糖>
extracted_blooddata <- blooddata %>% select(ID, bl_hbalc, bl_hdl, bl_tg, bl_glu)

# 查找统计缺失值
num_hba1c_mis <- sum(is.na(extracted_blooddata$bl_hbalc))
num_hdl_mis <- sum(is.na(extracted_blooddata$bl_hdl))
num_tg_mis <- sum(is.na(extracted_blooddata$bl_tg))
num_glu_mis <- sum(is.na(extracted_blooddata$bl_glu))

#清理变量
rm(blooddata)

##### 数据整合 #####

# 选择合并列
merge_cols <- c("ID", "householdID", "communityID")

# 合并体检数据和健康数据
merged_data_1 <- full_join(extracted_biodata, extracted_healthdata, by = merge_cols)

# 合并血检数据
merged_data_2 <- full_join(merged_data_1, extracted_blooddata, by = "ID")

# 剔除<身高, 腰围, 血检四项数据>缺失值
cleaned_data_1 <- merged_data_2 %>% filter(!is.na(qi002) & !is.na(qm002) & !is.na(bl_glu) & !is.na(bl_hbalc) & !is.na(bl_hdl) & !is.na(bl_tg))

# 导出数据
write_dta(cleaned_data_1, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/2025.3.16/CleanedData.dta")
write.csv(cleaned_data_1, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/2025.3.16/CleanedData.csv")
