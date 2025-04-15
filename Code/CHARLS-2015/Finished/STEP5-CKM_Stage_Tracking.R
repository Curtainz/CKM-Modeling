library(haven)
library(dplyr)

# 读取数据
data_11 <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/7-final_cleaned_data.csv")
data_15 <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/3-stage_data_new.dta")

# 选择ID和stage并重命名
data_11 <- data_11 %>% select(ID, stage)
data_11 <- data_11 %>% rename(stage_11 = stage)
data_15 <- data_15 %>% select(ID, stage)
data_15 <- data_15 %>% rename(stage_15 = stage)

# 根据ID合并数据
data_merged <- merge(data_11, data_15, by = "ID")

# 去除含有空值的行
data_merged <- na.omit(data_merged)

table(data_merged$stage_11)
table(data_merged$stage_15)

# 保存数据
write.csv(data_merged, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/CKM_stage_tracking.csv", row.names = FALSE)
