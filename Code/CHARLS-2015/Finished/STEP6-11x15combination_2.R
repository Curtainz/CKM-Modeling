library(dplyr)

# 读取数据
data_s15 <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/CKM_stage_tracking.csv")
data <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/7-final_cleaned_data.csv")

# 整理data列
data <- data %>% select(-X.1, -X, -householdID, -communityID, -stage) # 移除rowname,stage和额外ID列

# 根据ID对数据取交集
data <- data %>% merge(data_s15, by = "ID") # 取交集

# 写出数据
write.csv(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/6-11x15_combination.csv", row.names = FALSE)
