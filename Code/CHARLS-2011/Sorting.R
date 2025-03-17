# 加载程序包
library(haven)
library(dplyr)

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.16/cleaned_data.dta")

# 重命名列
data <- data %>% rename(height = qi002)
data <- data %>% rename(weight = ql002)
data <- data %>% rename(waist = qm002)
data <- data %>% rename(hypert = da007_1_)

# 将身高小于10的每个数据乘以100，并覆盖原数据
data <- data %>% mutate(height = ifelse(height < 10, height * 100, height))

# 更新体重值，如果体重判断为5，则赋值为150
data <- data %>%
  mutate(weight = case_when(
    pl001 == 5 ~ 150,
    TRUE ~ weight
  ))

# 剔除体重判断列
data <- data %>% select(-pl001)

# 剔除体重空值行
data <- data %>% filter(!is.na(weight))

# 剔除身高体重异常值
data <- data %>% filter(height >= 80 & height <= 200 & weight >= 10)

# 计算 BMI 值，BMI = 体重(kg) / (身高(m))^2
data$BMI <- data$weight / (data$height / 100)^2

# 更新血压，如果血压确认为1，则赋值为1
data <- data %>%
  mutate(hypert = case_when(
    da008_1_ == 1 ~ 1,
    TRUE ~ hypert
  ))

# 剔除血压确认列
data <- data %>% select(-da008_1_)

# 缺失值填充
data <- data %>%
  mutate(
    hypert = ifelse(is.na(hypert), 2, hpyert),
    da007_3_ = ifelse(is.na(da007_3_), 2, da007_3_),
    da007_7_ = ifelse(is.na(da007_7_), 2, da007_7_),
    da007_9_ = ifelse(is.na(da007_9_), 2, da007_9_)
  )

# 将ID列转换为字符类型，以便提取最后一位
data$ID <- as.character(data$ID)

# 提取ID列的最后一位数字
last_digit <- substr(data$ID, nchar(data$ID), nchar(data$ID))

# 将最后一位数字转换为数字类型
last_digit <- as.numeric(last_digit)

# 根据最后一位数字拆分数据
data_male <- data[last_digit == 1, ]
data_female <- data[last_digit == 2, ]

# 保存数据
write_dta(data_male, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.17/data_male.dta")
write_dta(data_female, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.17/data_female.dta")
