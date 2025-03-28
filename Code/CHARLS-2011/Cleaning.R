# 加载程序包
library(haven)
library(dplyr)

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/combined_data.dta")

# 重命名列
data <- data %>% 
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

# 将身高小于10的每个数据乘以100，并覆盖原数据
#data <- data %>% mutate(height = ifelse(height < 10, height * 100, height))

# 身高异常值标记，规则：小于80或大于200
data <- data %>%
  mutate(
    height_def = case_when(
      is.na(height) ~ "outlier",
      height < 80 ~ "outlier",
      height > 200 ~ "outlier",
      TRUE ~ "normal"
    )
  )

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, everything())

# 更新体重值，如果体重判断为5，则赋值为150
data <- data %>%
  mutate(weight = case_when(
    pl001 == 5 ~ 150,
    TRUE ~ weight
  ))

# 剔除体重判断列
data <- data %>% select(-pl001)

# 剔除体重空值行
#data <- data %>% filter(!is.na(weight))

# 剔除身高体重异常值
#data <- data %>% filter(height >= 80 & height <= 200 & weight >= 10)

# 体重异常值标记，规则：小于20或大于150
data <- data %>%
  mutate(
    weight_def = case_when(
      is.na(weight) ~ "outlier",
      weight < 20 ~ "outlier",
      weight > 150 ~ "outlier",
      TRUE ~ "normal"
    )
  )

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, everything())

# 计算 BMI 值，BMI = 体重(kg) / (身高(m))^2
data$BMI <- data$weight / (data$height / 100)^2

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, everything())

######
#腰围#
######

# 腰围异常值标记，规则：小于40或大于200
data <- data %>%
  mutate(
    waist_def = case_when(
      is.na(waist) ~ "outlier",
      waist < 40 ~ "outlier",
      waist > 200 ~ "outlier",
      TRUE ~ "normal"
    )
  )

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, everything())

######
#血压#
######

# 计算每个人的低压平均值，不包括异常值和缺失值
data <- data %>%
  rowwise() %>%
  mutate(
    systolic = mean(c(systolic, qa007, qa011)[c(systolic, qa007, qa011) <= 900], na.rm = TRUE)
  ) %>%
  ungroup()

# 计算每个人的高压平均值，不包括异常值和缺失值
data <- data %>%
  rowwise() %>%
  mutate(
    diastolic = mean(c(diastolic, qa008, qa012)[c(systolic, qa008, qa012) <= 900], na.rm = TRUE)
  ) %>%
  ungroup()

# 剔除血压
data <- data %>% select(-qa007, -qa008, -qa011, -qa012)

# 血压异常值标记，规则：收缩压小于舒张压、收缩压小于50或大于300、舒张压小于30或大于180
data <- data %>%
  mutate(
    bp_def = case_when(
      is.na(systolic) ~ "outlier",
      is.na(diastolic) ~ "outlier",
      systolic < 50 ~ "outlier",
      systolic > 300 ~ "outlier",
      diastolic < 30 ~ "outlier",
      diastolic > 180 ~ "outlier",
      systolic < diastolic ~ "outlier",
      TRUE ~ "normal"
    )
  )

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, systolic, diastolic, bp_def, everything())

# 更新血压，如果血压确认为1，则赋值为1
data <- data %>%
  mutate(hypert = case_when(
    da008_1_ == 1 ~ 1,
    TRUE ~ hypert
  ))

# 剔除血压确认列
data <- data %>% select(-da008_1_)

# 缺失值填充
#data <- data %>%
#  mutate(
#    hypert = ifelse(is.na(hypert), 2, hypert),
#    diabetes_hbs = ifelse(is.na(diabetes_hbs), 2, diabetes_hbs),
#    heart_disease = ifelse(is.na(heart_disease), 2, heart_disease),
#    stroke = ifelse(is.na(stroke), 2, stroke),
#    kidney_disease = ifelse(is.na(kidney_disease), 2, kidney_disease)
#  )

# 检查用药情况
data <- data %>%
  mutate(
    hyp_med = ifelse(da011s1 == 1 | da011s2 == 1, 1, NA)
  )

# 剔除中西药列
data <- data %>% select(-da011s1, -da011s2)

# 移动列
data <- data %>% select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, waist, waist_def, systolic, diastolic, bp_def, hypert, hyp_med, diabetes_hbs, heart_disease, stroke, kidney_disease, everything())

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
write_dta(data_male, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/cleaned_data_male.dta")
write_dta(data_female, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/cleaned_data_female.dta")
write_dta(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/cleaned_data.dta")
write.csv(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/cleaned_data.csv")