library(haven)
library(dplyr)

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/2025.3.19/cleaned_data.dta")

# 去除4个血检数据都没有的样本
data <- data %>% filter(!is.na(newhba1c) & !is.na(newhdl) & !is.na(newtg) & !is.na(newglu))

# 去除身高体重腰围血压都缺失的样本
data <- data %>% filter(!is.na(height) & !is.na(weight) & !is.na(waist) & !is.na(systolic) & !is.na(diastolic))

# 定义并排序BMI异常值
data <- data %>%
  mutate(
    BMI_def = ifelse(height_def == "normal" & weight_def == "normal", "normal", "outlier")
  )
data <- data %>%
  select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, BMI_def, everything())

# 定义MetS判断函数
check_mets <- function(row) {
  
  # 变量提取，仅使用正常数据
  waist <- ifelse(row["waist_def"] == "normal", row["waist"], NA) # 腰围
  sbp <- ifelse(row["bp_def"] == "normal", row["systolic"], NA)   # 收缩压
  dbp <- ifelse(row["bp_def"] == "normal", row["diastolic"], NA)  # 舒张压
  med <- row["hyp_med"]                                           # 是否使用降压药（1=有，NA=无OR缺失）
  fbg <- row["newglu"]                                            # 空腹血糖
  hdl <- row["newhdl"]                                            # 高密度脂蛋白 
  tg <- row["newtg"]                                              # 甘油三酯
  gender <- as.numeric(substr(row["ID"], nchar(row["ID"]), nchar(row["ID"])))  # 通过 ID 最后一位判断性别
  
  # 计算异常指标数量
  abnormal_count <- sum(
    !is.na(waist) & ((gender == 1 & waist > 90) | (gender == 2 & waist > 80)),
    !is.na(hdl) & ((gender == 1 & hdl < 40) | (gender == 2 & hdl < 50)),
    !is.na(tg) & tg >= 150,
    ((!is.na(sbp) & sbp >= 130) | (!is.na(dbp) & dbp >= 80) | !is.na(med)),
    !is.na(fbg) & fbg >= 100
  )
  if (abnormal_count >= 3) {
    return("1")
  } else {
    return("2")
  }
}

# 应用函数到每一行并创建新的MetS列
data$MetS <- apply(data, 1, check_mets)

# 判断代谢风险
metabolic_risk = ifelse(
  ("" == 1) |
    ( >= 135) |
    (`糖尿病/高血糖情况` == 1) |
    (空腹血糖 >= 100 & 空腹血糖 < 126),
  TRUE, FALSE
),