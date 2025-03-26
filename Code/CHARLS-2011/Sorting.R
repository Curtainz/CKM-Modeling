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

# 转换数据类型
data <- data %>%
  mutate(across(c(diabetes_hbs, kidney_disease, heart_disease, stroke), as.numeric))

# 新建慢性病判断列以替代原有列
data <- data %>%
  mutate(
    new_diabetes_hbs = ifelse(diabetes_hbs == 1, 1, ifelse(diabetes_hbs == 2, 2, NA)),
    new_kidney_disease = ifelse(kidney_disease == 1, 1, ifelse(kidney_disease == 2, 2, NA)),
    new_heart_disease = ifelse(heart_disease == 1, 1, ifelse(heart_disease == 2, 2, NA)),
    new_stroke = ifelse(stroke == 1, 1, ifelse(stroke == 2, 2, NA))
  )

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
  
# 定义CKM分期函数
ckm_stage <- function(row) {
  
  # 变量提取，仅使用正常数据
  bmi <- ifelse(row["BMI_def"] == "normal", row["BMI"], NA)                    # BMI
  waist <- ifelse(row["waist_def"] == "normal", row["waist"], NA)              # 腰围
  sbp <- ifelse(row["bp_def"] == "normal", row["systolic"], NA)                # 收缩压
  dbp <- ifelse(row["bp_def"] == "normal", row["diastolic"], NA)               # 舒张压
  med <- ifelse(!is.na(row["hyp_med"]), row["hyp_med"], NA)                    # 是否使用降压药（1=有，NA=无OR缺失）
  hba1c <- ifelse(!is.na(row["newhba1c"]), row["newhba1c"], NA)                # 糖化血红蛋白
  fbg <- ifelse(!is.na(row["newglu"]), row["newglu"], NA)                      # 空腹血糖
  hdl <- ifelse(!is.na(row["newhdl"]), row["newhdl"], NA)                      # 高密度脂蛋白 
  tg <- ifelse(!is.na(row["newtg"]), row["newtg"], NA)                         # 甘油三酯
  mets <- ifelse(!is.na(row["MetS"]), row["MetS"], NA)                         # MetS状态（1=有，2=无）
  ckd <- row["new_kidney_disease"]                                             # CKD状态（1=有，2=无）
  gender <- as.numeric(substr(row["ID"], nchar(row["ID"]), nchar(row["ID"])))  # 通过 ID 最后一位判断性别
  
  # **Stage 0: 无 CKM 风险**
  if ((!is.na(bmi) & bmi < 23) &                               # BMI小于23
      (!is.na(sbp) & sbp < 130) & (!is.na(dbp) & dbp < 80) &   # 收缩压小于130，舒张压小于80
      (!is.na(fbg) & fbg < 100) &                              # 空腹血糖小于100
      (!is.na(tg) & tg < 135) &                                # 甘油三酯小于135
      (!is.na(mets) & mets == 2) &                             # 无 MetS
      (!is.na(ckd) & ckd > 1) #&                                 # 无 CKD
#      (!is.na(hd) & hd == 2) &                                 # 无 CVD
#      (!is.na(stk) & stk == 2) &                               # 无卒中
#      (!is.na(dhbs) & dhbs == 2)                               # 无 糖尿病/高血糖
    ){
    return(0)
  }
  
  # **Stage 1: 过度或失调性肥胖**
#  if (!is.na(bmi) & bmi >= 23 &                                 # BMI大于等于23
#      (!is.na(sbp) & sbp < 130) & (!is.na(dbp) & dbp < 80) &    # 收缩压小于130，舒张压小于80
#      !is.na(tg) & tg < 135 &                                   # 甘油三酯小于135
#      !is.na(kidney_disease) & kidney_disease == 2 &            # 无 CKD
#      ((!is.na(waist) & waist >= ifelse(gender == 1, 90, 80)) | # 腰围男性大于等于90，女性大于等于80
#       (!is.na(fbg) & fbg >= 112) |                             # 空腹血糖大于等于112
#       (!is.na(hba1c) & hba1c >= 5.7 & hba1c <= 6.4))           # 糖化血红蛋白位于5.7-6.4之间
#    ){        
#    return(1)
#  }
  
  # **Stage 2: 代谢风险因素和CKD**
#  if ((!is.na(sbp) & sbp >= 130) |                      # 收缩压大于等于130
#      (!is.na(dbp) & dbp >= 80) |                       # 舒张压大于等于80
#      (!is.na(fbg) & fbg >= 100) |                      # 空腹血糖大于等于100
#      (!is.na(tg) & tg >= 135) |                        # 甘油三酯大于等于135
#      (!is.na(mets) & mets == 1) #|                      # 有 MetS
#      (!is.na(diabetes_hbs) & diabetes_hbs == 1) |      # 有 糖尿病/高血糖
#      (!is.na(kidney_disease) & kidney_disease == 1)    # 有 CKD
#    ){ 
#    return(2)
#  }
  
  return(NA)  # 未分类
}

# 应用 CKM 分期
data$CKM_stage <- apply(data, 1, ckm_stage)

################################################################################
################################################################################
################################################################################

# **Stage 3: 亚临床/临床 CVD in CKM**
#if ((ckm_stage(row) == 1 | ckm_stage(row) == 2) & 
#    (heart_disease == 1 | stroke == 1)) {
#  return(3)
#}

