library(haven)
library(dplyr)

# 读取数据
data <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/2-cleaned_data.dta")

# 定义并排序BMI异常值
data <- data %>%
  mutate(
    BMI_def = ifelse(height_def == "normal" & weight_def == "normal", "normal", "outlier")
  )
data <- data %>%
  select(ID, householdID, communityID, height, height_def, weight, weight_def, BMI, BMI_def, everything())


# 去除4个血检数据都没有的样本
data <- data %>% filter(!is.na(bl_hbalc) & !is.na(bl_hdl) & !is.na(bl_tg) & !is.na(bl_glu))

# 去除身高体重腰围血压都缺失的样本
data <- data %>% filter(!is.na(height) & !is.na(weight) & !is.na(waist) & !is.na(systolic) & !is.na(diastolic))

# 去除异常值
#data <- data %>%
#  filter(!if_any(everything(), ~ grepl("outlier", .)))

# 读取慢性病数据
data_cd <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/STEP9-CD_combined_data.dta")

# 删除原本的慢性病数据
data <- data %>% select(-hypert, -diabetes_hbs, -heart_disease, -kidney_disease, -stroke)

# 取交集
data <- merge(data, data_cd, by = "ID")

# 定义MetS判断函数
data <- data %>%
  rowwise() %>%
  mutate(
    gender = as.numeric(substr(ID, nchar(ID), nchar(ID))),
    abnormal_count = sum(
      !is.na(ifelse(waist_def == "normal", waist, NA)) & ((gender == 1 & waist >= 90) | (gender == 2 & waist >= 80)),
      !is.na(ifelse(bl_hdl == "normal", bl_hdl, NA)) & ((gender == 1 & bl_hdl < 40) | (gender == 2 & bl_hdl < 50)),
      !is.na(bl_tg) & bl_tg >= 150,
      ((!is.na(ifelse(bp_def == "normal", systolic, NA)) & ifelse(bp_def == "normal", systolic, NA) >= 130) |
         (!is.na(ifelse(bp_def == "normal", diastolic, NA)) & ifelse(bp_def == "normal", diastolic, NA) >= 80) |
         !is.na(hyp_med)),
      !is.na(bl_glu) & bl_glu >= 100
    ),
    MetS = ifelse(abnormal_count >= 3, "1", "2")
  ) %>%
  ungroup() %>%
  select(-gender, -abnormal_count)

# 定义性别
data <- data %>%
  mutate(gender = as.numeric(substr(data$ID, nchar(data$ID), nchar(data$ID))))

# 给符合stage0的样本进行分类
data <- data %>%
  mutate(stage_0 = ifelse((!is.na(data$BMI) & data$BMI < 23) &
                            (!is.na(data$MetS) & data$MetS == 2) &
                            (!is.na(data$bl_tg) & data$bl_tg < 150) &
                            (!is.na(data$hypert) & data$hypert == 2) &
                            (!is.na(data$diabetes_hbs) & data$diabetes_hbs == 2) &
                            (!is.na(data$heart_disease) & data$heart_disease == 2) &
                            (!is.na(data$kidney_disease) & data$kidney_disease == 2),
                          0, NA))

# 给符合stage1的样本进行分类
data <- data %>%
  mutate(stage_1 = ifelse(
                          (
                            (!is.na(data$BMI) & data$BMI >= 23) |
                            (
                              ((!is.na(waist) & ((gender == 1 & waist >= 90) | (gender == 2 & waist >= 80)))) |
                              ((!is.na(data$bl_glu) & data$bl_glu >= 124)) |
                              ((!is.na(data$bl_hbalc) & (data$bl_hbalc <= 6.4) & (data$bl_hbalc >= 5.7)))
                            )
                          ) &
                            (!is.na(data$kidney_disease) & data$kidney_disease != 1),
                          1, NA))

table(data$stage_1)

# 给符合stage2的样本进行分类
data <- data %>%
  mutate(stage_2 = ifelse((!is.na(data$bl_tg) & data$bl_tg >= 135) |
                            (!is.na(data$hypert) & data$hypert == 1) |
                            (!is.na(data$MetS) & data$MetS == 1) |
                            (!is.na(data$diabetes_hbs) & data$diabetes_hbs == 1) |
                            (!is.na(data$kidney_disease) & data$kidney_disease == 1),
                          2, NA))
table(data$stage_2)

# 给符合stage3的样本进行分类
data <- data %>%
  mutate(stage_3 = ifelse((
                            (!is.na(data$stage_1) & data$stage_1 == 1) |
                            (!is.na(data$stage_2) & data$stage_2 == 2)
                          ) &
                            (
                              (!is.na(data$stoke) & data$stoke == 1) |
                              (!is.na(data$heart_disease) & data$heart_disease == 1) |
                              (!is.na(data$kidney_disease) & data$kidney_disease == 1)
                            ),
                          3, NA))
table(data$stage_3)

# stage套
data <- data %>%
  mutate(stage = pmax(stage_0, stage_1, stage_2, stage_3, na.rm = TRUE))
table(data$stage)

write_dta(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/3-stage_data_new.dta")
write.csv(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2015/Completed/3-stage_data_new.csv", row.names = FALSE)

