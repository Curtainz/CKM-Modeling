# 加载程序包
library(haven)
library(dplyr)
library(mice)

# 读取数据
data <- read.csv("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/6-merged_data.csv")

# 定义性别，将ID的最后一位数覆盖到gender
data <- data %>%
  mutate(gender = substr(ID, nchar(ID), nchar(ID)))

# 剔除异常值
data <- data %>%
  filter(!if_any(everything(), ~ grepl("outlier", .)))

# 删除异常值定性列
data <- data %>%
  select(-height_def, -weight_def, -waist_def, -bp_def)

# 剔除<身高, 体重, 腰围, 低压, 高压, 阶段>空缺值
data <- data %>%
  filter(!is.na(height), !is.na(weight), !is.na(waist), !is.na(systolic), !is.na(diastolic), !is.na(stage))

# 整理保险数据
data <- data %>%
  mutate(mi = case_when(
    !is.na(urbanEmployeeMI) | !is.na(urbanResidentMI) | !is.na(newCooperativeMI) | !is.na(urbanAndRuralResidentMI) | !is.na(governmentMI) | !is.na(medicalAid) | !is.na(privateMIbyUnion) | !is.na(privateMIbyIndividual) | !is.na(otherMI) ~ 1,
    !is.na(noMI) ~ 0,
    TRUE ~ 0  # 如果没有数值，赋值为 NA
  ))

# 移除其它保险列
data <- data %>%
  select(-urbanEmployeeMI, -urbanResidentMI, -newCooperativeMI, -urbanAndRuralResidentMI, -governmentMI, -medicalAid, -privateMIbyUnion, -privateMIbyIndividual, -otherMI, -noMI)

# 移除bodyAcheOrPain至depression列
data <- data %>%
  select(-bodyAcheOrPain, -sleepingProblem, -walkingProblem, -concentrationOrRememberingProblem, -breathShortness, -depression)

# 移除drawing列，iadl_status, badl_status, cognitive_status, depression_score
data <- data %>%
  select(-drawing, -iadl_status, -badl_status, -cognitive_status, -depression_score)

# 使用 mice 进行插补，默认 method 会根据变量类型自动选择
temp_data <- mice(data[, c("region", "age", "selfratedhealth", "physicalDisability", "mentalDisability", "visionProblem", "hearingProblem", "speechImpediment", "hypertension",
                           "dyslipidemia", "diabetesOrHighBloodSugar", "cancerOrMalignantTumor", "chronicLungDisease", "liverDisease", "heartProblem", "stroke", "kidneyDisease",
                           "stomachOrOtherDisgestiveDisease", "emotionalOrPsychiatricDisease", "memoryRelatedDisease", "arthritisOrRheumatism", "asthma", 
                           "childhoodHealth", "avgSleepTime", "badl_score", "iadl_score", "cognitive_score", "depression_status")], m = 1, seed = 123)
temp_data_ <- complete(temp_data)

# 提取填补完成的数据集
data$region <- temp_data_$region
data$age <- temp_data_$age
data$selfratedhealth <- temp_data_$selfratedhealth
data$physicalDisability <- temp_data_$physicalDisability
data$mentalDisability <- temp_data_$mentalDisability
data$visionProblem <- temp_data_$visionProblem
data$hearingProblem <- temp_data_$hearingProblem
data$speechImpediment <- temp_data_$speechImpediment
data$hypertension <- temp_data_$hypertension
data$dyslipidemia <- temp_data_$dyslipidemia
data$diabetesOrHighBloodSugar <- temp_data_$diabetesOrHighBloodSugar
data$cancerOrMalignantTumor <- temp_data_$cancerOrMalignantTumor
data$chronicLungDisease <- temp_data_$chronicLungDisease
data$liverDisease <- temp_data_$liverDisease
data$heartProblem <- temp_data_$heartProblem
data$stroke <- temp_data_$stroke
data$kidneyDisease <- temp_data_$kidneyDisease
data$stomachOrOtherDisgestiveDisease <- temp_data_$stomachOrOtherDisgestiveDisease
data$emotionalOrPsychiatricDisease <- temp_data_$emotionalOrPsychiatricDisease
data$memoryRelatedDisease <- temp_data_$memoryRelatedDisease
data$arthritisOrRheumatism <- temp_data_$arthritisOrRheumatism
data$asthma <- temp_data_$asthma
data$childhoodHealth <- temp_data_$childhoodHealth
data$avgSleepTime <- temp_data_$avgSleepTime
data$badl_score <- temp_data_$badl_score
data$iadl_score <- temp_data_$iadl_score
data$cognitive_score <- temp_data_$cognitive_score
data$depression_status <- temp_data_$depression_status


# 剔除mice插补后的空缺值
data <- data %>%
  filter(!is.na(region), !is.na(age), !is.na(selfratedhealth), !is.na(physicalDisability), !is.na(mentalDisability), !is.na(visionProblem), !is.na(hearingProblem), !is.na(speechImpediment),
         !is.na(hypertension), !is.na(dyslipidemia), !is.na(diabetesOrHighBloodSugar), !is.na(cancerOrMalignantTumor), !is.na(chronicLungDisease), !is.na(liverDisease),
         !is.na(heartProblem), !is.na(stroke), !is.na(kidneyDisease), !is.na(stomachOrOtherDisgestiveDisease), !is.na(emotionalOrPsychiatricDisease),
         !is.na(memoryRelatedDisease), !is.na(arthritisOrRheumatism), !is.na(asthma), !is.na(childhoodHealth), !is.na(avgSleepTime))

# 清理环境变量
rm(temp_data, temp_data_)

##############################################################
# 将 "0" 添加到 householdID 的末尾
data$householdID <- paste0(data$householdID, "0")

# 将 ID 更新为 householdID 加上 ID 的最后两个字符
data$ID <- paste0(data$householdID, substr(data$ID, nchar(data$ID)-1, nchar(data$ID)))
##############################################################

# 精简education数据，1=1，2 3 4 5=2，6 7 8 9 10 11 =3
data <- data %>%
  mutate(education = case_when(
    education == 1 ~ 1,
    education %in% c(2, 3, 4, 5) ~ 2,
    education %in% c(6, 7, 8, 9, 10, 11) ~ 3,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))

# 精简marital数据，1 2=1，3 4 5 6=2
data <- data %>%
  mutate(marital = case_when(
    marital %in% c(1, 2) ~ 1,
    marital %in% c(3, 4, 5, 6) ~ 2,
    TRUE ~ NA_real_ # 如果没有数值，赋值为 NA
  ))

# 导出数据
write.csv(data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/7-final_cleaned_data.csv")

