# 加载程序包
library(haven)
library(dplyr)
library(table1)

# 读取数据
data_var <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/5-cleaned_variable_data.dta")

# 改名
data_var <- data_var %>%
  rename(
    region = a001,
    age = ba002_1,
    education = bd001,
    marital = be001,
    selfratedhealth = da001,
    physicalDisability = da005_1_,
    mentalDisability = da005_2_,
    visionProblem = da005_3_,
    hearingProblem = da005_4_,
    speechImpediment = da005_5_,
    hypertension = da007_1_,
    dyslipidemia = da007_2_,
    diabetesOrHighBloodSugar = da007_3_,
    cancerOrMalignantTumor = da007_4_,
    chronicLungDisease = da007_5_,
    liverDisease = da007_6_,
    heartProblem = da007_7_,
    stroke = da007_8_,
    kidneyDisease = da007_9_,
    stomachOrOtherDisgestiveDisease = da007_10_,
    emotionalOrPsychiatricDisease = da007_11_,
    memoryRelatedDisease = da007_12_,
    arthritisOrRheumatism = da007_13_,
    asthma = da007_14_,
    childhoodHealth = da048,
    avgSleepTime = da049,
    tooluseInCalculation = dc024,
    drawing = dc025,
    bodyAcheOrPain = de001,
    sleepingProblem = de002,
    walkingProblem = de003,
    concentrationOrRememberingProblem = de004,
    breathShortness = de005,
    depression = de006,
    urbanEmployeeMI = ea001s1,
    urbanResidentMI = ea001s2,
    newCooperativeMI = ea001s3,
    urbanAndRuralResidentMI = ea001s4,
    governmentMI = ea001s5,
    medicalAid = ea001s6,
    privateMIbyUnion = ea001s7,
    privateMIbyIndividual = ea001s8,
    otherMI = ea001s9,
    noMI = ea001s10,
    physDis_score = adl_score
  )

# 读取数据
data_stage <- read_dta("D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/3-stage_data.dta")

# 选择变量
data_stage <- data_stage %>%
  select(
    ID, householdID, communityID, gender,
    height, height_def, weight, weight_def, waist, waist_def,
    systolic, diastolic, bp_def,
    stage
    )

# 选择变量
data_var <- data_var %>%
  select(
    ID, householdID, communityID, 
    region, age, education, marital, selfratedhealth, 
    physicalDisability, mentalDisability, visionProblem, hearingProblem, speechImpediment, 
    hypertension, 
    dyslipidemia, 
    diabetesOrHighBloodSugar, 
    cancerOrMalignantTumor, 
    chronicLungDisease, 
    liverDisease, 
    heartProblem, 
    stroke, 
    kidneyDisease, 
    stomachOrOtherDisgestiveDisease, 
    emotionalOrPsychiatricDisease, 
    memoryRelatedDisease, 
    arthritisOrRheumatism, 
    asthma, 
    childhoodHealth, avgSleepTime, 
    drawing, 
    bodyAcheOrPain, sleepingProblem, walkingProblem, concentrationOrRememberingProblem, breathShortness, depression, 
    urbanEmployeeMI, urbanResidentMI, newCooperativeMI, urbanAndRuralResidentMI, governmentMI, medicalAid, privateMIbyUnion, privateMIbyIndividual, otherMI, noMI,
    chronic_disease_count,
    social_freq_score,
    physDis_score,
    badl_score,
    badl_status,
    iadl_score,
    iadl_status,
    cognitive_score,
    cognitive_status,
    depression_score,
    depression_status
    )

# 合并数据
#merge_cols <- c("ID", "householdID", "communityID")
#merged_data <- full_join(data_stage, data_var, by = merge_cols)
merged_data <- merge(data_stage, data_var, by="ID")
merged_data <- merged_data %>%
  rename(
    householdID = householdID.x,
    communityID = communityID.x,
  )
merged_data <- merged_data %>%
  select(-c(householdID.y, communityID.y))

# 保存数据
write.csv(merged_data, "D:/GitHub/CKM-Modeling/ProcessedData/CHARLS-2011/Completed/6-merged_data.csv")

# 分析变量
table1(~ region + age + education + marital + selfratedhealth + physicalDisability + 
         mentalDisability + visionProblem + hearingProblem + speechImpediment + 
         hypertension + dyslipidemia + diabetesOrHighBloodSugar + cancerOrMalignantTumor + 
         chronicLungDisease + liverDisease + heartProblem + stroke + kidneyDisease + 
         stomachOrOtherDisgestiveDisease + emotionalOrPsychiatricDisease + memoryRelatedDisease + 
         arthritisOrRheumatism + asthma + childhoodHealth + avgSleepTime + drawing + 
         bodyAcheOrPain + sleepingProblem + walkingProblem + concentrationOrRememberingProblem + 
         breathShortness + depression + urbanEmployeeMI + urbanResidentMI + newCooperativeMI + 
         urbanAndRuralResidentMI + governmentMI + medicalAid + privateMIbyUnion + 
         privateMIbyIndividual + otherMI + noMI + chronic_disease_count + social_freq_score + 
         physDis_score + badl_score + badl_status + iadl_score + iadl_status + cognitive_score + 
         cognitive_status + depression_score + depression_status | stage, data = merged_data)










