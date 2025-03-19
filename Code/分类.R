##stage1.1
data_male$stage1.1 <- ifelse(data_male$BMI >= 23, 1, 0)
##stage1.2
data_male$stage1.2 <- ifelse(data_male$waist >= 90, 1, 0)
##stage1.3
data_male$stage1.3 <- ifelse(data_male$newglu >= 100 & data_male$newglu <= 124, 1, 0)
##stage1.4
data_male$stage1.4 <- ifelse(data_male$newhba1c >= 5.7 & data_male$newhba1c <= 6.4, 1, 0)
##stage2.1
data_male$stage2.1 <- ifelse(data_male$newtg >= 135, 1, 0)
##stage2.2
data_male$stage2.2 <- ifelse(data_male$hypert == 1, 1, 
                                ifelse(data_male$hypert == 2, 0, NA))
##stage2.3.1
data_male$stage2.3.1 <- ifelse(data_male$waist >= 90, 1, 0)
##stage2.3.2
data_male$stage2.3.2 <- ifelse(data_male$newhdl < 40, 1, 0)
##stage2.3.3
data_male$stage2.3.2 <- ifelse(data_male$newtg >= 150, 1, 0)
##stage2.3.4.1
data_male$stage2.3.4.1 <- ifelse(data_male$高压 >= 130, 1, 0)
##stage2.3.4.2
data_male$stage2.3.4.2 <- ifelse(data_male$低压 >= 80, 1, 0)
##stage2.3.5
data_male$stage2.3.5 <- ifelse(data_male$newglu >= 100, 1, 0)
##stage2.4
data_male$stage2.4 <- ifelse(data_male$da007_3_ == 1, 1, 
                             ifelse(data_male$da007_3_ == 2, 0, NA))
##stage4.1
data_male$stage2.4 <- ifelse(data_male$da007_7_ == 1, 1, 
                             ifelse(data_male$da007_7_ == 2, 0, NA))