library(mice)
library(dplyr)

heart1 = read.csv("healthcare-dataset-stroke-data.csv")
unique(heart1$Residence_type)
unique(heart1$ever_married)

heart = heart1[-1]
heart$ever_married = as.factor(ifelse(heart$ever_married == "Yes", 1, 0))
heart = heart[-6]
heart$Residence_type = as.factor(ifelse(heart$Residence_type == "Urban", 1, 0))
unique(heart$Residence_type)
unique(heart$ever_married)
unique(heart$smoking_status)
unique(heart$gender)
sum(heart$gender == "Other")
which(heart$gender == "Other")
heart <- heart[-c(3117),]
summary(heart1$age)
min(heart1$age)
max(heart1$age)
summary(heart1$avg_glucose_level)
summary(heart1$bmi)
unique(heart1$bmi)
as.numeric(heart1$bmi)
heart$gender = as.factor(ifelse(heart$gender == "Female", 1, 0))
heart$hypertension = as.factor(heart$hypertension)
heart$heart_disease = as.factor(heart$heart_disease)
heart$heart_disease = as.factor(heart$heart_disease)
heart$stroke = as.factor(heart$stroke)

boxplot(heart$age)

### average glucose level ####
boxplot(heart$avg_glucose_level)
sum(heart$avg_glucose_level > 200)
sum(heart$avg_glucose_level < 200)
hist(heart$avg_glucose_level)
mean(heart$avg_glucose_level)
median(heart$avg_glucose_level)

## removing the outliers ##
set.seed(1)
quartiles <- quantile(heart$avg_glucose_level, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(heart$avg_glucose_level)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR

hearts <- heart
hearts <- hearts[!(hearts$avg_glucose_level < Lower | hearts$avg_glucose_level > Upper),]
boxplot(hearts$avg_glucose_level)
hist(hearts$avg_glucose_level)
sum(hearts$avg_glucose_level > 160 & hearts$avg_glucose_level <= 169)

### bmi levels ###
sum(hearts$bmi == "N/A")
hearts$bmi[hearts$bmi == "N/A"] <- NA
hearts$bmi <- as.numeric(hearts$bmi)
sum(is.na(hearts$bmi)) # 140
sum(heart1$bmi == "N/A") ##201
sum(heart1 == "N/A") #201

heart1$bmi[heart1$bmi == "N/A"] <- NA
heart1$bmi <- as.numeric(heart1$bmi)

### smoking status ###
hearts$smoking_status[hearts$smoking_status == "Unknown"] <- NA
hearts$smoking_status[hearts$smoking_status == "never smoked"] <- 1
hearts$smoking_status[hearts$smoking_status == "smokes"] <- 2
hearts$smoking_status[hearts$smoking_status == "formerly smoked"] <- 3

hearts$smoking_status <- as.factor(hearts$smoking_status)

## replacing null values with rf algorithm ##
set.seed(1)
imp <- mice(hearts, method = "rf")
hearts <- complete(imp)

sum(is.na(hearts)) 

set.seed(2)
imp <- mice(heart1, method = "rf")
heart1 <- complete(imp)

sum(is.na(hearts)) 

## bmi outliers ##
sum(hearts$bmi <= 20) #545
sum(hearts$bmi < 16) #81
sum(hearts$bmi > 45)
hist(hearts$bmi)
hearts = hearts %>% filter(hearts$bmi > 16 & hearts$bmi < 45)

write.csv(hearts, file = "hearts.csv", row.names = FALSE)

###### plots ##########

Age.d<- ggplot(Heart,aes(x = age)) +
  geom_histogram(color = "#B8B7ED", fill="#B8B7ED", alpha=0.3) +
  scale_color_manual(values = c("black", "blue", "green", "red", "purple")) +
  theme_bw() + theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Age") + ylab(" ")
Age.d
Age.d + Bmi.d + Glucose.d 

Bmi.d <- ggplot(Heart,aes(x = bmi)) +
  geom_histogram(color = "lightgreen", fill="lightgreen", alpha=0.3) +
  scale_color_manual(values = c("black", "blue", "green", "red", "purple")) +
  theme_bw() + theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("BMI") + ylab(" ")
Bmi.d


Glucose.d <- ggplot(Heart,aes(x = avg_glucose_level)) +
  geom_histogram(aes(y = ..density..), bins = 30, colour = "lightblue", fill = "lightblue", alpha = 0.3) +
  scale_color_manual(values = c("black", "blue", "green", "red", "purple")) + theme_bw()+
  theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Average Glucose Level") + ylab(" ")

Glucose.d

Gender <- ggplot(Heart, aes(x = gender)) + geom_bar(aes(y = ..count..), colour = "#E0C0FD", fill = "#E0C0FD", alpha = 0.6)+ 
  scale_color_manual(values = c("black", "blue", "green", "red", "purple", "#E0C0FD")) + theme_bw()+
  theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Gender") + ylab(" ")

Hypertension <- ggplot(Heart, aes(x = hypertension)) + geom_bar(aes(y = ..count..), colour = "#FDCCC0", fill = "#FDCCC0", alpha = 0.8)+ 
  scale_color_manual(values = c("black", "blue", "green", "red", "purple", "#E0C0FD")) + theme_bw()+
  theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Hypertension") + ylab(" ")
Hypertension

Heart_disease <- ggplot(Heart, aes(x = hypertension)) + geom_bar(aes(y = ..count..), colour = "#88D595", fill = "#88D595", alpha = 0.8)+ 
  scale_color_manual(values = c("black", "blue", "green", "red", "purple", "#E0C0FD")) + theme_bw()+
  theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Heart Disease") + ylab(" ")
Heart_disease

Stroke <- ggplot(Heart, aes(x = hypertension)) + geom_bar(aes(y = ..count..), colour = "#B8B7ED", fill = "#B8B7ED", alpha = 0.8)+ 
  scale_color_manual(values = c("black", "blue", "green", "red", "purple", "#E0C0FD")) + theme_bw()+
  theme(text = element_text(family = "Georgia", color = "grey7"), axis.ticks = element_blank()) + xlab("Stroke") + ylab(" ")
Stroke
colnames(Heart)

par(mfrow = c(1, 3))
boxplot(heart1$age, xlab = "Age") 
boxplot(heart1$avg_glucose_level, xlab = "Average Glucose Level" ) 
boxplot(heart1$bmi, xlab = "BMI")

par(mfrow = c(1, 3))
boxplot(heart$age, xlab = "Age") 
boxplot(heart$avg_glucose_level, xlab = "Average Glucose Level" ) 
boxplot(heart$bmi, xlab = "BMI")

par(mfrow = c(1, 3))
boxplot(Heart$age, xlab = "Age") 
boxplot(Heart$avg_glucose_level, xlab = "Average Glucose Level" ) 
boxplot(Heart$bmi, xlab = "BMI")


heart = read.csv("hearts.csv")
Heart = read.csv("Heart.csv")
summary(heart$bmi)

heart$gender = as.factor(heart$gender)
heart$hypertension = as.factor(heart$hypertension)
heart$heart_disease = as.factor(heart$heart_disease)
heart$heart_disease = as.factor(heart$heart_disease)
heart$stroke = as.factor(heart$stroke)
heart$ever_married = as.factor(heart$ever_married)
heart$Residence_type = as.factor(heart$Residence_type)
heart$smoking_status = as.factor(heart$smoking_status)

mean(heart$gender == 1) #0.6

hist(heart$age)
hist(heart$avg_glucose_level)
mean(heart$avg_glucose_level) 
hist(heart$bmi)

num = heart[c("age", "avg_glucose_level", "bmi")]
head(num)
norm = normalize(num, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
summary(norm)
hc.c = select(heart, subset = -c("age", "avg_glucose_level", "bmi"))
hc.c = cbind(hc.c, norm)
head(hc.c)


write.csv(hc.c, file = "Heart.csv", row.names = FALSE)

