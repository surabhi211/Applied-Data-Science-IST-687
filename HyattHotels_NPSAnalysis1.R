#jan data

Jan <- read.csv(file="out-201501.csv", header=TRUE, sep=",")

Jandata <- Jan[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

jandata1 <- na.omit(Jandata)

jandata1 <- jandata1[jandata1$Location_PL == "Airport",]
jandata1 <- jandata1[jandata1$Country_PL == "United States",]
View(jandata1)

#jan  liner modle


lmguestroom1 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data = jandata1)
lmTranquility1 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data = jandata1)
lmCondition_Hotel1 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data = jandata1)
lmCustomer_SVC1 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data = jandata1)
lmStaff_Cared1 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data = jandata1)
lmCheck_In1 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H, data = jandata1)
lmF.B_FREQ1 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H, data = jandata1)
lmF.B_Overall1 <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = jandata1)


summary(lmguestroom1)
#0.5416
summary(lmTranquility1)
#0.3871
summary(lmCondition_Hotel1)
#0.5257
summary(lmCustomer_SVC1)
#0.4493
summary(lmStaff_Cared1)
#0.3342
summary(lmCheck_In1)
#0.2461
summary(lmF.B_FREQ1)
#0.000003584
summary(lmF.B_Overall1)
#0.2342

# jan association rules

install.packages("arules")
install.packages("arulesViz")
library("arules")
library("arulesViz")

jacc <- jandata1[-c(1:20)]
jacc <- na.omit(jacc)
str(jacc)
janrule <- apriori(jacc,parameter = list(support=0.06,confidence=0.82),appearance  = list(rhs=c("NPS_Type=Promoter","NPS_Type=Detractor","NPS_Type=Passive"),default="lhs"))
janrule

inspect(janrule)
plot(janrule)





#april

april <- read.csv(file="out-201404.csv", header=TRUE, sep=",")

aprildata <- april[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

aprildata1 <- na.omit(aprildata)

aprildata1 <- aprildata1[aprildata1$Location_PL == "Airport",]
aprildata1 <- aprildata1[aprildata1$Country_PL == "United States",]
View(aprildata1)



#liner modle for april


lmguestroom4 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data = aprildata1)
lmTranquility4 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data = aprildata1)
lmCondition_Hotel4 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data = aprildata1)
lmCustomer_SVC4 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data = aprildata1)
lmStaff_Cared4 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data = aprildata1)
lmCheck_In4 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H, data = aprildata1)
lmF.B_FREQ4 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H, data = aprildata1)
lmF.B_Overall4 <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = aprildata1)


summary(lmguestroom4)
#0.4965
summary(lmTranquility4)
#0.3903
summary(lmCondition_Hotel4)
#0.4597
summary(lmCustomer_SVC4)
#0.4874
summary(lmStaff_Cared4)
#0.395
summary(lmCheck_In4)
#0.2108
summary(lmF.B_FREQ4)
#-0.0003104
summary(lmF.B_Overall4)
#0.2519

# April association rules



apcc <- aprildata1[-c(1:20)]
apcc <- na.omit(apcc)
str(apcc)
aprule <- apriori(apcc,parameter = list(support=0.05,confidence=0.81),appearance  = list(rhs=c("NPS_Type=Promoter","NPS_Type=Detractor","NPS_Type=Passive"),default="lhs"))
aprule

inspect(aprule)
plot(aprule)



#july data

July <- read.csv(file="out-201407.csv", header=TRUE, sep=",")

Julydata <- July[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

Julydata1 <- na.omit(Julydata)

Julydata1 <- Julydata1[Julydata1$Location_PL == "Airport",]
Julydata1 <- Julydata1[Julydata1$Country_PL == "United States",]
View(Julydata1)

# liner modle for july

lmguestroom7 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data = Julydata1)
lmTranquility7 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data = Julydata1)
lmCondition_Hotel7 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data = Julydata1)
lmCustomer_SVC7 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data = Julydata1)
lmStaff_Cared7 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data = Julydata1)
lmCheck_In7 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H, data = Julydata1)
lmF.B_FREQ7 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H, data = Julydata1)
lmF.B_Overall7 <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = Julydata1)


summary(lmguestroom7)
#0.4854
summary(lmTranquility7)
#0.337
summary(lmCondition_Hotel7)
#0.4237
summary(lmCustomer_SVC7)
#0.4655
summary(lmStaff_Cared7)
#0.3677
summary(lmCheck_In7)
#0.2157
summary(lmF.B_FREQ7)
#-0.0003547
summary(lmF.B_Overall7)
#0.2212

data("Groceries")
View(Groceries)
Groceries
# July association rules



Julycc <- Julydata1[-c(1:20)]
Julycc <- na.omit(Julycc)
str(Julycc)
itemFrequencyPlot(Julycc)
Julycc = as(Julycc, "transactions");
Jurule <- apriori(Julycc,parameter = list(support=0.029,confidence=0.86),appearance  = list(rhs=c("NPS_Type=Promoter","NPS_Type=Detractor","NPS_Type=Passive"),default="lhs"))
Jurule

inspect(Jurule)
Julycc <Julycc[-Julycc$Convention_PL,]





#Oct data

Oct <- read.csv(file="out-201410.csv", header=TRUE, sep=",")

Octdata <- Oct[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

Octdata1 <- na.omit(Octdata)

Octdata1 <- Octdata1[Octdata1$Location_PL == "Airport",]
Octdata1 <- Octdata1[Octdata1$Country_PL == "United States",]
View(Octdata1)


#liner modle for Oct
lmguestroom10 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data = Octdata1)
lmTranquility10 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data = Octdata1)
lmCondition_Hotel10 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data = Octdata1)
lmCustomer_SVC10 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data = Octdata1)
lmStaff_Cared10 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data = Octdata1)
lmCheck_In10 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H, data = Octdata1)
lmF.B_FREQ10 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H, data = Octdata1)
lmF.B_Overall10 <- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = Octdata1)


summary(lmguestroom10)
#0.4647
summary(lmTranquility10)
#0.3255
summary(lmCondition_Hotel10)
#0.4023
summary(lmCustomer_SVC10)
#0.3865
summary(lmStaff_Cared10)
#0.3535
summary(lmCheck_In10)
#0.1916
summary(lmF.B_FREQ10)
#-0.0003526
summary(lmF.B_Overall10)
#0.2393

# Oct. association rules



Octcc <- Octdata1[-c(1:20)]
Octcc <- na.omit(Octcc)
str(Octcc)
Octrule <- apriori(Octcc,parameter = list(support=0.03,confidence=0.84),appearance  = list(rhs=c("NPS_Type=Promoter","NPS_Type=Detractor","NPS_Type=Passive"),default="lhs"))
Octrule

inspect(Octrule)



association <- read.csv(file = "Association Rules Calculation.csv")

association

install.packages("ggplot2")
library("ggplot2")

plot(association)


ggplot(association, aes(x= object, y = number)) + geom_bar(stat="identity")