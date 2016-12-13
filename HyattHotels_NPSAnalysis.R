# jandata

Jan <- read.csv(file="out-201501.csv", header=TRUE, sep=",")

Jandata <- Jan[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

jandata1 <- na.omit(Jandata)

jandata1 <- jandata1[jandata1$Location_PL == "Airport",]
jandata1 <- jandata1[jandata1$Country_PL == "United States",]
View(jandata1)


#jan ksvm
install.packages("kernlab")
library("kernlab")


ksvm(Likelihood_Recommend_H ~ Overall_Sat_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.1798
ksvm(Likelihood_Recommend_H ~ Guest_Room_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.463584
ksvm(Likelihood_Recommend_H ~ Tranquility_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.62474
ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.484847
ksvm(Likelihood_Recommend_H ~ Customer_SVC_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.560573
ksvm(Likelihood_Recommend_H ~ Staff_Cared_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.681103
ksvm(Likelihood_Recommend_H ~ Check_In_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.77012
ksvm(Likelihood_Recommend_H ~ F.B_FREQ_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#1.143793
ksvm(Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.745901

ksvm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H + Customer_SVC_H, data=jandata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.250704



#April data
april <- read.csv(file="out-201404.csv", header=TRUE, sep=",")

aprildata <- april[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

aprildata1 <- na.omit(aprildata)

aprildata1 <- aprildata1[aprildata1$Location_PL == "Airport",]
aprildata1 <- aprildata1[aprildata1$Country_PL == "United States",]
View(aprildata1)


#april ksvm

ksvm(Likelihood_Recommend_H ~ Overall_Sat_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.0.200486
ksvm(Likelihood_Recommend_H ~ Guest_Room_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.514624
ksvm(Likelihood_Recommend_H ~ Tranquility_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.629776
ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.549731
ksvm(Likelihood_Recommend_H ~ Customer_SVC_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.522276
ksvm(Likelihood_Recommend_H ~ Staff_Cared_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.621754
ksvm(Likelihood_Recommend_H ~ Check_In_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.832011
ksvm(Likelihood_Recommend_H ~ F.B_FREQ_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#1.095466
ksvm(Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.760601

ksvm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H + Customer_SVC_H, data=aprildata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.292883



#julydata

July <- read.csv(file="out-201407.csv", header=TRUE, sep=",")

Julydata <- July[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

Julydata1 <- na.omit(Julydata)

Julydata1 <- Julydata1[Julydata1$Location_PL == "Airport",]
Julydata1 <- Julydata1[Julydata1$Country_PL == "United States",]
View(Julydata1)


#july ksvm

ksvm(Likelihood_Recommend_H ~ Overall_Sat_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.203012
ksvm(Likelihood_Recommend_H ~ Guest_Room_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.528223
ksvm(Likelihood_Recommend_H ~ Tranquility_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.663344
ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.586719
ksvm(Likelihood_Recommend_H ~ Customer_SVC_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.538674
ksvm(Likelihood_Recommend_H ~ Staff_Cared_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.643344
ksvm(Likelihood_Recommend_H ~ Check_In_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.828464
ksvm(Likelihood_Recommend_H ~ F.B_FREQ_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#1.124516
ksvm(Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.808481

ksvm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H + Customer_SVC_H, data=Julydata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.29536



#Octdata

Oct <- read.csv(file="out-201410.csv", header=TRUE, sep=",")

Octdata <- Oct[-c(1:18,20:55,57:65,67:82,84:106,108:136,148:170,172:181,183:195,197:198,199,201,203,204,209,210,211,213,214,216,217,218,220,222,223,224,225,226,227,228:231,233:237)]

Octdata1 <- na.omit(Octdata)

Octdata1 <- Octdata1[Octdata1$Location_PL == "Airport",]
Octdata1 <- Octdata1[Octdata1$Country_PL == "United States",]
View(Octdata1)

#Oct ksvm


ksvm(Likelihood_Recommend_H ~ Overall_Sat_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.189875
ksvm(Likelihood_Recommend_H ~ Guest_Room_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.552046
ksvm(Likelihood_Recommend_H ~ Tranquility_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.683657
ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.609817
ksvm(Likelihood_Recommend_H ~ Customer_SVC_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.630506
ksvm(Likelihood_Recommend_H ~ Staff_Cared_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.660127
ksvm(Likelihood_Recommend_H ~ Check_In_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.838814
ksvm(Likelihood_Recommend_H ~ F.B_FREQ_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#1.134968
ksvm(Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.771801

ksvm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H + Customer_SVC_H, data=Octdata1, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)
#0.348105



#g
install.packages("ggplot2")
library("ggplot2")
jang <- read.csv(file = "1.csv")
jang
ggplot(jang, aes(x= Factor, y = Training.Error)) + geom_bar(stat="identity")+theme(axis.text.x = element_text( angle=90))



aprg <- read.csv(file = "4.csv")
aprg
ggplot(aprg, aes(x= Factor, y = Training.Error)) + geom_bar(stat="identity")+theme(axis.text.x = element_text( angle=90))


julyg <- read.csv(file = "7.csv")
julyg
ggplot(julyg, aes(x= Factor, y = Training.Error)) + geom_bar(stat="identity")+theme(axis.text.x = element_text( angle=90))


octg<- read.csv(file = "10.csv")
octg
ggplot(octg, aes(x= Factor, y = Training.Error)) + geom_bar(stat="identity")+theme(axis.text.x = element_text( angle=90))
