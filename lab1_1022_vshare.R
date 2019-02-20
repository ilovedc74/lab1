#------------------------ 前置作業開始----------------------#

setwd("~/Documents/Panc/Research/Research skills/lab1")

gMSI_STOMP = read.csv("_Lab1_dataset_v2.csv")

# install.packages("data.table")
library(data.table)
gMSI_STOMP = as.data.table(gMSI_STOMP)

View(gMSI_STOMP)
# colnames(gMSI_STOMP)

# filter out outlier
gMSI_STOMP = gMSI_STOMP[F1ActiveEngagement<100,]
gMSI_STOMP = gMSI_STOMP[,-14]

setnames(gMSI_STOMP,c("ParticipantNo","Age","Gender",
                      "F1ActiveEngagement","F2PerceptualAbilities",
                      "F3MusicalTraining","F5Emotions","F4SingingAbilities",
                      "FGGeneralSophistication","ReflectiveComplex",
                      "IntenseRebellious","UpbeatConventional","EnergeticRhythmic"),
         c('No', 'Age', 'Gender', 'F1AE', 'F2PA', 'F3MT',
           'F5EM', 'F4SA', 'FG', 'S1RC', 'S2IR', 'S3UB','S4ER'))

View(gMSI_STOMP)
# seperate
# gMSI = gMSI_STOMP[,c(1:9,15:19)]
# View(gMSI)
# 
# STOMP = gMSI_STOMP[,c(1:3,10:13)]
# View(STOMP)

# pearson's correlation
# spearman's correlation
library(Hmisc)

gmsi_origin = gMSI_STOMP[,c(1:13)]
# stomp = STOMP[,c(2,4:7)]
gmsi_sqrt = gMSI_STOMP[,c(1:3,10:19)]
# View(gmsi_origin)
# View(gmsi_sqrt)
#------------------------ 前置作業end----------------------#
summary(gMSI_STOMP)
install.packages("lattice")
install.packages("ggplot2")
# install.packages("survival")
install.packages("Hmisc")

# no transformation
corr_s = rcorr(as.matrix(gmsi_origin[,c(2:13)]), type = "spearman")

print(corr_s)
# transformation
corr_p = rcorr(as.matrix(gmsi_sqrt[,c(2:13)]), type = "pearson")
print(corr_p)


#------------------plots--------------------#


install.packages("gglot2")
library(ggplot2)

gMSI_STOMP$Gender = as.character(gMSI_STOMP$Gender)
View(gMSI_STOMP)
gMSI_STOMP$Gender[gMSI_STOMP$Gender == "1"] <- "Female"
gMSI_STOMP$Gender[gMSI_STOMP$Gender == "2"] <- "Male"
gMSI_STOMP$Gender = as.factor(gMSI_STOMP$Gender)

# gMSI_STOMP[Gender == "1", ifelse(Gender == "1","Female","Male"]
# View(gMSI_STOMP)

# F5//S1RC

ggplot(gMSI_STOMP, aes(x = F5EM, y = S1RC, color = Gender)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.title.y = element_text(size = rel(1.5)))+
  labs(
    title = "Correlation of Emotion and Reflective & Complex",
    x = "Gold MSI (Emotion)",
    y = "STOMP (Reflective & Complex)"
  )



# F5//S4ER
ggplot(gMSI_STOMP, aes(x = F5EM, y = S4ER, color = Gender)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.title.y = element_text(size = rel(1.5), angle = 90))+
  labs(
    title = "Correlation of Emotion and Energetic & Rhythmic",
    x = "Gold MSI (Emotion)",
    y = "STOMP (Energetic & Rhythmic)"
  )
gMSI_STOMP[, lapply(.SD, mean) ,by = Gender]
gMSI_STOMP[, lapply(.SD, sd) ,by = Gender]
gMSI_STOMP[, lapply(.SD, mean),]
gMSI_STOMP[, lapply(.SD, sd),]


  

