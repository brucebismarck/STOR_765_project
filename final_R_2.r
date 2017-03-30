rm(list=ls())

library(ggplot2)
library(MASS)
#install.packages('ggfortify')
library(ggfortify)
library(scatterplot3d)
library(dplyr)
library(forecast)
library(grid)
library(scales)
library(combinat)


setwd("C:/Users/bruce/Google Drive/765 Project/765_project")
setwd("~/Google Drive/765 Project/765_project")
data <- as.data.frame(read.csv('data-wenyue.csv'))
data<- data[,!(colnames(data) %in% c('F2.F1','F3.F2'))]
data[data == '--undefined--'] <- NA

unique((data%>%filter(type =='glide'))$sound)
sum(is.na(data$F4))

######################################################################
'Histogram visualization part'
'Before any transformation'
######################################################################
head(data)
hist(data$duration,xlab = 'Phone Duration' , breaks = 20, main = 'Histogram of feature Duration')
hist(data$intensity, xlab = 'Intensity', breaks = 20, main = 'Histogram of feature Intensity')
hist(as.numeric(data$AvePitch), xlab = 'Average Pitch',breaks = 20, main = 'Histogram of feature Average Pitch')
hist(data$AveHarmonicity, xlab = 'Average Harmonicity',breaks = 20, main = 'Histogram of feature Average Harmonicity')
hist(data$F1, xlab = 'F1' ,breaks = 20, main ='Histogram of feature F1')
hist(data$F2, xlab = 'F2' , breaks = 20, main ='Histogram of feature F2')
hist(data$F3, xlab = 'F3' , breaks = 20,main ='Histogram of feature F3')
hist(as.numeric(data$F4), xlab = 'F4' ,breaks = 20, main ='Histogram of feature F4')
hist(as.numeric(data$F5), xlab = 'F5' ,breaks = 20, main ='Histogram of feature F5')
hist(data$F1_bandwidth, xlab = 'F1_bandwidth',breaks = 20, main = 'Histogram of feature F1 bandwidth')
hist(data$F2_bandwidth, xlab = 'F2_bandwidth', breaks = 20, main = 'Histogram of feature F2 bandwidth')
hist(data$F3_bandwidth, xlab = 'F3_bandwidth', breaks = 20,main = 'Histogram of feature F3 bandwidth')
hist(as.numeric(data$F4_bandwidth), xlab = 'F4_bandwidth', breaks = 20,main = 'Histogram of feature F4 bandwidth')
hist(as.numeric(data$F5_bandwidth), xlab = 'F5_bandwidth',breaks = 20, main = 'Histogram of feature F5 bandwidth')

######################################################################
'find out features with NA values'
#####################################################################
na_row = c()
for (item in colnames(data)){
  if (sum(is.na(data[item])) > 0| sum(data[item] == '--undefined--') > 0 ){ 
    na_row <- c(na_row,item)
  }
}
na_row

data <- data[is.na(data[,'F4']) != T,]
data <- data[is.na(data[,'AvePitch']) != T,]
data <- data[,c(-17,-22)]  # delete F5 and F5_bandwidth because of ba
####################################################################
'Use a function to plot multiple ggplots in same plot'
###################################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} 

####################################################################
'data type conversion'
####################################################################

data$intensity <- as.double(data$intensity)        # convert from integer to double
data$F4 <- as.double(as.character(data$F4))         # convert from integer to double
#data$F5 <- as.double(data$F5)                     # convert from integer to double
data$F4_bandwidth <- as.double(data$F4_bandwidth) # convert from integer to double
#data$F5_bandwidth <- as.double(data$F5_bandwidth) # convert from integer to double
data$AvePitch <- as.double(data$AvePitch)

#maybe we need give it a log function
hist(data$duration, xlab = 'Phoneme Duration' ,breaks = 20, main = 'Histogram of feature Duration') 
p <- ggplot(data, aes(sample = duration))
p + stat_qq() + ggtitle('QQ-plot of Duration before transformation')
# for duration, we need to give it a transformation, log 10
data$duration = log10(data$duration) 
hist(data$duration,xlab = 'Phoneme Duration' ,breaks = 20, main = 'Histogram of feature Duration after log10 transformation')
# much better after log 10
p <- ggplot(data, aes(sample = duration))
p + stat_qq() + ggtitle('QQ-plot of Duration after log10 transformation')
##########################################################################
hist(data$intensity, xlab = 'intensity', main = 'Histogram of feature intensity',breaks = 20)
p <- ggplot(data, aes(sample = intensity))
p + stat_qq() + ggtitle('QQ-plot of intensity before transformation')
BoxCox.lambda(data$intensity, method = "loglik", lower = -5, upper = 5)
data$intensity <- BoxCox(data$intensity, lambda = 3.5)
hist(data$intensity,xlab = 'intensity', main = 'Histogram of feature intensity after \n lambda 3.5 boxcox transformation', breaks = 20)
p <- ggplot(data, aes(sample = intensity))
p + stat_qq() + ggtitle('QQ-plot of intensity after boxcox transformation')
########################################################################
hist(as.numeric(data$AvePitch), xlab = 'Average Pitch',breaks = 20, main = 'Histogram of feature Average Pitch')
p <- ggplot(data, aes(sample = AvePitch))
p + stat_qq() +ggtitle('QQ-plot of average pitch before transformation')
BoxCox.lambda(data$AvePitch, method = 'loglik')
data$AvePitch <- BoxCox(data$AvePitch, lambda = 0.5)
hist(data$AvePitch, breaks = 20, xlab = 'Average Pitch', main = 'Histogram of feature Average Pitch after \n lambda 0.5 boxcox transformation')
p <- ggplot(data, aes(sample = AvePitch))
p + stat_qq() +ggtitle('QQ-plot of average pitch after square root transformation')
#######################################################################
hist(data$AveHarmonicity, xlab = 'Average Harmonicity',breaks = 20, main = 'Histogram of feature Average Harmonicity')
hist(data$AveHarmonicity, xlab = 'Average Harmonicity',breaks = 20, main = 'Histogram of feature Average Harmonicity \n without transformation')
p <- ggplot(data, aes(sample = AveHarmonicity))
p + stat_qq() +ggtitle('QQ-plot of average harmonicity without transformation')
########################################################################
hist(data$F1, xlab = 'F1' , main ='Histogram of feature F1', breaks =20)
p <- ggplot(data, aes(sample = F1))
p + stat_qq() + ggtitle('QQ-plot of F1 before transformation')
data$F1 <- log10(data$F1)
hist(data$F1, xlab = 'F1' , main ='Histogram of feature F1 after log10 transformation', breaks = 20)
p <- ggplot(data, aes(sample = F1))
p + stat_qq() + ggtitle('QQ-plot of F1 after log10 transformation')
##########################################################################
hist(data$F2, xlab = 'F2' , main ='Histogram of feature F2', breaks = 20)
p <- ggplot(data, aes(sample = F2))
p + stat_qq() + ggtitle('QQ-plot of F2 before transformation')
BoxCox.lambda(data$F2, method = 'loglik',lower = -5, upper  =5 )
data$F2 <- BoxCox(data$F2, lambda = 2.15)
hist(data$F2, xlab = 'F2' , main ='Histogram of feature F2 after \n lambda 2.15 boxcox transformation', breaks =20)
p <- ggplot(data, aes(sample = F2))
p + stat_qq() + ggtitle('QQ-plot of F2 after boxcox transformation')
###########################################################################
hist(data$F3, xlab = 'F3' , main ='Histogram of feature F3', breaks = 20)
p <- ggplot(data, aes(sample = F3))
p + stat_qq() + ggtitle('QQ-plot of F3 before transformation')
BoxCox.lambda(data$F3, lower = -5, upper  =5 ,method = 'loglik' )
data$F3 = BoxCox(data$F3, lambda = 0.4)
hist(data$F3, xlab = 'F3' , main ='Histogram of feature F3 after \n lambda 0.4 boxcox transformation', breaks =20)
p <- ggplot(data, aes(sample = F3))
p + stat_qq() + ggtitle('QQ-plot of F3 after boxcox transformation')
##########################################################################
hist(data$F4, xlab = 'F4' , main ='Histogram of feature F4', breaks = 20)
p <- ggplot(data, aes(sample = F4))
p + stat_qq() + ggtitle('QQ-plot of F4 before transformation')
BoxCox.lambda(data$F4, lower = -5, upper  =5 , method = 'loglik')
data$F4 = BoxCox(data$F4, lambda = 0.4)
hist(data$F4, xlab = 'F4' , main ='Histogram of feature F4 after \n lambda 0.4 boxcox transformation', breaks =20)
p <- ggplot(data, aes(sample = F4))
p + stat_qq() + ggtitle('QQ-plot of F4 after boxcox transformation')
##########################################################################
hist(data$F1_bandwidth, xlab = 'F1_bandwidth', main = 'Histogram of feature F1 bandwidth', breaks = 20)
p <- ggplot(data, aes(sample = F1_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F1_bandwidth before transformation')
data$F1_bandwidth <- log10(data$F1_bandwidth)
hist(data$F1_bandwidth, xlab = 'F1_bandwidth', main = 'Histogram of feature F1 bandwidth after \n log10 transformation', breaks = 20)
p <- ggplot(data, aes(sample = F1_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F1_bandwidth after log10 transformation')
##########################################################################
hist(data$F2_bandwidth, xlab = 'F2_bandwidth', main = 'Histogram of feature F2 bandwidth', breaks = 20)
p <- ggplot(data, aes(sample = F2_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F2_bandwidth before transformation')
data$F2_bandwidth <- log10(data$F2_bandwidth)
hist(data$F2_bandwidth, xlab = 'F2_bandwidth', main = 'Histogram of feature F2 bandwidth after \n log10 transformation', breaks = 20)
p <- ggplot(data, aes(sample = F2_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F2_bandwidth after log10 transformation')
##########################################################################
hist(data$F3_bandwidth, xlab = 'F3_bandwidth', main = 'Histogram of feature F3 bandwidth', breaks = 20)
p <- ggplot(data, aes(sample = F3_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F3_bandwidth before transformation')
data$F3_bandwidth <- log10(data$F3_bandwidth)
hist(data$F3_bandwidth, xlab = 'F3_bandwidth', main = 'Histogram of feature F3 bandwidth after \n log10 transformation', breaks =20)
p <- ggplot(data, aes(sample = F3_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F3_bandwidth after log10 transformation')
########################################################################
hist(data$F4_bandwidth, xlab = 'F4_bandwidth', main = 'Histogram of feature F4 bandwidth', breaks =20)
p <- ggplot(data, aes(sample = F4_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F4_bandwidth before transformation')
BoxCox.lambda(data$F4_bandwidth, lower = -5, upper = 5, method = 'loglik')
data$F4_bandwidth<- BoxCox(data$F4_bandwidth, lambda = 0.65)
hist(data$F4_bandwidth, xlab = 'F4_bandwidth', main = 'Histogram of feature F4 bandwidth after \n lambda 0.65 boxcox transformation ', breaks = 20)
p <- ggplot(data, aes(sample = F4_bandwidth))
p + stat_qq() + ggtitle('QQ-plot of F4_bandwidth after boxcox transformation')
########################################################################

head(data)
test_data <- data[,c(5:20)]
test_data$type

vowel <- test_data%>%filter(type == 'vowel')
glide <- test_data%>%filter(type == 'glide')
nrow(vowel) + nrow(glide) == nrow(test_data)

t.test(vowel$F1, glide$F1) #significant   e-16
t.test(vowel$F2, glide$F2) #significant   e-6
t.test(vowel$F3, glide$F3) #not significant
t.test(vowel$F4, glide$F4) # not significant
t.test(vowel$F1_bandwidth,  glide$F1_bandwidth) # not significant
t.test(vowel$F2_bandwidth,  glide$F2_bandwidth) # significant   0.002355
t.test(vowel$F3_bandwidth,  glide$F3_bandwidth) # not significant
t.test(vowel$F4_bandwidth,  glide$F4_bandwidth) # not significant
t.test(vowel$duration,      glide$duration)     # significant  e-16
t.test(vowel$intensity,     glide$intensity)    # significant  e-7
t.test(vowel$AvePitch,      glide$AvePitch)     # significant   0.15
t.test(vowel$AveHarmonicity,glide$AveHarmonicity) # not significant


two_way_data <- test_data[,c(-3,-4)]
two_way_data$sound <- as.character(two_way_data$sound)
two_way_data$sound[two_way_data$sound == 'i'| two_way_data$sound == 'j'] <- 'ji'
two_way_data$sound[two_way_data$sound == 'y'| two_way_data$sound == 'h'] <- 'hy'
two_way_data$sound[two_way_data$sound == 'u'| two_way_data$sound == 'w'] <- 'wu'

head(two_way_data)
aov_duration <- aov(duration ~ sound * type,data = two_way_data)
summary(aov_duration)
aov_intensity <- aov(intensity ~ sound * type,data = two_way_data)
summary(aov_intensity)
aov_AvePitch <- aov(AvePitch ~ sound * type,data = two_way_data)
summary(aov_AvePitch)
aov_AveHarmonicity <- aov(AveHarmonicity ~ sound * type,data = two_way_data)
summary(aov_AveHarmonicity)
aov_F1 <- aov(F1 ~ sound * type,data = two_way_data)
summary(aov_F1)
aov_F2 <- aov(F2 ~ sound * type,data = two_way_data)
summary(aov_F2)
aov_F3 <- aov(F3 ~ sound * type,data = two_way_data)
summary(aov_F3)
aov_F4 <- aov(F4 ~ sound * type,data = two_way_data)
summary(aov_F4)
aov_F1_bandwidth <- aov(F1_bandwidth ~ sound * type,data = two_way_data)
summary(aov_F1_bandwidth)
aov_F2_bandwidth <- aov(F2_bandwidth ~ sound * type,data = two_way_data)
summary(aov_F2_bandwidth)
aov_F3_bandwidth <- aov(F3_bandwidth ~ sound * type,data = two_way_data)
summary(aov_F3_bandwidth)
aov_F4_bandwidth <- aov(F4_bandwidth ~ sound * type,data = two_way_data)
summary(aov_F4_bandwidth)

sound_data = test_data[,-2]
head(sound_data)

sound.lda <- lda(sound ~ ., data = sound_data)
sound.lda.values <- predict(sound.lda)
sound.lda.values$x[,1]
ldahist(data = sound.lda.values$x[,1],g=sound_data[,1])
#The interesting thing is, u and w are two different sound!

type_data = test_data[,-1]
head(type_data)
type.lda <- lda(type ~ ., data = type_data)
type.lda.values <- predict(type.lda)
ldahist(data = type.lda.values$x[,1],g=type_data[,1], col = 1 )
ldahist(data = data.pca$x[,3],g=type_data[,1], col = 1) 

##############################################
# LDA for pairs
###############################3##############
pairs_lda_data <- two_way_data[,-2]
pair.lda <-lda(sound ~., data = pairs_lda_data)
pair.lda.values <- predict(pair.lda)
ldahist(data = pair.lda.values$x[,1],g=pairs_lda_data[,1], col = 1 )
ldahist(data = pair.lda.values$x[,2],g=pairs_lda_data[,1], col = 1 )


pca_data <- na.omit(data[,colnames(data)[9:20]])
pca_data <- data[,colnames(data)[9:20]]
data.pca<- prcomp(na.omit(pca_data), center=T, scale. = T)
str(data)
plot(data.pca, type = 'lines')
summary(data.pca)

plot_data <- as.data.frame(data.pca$x[,1:6])
plot_data<-cbind(sapply(data$sound, as.character), sapply(data$type, as.character),plot_data)
colnames(plot_data) <- c('sound','type','PC1','PC2','PC3','PC4','PC5','PC6')
plot_data<-as.data.frame(plot_data)
plot_data <- plot_data[sample(nrow(plot_data)),] # shuffle data 
str(plot_data)


#######################
# visualization of glide 
#######################
glide_data_pca <- plot_data %>% filter(type == 'glide')
p1 <- ggplot(glide_data_pca, aes(x=PC1, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p2 <- ggplot(glide_data_pca, aes(x=PC1, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p3 <- ggplot(glide_data_pca, aes(x=PC1, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p4 <- ggplot(glide_data_pca, aes(x=PC2, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p5 <- ggplot(glide_data_pca, aes(x=PC2, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p6 <- ggplot(glide_data_pca, aes(x=PC2, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p7 <- ggplot(glide_data_pca, aes(x=PC3, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p8 <- ggplot(glide_data_pca, aes(x=PC3, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p9 <- ggplot(glide_data_pca, aes(x=PC3, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p10 <- ggplot(glide_data_pca, aes(x=PC4, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p11 <- ggplot(glide_data_pca, aes(x=PC4, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p12 <- ggplot(glide_data_pca, aes(x=PC4, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, cols=4)

#######################
# visualization of vowel
#######################
glide_data_pca <- plot_data %>% filter(type == 'vowel')
p1 <- ggplot(glide_data_pca, aes(x=PC1, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p2 <- ggplot(glide_data_pca, aes(x=PC1, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p3 <- ggplot(glide_data_pca, aes(x=PC1, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p4 <- ggplot(glide_data_pca, aes(x=PC2, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p5 <- ggplot(glide_data_pca, aes(x=PC2, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p6 <- ggplot(glide_data_pca, aes(x=PC2, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p7 <- ggplot(glide_data_pca, aes(x=PC3, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p8 <- ggplot(glide_data_pca, aes(x=PC3, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p9 <- ggplot(glide_data_pca, aes(x=PC3, y =PC4, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
p10 <- ggplot(glide_data_pca, aes(x=PC4, y =PC1, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p11 <- ggplot(glide_data_pca, aes(x=PC4, y =PC2, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5)+ theme(legend.position = "none")
p12 <- ggplot(glide_data_pca, aes(x=PC4, y =PC3, color = sound)) + geom_point(alpha= 0.5)  + xlim(-5, 5) + ylim(-5, 5) + theme(legend.position = "bottom")
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, cols=4)



q1 <- ggplot(plot_data, aes(x=PC1, y =PC2, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q2 <- ggplot(plot_data, aes(x=PC1, y =PC3, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q3 <- ggplot(plot_data, aes(x=PC1, y =PC4, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom")
q4 <- ggplot(plot_data, aes(x=PC2, y =PC1, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q5 <- ggplot(plot_data, aes(x=PC2, y =PC3, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q6 <- ggplot(plot_data, aes(x=PC2, y =PC4, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom")
q7 <- ggplot(plot_data, aes(x=PC3, y =PC1, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q8 <- ggplot(plot_data, aes(x=PC3, y =PC2, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q9 <- ggplot(plot_data, aes(x=PC3, y =PC4, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom")
q10 <- ggplot(plot_data, aes(x=PC4, y =PC1, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q11 <- ggplot(plot_data, aes(x=PC4, y =PC2, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none")
q12 <- ggplot(plot_data, aes(x=PC4, y =PC3, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom")
multiplot(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, cols=4)
 
head(data)
test_data <- data[,c(5:20)]
test_data$type
head(test_data)
vowel <- test_data%>%filter(type == 'vowel')
glide <- test_data%>%filter(type == 'glide')
nrow(vowel) + nrow(glide) == nrow(test_data)

matlab_data <- test_data[,c(-1,-3,-4)]
matlab_data$type <- as.character(matlab_data$type)
matlab_data$type[matlab_data$type == 'vowel'] <- 1
matlab_data$type[matlab_data$type == 'glide'] <- 2
group_data <- as.integer(matlab_data[,1])

matlab_data <- matlab_data[,-1]
matlab_data <- t(matlab_data)

glide_group <- glide[,1]
glide_swiss_data <- glide[,c(-1,-2)]
glide_group <- as.character(glide_group)
glide_group[glide_group == 'h'] <- 1
glide_group[glide_group == 'j'] <- 2
glide_group[glide_group == 'w'] <- 3
glide_group <- as.integer(glide_group)

glide_hj <- glide%>% filter(sound == 'h'| sound =='j')
glide_hj_group <-glide_hj[,1]
glide_hj_data <- glide_hj[,c(-1,-2)]
glide_hj_group <- as.character(glide_hj_group)
glide_hj_group[glide_hj_group == 'h'] <- 1
glide_hj_group[glide_hj_group == 'j'] <- 2
glide_hj_group <- as.integer(glide_hj_group)

glide_hw <- glide%>% filter(sound == 'h'| sound =='w')
glide_hw_group <-glide_hw[,1]
glide_hw_data <- glide_hw[,c(-1,-2)]
glide_hw_group <- as.character(glide_hw_group)
glide_hw_group[glide_hw_group == 'h'] <- 1
glide_hw_group[glide_hw_group == 'w'] <- 2
glide_hw_group <- as.integer(glide_hw_group)

glide_jw <- glide%>% filter(sound == 'j'| sound =='w')
glide_jw_group <-glide_jw[,1]
glide_jw_data <- glide_jw[,c(-1,-2)]
glide_jw_group <- as.character(glide_jw_group)
glide_jw_group[glide_jw_group == 'j'] <- 1
glide_jw_group[glide_jw_group == 'w'] <- 2
glide_jw_group <- as.integer(glide_jw_group)

vowel_group <- vowel[,1]
vowel_swiss_data <- vowel[,c(-1,-2)]
vowel_group <- as.character(vowel_group)
vowel_group[vowel_group == 'i'] <- 1
vowel_group[vowel_group == 'u'] <- 2
vowel_group[vowel_group == 'y'] <- 3
vowel_group <- as.integer(vowel_group)

vowel_iu <- vowel%>% filter(sound == 'i'| sound =='u')
vowel_iu_group <- vowel_iu[,1]
vowel_iu_data <-  vowel_iu[,c(-1,-2)]
vowel_iu_group <- as.character(vowel_iu_group)
vowel_iu_group[vowel_iu_group == 'i'] <- 1
vowel_iu_group[vowel_iu_group == 'u'] <- 2
vowel_iu_group <- as.integer(vowel_iu_group)

vowel_iy <- vowel%>% filter(sound == 'i'| sound =='y')
vowel_iy_group <- vowel_iy[,1]
vowel_iy_data <-  vowel_iy[,c(-1,-2)]
vowel_iy_group <- as.character(vowel_iy_group)
vowel_iy_group[vowel_iy_group == 'i'] <- 1
vowel_iy_group[vowel_iy_group == 'y'] <- 2
vowel_iy_group <- as.integer(vowel_iy_group)

vowel_uy <- vowel%>% filter(sound == 'u'| sound =='y')
vowel_uy_group <- vowel_uy[,1]
vowel_uy_data <-  vowel_uy[,c(-1,-2)]
vowel_uy_group <- as.character(vowel_uy_group)
vowel_uy_group[vowel_uy_group == 'u'] <- 1
vowel_uy_group[vowel_uy_group == 'y'] <- 2
vowel_uy_group <- as.integer(vowel_uy_group)


head(two_way_data)
swiss_total <- two_way_data[,-2]

swiss_total_group <- swiss_total[,1]
swiss_total_data <- swiss_total[,-1]
swiss_total_group <- as.character(swiss_total_group)
swiss_total_group[swiss_total_group == 'ji'] <- 1
swiss_total_group[swiss_total_group == 'wu'] <- 2
swiss_total_group[swiss_total_group == 'hy'] <- 3
swiss_total_group <- as.integer(swiss_total_group)

ji_hy <- swiss_total%>%filter(sound =='ji'|sound =='hy')
ji_hy_group <- ji_hy[,1]
ji_hy_data <- ji_hy[,-1]
ji_hy_group <- as.character(ji_hy_group)
ji_hy_group[ji_hy_group == 'ji'] <-1
ji_hy_group[ji_hy_group == 'hy'] <-2
ji_hy_group <- as.integer(ji_hy_group)

ji_uw <- swiss_total%>%filter(sound =='ji'|sound =='wu')
ji_uw_group <- ji_uw[,1]
ji_uw_data <- ji_uw[,-1]
ji_uw_group <- as.character(ji_uw_group)
ji_uw_group[ji_uw_group == 'ji'] <-1
ji_uw_group[ji_uw_group == 'wu'] <-2
ji_uw_group <- as.integer(ji_uw_group)

hy_uw <- swiss_total%>%filter(sound =='hy'|sound =='wu')
hy_uw_group <- hy_uw[,1]
hy_uw_data <- hy_uw[,-1]
hy_uw_group <- as.character(hy_uw_group)
hy_uw_group[hy_uw_group == 'hy'] <-1
hy_uw_group[hy_uw_group == 'wu'] <-2
hy_uw_group <- as.integer(hy_uw_group)

head(test_data)
ji_pair <- test_data[,c(-2,-3,-4)] %>% filter(sound == 'i'| sound == 'j')
ji_pair_group <- as.character(ji_pair$sound)
ji_pair_group[ji_pair_group == 'i'] <- 1
ji_pair_group[ji_pair_group == 'j'] <- 2
ji_pair_group <- as.integer(ji_pair_group)
ji_pair_data <- ji_pair[,-1]


hy_pair <- test_data[,c(-2,-3,-4)] %>% filter(sound == 'h'| sound == 'y')
hy_pair_group <- as.character(hy_pair$sound)
hy_pair_group[hy_pair_group == 'h'] <- 1
hy_pair_group[hy_pair_group == 'y'] <- 2
hy_pair_group <- as.integer(hy_pair_group)
hy_pair_data <- hy_pair[,-1]


wu_pair <- test_data[,c(-2,-3,-4)] %>% filter(sound == 'w'| sound == 'u')
wu_pair_group <- as.character(wu_pair$sound)
wu_pair_group[wu_pair_group == 'w'] <- 1
wu_pair_group[wu_pair_group == 'u'] <- 2
wu_pair_group <- as.integer(wu_pair_group)
wu_pair_data <- wu_pair[,-1]

type_pair <- test_data[,c(-1,-3,-4)]
type_pair_group <- as.character(type_pair[,1])
type_pair_group[type_pair_group =='vowel']<-1
type_pair_group[type_pair_group == 'glide']<-2
type_pair_group <- as.integer(type_pair_group)
type_pair_data <- type_pair[,-1]



##############################################################################
## SWISS.R
## Compute the standardized within class sum of square score.
## Author: Meilei
##############################################################################


swiss = function(dat, class){
  # @ dat: data matrix, rows are samples and columns are features
  # @ class: class label of samples 
  group = unique(class)
  gpairs = combn(group,2)
  n = dim(gpairs)[2]
  sw = NULL
  if(is.null(n)){
    g1 = gpairs[1]
    g2 = gpairs[2]
    c1 = as.matrix(dat[which(class == g1),])
    c2 = as.matrix(dat[which(class == g2),])
    c = rbind(c1, c2)
    
    sc1 = scale(c1, center = T, scale = F)
    sc2 = scale(c2, center = T, scale = F)
    sc  = scale(c, center = T, scale = F)
    sw = (norm(sc1,"F")^2 + norm(sc2,"F")^2)/norm(sc,"F")^2
  }else{
    for(i in 1:n){
      g1 = gpairs[1,i]
      g2 = gpairs[2,i]
      c1 = as.matrix(dat[which(class == g1),])
      c2 = as.matrix(dat[which(class == g2),])
      c = rbind(c1, c2)
      
      sc1 = scale(c1, center = T, scale = F)
      sc2 = scale(c2, center = T, scale = F)
      sc  = scale(c, center = T, scale = F)
      sw[i] = (norm(sc1,"F")^2 + norm(sc2,"F")^2)/norm(sc,"F")^2
    }
  }
  
  return(mean(sw))
}

swiss(glide_swiss_data,glide_group)
swiss(glide_hj_data,glide_hj_group)
swiss(glide_hw_data,glide_hw_group)
swiss(glide_jw_data,glide_jw_group)

swiss(vowel_swiss_data,vowel_group)
swiss(vowel_iu_data,vowel_iu_group)
swiss(vowel_iy_data,vowel_iy_group)
swiss(vowel_uy_data,vowel_uy_group)

swiss(swiss_total_data, swiss_total_group)
swiss(ji_hy_data, ji_hy_group)
swiss(ji_uw_data, ji_uw_group)
swiss(hy_uw_data, hy_uw_group)

swiss(type_pair_data,type_pair_group)
swiss(ji_pair_data,ji_pair_group)
swiss(hy_pair_data,hy_pair_group)
swiss(wu_pair_data,wu_pair_group)


###################################################
# Use fewer features to for separation.
##################################################


####
# less than 4 features, how to get lowest swiss score
####
library(utils)
swiss_score_vector <- n_vector <- sub_n_vector <- c()
for (n in c(2,3,4)){
  names <- colnames(type_pair_data)
  sub_n <- ncol(combn(names,n))
  for (item in c(1:sub_n)){
    swiss_score <- swiss(type_pair_data[,combn(names,n)[,item]],type_pair_group)
    swiss_score_vector <- c(swiss_score_vector,swiss_score)
    n_vector <- c(n_vector,n)
    sub_n_vector <- c(sub_n_vector,item)
  }
} 

min(swiss_score_vector)
best_n <- n_vector[which.min(swiss_score_vector)]
best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
combn(names,best_n)[,best_sub_n]

###################################################################
swiss_score_vector <- n_vector <- sub_n_vector <- c()
for (n in c(2,3,4)){
  names <- colnames(ji_pair_data)
  sub_n <- ncol(combn(names,n))
  for (item in c(1:sub_n)){
    swiss_score <- swiss(ji_pair_data[,combn(names,n)[,item]],ji_pair_group)
    swiss_score_vector <- c(swiss_score_vector,swiss_score)
    n_vector <- c(n_vector,n)
    sub_n_vector <- c(sub_n_vector,item)
  }
} 

min(swiss_score_vector)
best_n <- n_vector[which.min(swiss_score_vector)]
best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
combn(names,best_n)[,best_sub_n]

###############################
swiss_score_vector <- n_vector <- sub_n_vector <- c()
names <- colnames(hy_pair_data)
for (n in c(2,3,4)){
  sub_n <- ncol(combn(names,n))
  for (item in c(1:sub_n)){
    swiss_score <- swiss(hy_pair_data[,combn(names,n)[,item]],hy_pair_group)
    swiss_score_vector <- c(swiss_score_vector,swiss_score)
    n_vector <- c(n_vector,n)
    sub_n_vector <- c(sub_n_vector,item)
  }
} 

min(swiss_score_vector)
best_n <- n_vector[which.min(swiss_score_vector)]
best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
combn(names,best_n)[,best_sub_n]
##################
swiss_score_vector <- n_vector <- sub_n_vector <- c()
names <- colnames(wu_pair_data)
for (n in c(2,3,4)){
  sub_n <- ncol(combn(names,n))
  for (item in c(1:sub_n)){
    swiss_score <- swiss(wu_pair_data[,combn(names,n)[,item]],wu_pair_group)
    swiss_score_vector <- c(swiss_score_vector,swiss_score)
    n_vector <- c(n_vector,n)
    sub_n_vector <- c(sub_n_vector,item)
  }
} 

min(swiss_score_vector)
best_n <- n_vector[which.min(swiss_score_vector)]
best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
combn(names,best_n)[,best_sub_n]
##################


################################
# in glide!!!!
#############################


glide_data_list <- list(glide_swiss_data[,c(-1,-2)], glide_hj_data[,c(-1,-2)], glide_hw_data[,c(-1,-2)], glide_jw_data[,c(-1,-2)])
glide_group_list <- list(glide_group, glide_hj_group, glide_hw_group, glide_jw_group)
names_new <- c('swiss', 'hj','hw','jw')
for (list_num in c(1:4)){
print(paste('this is ', names_new[list_num] ,'group'))
swiss_score_vector <- n_vector <- sub_n_vector <- c()
names <- colnames(as.data.frame(glide_data_list[list_num]))
for (n in c(2,3,4)){
  sub_n <- ncol(combn(names,n))
  for (item in c(1:sub_n)){
    swiss_score <- swiss(as.data.frame(glide_data_list[list_num])[,combn(names,n)[,item]],unlist(glide_group_list[list_num]))
    swiss_score_vector <- c(swiss_score_vector,swiss_score)
    n_vector <- c(n_vector,n)
    sub_n_vector <- c(sub_n_vector,item)
  }
} 
print(min(swiss_score_vector))
best_n <- n_vector[which.min(swiss_score_vector)]
best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
print(combn(names,best_n)[,best_sub_n])
}
############################################
#swiss(vowel_swiss_data,vowel_group)
#swiss(vowel_iu_data,vowel_iu_group)
#swiss(vowel_iy_data,vowel_iy_group)
#swiss(vowel_uy_data,vowel_uy_group)
#############################################

vowel_data_list <- list(vowel_swiss_data[,c(-1,-2)], vowel_iu_data[,c(-1,-2)], vowel_iy_data[,c(-1,-2)], vowel_uy_data[,c(-1,-2)])
vowel_group_list <- list(vowel_group, vowel_iu_group, vowel_iy_group, vowel_uy_group)
names_new <- c('swiss', 'iu','iy','uy')
for (list_num in c(1:4)){
  print(paste('this is ', names_new[list_num] ,'group'))
  swiss_score_vector <- n_vector <- sub_n_vector <- c()
  names <- colnames(as.data.frame(vowel_data_list[list_num]))
  for (n in c(2,3,4)){
    sub_n <- ncol(combn(names,n))
    for (item in c(1:sub_n)){
      swiss_score <- swiss(as.data.frame(vowel_data_list[list_num])[,combn(names,n)[,item]],unlist(vowel_group_list[list_num]))
      swiss_score_vector <- c(swiss_score_vector,swiss_score)
      n_vector <- c(n_vector,n)
      sub_n_vector <- c(sub_n_vector,item)
    }
  } 
  print(min(swiss_score_vector))
  best_n <- n_vector[which.min(swiss_score_vector)]
  best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
  print(combn(names,best_n)[,best_sub_n])
}

#################################
#swiss(swiss_total_data, swiss_total_group)
#swiss(ji_hy_data, ji_hy_group)
#swiss(ji_uw_data, ji_uw_group)
#swiss(hy_uw_data, hy_uw_group)

total_data_list <- list(swiss_total_data, ji_hy_data, ji_uw_data, hy_uw_data)
total_group_list <- list(swiss_total_group, ji_hy_group, ji_uw_group, hy_uw_group)
names_new <- c('swiss', 'ji_hy','ji_uw','hy_uw')
for (list_num in c(1:4)){
  print(paste('this is ', names_new[list_num] ,'group'))
  swiss_score_vector <- n_vector <- sub_n_vector <- c()
  names <- colnames(as.data.frame(total_data_list[list_num]))
  for (n in c(2,3,4)){
    sub_n <- ncol(combn(names,n))
    for (item in c(1:sub_n)){
      swiss_score <- swiss(as.data.frame(total_data_list[list_num])[,combn(names,n)[,item]],unlist(total_group_list[list_num]))
      swiss_score_vector <- c(swiss_score_vector,swiss_score)
      n_vector <- c(n_vector,n)
      sub_n_vector <- c(sub_n_vector,item)
    }
  } 
  print(min(swiss_score_vector))
  best_n <- n_vector[which.min(swiss_score_vector)]
  best_sub_n<- sub_n_vector[which.min(swiss_score_vector)]
  print(combn(names,best_n)[,best_sub_n])
}



scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scientific_format()(x)))
}


test_data = test_data[sample(nrow(test_data)),] 
c1 <- ggplot(test_data, aes(x=F1, y =F2, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none") +scale_y_continuous(label=scientific_format())
c2 <- ggplot(test_data, aes(x=F1, y =intensity, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none") +scale_y_continuous(label=scientific_format())
c3 <- ggplot(test_data, aes(x=F1, y =duration, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom") 
c4 <- ggplot(test_data, aes(x=F2, y =F1, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) +scale_x_continuous(label=scientific_format())
c5 <- ggplot(test_data, aes(x=F2, y =intensity, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) +scale_y_continuous(label=scientific_format())  +scale_x_continuous(label=scientific_format())
c6 <- ggplot(test_data, aes(x=F2, y =duration, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1)) +scale_x_continuous(label=scientific_format())
c7 <- ggplot(test_data, aes(x= intensity, y = F1, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))  +scale_x_continuous(label=scientific_format())
c8 <- ggplot(test_data, aes(x= intensity, y = F2, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) +scale_y_continuous(label=scientific_format()) +scale_x_continuous(label=scientific_format())
c9 <- ggplot(test_data, aes(x= intensity, y =duration, color = type)) + geom_point(alpha= 0.5) + theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))  +scale_x_continuous(label=scientific_format())
c10 <- ggplot(test_data, aes(x= duration, y = F1, color = type)) + geom_point(alpha= 0.5)+ theme(legend.position = "none")
c11 <- ggplot(test_data, aes(x= duration, y = F2, color = type)) + geom_point(alpha= 0.5)+ theme(legend.position = "none") +scale_y_continuous(label=scientific_format())
c12 <- ggplot(test_data, aes(x= duration, y = intensity, color = type)) + geom_point(alpha= 0.5)+ theme(legend.position = "bottom")+scale_y_continuous(label=scientific_format())
multiplot(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12,  cols=4)


######################################################################
# use One way anova and box plot to detect differences between phonemes in same type.
######################################################################
# first we use glide data 
glide_data <- test_data%>% filter(type == 'glide')
vowel_data <- test_data%>% filter(type == 'vowel')

ggplot(glide_data, aes(x = sound, y = duration)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Duration of each glide sound")

ggplot(glide_data, aes(x = sound, y = F4_bandwidth)) +  # F1 works   # F2 significant!  #F3 also works  # F4 works
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Duration of each glide sound")

ggplot(vowel_data, aes(x = sound, y = F4_bandwidth)) +   # F2 works # F3 probably # F4 important
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Duration of each glide sound")
#################################################################
options(contrasts=c("contr.treatment", "contr.treatment"))
lm_glide_1 <- lm(F1 ~ sound, data = glide_data) # F1
summary(lm_glide_1)

lm_glide_2 <- lm(F2 ~ sound, data = glide_data) # F2
summary(lm_glide_2) 

lm_glide_3 <- lm(F3 ~ sound, data = glide_data) # F3
summary(lm_glide_3) 

lm_glide_4 <- lm(F4 ~ sound, data = glide_data) # F4
summary(lm_glide_4) 
# all these four are significant!
##################################################
options(contrasts=c("contr.treatment", "contr.treatment"))

lm_vowel_2 <- lm(F2 ~ sound, data = vowel_data) # F2  less than e-16
summary(lm_vowel_2) 

lm_vowel_3 <- lm(F3 ~ sound, data = vowel_data) # F3 less than e-16
summary(lm_vowel_3) 

lm_vowel_4 <- lm(F4 ~ sound, data = vowel_data) # F4 less than e-16
summary(lm_vowel_4)

lm_vowel_5 <- lm(duration ~ sound, data = vowel_data) # duration works!
summary(lm_vowel_5)

lm_vowel_6 <- lm(AvePitch ~ sound, data = vowel_data) # AvePitch works!
summary(lm_vowel_6)

lm_vowel_7 <- lm(F1_bandwidth ~ sound, data = vowel_data) # F1_bandwidth
summary(lm_vowel_7)

lm_vowel_8 <- lm(F2_bandwidth ~ sound, data = vowel_data) # F2_bandwidth
summary(lm_vowel_8)

lm_vowel_9 <- lm(F3_bandwidth ~ sound, data = vowel_data) # F3_bandwidth
summary(lm_vowel_9)

lm_vowel_10 <- lm(F4_bandwidth ~ sound, data = vowel_data) # F4_bandwidth really significant
summary(lm_vowel_10)
'F1, F2, F3, F4, duration, AvePitch, F1_bandwidth, F2_bandwidth, F3_bandwidth, F4_bandwidth'

###################################################
lm_all_1 <- lm(F1 ~ sound, data = test_data) # Only F1 significant in all six sounds
summary(lm_all_1)
################################################


head(vowel_data)

summary(aov(duration ~ sound, data = vowel_data))
summary(aov(intensity ~ sound, data = vowel_data))
summary(aov(AvePitch ~ sound, data = vowel_data))
summary(aov(AveHarmonicity ~ sound, data = vowel_data))
summary(aov(F1 ~ sound, data = vowel_data))
summary(aov(F2 ~ sound, data = vowel_data))
summary(aov(F3 ~ sound, data = vowel_data))
summary(aov(F4 ~ sound, data = vowel_data))
summary(aov(F1_bandwidth ~ sound, data = vowel_data))
summary(aov(F2_bandwidth ~ sound, data = vowel_data))
summary(aov(F3_bandwidth ~ sound, data = vowel_data))
summary(aov(F4_bandwidth ~ sound, data = vowel_data))


head(glide_data)

summary(aov(duration ~ sound, data = glide_data))
summary(aov(intensity ~ sound, data = glide_data))
summary(aov(AvePitch ~ sound, data = glide_data))
summary(aov(AveHarmonicity ~ sound, data = glide_data))
summary(aov(F1 ~ sound, data = glide_data))
summary(aov(F2 ~ sound, data = glide_data))
summary(aov(F3 ~ sound, data = glide_data))
summary(aov(F4 ~ sound, data = glide_data))
summary(aov(F1_bandwidth ~ sound, data = glide_data))
summary(aov(F2_bandwidth ~ sound, data = glide_data))
summary(aov(F3_bandwidth ~ sound, data = glide_data))
summary(aov(F4_bandwidth ~ sound, data = glide_data))



