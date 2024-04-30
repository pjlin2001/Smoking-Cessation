data <- read.table(file='C:\\Users\\Phili\\Downloads\\smoking.txt')
head(data)
treatment <- factor(data$trt)
block1 <- factor(data$pa)
block2 <- factor(data$age)
block3 <- factor(data$sex)
block4 <- factor(data$race)

# Fit an RCBD model with treatment and block effects
rcbd_model1 <- aov(status ~ treatment + Error(block1/treatment), data = data)
rcbd_model2 <- aov(status ~ treatment + Error(block2/treatment), data = data)
rcbd_model3 <- aov(status ~ treatment + Error(block3/treatment), data = data)
rcbd_model4 <- aov(status ~ treatment + Error(block4/treatment), data = data)
rcbd_model5 <- aov(status ~ treatment + Error((block1+block3)/treatment), data = data)

# View the ANOVA table
summary(rcbd_model1)
summary(rcbd_model2)
summary(rcbd_model3)
summary(rcbd_model4)
summary(rcbd_model5)
