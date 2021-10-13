rm(list = ls()) #Clearing the current workspace
Gas_vapor_data<-data.frame(y=c(29,24,26,22,27,21,33,34,32,34,20,36,34,23,24,32,40,46,55,52,29,22,31,45,37,37,33,27,34,19,16,22), x1=c(33,31,33,37,36,35,59,60,59,60,34,60,60,60,62,62,90,90,92,91,61,59,88,91,63,60,60,59,59,37,35,37), x2=c(53,36,51,51,54,35,56,60,60,60,35,59,62,36,38,61,64,60,92,92,62,42,65,89,62,61,62,62,62,35,35,37), x3=c(3.32,3.1,3.18,3.39,3.2,3.03,4.78,4.72,4.6,4.53,2.9,4.4,4.31,4.27,4.41,4.39,7.32,7.32,7.45,7.26,4.08,3.45,5.8,6.6,4.3,4.1,3.89,4.02,4.53,2.64,2.59,2.59), x4=c(3.42,3.26,3.18,3.08,3.41,3.03,4.57,4.72,4.41,4.53,2.95,4.36,4.42,3.94,3.49,4.39,6.7,7.2,7.45,7.26,4.08,3.45,5.8,6.6,4.3,4.1,3.89,4.02,4.53,2.64,2.59,2.59))
Gas_vapor_data
summary(Gas_vapor_data$x3)
summary(Gas_vapor_data)
?ggplot2
?ggplot
library(ggplot2)
gg=ggplot(data = Gas_vapor_data, aes(x1,y))+geom_smooth(method = "lm")+geom_point()
gg= gg+xlab("Tank temperature")+ylab("Gasoline temperature")
gg
qq=ggplot(data = Gas_vapor_data, aes(x1,y))+geom_smooth(method = "loess",span=1)+geom_point()
qq= qq+xlab("Tank temperature")+ylab("Gasoline temperature")
qq

#Assess Normality  of Y
#Shapiro Wilk Test(Null:distribution is normal)
shapiro.test(Gas_vapor_data$y)
shapiro.test(Gas_vapor_data$x1)

#Histogram w Overlay Normal - Basic
#Command is histogram in package=mosaic
#Tip - might want to tweak width=1000
library(mosaic)
histogram(Gas_vapor_data$x1, width=1000,main="Tank temperature w Overlay Normal",xlab = "Tank temperature", fit="normal")
#For ggplot2 fans
library(ggplot2)

#Histogram w Overlay Normal - w Aesthetics
#Tip - might want to tweak binwidth=1000
#ggplot(DATAFRAME, aes(x=VARIABLENAME)) + stuff below
ff<-ggplot(Gas_vapor_data, aes(x=x1))
ff<-ff + geom_histogram(binwidth = 100, colour="yellow", aes(y=..density..))
ff<-ff + stat_function(fun = dnorm,
                       color="red",
                       args=list(mean=mean(Gas_vapor_data$x1),
                                 sd=sd(Gas_vapor_data$x1)))
ff<-ff + ggtitle("Distribution of x1 w Overlay Normal")
ff<-ff + xlab("Tank temperature") + ylab("Density")
plot_histogramx1<- ff + theme_bw()
plot_histogramx1

#fit a model using the x variables
model1 <- lm(Gas_vapor_data$y~Gas_vapor_data$x1+Gas_vapor_data$x2+Gas_vapor_data$x3+Gas_vapor_data$x4)
model1

#Get summary of the model
summary(model1)

#Calculate Pearson's correlation between the regressors
cor(Gas_vapor_data$x1,Gas_vapor_data$y, method="pearson")
cor(Gas_vapor_data$x2,Gas_vapor_data$y, method="pearson")
cor(Gas_vapor_data$x3,Gas_vapor_data$y, method="pearson")
cor(Gas_vapor_data$x4,Gas_vapor_data$y, method="pearson")

#Ask for confidence interval for the model coefficients
confint(model1, conf.level=0.95)

#Check the regression diagnostic plots for the above model
plot(model1)

#convert numeric to factor
?cut

#We will create x1 categories of A=<20, B=20-35, C=35-55,
#                                D=55-70, E=70-90, F=90+
Catx1 <- cut(Gas_vapor_data$x1, breaks = c(0,20,35,55,70,90,100), labels=c("A","B","C","D","E","F"))
Catx1
Catx1[1:20]
Gas_vapor_data$x1[1:20]
