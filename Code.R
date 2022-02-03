#Anooshka Bajaj

library(ggplot2)
library(readxl)
library(ggpubr)
library(dplyr)
library(tidyr)
library(MASS)
library(scales)
library(ggeasy)
library(epitools)
library(tibble)

rm (list = ls(all = T)) 

------------------------------------------------------------------------------------
#2.1 Two-way ANOVA
-----------------------------------------------------------------------------------    

#1
r1 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/Data_LA2.xlsx", sheet = 1)
df <- data.frame(r1)          #creating data frame of given excel sheet


df5 <- data.frame(df[2],df[3],df[4],df[5],df[6],df[7])
df5_1 <- data.frame(Value = unlist(df5))

Group <- c(rep("Placebo",25),rep("Drug A",25),rep("Drug B",25),rep("Placebo",25),rep("Drug A",25),rep("Drug B",25))
Factor <- c(rep("Exercise",75),rep("Diet",75))
df5_1['Group'] <- Group
df5_1['Factor'] <- Factor

df5_1 <- df5_1 %>%relocate(Value, .after = Factor)
view(df5_1)                   #creating required data frame (3XN)    


-----------------------------------------------------------------------------------
#2
                              #2 way ANOVA for Groups and Factors
anova2 <- aov(Value ~ Group + Factor, data = df5_1)
summary(anova2)

                              #2 way ANOVA for Interaction between Factors and Groups
anova2_1 <- aov(Value ~ Group * Factor, data = df5_1)
anova2_1 <- aov(Value ~ Group + Factor + Group:Factor, data = df5_1)
summary(anova2_1)


-----------------------------------------------------------------------------------
#3
                              #creating box plot using ggplot                 
p1 <- ggplot(data=df5_1, aes(x=Group,y=Value)) + 
      geom_boxplot(aes(fill=Factor)) +
      labs(x="Groups", y="Cholesterol Level LDL [mg/L]", fill="Factor")+
      ggtitle("Cholesterol Level of Different Groups") +
      theme_bw () +
      theme (axis.text = element_text(size = 12) ,
            axis.title = element_text(size = 12) ,
            plot.title = element_text(hjust = 0.5))
plot (p1)


                              #creating the interaction plot
interaction.plot(x.factor = df5_1$Group, trace.factor = df5_1$Factor, 
                 response = df5_1$Value, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Groups", ylab="Cholesterol Level LDL [mg/L]",
                 main = "Interaction Plot",
                 pch=c(1,19), col = c("#FA8072", "#20B2AA"))


-----------------------------------------------------------------------------------
#4
tukeytest <- TukeyHSD(anova2 , ordered = TRUE )  #conducting post-hoc test (Tukey test)
tukeytest


-----------------------------------------------------------------------------------
#2.2 Linear Regression
-----------------------------------------------------------------------------------

#5
r2 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/Data_LA2.xlsx", sheet = 2)      #importing the excel data set

dff <- data.frame(r2)         #data frame of the required excel sheet
view(dff)   

meanx <- mean(dff$x)          #mean and variance of explanatory variable
varx <- var(dff$x)
meany <- mean(dff$y)          #mean and variance of dependent variable
vary <- var(dff$y)

meanx
varx

meany
vary

p2 <- ggplot(dff, aes(x=x, y=y))+geom_point() +                            #scatter plot
   labs(title="Cholesterol Level and Animal Weight Scatter Plot", x="Weight of animal species [mg]", y="Cholesterol level LDL [mg/L]") + 
   theme(plot.title = element_text(hjust = 0.5, size=12,face="bold"))
plot(p2)


-----------------------------------------------------------------------------------
#6
x <- dff$x
y <- dff$y
model <- glm(y ~ x)           #glm(dependent variable ~ explanatory variable)
summary(model)


-----------------------------------------------------------------------------------
#7
m <- (sum((x - meanx)*(y - meany)))/(sum((x - meanx)^2))
b <- meany - m*meanx
regression <- m*x + b
 
                             #computing Standard Errors (SE)
RSS <- sum((regression - y)^2)              
SEm <-  sqrt((RSS) / ((length(y)-2)*sum((x-meanx)^2)))           

fac1 <- RSS / (length(y)-2)
sum1 <- 1/length(y)
sum2 <- meanx^2/(sum((x-meanx)^2))
SEb <-  sqrt(fac1*(sum1+sum2))

                              #computing confidence intervals with alpha = 5% and df = 98
CI_mmax <- m+qt(0.975,98)*SEm       
CI_mmin <- m-qt(0.975,98)*SEm 

CI_bmax <- b+qt(0.975,98)*SEb 
CI_bmin <- b-qt(0.975,98)*SEb 

CI_mmax                       #confidence interval for m
CI_mmin 
CI_bmax                       #confidence interval for b
CI_bmin 


-----------------------------------------------------------------------------------
#8
Reg_max <- CI_bmax + CI_mmin*x   #linear regression with larger slope and smaller y-intercept
Reg_min <- CI_bmin + CI_mmax*x   #linear regression with smaller slope

p3 <- ggplot()+ 
   geom_point(aes(x=x, y=y))+                         #scatter Plot
   geom_line(aes(x=x, y=regression), color = "blue")+ #optimal regression line
   geom_line(aes(x=x, y=Reg_max), color = "red")+     #regression with upper-limit slope 
   geom_line(aes(x=x, y=Reg_min), color = "red")+     #regression with lower-limit slope 
   geom_smooth(aes(x=x, y=y), method = lm)+           #auto-generated slope
   stat_regline_equation(aes(x=x, y=y))+              #inserting regression equation 
   theme_bw()+  
   labs(x ="Weight of animal species  [mg]",                             
        y = "Cholesterol level LDL [mg/L]")+
   ggtitle("Cholesterol Level Dependence on Animal Weight")+
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=12), 
         plot.title = element_text(hjust = 0.5, face= "bold"))  
plot(p3)


-----------------------------------------------------------------------------------
#9
plot(x, resid(model),pch=16,                          #scatter plot of residuals
        xlab="Weight of animal species [mg]",
        ylab="Residuals",
        main="Scatter Plot of Residuals")

abline(h=0, lwd=2, lty=2, 
       col="blue")












