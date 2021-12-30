# Project Title: PIMA Diabetes in Indian Women
# NAME: Anuradha Vaidya
# EMAIL: anuvaidya11@gmail.com
# COLLEGE: IIT, Madras

pima.df <- read.csv(paste("pima.csv", sep = ""))

View = pima.df
summary(pima.df)

head(pima.df)

plot(sort(pima.df$bp))

hist(pima.df$bp)

plot(density(pima.df$bp ,na.rm=TRUE))

plot(triceps~bmi , pima.df)

boxplot(diabetes ~ class , pima.df)






library(car)

boxplot(pima.df$diabetes ~ pima.df$glucose, data=pima.df, horizontal=TRUE, yaxt="n",
        ylab="glucose", xlab="diabetes", 
        main="Factors affecting diabetes in pima indian women") 



library(car)

scatterplot(diabetes ~ glucose, data=pima.df, 
            spread=FALSE, smoother.args=list(lty=2), 
            pch=19, main="Scatterplot of diabetes vs. glucose", 
            xlab="glucose", ylab="diabetes")


library(car)

scatterplot(diabetes ~ bmi, data=pima.df, 
            spread=FALSE, smoother.args=list(lty=2), 
            pch=19, main="Scatterplot of diabetes vs. bmi", 
            xlab="bmi", ylab="diabetes")

cor.test(pima.df[,"diabetes"], pima.df[,"glucose"])

library(car) 
scatterplotMatrix(pima.df[,c("diabetes","glucose","bmi")], 
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")


model <- lm(pima.df$diabetes ~ pima.df$glucose , data=pima.df) #the '.' means 'all' 
summary(model)

model <- lm(pima.df$diabetes ~ . , data=pima.df) #the '.' means 'all' 
summary(model)


model <- lm(pima.df$diabetes ~  pima.df$glucose  + pima.df$bmi + 
              pima.df$bp, data=pima.df) #the '.' means 'all' 
summary(model)









