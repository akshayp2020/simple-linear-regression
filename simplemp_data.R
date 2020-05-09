emp_data = read.csv(choose.files())
View(emp_data)
summary(emp_data)

# box plot
boxplot(emp_data$Salary_hike,emp_data$Churn_out_rate)

#scatter plot
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)

#correlation
asl1 <- (emp_data$Salary_hike)
comp <-(emp_data$Churn_out_rate)
cor(asl1,comp)

#model build model name asl2
asl2 <- lm(asl1 ~ comp) # lm(Y ~ X)
summary(asl2)  #rsquared value 0.8312

#prediction
pred <- predict(asl2)
pred
asl2$residuals
sum(asl2$residuals)
mean(asl2$residuals)  #3.55


#RMSE value
sqrt(sum(asl2$residuals^2)/nrow(emp_data))
sqrt(mean(asl2$residuals^2))

#confidance interval
confint(asl2,level=0.95)
predict(asl2,interval="predict")

#Cubic model1
cub_log <- lm(Churn_out_rate~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3),data=emp_data)
summary(cub_log) # R-srquared 0.9893,adjusted 0.984
confint(cub_log,levels=0.95)
pred <-predict(cub_log, interval = "predict")
pred

cub_log$residuals
sum(cub_log$residuals)
mean(cub_log$residuals)

rmse <- sqrt(mean((pred-Churn_out_rate)^2))
rmse
plot(cub_log)
