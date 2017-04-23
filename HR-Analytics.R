library(plyr)
library(dplyr)
library(ggplot2)
library(caTools) # for sample splitting
library(RColorBrewer)
library(rattle)
library(rpart.plot)
library(ellipse)
library(car)
library(faraway)
library(ROCR)

# PROBLEM STATEMENT
# The specific goal here is to predict whether an employee will stay or voluntarily leave within the next year. In the present data, this means predicting the variable "vol_leave" (0 = stay, 1 = leave) using the other columns of data. You can think of this data as historical data which tells us who did and who did not leave within the last year.

# DATA
HRAnalytics <- read.csv("C:/Users/Shraddha Somani/Desktop/humanresource.csv")
str(HRAnalytics)
head(HRAnalytics)
summary(HRAnalytics)
# Analysis:
# - The summary information lets us know that we have 5 fundamental roles: CEO, Director, Individual Contributors, Manager and VP. 
# - Since CEOs and VPs encounter an altogether different labor market than the Directors, Managers, and Individuals, incorporating them in our modeling doesn't bode well.
# - Resetting the data
HRAnalytics = filter(HRAnalytics, HRAnalytics$role == "Ind" | HRAnalytics$role == "Manager" | HRAnalytics$role == "Director")
HRAnalytics$role <- factor(HRAnalytics$role) 
summary(HRAnalytics)

# VISUALIZATION
# As the response output variable consist of two groups (0, 1), comparing it with other columns would be much easier if we use aggregate along with the mean function.

# (a) Performance v/s Voluntarily Leaving
performance_agg = aggregate(vol_leave ~ perf, data = HRAnalytics, mean)
performance_agg
ggplot(performance_agg, aes(x = perf, y = vol_leave)) + geom_bar(stat = "identity", fill = 'turquoise', colour = 'black') + ggtitle("Voluntary Leaving Rate by Performance Rating") + labs(y = "Proportion Leaving", x = "Performance Rating") 
# Analysis:
# - Employees with performance rating 3 are likely to leave the company next year

# (b) Sex v/s Voluntarily Leaving
sex_agg = aggregate(vol_leave ~ sex, data = HRAnalytics, mean)
sex_agg
ggplot(sex_agg, aes(x = sex, y = vol_leave)) + geom_bar(stat = "identity", fill = 'turquoise', colour = 'black') + ggtitle("Voluntary Leaving Rate by Sex") + labs(y = "Proportion Leaving", x = "Sex")
# Analysis:
# - Female attrition rate is higher than the males in the entire organization
# - Females are more prone to voluntarily leaving the company

# (c) Business Area v/s Voluntarily Leaving
area_agg = aggregate(vol_leave ~ area, data = HRAnalytics, mean)
area_agg
ggplot(area_agg, aes(x = area, y = vol_leave, fill = area)) + geom_bar(stat = "identity", colour = "black") + scale_fill_brewer() + ggtitle("Voluntary Leaving Rate by Business Area") + labs(y = "Proportion Leaving", x = "Business Area")
# Analysis:
# - People working in Sales department are much more likely to leave the job reason being:
#   - Most sales jobs are paid less and mundane
#   - No fixed working hours
#   - Work timing extends to late nights as well.

# (d) Business Area and Gender v/s Voluntarily Leaving
area_sex_agg = aggregate(vol_leave ~ area + sex, data = HRAnalytics, mean)
area_sex_agg
ggplot(area_sex_agg, aes(x = area, y = vol_leave)) + geom_bar(aes(fill = sex), stat = "identity", colour = "black", position = position_dodge()) + ggtitle("Voluntary Leaving Rate by Area & Sex") + labs(y = "Proportion Leaving", x = "Business Area") 
# Analysis: 
# - Voluntary termination is higher in females.
# - Under sales department, all employees are nearly unhappy

# (e)	Role v/s Voluntarily Leaving
role_agg = aggregate(vol_leave ~ role, data = HRAnalytics, mean)
role_agg
ggplot(role_agg, aes(x = role, y = vol_leave, fill = role)) + geom_bar(stat = "identity", width = .7, colour = 'black') + scale_fill_brewer() + ggtitle("Voluntary Leaving Rate by Role") + labs (y = "Proportion Leaving", x = "Role") 
# Analysis:
# - Managers have higher attrition rate
# - Directors have a longer run at a company.

# (f) Analyzing the age of the employee
hist(HRAnalytics$age, breaks = 100, main = "Age Distribution", border = F, xlab = "Age", col = 'turquoise')
quantile(HRAnalytics$age)
# Analysis:
# - Skewness is present here with half of our workforce somewhere around 22 and 26 years old.
# - However there are three distinct levels: people, supervisors and executives. It will be more informative to see how those ages breakdown when we take that into account. Therefore box plots have been utilized for this purpose.

boxplot(age ~ role, data = HRAnalytics, col = 'turquoise', xlab = 'Role', ylab = 'Age', main = 'Role v/s Age')
# Analysis:
# - There is a solid relationship between role and age
# - Since the variable age is skewed, we will take the log of age while fitting a model.

HRAnalytics$log_age = log(HRAnalytics$age)
summary(HRAnalytics$log_age)

# Segmenting age variable even further to get proper insights
age_agg = aggregate(x = HRAnalytics$vol_leave, by = list(cut(HRAnalytics$age, 10)), mean)
age_agg
names(age_agg) = c("Age", "Probability")
ggplot(age_agg, aes(x = Age, y = Probability, fill = Age)) + geom_bar(stat = "identity", width = .7, colour = 'black') + scale_fill_brewer() + ggtitle("Voluntary Leaving Rate by Role and Age") + labs(y = "Proportion Leaving", x = "Age Cut") 
# Analysis:
# - This shows that People within 34-54 age group terminate the company more likely than the people within 22-34 who might be individual employees.
# - Age group of 54- 62 is at Director level and the attrition is least in that age group.

# (g) Analyzing the salary pattern
summary(HRAnalytics$salary)
quantile(HRAnalytics$salary, probs = seq(0,1,.2))
hist(HRAnalytics$salary, breaks = 50, col = 'turquoise', main = "Salary Distribution", xlab = "Salary")
# Analysis:
# - The median salary is 60800, with the max being 1000000 and the min being 42170.
# - Salary variable is highly skewed with almost 80% of the people earning till $66173.65.
# - Segmenting salary division based on role.

salary_agg = aggregate(salary ~ role, data = HRAnalytics, median)
salary_agg
# Plot
boxplot(salary ~ role, data = HRAnalytics, col = 'turquoise', xlab = 'Role', ylab = 'Salary', main = 'Role v/s Salary')

# Creating normalized variable based on median values obtained above
names(salary_agg)[2] = "role_mean_salary" 
HRAnalytics = merge(HRAnalytics, salary_agg, by = "role")
HRAnalytics$salary_diff = HRAnalytics$salary - HRAnalytics$role_mean_salary

# Analyzing normalized salary
hist(HRAnalytics$salary_diff, breaks = 50, main = "Distribution of Salary Differences \n from Role Median Salary", col = 'turquoise', xlab = "Difference from Median")

# DATA MODELING
# Before we start creating models, we need to split our data into a training set and a test set. We will utilize two-thirds of the data for training and model development and one third of the data for testing the models.
# We set the random seed to a particular number so we can simply replicate our outcomes.
set.seed(42) # setting the random seed for replication
sample = sample.split(HRAnalytics$vol_leave, 2/3)
train = HRAnalytics[sample,]
test = HRAnalytics[!sample,]

# We will be using two techniques,
# (a) Logistic Regression 
# (b) Decision Tree
# Logistic regression builds a condition that as a result predicts the probability of a two-class result (staying or leaving) utilizing the chosen indicators. Each of the indicators are connected with a "significance"" pointer that lets you know whether the indicator is helpful or not. 
# On contrast, decision trees work by utilizing the indicators to part the data into buckets using a set of decision rules.

# (a) LOGESTIC REGRESSION
test_mean = mean(test$vol_leave)
train_mean = mean(train$vol_leave)
print(c(test_mean, train_mean))

# (1) Fit the model
fit = glm(vol_leave ~ role + perf + area + sex + log_age + salary_diff, data = HRAnalytics, family = 'binomial')
summary(fit)
# Analysis:
# - First of all, we can see that areaFinance, areaMarketing and areaOther is not statistically significant.
# - As for the statistically significant variables, salary, areaSales and perf has the lowest p-value suggesting a strong association of these variable with the probability of leaving the company.
# - Now we can run the anova() function on the model to analyze the table of deviance

# (2) Chi Square Test
anova(fit, test = "Chisq")
# Analysis:
# - The difference between the null deviance and the residual deviance shows how our model is doing against the null model (a model with only the intercept). The wider this gap, the better
# - A smaller p-value here indicates that all the variables in the model are significant

# (3) Assessing the predictive ability of the model
fitted.results = predict(fit, test, type = 'response')
fitted.results = ifelse(fitted.results > 0.5,1,0)
misClasificError = mean(fitted.results != test$vol_leave)
# Confusion Matrix
table(actual = test$vol_leave, prediction = fitted.results)
# Accuracy
print(paste('Accuracy', 1 - misClasificError))
# Analysis:
# - The accuracy of our model is 68%

# (4) ROC Curve & AUC
# As a last step, we are going to plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
p = predict(fit, test, type = "response")
pr = prediction(p, test$vol_leave)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc
# Analysis:
# - The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings.
# - The AUC is the area under the ROC curve. 
# - As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
# - Based on the value of AUC for our dataset, we can say that it has good predictive ability.

# (b) DECISION TREE
# We have already divided our dataset into training and testing. So we proceed further by making the decision tree
# (1) Fit the model
set.seed(42)
decision_fit = rpart(vol_leave ~ role + perf + age + sex + area + salary, data = train, method = "class")
decision_fit

# Plot the tree
par(mar = c(5,4,1,2))
fancyRpartPlot(decision_fit, sub = NULL, main = "Basic Decision Tree")
# Analysis:
# - The first node is alluded to as the root. The '0' alludes to the dominate case. Here, 62% of those in our training data have 0 (Stay) for the response variable and 38% have a 1 (Leave).
# - Below that, we see our first decision node. In the event that our workers are in the Accounting, Finance, Marketing, or Other regions, then we say 'yes' and take the left branch. On the off chance that the answer is 'no' (i.e. they are in Sales), then we take the right branch.
# - After the left branch, we see that it ends into a solitary node. Think of this node like a bucket for all of those who are not in Sales. For all of these people, the most common response is '0' (Stay), with 70% employee who will stay in the company and only 30% in this bucket will leave the company. The '70%' reported in the bottom of the node tells us that this single bucket accounts for 70% of the total sample we are modeling.
# - On following the right branch, we see that the most well-known reaction is '1' for the employee who will leave the company. Moreover, the node is likewise letting us know 42% of employees in this bucket will stay while 58% will leave.
# - Proceeding with the right branch is further, if the worker is male, we say 'yes' and go to the left side. On the off chance that the worker is female, we go right.
# - For females, we wind up in a terminating node that has a dominant response of 1 (33% - Stay and 67% - Leave). This ending node represents 16% of the aggregate populace.
# - For male, we further go down to performance variable. If the performance is less than 2.5 we go left else we go right.
# - For performance less than 2.5, we wind up in a terminating node that has a dominant response of 0 (59% - Stay and 41% - Leave). This ending node represents 16% of the aggregate populace.
# - For performance greater than 2.5, we wind up in a terminating node that has a dominant response of 1 (33% - Stay and 67% - Leave). This ending node represents 4% of the aggregate populace.

# (2) Assessing the predictive ability of the model
t_pred = predict(decision_fit, test, type = 'class')
# Confusion Matrix
confMat = table(actual = test$vol_leave, prediction = t_pred)
confMat
# Accuracy
accuracy = sum(diag(confMat))/sum(confMat)
accuracy

# CONCLUSION
# - Logistic regression is better than decision tree in predicting the output response variable.
# - To play more important and vital part in the organization, the HR function needs to move past beyond mere reporting to precise expectation. 
# - Rather than simply creating receptive reports, it needs to grasp advanced analytics and predictive techniques that bolster key organizational objectives.