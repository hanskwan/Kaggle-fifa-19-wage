# Issue: 
# 1.categorical data with too many levels that one hot encoder generate too many columns which leads to 
# overfitting
# 
# 2. Mukticollinearity
# VIF and correlation not necessary means collinearity, try PCA, Ridge condition index
# 
# 3. accuracy
# 10 fold cross validation
# Further devvelopment:
#
# 4. Seperate Goalkeeper and other players?
#
# Group wage group and build a tree
# 
##########################################################################################################

fifa <- read.csv("data_cleaned_03.csv", stringsAsFactors = FALSE)

# Install library
#
# install.packages(dplyr)
# install.packages(car)
# install.packages(corrplot)
# install.packages(factoextra)
# install.packages(ggplot2)
# install.packages(MASS)

#data cleaning - remove rows do not contain values, redundant column
complete.cases(fifa)
fifa<-fifa[complete.cases(fifa),]

str(fifa)
class(fifa)
head(fifa)
wage <- fifa$Wage
summary(wage)
is.numeric(fifa$Wage)
class(fifa$Value)
fifa$Value <- as.numeric(as.character(fifa$Value))

###
#plot of important variables
#age
hist(fifa$Age, breaks = 20)
plot(fifa$Age, wage,xlab = "Age", ylab = "Wage", main = "Scatterplot")
# age is normally distrubuted which is as expected

#value
hist(fifa$Value, breaks = 40)
plot(fifa$Value, wage,xlab = "Value", ylab = "Wage", main = "Scatterplot")
# quite a nice linear relationship(positive)

#international reputation
hist(fifa$International.Reputation, breaks = 20)
plot(fifa$International.Reputation, wage,xlab = "International Reputation", ylab = "Wage", main = "Scatterplot")
# one weird data point here, there is one data point with 0 wage given it has 5 in reputation
#dig into this data point
print(fifa[which(fifa$International.Reputation==5),])
# Compare back to the original dataset, this data point actually is z.ibrahimovic, one of the best player of all time
# He got a huge cut in his salary after he joined LA Galaxy which is reported on news
# https://www.cnbc.com/2018/03/26/zlatan-ibrahimovic-is-taking-a-95-percent-pay-cut-to-join-the-la-galaxy.html
# Which we should discard this observation as it is an exceptional case, all football fans would agree on that 

#skill moves
hist(fifa$Skill.Moves, breaks = 20)
plot(fifa$Skill.Moves, wage, xlab = "Skill Moves", ylab = "Wage", main = "Scatterplot")
# seems skill moves isn't that an major factor to wage

#defence work rate
hist(fifa$Defence.Work.Rate, breaks = 20)
plot(fifa$Defence.Work.Rate, wage, xlab = "Defence Work Rate", ylab = "Wage", main = "Scatterplot")
#seems similar wage for 2.0 and 3.0

#ball control
hist(fifa$BallControl, breaks = 20)
plot(fifa$BallControl, wage, xlab = "Ball Control", ylab = "Wage", main = "Scatterplot")
# can see a major effect on wage, positive

#release clause
hist(fifa$Release.Clause, breaks = 20)
plot(fifa$Release.Clause, wage, xlab = "Release Clause", ylab = "Wage", main = "Scatterplot")
# and of course very directly, higher the release clause =higher wage, logical
###


# discard z.ibrahimovic
fifa_no_zb <- fifa[-which(fifa$International.Reputation ==5 & fifa$Club == "LA Galaxy" ),]
# discard na observation
fifa_na <- na.omit(fifa_no_zb)

# Count the categorical data
library(dplyr)
set.seed(1)
fifa_na %>% 
  group_by(Nationality) %>%
  summarise(no_rows = length(Nationality))
set.seed(2)
fifa_na %>% 
  group_by(Club) %>%
  summarise(no_rows = length(Club))

# full model with categorical data
m_0 <- lm(Wage~., data=fifa_na)
summary(m_0)
library(car) 
vif(m_0)

# convert these categorical variables into numeric using one hot encoding
# Too many levels for categorical data which lead to one hot encoding overfit coz too many columns generated
#library(dummies)
#fifa_na_cat_X <- dummy.data.frame(fifa_na, names = c("Nationality","Club","Position"))
#fifa_na_cat <- fifa_na_cat_X[,2:ncol(fifa_na_cat_X)]

# subset out the categorical data 
str(fifa_na)
fifa_na_cat <- fifa_na[,2:62]
hist(fifa_na_cat$Wage, breaks =100)
boxplot(fifa_na_cat$Wage)

library(corrplot)
corr_plot_1 <- corrplot(cor(fifa_na_cat), method = "circle", type = "upper")

res_2 <- cor.mtest(cor(fifa_na_cat), conf.level =.95)
corrplot_2 <- corrplot(cor(fifa_na_cat), p.mat = res_2$p, sig.level =.5, type = "upper", method = "ellipse")

corrplot_3 <- corrplot(cor(fifa_na_cat), diag = FALSE, order = "FPC",
                       tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")
# Look at the plot and discard any variables that have heavy coreelation


###### PCA
library(factoextra)

#normalized "scale. = T"
pca.fifa <- prcomp(fifa_na_cat, center = TRUE, scale = TRUE)
summary(pca.fifa)
plot(pca.fifa)
biplot(pca.fifa)

#check out the eigens
fviz_eig(pca.fifa)
fviz_pca_var(pca.fifa,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     ) # Avoid text overlapping
# First two component explained more than 50%
plot(pca.fifa$x[,1],pca.fifa$x[,2], xlab="PC1 (34.49%)", ylab = "PC2 (18.18%)", main = "PC1 / PC2 - plot")

fviz_pca_ind(pca.fifa, col.ind="cos2", geom = "point",
             gradient.cols = c("blue", "#2E9FDF", "#FC4E07" ))

#create new column Wage.group for grouping salary
library(ggplot2)

# First group with four group
fifa_na_cat_new_4 <-fifa_na_cat %>%
  mutate(Wage.group = case_when(Wage <= 1000 ~ 'a_0k~1k',
                                Wage <= 3000 ~ 'b_1k~3k',
                                Wage <= 10000 ~ 'c_3k~10k',
                                TRUE ~ 'f_above_10K'))

barplot(prop.table(table(fifa_na_cat_new_4$Wage.group)))

pc4 <- fviz_pca_ind(pca.fifa, label="none", habillage=fifa_na_cat_new_4$Wage.group,
             addEllipses=TRUE, ellipse.level=0.95, palette = "nejm")

pc4

# Second group with Three group
fifa_na_cat_new_3 <-fifa_na_cat %>%
  mutate(Wage.group = case_when(Wage <= 1000 ~ 'a_0k~1k',
                                Wage <= 10000 ~ 'b_1k~10k',
                                TRUE ~ 'f_above_10K'))

barplot(prop.table(table(fifa_na_cat_new_3$Wage.group)))

pc3 <- fviz_pca_ind(pca.fifa, label="none", habillage=fifa_na_cat_new_3$Wage.group,
             addEllipses=TRUE, ellipse.level=0.95, palette = "nejm")

pc3

#biplot for dim 1 & 2
fviz_pca_biplot(pca.fifa, label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal())

fviz_pca_biplot(pca.fifa, label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, invisible = "ind")

fviz_pca_biplot(pca.fifa, label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, col.var = "red")

#biplot for dim 3 & 4
fviz_pca_biplot(pca.fifa, axes = c(3,4), label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, invisible = "ind")


######

fviz_pca_biplot(pca.fifa, axes = c(3,4), label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, invisible = "ind")


# train and test set, 8:2
train_fifa_na_cat <- sample(1:nrow(fifa_na_cat), 0.8*nrow(fifa_na_cat))
test_fifa_na_cat  <- setdiff(1:nrow(fifa_na_cat), train_fifa_na_cat)

# X_train, y_train, X_test, y_test
X_train <- fifa_na_cat[train_fifa_na_cat,] 
y_train <- fifa_na_cat[train_fifa_na_cat,"Wage"]

X_test <- fifa_na_cat[test_fifa_na_cat, -5]   # discard column 5 "Wage"
y_test <- fifa_na_cat[test_fifa_na_cat,"Wage"]

##########
# Model m1, without categorical data
m1_train <- lm(Wage~., data = X_train)
summary(m1_train)
plot(m1_train)
AIC(m1_train)
vif(m1_train)

#Find out the RMSE
p_m1_test <- predict(m1_train, X_test)
error_m1 <- (p_m1_test-y_test)
RMSE_m1 <- sqrt(mean(error_m1^2))
RMSE_m1

#Total sum of square
TSS_m1 <- sum(residuals(m1_train)^2)

# R2
R2_m1 <- 1-((2946*RMSE_m1^2)/TSS_m1)
R2_m1
##########


##########
# Model m2, chose *** from m0
m2_train <- lm(Wage~Age+Value+International.Reputation+Skill.Moves+Defence.Work.Rate+CDM+ShortPassing+BallControl
         +Reactions+Stamina+Interceptions+StandingTackle+Release.Clause, data=X_train)
summary(m2_train)
plot(m2_train)
AIC(m2_train)
vif(m2_train)

#Find out the RMSE
p_m2_test <- predict(m2_train, X_test)
error_m2 <- (p_m2_test-y_test)
RMSE_m2 <- sqrt(mean(error_m2^2))
RMSE_m2

#Total sum of square
TSS_m2 <- sum(residuals(m2_train)^2)

# R2
R2_m2 <- 1-((2946*RMSE_m2^2)/TSS_m2)
R2_m2
##########


##########
# Model m3, chose *** from m0
m3_train <- lm(Wage~Age+Value+International.Reputation+Skill.Moves+CDM+BallControl
               +Release.Clause, data=X_train)
summary(m3_train)
plot(m3_train)
AIC(m3_train)
vif(m3_train)

#Find out the RMSE
p_m3_test <- predict(m3_train, X_test)
error_m3 <- (p_m3_test-y_test)
RMSE_m3 <- sqrt(mean(error_m3^2))
RMSE_m3

#Total sum of square
TSS_m3 <- sum(residuals(m3_train)^2)

# R2
R2_m3 <- 1-((2946*RMSE_m3^2)/TSS_m3)
R2_m3
##########

##########
# Model m4, chose *** from m0
m4_train <- lm(Wage~Age+Value+International.Reputation+Skill.Moves+CDM
               +Release.Clause, data=X_train)
summary(m4_train)
plot(m4_train)
AIC(m4_train)
vif(m4_train)

#Find out the RMSE
p_m4_test <- predict(m4_train, X_test)
error_m4 <- (p_m4_test-y_test)
RMSE_m4 <- sqrt(mean(error_m4^2))
RMSE_m4

#Total sum of square
TSS_m4 <- sum(residuals(m4_train)^2)

# R2
R2_m4 <- 1-((2946*RMSE_m4^2)/TSS_m4)
R2_m4
##########

## M1-M4 using straigh line method looking at the *** and discard

##########
# Model m5, use corr_plot_3
corrplot_3 <- corrplot(cor(fifa_na_cat), diag = FALSE, order = "FPC",
                       tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

m5_train <- lm(Wage~ Body.Type+ Jumping+Weak.Foot+Age+Attack.Work.Rate+Stamina+International.Reputation
               , data=X_train)
summary(m5_train)
plot(m5_train)
AIC(m5_train)
vif(m5_train)

#Find out the RMSE
p_m5_test <- predict(m5_train, X_test)
error_m5 <- (p_m5_test-y_test)
RMSE_m5 <- sqrt(mean(error_m5^2))
RMSE_m5

#Total sum of square
TSS_m5 <- sum(residuals(m5_train)^2)

# R2
R2_m5 <- 1-((2946*RMSE_m5^2)/TSS_m5)
R2_m5
##########

##########
# Model m6, from PC 1,2,3,4
fviz_pca_biplot(pca.fifa, label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, invisible = "ind")

#biplot for dim 3 & 4
fviz_pca_biplot(pca.fifa, axes = c(3,4), label = "var", habillage=fifa_na_cat_new_3$Wage.group,
                addEllipses=TRUE, ellipse.level=0.90,
                ggtheme = theme_minimal(), repel = TRUE, invisible = "ind")

# Choosing the var that is more correlated to Wage vector
m6_train <- lm(Wage~ CM+Special+ShortPassing+Release.Clause+ Potential +Value+ Composure+
                 International.Reputation+LongPassing+Reactions+Overall+Stamina+Age
               ,data=X_train)
summary(m6_train)
plot(m6_train)
AIC(m6_train)
vif(m6_train)

#Find out the RMSE
p_m6_test <- predict(m6_train, X_test)
error_m6 <- (p_m6_test-y_test)
RMSE_m6 <- sqrt(mean(error_m6^2))
RMSE_m6

#Total sum of square
TSS_m6 <- sum(residuals(m6_train)^2)

# R2
R2_m6 <- 1-((2946*RMSE_m6^2)/TSS_m6)
R2_m6
##########


#########

# model 1-4 only use lm result and choose from ***
# model 5 look from corr_plot_3
# model 6 use PCA component that is close to wage

##########






# Stepwise
library(MASS)
step_m1 <- stepAIC(m1_train, direction = "forward",trace = 1)
summary(step_m1)

#Find out the RMSE
p_s1_test <- predict(step_m1, X_test)
error_s1 <- (p_s1_test-y_test)
RMSE_s1 <- sqrt(mean(error_s1^2))
RMSE_s1

#Total sum of square
TSS_s1 <- sum(residuals(step_m1)^2)

# R2
R2_s1 <- 1-((2946*RMSE_s1^2)/TSS_s1)
R2_s1



# set function
# To output summary, VIF,AIC, R2
#
#
#
m_summary <- function(x){
  summary(x_train)
  AIC(x_train)
  vif(x_train)
  
  #Find out the RMSE
  p_x_test <- predict(x_train, fifa_test)
  error_x <- (p_x_test-fifa_test$Wage)
  RMSE_x <- sqrt(mean(error_x^2))
  RMSE_x
  
  #Total sum of square
  TSS_x <- sum(residuals(x_train)^2)
  
  # R2
  R2_x <- 1-((2946*RMSE_x^2)/TSS_x)
  R2_x
}



