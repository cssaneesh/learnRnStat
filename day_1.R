# load data----
load("C:/Users/ty00osat/Desktop/R_projects/HIDA_stat_course/Materials_Statistics/Data/NHANES.RData")
# question 1----
str(health)
# gender factor
# BMI num
# BPSysAve int
# WT_status factor
# BP Cat factor

# question 3-----
# variable type 3
table(health$Gender)
table(health$WT_STATUS)
table(health$BP_CAT)

# absolute frequencies
barplot(table(health$BP_CAT))

# relative frequencies 
barplot(prop.table(table(health$BP_CAT)))

barplot(table(health$Gender))
summary(health)
head(health)
tail(health)


pie(table(health$WT_STATUS))

# Mode of BP_CAT 
table(health$BP_CAT) # normal
table(health$WT_STATUS) # normal



mean(health$BPSysAve)
median(health$BPSysAve)

mode(cut(health$BPSysAve, breaks = 20) )

range(health$Age)
IQR(health$Age)
# nterdecile range
42.50-23
summary(health$Age)

var(health$Age)


# sd of bp
sd(health$BPSysAve)

hist(health$BPSysAve)
hist(health$BPSysAve, breaks = 10)
hist(health$BPSysAve, breaks = 20)
hist(health$BPSysAve, breaks = 30)


plot(density(health$BPSysAve))
plot(density(health$BPSysAve, bw = 5))
plot(density(health$BPSysAve, bw = 10))
plot(density(health$BPSysAve, bw = 20))
plot(density(health$BPSysAve, bw = 30))


boxplot(health$BPSysAve)
abline(mean(health$BPSysAve))

ggplot(health, aes(y=BPSysAve))+
  geom_boxplot()+
  geom_hline(yintercept = mean(health$BPSysAve), col= 'red', linetype= 'dotted')+
  geom_hline(yintercept = median(health$BPSysAve), col= 'blue', linetype= 'dotted')



ggplot(health, aes(y=BPSysAve))+
  geom_boxplot()+
  geom_hline(yintercept = mean(health$BPSysAve), col= 'red', linetype= 'dotted')+
  geom_hline(yintercept = median(health$BPSysAve), col= 'blue', linetype= 'dotted')



table(health$Gender, health$BP_CAT)
round(prop.table(table(health$Gender, health$BP_CAT)), digits = 2)

(tab <- table(x= health$BP_CAT, y= health$Gender ))

barplot(tab, beside = F)
barplot(tab, beside = T)

round(prop.table(table(x= health$BP_CAT, y= health$Gender), margin=NULL), digits = 2)
table(health$BP_CAT, health$Gender)

round(prop.table(table(x= health$BP_CAT, y= health$Gender), margin=1), digits = 2)

health %>% group_by(Gender) %>% 
  summarise(min= min(BMI),
            max= max(BMI),
            range= mean(BMI))

health %>% group_by(Gender) %>% 
  summarise(median= median(BMI),
            mean= mean(BMI))

health %>% group_by(Gender) %>% 
  summarise(IQR= IQR(BMI, type = 2, ))

health %>% group_by(Gender) %>% 
  summarise(var= var(BMI),
            sd= sd(BMI))

ggplot(health, aes(x= Gender, y= BMI ))+
  geom_col(position = 'identity')

ggplot(health, aes(x= Gender, y= BMI ))+
  geom_boxplot()+
  stat_summary(geom = 'point', 
               fun= mean)


ggplot(health, aes(x= Gender, y= BMI ))+
  geom_violin()+
  stat_summary(geom = 'point', 
               fun= mean,
               col='red')

plot(Height ~ Weight, data= health)

ggplot(health, aes(Weight, Height))+
  geom_point()

health %>% 
  summarise(cor=cor(Weight, Height, method = "pearson"))
health %>% 
  summarise(cor=cor(Weight, Height, method = "spearman"))


health %>% group_by(Gender) %>% 
  summarise(cor_P=cor(Weight, Height, method = "pearson")) %>% 
  as_tibble()

ggplot(health, aes(Height, Weight, col= Gender))+
  geom_point()


set.seed(1980)
sample(x = 1:6, size = 10, replace = TRUE, prob = rep(0.1, times= 10) )




# Simulate 100 realizations of a N (μ = 2, σ2 = 4) distributed variable. 
# Hint: The rnorm function helps doing so.


test1 <- rnorm(n = 100, mean = 2, sd = 4)
hist(test1, probability = TRUE)
lines(density(test1), col = "red", lwd = 2)

table(test1) # can't understand anything from the table!!

mean1 <- mean(test1) # observed
sd1 <- sd(test) # observed

2-mean1 #expected - observed
4- sd1 #expected - observed


test2 <- rnorm(n = 1000, mean = 2, sd = 4)

mean2 <- mean(test2)
sd2 <- mean(test2)

mean1- mean2
sd1- sd2


library(DescTools)
MeanCI(test1, conf.level = 0.95)

MeanCI(test2, conf.level = 0.95)

a <- MeanCI(test1, conf.level = 0.95)
b <- MeanCI(test2, conf.level = 0.95)

a
b
a-b

sdtest1 <- sd(test1)
sdtest2 <- sd(test2)
sdtest1-sdtest2


load("C:/Users/ty00osat/Desktop/R_projects/HIDA_stat_course/Materials_Statistics/Data/NHANES.RData")
health
?t.test

library(tidyverse)
View (health)


ggplot(health, aes(y=Height, group= Gender))+
  geom_boxplot()

t.test_health <- health %>% t.test (Height ~ Gender, data=., var= TRUE )

t.test_health



qqnorm(health$Height)
qqline(health$Height)

shapiro.test(x = health$Height)
bartlett.test(x = health$Height, g = health$Gender )
bartlett.test(Height ~ Gender, data= health )

?bartlett.test

hist(health$Height)


wilcox.test(health$DirectChol~ health$Gender, data= health)
library(coin)
wilcox.test(health$DirectChol~ health$Gender, data= health)

wilcox.test(health$BPSysAve ~ health$SmokeNow, data= health)

boxplot(health$BPSysAve ~ health$SmokeNow, data= health)

library(ggplot2)

ggplot(health, aes(SmokeNow, BPSysAve ))+
  geom_violin()+
  labs(y='Systolic blood pressure', x= 'Smokers and non-smokers')+
  geom_jitter()

ggplot(health, aes(SmokeNow, BPSysAve ))+
  geom_boxplot()+
  labs(y='Systolic blood pressure', x= 'Smokers and non-smokers')+
  geom_jitter()

library(tidyverse)
health %>% wilcox.test(DirectChol~ Gender, data= .)

ggplot(health, aes(SmokeNow, Weight))+
  geom_boxplot()

health %>% group_by(SmokeNow) %>% 
  count(WT_STATUS)

c.test <- chisq.test(x= health$WT_STATUS, y= health$SmokeNow)
c.test


ggplot(health, aes(BP_CAT, BMI ))+
  geom_boxplot(outlier.colour = 'red')+
  geom_jitter(width = 0.2)

one.anov <- health %>% oneway.test(BMI ~BP_CAT, data = . , var.equal = TRUE)
one.anov

pairwise.t.test (health$BMI, health$BP_CAT, p.adjust.method = 'holm')


# linear regression

load("C:/Users/ty00osat/Desktop/R_projects/HIDA_stat_course/Materials_Statistics/Data/NHANES.RData")
names(health)

library(tidyverse)

model1 <- health %>% lm(BMI ~ Age, data=.)
model1
summary(model1)

ggplot(health, aes(Age, BMI))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)

# for every year your BMI will increase about 0.14

model2 <- health %>% lm(Weight ~ Age, data=.)
model2
summary(model2)

# for every year your weight will increase about 0.45 units(kilograms?)

ggplot(health, aes(Age, Weight))+
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE)


mode_WT_0 <- health %>% lm(Weight ~ Age+ Height, data=.)
mode_WT_0

summary(mode_WT_0)

mode_WT_0

library(arm)
coefplot(mode_WT_0, )
coefplot(mode_WT_0, vertical = FALSE)
coefplot(mode_WT_0, CI = 2, vertical = FALSE)

library(effects)
plot(predictorEffects(mod = mode_WT_0), ylim=c(50, 90)) 


model_WT_0 <- health %>% lm(Weight ~ Age + Gender, data= .)
model_WT_0
summary(model_WT_0)

coefplot(model_WT_0, vertical= FALSE)

plot(predictorEffects(model_WT_0))


model_BP_1 <- health %>% lm(BPSysAve ~ DirectChol+ WT_STATUS, data=.)
summary(model_BP_1)

coefplot(model_BP_1, vertical= FALSE)

plot(predictorEffects(model_BP_1))

health$WT_STATUS2 <- relevel(health$WT_STATUS, 
                             ref = 'Obese')
model_BP_2 <- lm(BPSysAve ~ DirectChol + WT_STATUS2,
                 data = health)


plot(model_BP_2, which = 1)
plot(model_BP_2, which = 2)
plot(model_BP_2, which = 3)
plot(model_BP_2, which = 4)

hist(model_BP_1$residuals)
plot(model_BP_1)
plot(model_BP_1, which=1)
plot(model_BP_1, which=2)
plot(model_BP_1, which=3)
plot(model_BP_1, which=4)


hist(health$BPSysAve)


health %>% 
model_BP_1 <- health %>% lm(BPSysAve ~ DirectChol+ WT_STATUS, data=.)


library("MASS")
model_a <- stepAIC(lm(BPSysAve ~ Gender + Age, data = health,
                      scope = list(lower = Gender ~ 1),
                      direction = "backward"))

model_a <- stepAIC(lm(BPSysAve ~ Gender + Age+ Weight+ Height, data = health,
                      scope = list(lower = Gender ~ 1),
                      direction = "backward"))

test1 <- health %>% filter(BPSysAve>=120) %>% mutate(BPstatus= as.factor('high'))
test2 <- health %>% filter(BPSysAve<=119) %>% mutate(BPstatus= as.factor('low'))

health$BP_high <- cut(health$BPSysAve, 
                      breaks = c(0, 120, 200), 
                      right = TRUE, 
                      labels = c("normal", "high"))

model_glm <- glm(BP_high ~ Age + Gender, 
                 family = "binomial",
                 data = health)
summary(model_glm)

