 
# P-Value is bad for determining the model structure. 

### Abstract:
This is a brief explanation of why p-value should not be used to determine the structure of the model. P-Value is misleading, especially when your dataset cannot represent the entire population. We should determine the model structure based on the theory or intuitions rather than whether P-value is significant or not.  P-value should not be the reason behind the logic of choosing a particular hypothesis or model. 

### What is P-Value? 
P value is the probability of obtaining an effect at least as extreme as the one in your sample data, assuming the truth of the null hypothesis. Suppose that a vaccine study produced a P value of 4%. This P value indicates that assuming the vaccine had no effect, which is the Null Hypothesis, you’d obtain the observed difference or more in 4% of studies due to random sampling error.  


### A Real Example: Hourly Wage growth 
We often see researchers control age as the independent variable when measuring dependent variables which are related to wage. By doing so, they can eliminate the effect of age on wage and capture the effect of other independent variables, such as marriage, gender and family background. Thus, figuring out how age affect wage is important. Let’s simplify the model to 2 variables only: Age and Hourly Payment, and then estimate it: 

The first model is: Log(Wage) = Alpha +Beta_1 Age + Error Term.

#### NYLS97 Dataset
I first used the data from the National Longitudinal Survey of Youth 1997, consisting of responses from approximately 9000 young people aged between 12 and 16 at the time of the first interview. The data I downloaded contains information collected in 2011. The reference number of Hourly payment is T6658700.

```{r}
#import NYLS97, preparation:
mydata <- read_csv("Downloads/NYLS97/NYLS97.csv")
colnames(mydata)<-c("year","hourpay")
mydata["age"]<-2011-mydata["year"]
NYLS97<-subset(mydata,hourpay>1)
NYLS97["hourpay"]<-log(NYLS97["hourpay"])

#run regression: linear
NYLSfit<-lm(hourpay~age, data=NYLS97)
coeftest(NYLSfit)
plot(hourpay~age, data=NYLS97, main="NYLS97 Wage VS Age",xlab="Age",ylab="Wage in log")
abline(lm(hourpay~age,data=NYLS97),col="red")
```
![NYLS97test](DoNotOpen/2011test.png) ![NYLS97plot](DoNotOpen/2011ln.png)

We can see from the coefficient test that age has significant impacts on the hourly wage. P-Value here tells us: if you assume that age has no impact on wage, the likelihood of obtaining this dataset is almost 0. Some people would feel that this is good because it indicates that including the variable Age would reduce improve the misspecification problem. However, can we stop here? This P-Value tells you neither how good this model structure is nor how to improve it. 

### Another Example: ISLR Data
Let’s try the same model with a different dataset. ISLR is the data stored in the R library, which contains sample subjects aged between 18 and 80. To make this regression comparable to the previous one, I first run a regression for sample subjects aged between 27 and 31 only.

```{r}
WageTest <- subset(Wage, Wage["age"]<32 & Wage["age"]>26)
Testfit<- lm(logwage ~ age, data=WageTest)
coeftest(Testfit)
plot(logwage~ age, data=WageTest)
abline(lm(logwage~age,data=WageTest),col="green")
```
![ISLRTtest](DoNotOpen/ISLRTtest.png) ![testln](DoNotOpen/testln.png)

The graph and the coefficient test look almost the same as the graph plotted by using NYLS97 dataset. Wow, replication! If you think this replication can prove that your model structure is good enough, then you are probably deceived by P-Value. P-value only tells you that: if age has no effect on wage, you’d obtain the observed difference or more in less than 1% of studies due to random sampling error. 

Then, let’s look at the same dataset contains sample subjects aged between 18 and 80.
```{r}
require(AER)
require(ISLR)
fit = lm(logwage ~ poly(age,4,raw=T),data=Wage)
coeftest(fit)
plot(logwage ~ age, data=Wage, main="ISLR data Wage VS Age", xlab="Age", ylab="Wage in log")
abline(fit,col="blue")
ggplot(Wage, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))
```
![ISLRTP](DoNotOpen/ISLRTP4.png)![Totalp4](DoNotOpen/Totalp4.png)

According to the graph, it’s clear that the effect of age on wage is nonlinear. we can see an upward trend between age 18 and 35, but a downward trend after age equal to 60. This is intuitively right, the change of the wage reflects the profit employees brought to the company. Young people are promoted easily and their wage is raised quickly due to the increased return of the work experience. But when people reach 35, wage doesn’t change too much. This is because people generally got married, their learning ability decreases, they have less ambitious, and less human capital investment form their employer. They have kids to look after; therefore, they have less time for self-studying. This makes sense. After 60 years old, people are going to retire and the wage will decrease. wage doesn’t increase forever when age increases. Therefore using the fourth-degree polynomial in age is more intuitionally correct. 


### Revise NYLS97 and Partical ISLR datasets. 
Now let’s try what if we run the regression with a fourth-degree polynomial in age using NYLS97 and Sub-ISLR datasets.
```{r}
require(ggplot2)
NYLSpoly <- lm(hourpay~poly(age,4,raw =T), data=NYLS97)
coeftest(NYLSpoly)
ggplot(NYLS97, aes(x=age, y=hourpay),main="polynomial to degree 4") +geom_point()+stat_smooth(se=F, method='lm'，formula=y~poly(x,4))

```
![N97p4test](DoNotOpen/2011P4test.png)
![NYLSP4](DoNotOpen/2011P4.png)
NYLS Dataset

```{r}
Testpoly <- lm(logwage ~ poly(age,4,raw=T),data=WageTest)
coeftest(Testpoly)
ggplot(WageTest, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))

```
![ISLRP4](DoNotOpen/ISLRP4.png)
![testp4](DoNotOpen/testp4.png)
Sub-ISLR Dataset

For both coefficient tests, P-Values are not significant. The interpretation is: assuming age has no effect on wage, the probabilities of obtaining such datasets are roughly 60%. Let's evaluate this result from a intuitive way: P-values for the revised models are not significant, but they are intuitively right. People aged between 27 and 32 compete on their leadership skills, their family background or their education, instead of age. Although p-values of linear models are significant, nonlinear models are more robust. 

### Conclusion: 
Theory and intuitions behind the model are evidence what we depend on when determinate the model, not the P-Value. Therefore, never say: I include this variable because it has significant P-Value!!. instead, you should say: I include/did not include this variable, because intuitively people in this age does..... or people do not.....






Reference: 

http://blog.minitab.com/blog/adventures-in-statistics-2/how-to-correctly-interpret-p-values




