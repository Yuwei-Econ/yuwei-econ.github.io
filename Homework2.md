 
# Do not let your research determined by P-Value

#### Abstruct: This is a brief outline of why p-value is not determinative enough when dataset is partial. One swallow does not make a summer; we should always ask this question: can your dataset represent the entire population?  

### 
We often times see researchers using age as the independent variable to measure changes in wage. This is prevalent in labour economics or any field having wage as dependent variable. I controled age when I was measuring the male wage premium as well. 
But for students who are new to econometrics, we create our prediction model by testing and choosing the one suits the data most. Here is an example: 


Import NYLS97 dataset.
```{r}
      #import NYLS97, preparation:
      mydata <- read_csv("Downloads/NYLS97/NYLS97.csv")
      mydata <- subset(mydata,select=-1)
      mydata <- subset(mydata,select=-1)
      mydata <- subset(mydata,select=-2)
      mydata <- subset(mydata,select=-3)
      mydata <- subset(mydata,select=-4)
      mydata <- subset(mydata,select=-3)

      #rename those variables.
      colnames(mydata)<-c("year","hourpay")
      # calculate the age of survey taker, the year was 2013.
      mydata["age"]<-2011-mydata["year"]
      #eliminate wage rate less than 0.
      NYLS97<-subset(mydata,hourpay>1 & hourpay<70000)
      #use log to eliminate the huge outlier effect.
      NYLS97["hourpay"]<-log(NYLS97["hourpay"]/100)

      #run regression: linear
      NYLSfit<-lm(hourpay~age, data=NYLS97)
      coeftest(NYLSfit)
      plot(hourpay~age, data=NYLS97, main="NYLS97 Wage VS Age",xlab="Age",ylab="Wage in log")
      abline(lm(hourpay~age,data=NYLS97),col="red")

```

This is NYLS97 with polynomial degree=4. very insignificant p-value.

```{r}
NYLSpoly <- lm(hourpay~poly(age,4,raw =T), data=NYLS97)
coeftest(NYLSpoly)
ggplot(NYLS97, aes(x=age, y=hourpay),main="polynomial to degree 4") +geom_point()+stat_smooth(se=F, method='lm'，formula=y~poly(x,4))

```



![NYLS97plot](DoNotOpen/NYLSln.png)
![NYLS97test](DoNotOpen/NYLS97test.png)
![N97p4](DoNotOpen/N97p4.png)

![NYLSP4](DoNotOpen/NYLSP4.png)

We can see that in age between .... the linear effect of age on wage is significant. 
if we include age^2, which is the polynomial to the degree 2, its not significant. but does it means we do not need to include age^2 in our model? according to p-value, yes. but should not because of p-value. 


When try to figure out how age affect people's hourly wage, we run regression x to y. For exmaple, import data from NYLS97. ...introduce what it is. Reference number, also say this is not a good model because theoratically there are other variables should be controled, not only the age. 


This is a linear NYLS97, very significant. p-value







## Including Plots

Lets look at another dataset which has wage data for people from 20 to 70. 
```{r}
require(AER)
require(ISLR)
fit = lm(logwage ~ poly(age,4,raw=T),data=Wage)
coeftest(fit)
plot(logwage ~ age, data=Wage, main="ISLR data Wage VS Age", xlab="Age", ylab="Wage in log")
abline(fit,col="blue")
ggplot(Wage, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))
```
![Totalp4](DoNotOpen/Totalp4.png)
its clear that the effect of age on wage is not linear. poly^4 explains better. 

What went wrong?
```{r}
WageTest <- subset(Wage, Wage["age"]<32 & Wage["age"]>26)
Testfit<- lm(logwage ~ age, data=WageTest)
coeftest(Testfit)
plot(logwage~ age, data=WageTest)
abline(lm(logwage~age,data=WageTest),col="green")
```

```{r}
Testpoly <- lm(logwage ~ poly(age,4,raw=T),data=WageTest)
coeftest(Testpoly)
ggplot(WageTest, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))

```




![ISLRP4](DoNotOpen/ISLRP4.png)

![ISLRTP](DoNotOpen/ISLRTP4.png)
![testln](DoNotOpen/testln.png)
![testp4](DoNotOpen/testp4.png)

![ISLRTtest](DoNotOpen/ISLRTtest.png)

which is better? consider this example: from ISRl dataset. 

intuitionly, wage increase farily quick before 35 years old, because of the increasing in return of the workexperience. After 35, people generally get married, the learning ability decreases and people turn to stay in the job, less ambitous and less human caputal investment form the workplacement. Thus, the salary turn to stay the same. also people get married and have children, less time for self-improvement and sel-studying. This make sense. After 60 years old, people are going to retire and the wage will decrease. wage doesnt increase forever when age incrases. therefore the polinomial to the degree of 4 is more intuitionally correct. 

lets look at the data only from 29 t 35 in this dataset. 


While, this gives the similar result as NYLS97 does. indicating a serious problem with samll range of dataset. 


insignificant

