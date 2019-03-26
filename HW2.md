 
# Be careful with P-Value


### R Markdown
When try to figure out how age affect people's hourly wage, we run regression x to y. For exmaple, import data from NYLS97. ...introduce what it is. Reference number, also say this is not a good model because theoratically there are other variables should be controled, not only the age. 


This is a linear NYLS97, very significant. p-value


![ISLP](Figures/DoNotOpen/ISLRP4.png)

![NYLS97plot](Figures/DoNotOpen/NYLSln.png)

![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
![3setup2](Figures/3setup2.png)
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



## Including Plots

This is NYLS97 with polynomial degree=4. very insignificant p-value.

```{r}
NYLSpoly <- lm(hourpay~poly(age,4,raw =T), data=NYLS97)
coeftest(NYLSpoly)
ggplot(NYLS97, aes(x=age, y=hourpay),main="polynomial to degree 4") +geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.





which is better? consider this example: from ISRl dataset. 

intuitionly, wage increase farily quick before 35 years old, because of the increasing in return of the workexperience. After 35, people generally get married, the learning ability decreases and people turn to stay in the job, less ambitous and less human caputal investment form the workplacement. Thus, the salary turn to stay the same. also people get married and have children, less time for self-improvement and sel-studying. This make sense. After 60 years old, people are going to retire and the wage will decrease. wage doesnt increase forever when age incrases. therefore the polinomial to the degree of 4 is more intuitionally correct. 
```{r}
require(AER)
require(ISLR)
fit = lm(logwage ~ poly(age,4,raw=T),data=Wage)
coeftest(fit)
plot(logwage ~ age, data=Wage, main="ISLR data Wage VS Age", xlab="Age", ylab="Wage in log")
abline(fit,col="blue")
ggplot(Wage, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))
```
lets look at the data only from 29 t 35 in this dataset. 
```{r}
WageTest <- subset(Wage, Wage["age"]<32 & Wage["age"]>26)
Testfit<- lm(logwage ~ age, data=WageTest)
coeftest(Testfit)
plot(logwage~ age, data=WageTest)
abline(lm(logwage~age,data=WageTest),col="green")

```

While, this gives the similar result as NYLS97 does. indicating a serious problem with samll range of dataset. 


```{r}
Testpoly <- lm(logwage ~ poly(age,4,raw=T),data=WageTest)
coeftest(Testpoly)
ggplot(WageTest, aes(x=age, y=logwage)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,4))

```
insignificant

