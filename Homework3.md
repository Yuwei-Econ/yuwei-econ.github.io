
## Challenge: The Battle of Sexes --- Game Theory with Pure Nash Equalibriums. 

#### Using Rstudio to simulate a discrete utility choice model, The Battle of Sexes, based on the concept of the game theory: the game with imperfect information. 

## The Battle of Sex:

#### In game theory, the battle of the sexes (BoS) is a two-player coordination game. Imagine a couple that agreed to meet this evening, but cannot recall if they will go for shopping or watching the football game (and the fact that they forgot is common knowledge). The husband would prefer to go for watching the football game, whereas the wife would go for shopping. Both would prefer to go to the same place rather than different ones. If they cannot communicate, where should they go? This is a classical decision-making problem when one player's behavior depends on another. 

#### The payoff matrix shown below is an example of Battle of the Sexes, where the wife chooses a row and the husband chooses a column. In each cell, the first number represents the payoff to the wife and the second number represents the payoff to the husband. The graph next to the payoff matrix is a mixed strategy with 3 nash equilibriums circled in red, meaning wife and husband both choosing shopping or football game or shopping with probabilities equal to 1/3(husband) and 2/3 (wife) at the same time would have no incentive to change their decisions. 

<img src="DoNotOpen/1.png/" width="450"><img src="DoNotOpen/3.png/" width="390"> 

## R code: 
```{r}
# Installing and importing packages:
install.packages("ramify")
require(ramify)
require(AER)
require(ggplot2)
require(MASS)
n<-200 # 200 wifes and 200 husbands. 
e1 <- rnorm(n,mean=0,sd=1) 
e2 <- rnorm(n,mean=0,sd=1)
e3 <- rnorm(n,mean=0,sd=1) 
e4 <- rnorm(n,mean=0,sd=1)
```
### Possibilities are exogeneously generated: 

```{r}
# p=Prob(shopping|Husband), q=Prob(shopping|Wife), random generation
p <- runif(n,min=0,max=1)
q <- runif(n,min=0,max=1)

#Utility Functions for each possible choice combination: 
U_shopping_wife <- 10*p+e1
U_football_wife <- 5*(1-p)+e2
U_shopping_hus <- 5*q+e3
U_football_hus <- 10*(1-q)+e4

# profit driven decision making
y_wife <- argmax(cbind(U_football_wife,U_shopping_wife))-1
y_hus <- argmax(cbind(U_football_hus,U_shopping_hus))-1
dataRandom <- data.frame(y_wife,y_hus,U_shopping_hus,U_football_hus,U_shopping_wife, U_football_wife, p , q)

#plot respectively:
ggplot(dataRandom, aes(x=p,y=y_wife))+geom_point()+stat_smooth(method="glm",method.args = list(family="binomial"), se=TRUE)
ggplot(dataRandom, aes(x=q,y=y_hus))+geom_point()+stat_smooth(method="glm",method.args = list(family="binomial"), se=TRUE)

wifefit <- glm(y_wife ~ p,family = binomial(link = "probit"))
coeftest(wifefit)
Rq <- fitted(wifefit) 

husfit <- glm(y_hus ~ q,family = binomial(link = "probit"))
coeftest(husfit)
Rp <- fitted(husfit) # wife Prob

plot(Rp,q,xlab = "p",ylab="q")
par(new=TRUE)
plot(p,Rq,xlab = "p",ylab="q")
```
#### We first compute this model with p and q values generated randomly. This gives the best response of wife and husband with respect to each possible p and q values. Utility functions are coded for each cell, allowing wife and husband to make a discrete choice: choosing either shopping or football game. 4 error terms are generated to make data points realistic and lines smoothly. As the mixed strategy graph below looks similar to the graph drawn above, it indicates a successful simulation. 
<img src="DoNotOpen/0.1.png/" width="420"><img src="DoNotOpen/0.2.png/" width="420">
<center>
  <img src="DoNotOpen/0.3.png/" width="500"> 
</center>

### Captureing the best response:

```{r}
n<-500 
## wife: t0-t1
e1 <- rnorm(n,mean=0,sd=1) 
e2 <- rnorm(n,mean=0,sd=1)
p_0 <- runif(n,min=0,max=1) #Hus Prob(random)
U_shopping_wife <- 10*p_0+e1
U_football_wife <- 5*(1-p_0)+e2
y_wife <- argmax(cbind(U_football_wife,U_shopping_wife))-1
wifefit <- glm(y_wife ~ p_0,family = binomial(link = "probit"))
coeftest(wifefit)
q_1 <- fitted(wifefit) # wife Prob

##Husband:t1-t2
e3 <- rnorm(n,mean=0,sd=1) 
e4 <- rnorm(n,mean=0,sd=1)
U_shopping_hus <- 5*q_1+e3
U_football_hus <- 10*(1-q_1)+e4
y_hus <- argmax(cbind(U_football_hus,U_shopping_hus))-1
husfit2 <- glm(y_hus ~ q_1,family = binomial(link="probit"))
coeftest(husfit2)
p_2 <- fitted(husfit2)

## Wife again:t2-t3
e5 <- rnorm(n,mean=0,sd=1) 
e6 <- rnorm(n,mean=0,sd=1)
U_shopping_wife2 <- 10*p_2+e5
U_football_wife2 <- 5*(1-p_2)+e6
y_wife2 <- argmax(cbind(U_football_wife2,U_shopping_wife2))-1
wifefit2 <- glm(y_wife ~ p_2,family = binomial(link = "probit"))
coeftest(wifefit2)
q_3 <- fitted(wifefit2) # wife Prob

##Hus again:t3-t4
e7 <- rnorm(n,mean=0,sd=1) 
e8 <- rnorm(n,mean=0,sd=1)
U_shopping_hus2 <- 5*q_3+e7
U_football_hus2 <- 10*(1-q_3)+e8
y_hus2 <- argmax(cbind(U_football_hus2,U_shopping_hus2))-1
husfit3 <- glm(y_hus ~ q_3,family = binomial(link="probit"))
coeftest(husfit3)
p_4 <- fitted(husfit3)

data<- data.frame(y_wife,y_hus,y_wife2,y_hus2,p_0,q_1,p_2,q_3,p_4, U_shopping_wife, U_football_wife, U_shopping_hus, U_football_hus,U_shopping_wife2,U_football_wife2 ,U_shopping_hus2,U_football_hus2 )

plot(p_0,q_1,col=rgb(1,0.2,0.2,0.2),pch=19,xlab = "p",ylab="q")
par(new=TRUE)
plot(p_2,q_1,col=rgb(0.2,0.2,1,0.2),pch=19,xlab="p",ylab="q")

plot(p_2,q_3,col=rgb(0.7,0.2,0.2,0.2),pch=19,xlab="p",ylab="q")
par(new=TRUE)
plot(p_4,q_3,col=rgb(0.2,0.2,0.7,0.2),pch=19,xlab="p",ylab="q")

```
#### Now let's compute wife and husband's best responses into each other's payoff functions. at time t0-t1, wife made her best response regards to husband's random probabilities of choosing shopping and then updated q. At time t1-t2, wife's updated probabilities are taken into account by husband and so forth.

#### The left graph shows the level 1 depth of reasoning of wife and husband. the right graph shows the level 2 depth reasoning. As we can see from the graphs, all points are moving towards bottom-left and up-right corners. Because this is a discrete utility choice model, only pure nash equilibriums can be captured. Thus, if we carry on the simulation and increase the depth of reasoning, we will shortly see all points assemble at (0,0) and (1,1). This coincides with the pure nash equilibriums: (shopping, shopping) and (football, football).

<img src="DoNotOpen/1.1.png/" width="420"><img src="DoNotOpen/1.2.png/" width="420"> 



