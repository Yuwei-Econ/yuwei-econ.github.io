Microeconometrics Homework 1.
Title: Deterministic Model and Probabilistic Model.
Class: Theory Report.

please click below to open the pdf document.

[HMK1](http://yuwei-econ.github.io/DoNotOpen/HW1_Microeconometrics.pdf)


#Deterministic model and Probabilistic model
####Yuwei Zheng
##1 Introduction
This short theoretical report aims to help students to understand what are deterministic and probabilistic models, what are their differences, and how to use them for learning.
##2 Deterministic Model
The deterministic model is a mathematical model in which outcomes are precisely determined through known relationships among states and events, without any room for random variation. In simple words, we can only observe one y value at a given value of x. This is also called a model without noisy. For example, the model for predicting stock prices is a deterministic model; at every 15 seconds, there is only 1 stock price generated.
###2.1 Estimation Process
If the model we learn is a deterministic model, we can simply do the following to find the best fit line:
1. Set a target function to predict Y: f : x → y
2. Randomly select training samples from the observed data set. D = {x1, y1; x2, y2; ...; xn, yn}
3. Define a set of hypothesis that you think are likely to represent the true function f:H = {h1,h2,...,hn}
4. Minimise the loss function to find the hypothesis that fits the training sample best.
L2 : Ein =
L1 : Ein =
􏰄 1 if yi ̸= h∗(xi) 􏰅 L0:Ein = 0if yi =h∗(xi)
Those loss functions measure how different the observed y values and it’s prediction value g∗(x) are by adding up the distance in between. Before minimising the loss function, you should choose the level of loss functions yourself for serving your needs. If you are afraid of the impact of outliers, then do not use the L2 function.
1n
􏰆 [yi − h∗(xi)]2
 N i=1 1n
􏰆 |yi − h∗(xi)| N i=1
 1


