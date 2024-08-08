
<<>>=
@ 
<<fig=T>>=
@
  
?function # makes it so you don't have to use Google

LIBRARY FUNCTIONS 

library(UsingR)
library(e1071)
install.packages("plotrix")


READING DATA IN 

dat=read.csv(" blah.csv ")
names(dat) #headers


CREATING DATA 

heights = rnorm(1000, mean=100, sd=12)


ANAYLIZING DATA 

str(dat) #tells you amount of dat points
summary( )
mean( ) #if data is normally distributed 
median( ) #if data is skewed
dat[18] #18th value
sd( )
dim( ) #Dimensions of a data set
skewness( ) #use library("e1071") - value left + value right
IQR( )
cv=(100*(sd(dat)/mean(dat)))
range( )
SEM=(sd(dat)/sqrt(length( )))
95 % #pg 44 book

USING WHICH 

which(dat==max(dat))





What is a? 
Statistic - avg sat of FOB students (subdivision)
Parameter - avg sat of bio students (all)
P value

Result Statement?

OUTLIER 

a=boxplot(dat)
a$out


TESTING DATA

shapiro.test( ) #test for normality, reject if p is greater than .05
var.test(dat,dat2) #if p is more than .05 do t test if less than do wilcox
t.test(mu, alt="") # normally distributed variance equal
wilcox.test( ) #not normally distrusted variance not equal
t.test(dat$Values, mu=100, alt= "g") #is dat higher than mu
alt="l" #is dat lower than mu
alt="two.sided" == t.test(dat,mu=100) #Can mu be found from FOB population

One way anova AOV  

aov( ) 
aov(y~x) 
prescowallace test? 
tukeyHSD( )#use output from aov test

2 way Anova 

2 beverages, 2 trace variables
aov(y~x1*x2)



factor = something ur interested in studying 
level = 

SORTING DATA

sort(dat, decreasing=TRUE)#sorts high to low
cheetah=order(dat$column) #gives rows 
dat[cheetah,] #need comma creates table of data sorted lowest to highest


PLOTS (1 per r chunk) 

par(mfrow=c(1,2)) # side by side graphs has to be first
simple.eda( ) #library(using R) Histogram, boxplot, q-q plot 
hist( )
plot(dat$Length,dat$Width,pch=24,las=1,col="blue",bg="red",lwd=2, 
     cex.lab=1.5,xlab="Length (cm)",
     ylab="Width (cm)")
boxplot( , ) #if more than one data set
abline(h or v = value, col="red", lwd=3, lty= 2) 


SIGNS

Greater than or equal to $\geq$ 
Less than or equal to $\leg$ 
$>$ greater than
$<$ less than


Two sample 

going into ecology 
Students who took bio stats (Set of grades)
students who didnt take bio stats (Set of grades)
One tailed - asking greater than 
shapiro.test( ) - normally distrubuted - null is normally distrubuted
Varience diff? -Var.test( ) - because 2 sample - null is equal 
t.test(bio, nonbio, alt="g") - if var is more than .05
wilcox.test(bio, nonbio, alt="g")-if var less than .05 


One Sample 
breath-approach 
t.test(T1,T2,paired=T)


One tailed 
aleternative dogs are faster than cats 
null dogs are slower or equal to cats 


Two Tailed 
alternative dogs are different than cats
null dogs are the same as the cats


Type 1
reject null wrongly 

Type 2 
Acept null hypothesis even if data is proving it wrong



ANOVA test ?

HA: 
The samples did not come from the same population.

H0:
The samples came from the same population.


TUKEY HSD ? 

HA: 
Significatally different 

HO: 
Not significatally different 

  
  
  
One way anova

M=tapply(milk.dat.stacked$values,list(milk.dat.stacked$ind),mean)
S=tapply(milk.dat.stacked$values,list(milk.dat.stacked$ind),sd)
sample.lengths=tapply(milk.dat.stacked$values,milk.dat.stacked$ind,length)
SEM= S/sqrt(sample.lengths)
CI95=qt(1-0.05/2, df = sample.lengths-1) * SEM

library(plotrix)
a = barplot(M,cex.lab=1.5,ylim=c(0,50),las=1,xlab="Types of Milk"
            ,ylab="CFU (1/ml)")
plotCI(a,M,CI95,pch = NA, add = T)
abline(h=0)
lets=c("C","AB","AB","A","BC")
text(a,M+CI95,pos=3,lets)





Two Way anova 

my.aov=aov(my.sticks$Length.cm~my.sticks$Temp*my.sticks$pH)
summary(my.aov)
TukeyHSD(my.aov)
R.sq = lm(my.sticks$Length.cm ~ my.sticks$Temp*my.sticks$pH)
summary(R.sq)

Normally distrubuted?

cold.high = my.sticks$Length.cm[which(my.sticks$Temp == "C")& my.sticks$pH == "H"]
shapiro.test(cold.high)

cold.low = my.sticks$Length.cm[which(my.sticks$Temp == "C")& my.sticks$pH == "L"]
shapiro.test(cold.low)

warm.high = my.sticks$Length.cm[which(my.sticks$Temp == "W")& my.sticks$pH == "H"]
shapiro.test(warm.high)

warm.low = my.sticks$Length.cm[which(my.sticks$Temp == "W")& my.sticks$pH == "L"]
shapiro.test(warm.low)



Anova table and R sq value? 
  
my.aov=aov(my.sticks$Length.cm~my.sticks$Temp*my.sticks$pH)
summary(my.aov)
TukeyHSD(my.aov)
R.sq = lm(my.sticks$Length.cm ~ my.sticks$Temp*my.sticks$pH)
summary(R.sq)

Two Graph comparison? 
  
par(mfrow=c(1,2))
library(plotrix)

M = tapply(my.sticks$Length.cm, my.sticks$Temp,mean)
S = tapply(my.sticks$Length.cm, my.sticks$Temp,sd)
CI95 = S/sqrt(length(x))
a = barplot(M, ylim = c(0,10),ylab="Lengths",las = 1, cex.lab = 1.5,
            xaxt = "n",main="Temp")
axis(1, at = a,
     cex.axis = 0.9, 
     labels = c("Cold","Warm"))
abline(h=0)
plotCI(a,M,CI95, add = T, pch = NA)

M = tapply(my.sticks$Length.cm, my.sticks$pH,mean)
S = tapply(my.sticks$Length.cm, my.sticks$pH,sd)
CI95 = S/sqrt(20)
a = barplot(M, ylim = c(0,10),ylab="Lengths",las = 1, cex.lab = 1.5,
            xaxt = "n",main="PH")
axis(1, at = a,
     cex.axis = 0.9, 
     labels = c("High","Low"))
abline(h=0)
plotCI(a,M,CI95, add = T, pch = NA)

One Graph ?

par(mfrow=c(1,1))
M = tapply(my.sticks$Length.cm, list(my.sticks$Temp,my.sticks$pH),mean)
S = tapply(my.sticks$Length.cm,list(my.sticks$Temp,my.sticks$pH),sd)
dat.lengths = tapply(my.sticks$Length.cm,list(my.sticks$Temp,
                                              my.sticks$pH),length)
library(plotrix)
a = barplot(M, beside = T, names = c("High Temp","Low Temp"), xlab = "Treatment",
            ylab = "Length (cm)", cex.lab = 1.5, las = 1,
            legend = c("Low PH","High PH"),ylim = c(0,10))
abline(h=0)
CI95.fun = function(x){SEM = S / sqrt(length(x))
         the.df = length(x)-1 
         the.CI = qt(1-0.05/2,df = the.df)*SEM
         return(the.CI)}
CI95 = tapply(fish.dat.stacked$mass,
              list(fish.dat.stacked$Lats,fish.dat.stacked$Sites),CI95.fun)

plotCI(a,M,CI95, pch = NA, add = T)
lets=c("B","B","A","C")
text(a,M+CI95,lets,pos = 3)


M = M[c(2,3,1)]
S = S[c(2,3,1)] 


Results Statement ? 

For the main effects and interaction results statements you should end each with
: (F; df1, df2; p-value). 
For example, the result for the main effect of latitude might be: 
  there was a decrease in the fish mass with increasing latitude (F = 39.74; df = 2, 114; p < 0.001). 
The df = w tells us there were three different levels for latitude. 
You would also need to provide the result for the sites and for the two-way interaction, 
since both of these were statistically significant.
Finally, include that the adjusted R2 = 0.655.



  
  
  



Chi square test 
  
As the obaserved and exp get more and more different the chi value increases 
if same = 0 
null=observed/rejected(exp) are the same
  
24 females 7 males 
62.4 %       37.6% 
  obs=c(24,7)
  exp.prob=c(0.624,0.376)
  exp.prop=c(3/4,1/4)  #3 to 1 ratio 
  exp.prob=c(9/16,3/16,3/16,1/16)
  out=chisq.test(obs,p=exp.prob)
  out$exp    #No more tha 20% of the expected freq should be less than or eq to 5 
             #All expected frequencies greater than or equal to 1 
  out
  
graph    
  
M=matrix(c(obs,out$exp),ncol=2,byrow=T)
M 
barplot(M,beside=T,ylim=c(0,25),las=1,cex.lab=1.5,legend=c("observed",
"Expected") ,names=c("Females","Males"),xlab="Sex",ylab="Frequency")
abline(h=0) 
  
Results statement 
  
I found that the sex ratio of the class was not statistically different from
that of students at geneseo ($\chi^2$=2.98,df=1,p=0.084)


  
Correlation 

Ex 1 #Normally Distributed 

femur = c(38,56,59,64,74)
humer = c(41,63,70,72,84)

shapiro.test(femur)$p = more than .05 normally distrubuted 
shapiro.test(humer)$p

cor.test(femur,humer) 

plot(femur,humer, pch = 16, xlab = "Femur (mm)", ylab = "Humerus (mm)",
      las = 1, cex.lab = 1.5, xlim = c(30,80), ylim = c(30,90))

Ex 2 #not normally distributed 

males = my.dat[which(my.dat$Sex == "M"),]
females = my.dat[which(my.dat$Sex == "F"),]

shapiro.test(males$Height)$p
 0.01689929
shapiro.test(males$ShoeSize)$p
 6.683133e-06
shapiro.test(females$Height)$p
 0.0009554094
shapiro.test(females$ShoeSize)$p
 0.002075046

 cor.test(males$ShoeSize, males$Height, method = "spearman") 
 # NOT NORMALLY DISTRUBUTED SO HAVE TO USE METHOD = SPEARMAN 
 # DO FEMALES NEXT SAME PROCESS AND THEN PLOT 

Results Statement 

I found that the data were
positively correlated (r = 0.87, t = 3.5, df = 9, p = 0.006 or $>$).
Always use "positive" or "negative"


 
 
LINEAR REGRESSION 

EX 1 

Day - x 
IN - Y 

mod = lm(I.N ~ day) # Y and then X 
plot(mod, which = 1) # Horizontal and evenly spaced along X axis means linear 

H0: slope = 0
HA: slope ??= 0
summary(mod)


I found no siginficant relationship between <variable y> and <variable x> 
(F = 0.17; df = 1, 8; p = 0.82; R2 = 0.02) 
#Not significant 

I found a positive relationship between <variable y> and <variable x> 
(F = 17.2; df = 1, 8; p < 0.001; R2 = 0.82; y = mx + b) 
#significant MUST ADD FORMULA TO END!!!!!!!!!!!!!!!!!!!!!!! 



NON Linear Regression 

Ex 1 

mod = lm(my.dat$Eagles ~ my.dat$Year) # Y ~ X 
plot(mod)

yrs = my.dat$Year - min(my.dat$Year) # new years var starting at zero
E = my.dat$Eagles # because nls() won't take dataframe data

mod.exp = nls(E ~ E[1]*exp(r*yrs), start = list(r = 1))
summary(mod.exp)


plot(yrs, E, pch = 16, las = 1, xaxt = "n", xlab = "Year", 
ylab = "Eagle Breeding Pairs", cex.lab = 1.5, 
xlim = c(1,30), ylim = c(0,400))
> axis(1, at = seq(0,30,by = 5), labels = seq(1990,2020,by = 5))
> xv = seq(0,27,by = 0.1)
> lines(xv,predict(mod.exp, list(yrs = xv)),lwd = 2)







Programming 

condition==TRUE 
else{ } 
(2==2)
(3==2|3==3)
(3==3 & 2==3)

my.sum = 0 
(i in 1:5)
my.sum = my.sum +i = 15 
 
my.sum = 0 
i=3 (i>0)
my.sum= my.sum+i = 6
i=i-1 






EXTRA 
mod.no.intercept = lm(I.N ~ day - 1) # if <intercept> is not through orgin 

plot(day,I.N, pch = 16, ylim = c(0,70),xlab = "Day", ylab = "Number Infected",
     las = 1, cex.lab = 1.5, xaxt = "n")
axis(1, at = 0:10)
prd = predict(mod, interval = "confidence")
lines(day,prd[,1]) # this is the best fit line
lines(day,prd[,2],lty = 2) # this is the lower conf. line
lines(day,prd[,3],lty = 2) # upper conf. line



































































N.samples = 5000 # the number of samples to test
count = 0 # this will hold the number of samples NOT normally dist.
for (i in 1:N.samples) {
  my.sample = rnorm(50) # draw a sample of 50 values from the standard normal distribution
  if (shapiro.test(my.sample)$p <=.05) {
    count = count + 1 # if NOT normally dist. then count it
  }
}
count/N.samples

