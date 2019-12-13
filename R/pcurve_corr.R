
#R Code running behind p-curve.com/App3
#Written by Uri Simonsohn (uws@wharton.upenn.edu)
#This version: 2015 03 02
#
#   <-- PLEASE CONTACT ME DIRECTLY IF YOU SEE ANY ERRORS OR HAVE SUGGESTIONS  -->

# Note: 
#The code below has some redundancies because it merges  code written for two different papers
#Some of the clunkiness of the code (e.g., how results are reported) arises because the code
#was written to run on a server and save results that are then presented on a website.
#It was adapted to be run on a personal computer introducing the least # of changes possible to the code.
#

############################################################################################################

#                R E A D     M E               #
#  (---------(0)------------(0)---------------)

#ENTER/LOAD YOUR TEST RESULTS HERE: (USE THIS SYNTAX)
# tests = c("t(88)=2.1",
#           "r(147)=.246",
#           "F(1,100)=9.1",
#           "f(2,210)=4.45",
#           "Z=3.45",
#           "chi2(1)=9.1",
#           "r(77)=.47",
#           "chi2(2)=8.74")

#tests = paste("r(",dat$ni,")=",dat$ri, sep = "")
#RUN ALL CODE UP TO #14 TO GET MAIN RESULTS, P-CURVE CHART, AND POWER CHART
#THEN RUN CODE #15 TO GET THE CUMULATIVE P-CURVE (DROPPING STUDIES)




pcurve_cor <- function(ni, ri) {


############################################################################################################

#Load libraries necessary to run programs
library(stringr)  #Library to process string variables (text of the entered tests)
library(poibin)   #This library has the Poisson binomial, the distribution of the sum of binomial with different 
#underlying probabilities used to compute the binomial test given that each test has a (slightly) 
#different probability of p<.025 depending on its own non-central parameter
#See Hong (2013) - "On computing the distribution function for the Poisson binomial distribution" Computational 
#Statistics and Data Analysis, V59, p.41-51 - http://dx.doi.org/10.1016/j.csda.2012.10.006 



#1. create empty vectors for
#1.1 pp-values
t.ppr=f.ppr=c.ppr=z.ppr=c();      	#right skew
t.ppl=f.ppl=c.ppl=z.ppl=c();       	#left
t.pp33=f.pp33=c.pp33=z.pp33=c();   	#33%

#1.2 proportions expected to be low (p<.025) for each  test
t.plow=f.plow=c.plow=z.plow=c()

#1.3 proportions for all tests are 0 so that if a type of test is missing we know it is not there when aggregating
t.prop1=t.prop2=t.prop3=t.prop4=t.prop5=0;
f.prop1=f.prop2=f.prop3=f.prop4=f.prop5=0;
z.prop1=z.prop2=z.prop3=z.prop4=z.prop5=0;
c.prop1=c.prop2=c.prop3=c.prop4=c.prop5=0;


#1.4 Function 1 - functions that find noncentrality parameter for t,f,chi distributions that gives 33% power for those d.f.

#t-test
ncp33t =function(df) 
{      
  xc=qt(p=1-.05/2, df=df)
  #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
  f = function(delta, pr, x, df) pt(x, df = df, ncp = delta) - 2/3
  out = uniroot(f, c(0, 37.62), x = xc, df = df)  
  return(out$root) 
}

#F-test
ncp33f =function(df1,df2) 
{      
  xc=qf(p=1-.05,df1=df1,df2=df2)
  f = function(delta, pr, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - 2/3
  out = uniroot(f, c(0, 37.62), x = xc, df1=df1, df2=df2)  
  return(out$root)       
}

#chi-square
ncp33chi =function(df) 
{      
  xc=qchisq(p=1-.05, df=df)
  #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
  f = function(delta, pr, x, df) pchisq(x, df = df, ncp = delta) - 2/3
  out = uniroot(f, c(0, 37.62), x = xc, df = df)  
  return(out$root)  
}


###############################################################################

#Function 2 - percent() : makes a number look like a percentage
percent <- function(x, digits = 0, format = "f", ...)   {
  paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
}
###############################################################################

#Create a "backup" so that final results table includes the n.s. results which will be excluded from most calculations
#(not really important here, key for the online app)
k=seq(from=1,to=length(tests))
backup=cbind(k,tests)

#(2) Split tests into t,F,X2 and Z

#2.1 Turn everything to lower case
tests=tolower(tests)

#2.2 Extract the type of test (stat={t,F,c,Z)
stat=substring(tests,1,1)   

#2.3 Split vector of tests into these
#get the t-tests
t.text=subset(tests,stat=="t")
#get the f-tests
f.text=subset(tests,stat=="f")
#get the chi2
c.text=subset(tests,stat=="c" | stat=="x")
#get the Z (normally distributed)
z.text=subset(tests,stat=="z")
#get the r (correlation) 
r.text=subset(tests,stat=="r")

#3 Get d.f. for the tests 
#3.1 t-test
#find the 2nd parenthesis
t.par=str_locate(t.text,")")[,1]
#Get the d.f. between both parenthesis
t.df=as.numeric(substring(t.text,3,t.par -1))

#3.2 f-test
#find the comma
f.comma=str_locate(f.text,",")[,1]
#find the 2nd parenthesis
f.par=str_locate(f.text,")")[,1]
#Get the df1  (between "(" and ","
f.df1=as.numeric(substring(f.text,3,f.comma -1))
#Get the df2  (between "," and ")"
f.df2=as.numeric(substring(f.text,f.comma +1,f.par -1))

#3.3 Chi-square  
#find the 1st parenthesis
c.par1=str_locate(c.text,"\\(")[,1]
#find the 2nd parenthesis
c.par2=str_locate(c.text,")")[,1]
#Get the d.f. between both parenthesis
c.df=as.numeric(substring(c.text,c.par1+1,c.par2 -1))

#3.4 Correlations
#fine the 2nd parenthesis
r.par=str_locate(r.text,"\\)")[,1]
#Get the d.f. between both parenthesis
r.df=as.numeric(substring(r.text,3,r.par -1))


#4 Get the test values
#4.1 Find the "=" sign
t.eq=str_locate(t.text,"=")[,1]
f.eq=str_locate(f.text,"=")[,1]
z.eq=str_locate(z.text,"=")[,1]
c.eq=str_locate(c.text,"=")[,1]
r.eq=str_locate(r.text,"=")[,1]  

#4.2 Get the number after the =
t.value=c();r.value=c()    

t.value=as.numeric(substring(t.text,t.eq+1,))
f.value=as.numeric(substring(f.text,f.eq+1,))
z.value=as.numeric(substring(z.text,z.eq+1,))
c.value=as.numeric(substring(c.text,c.eq+1,))
r.value=as.numeric(substring(r.text,r.eq+1,)) 

#4.3 merge r() with t-tests
rt.value=r.value/(sqrt((1-r.value**2)/r.df))  #r() expressed as a t-value 
t.value=c(t.value,rt.value)                     
t.df=c(t.df,r.df)
t.text=c(t.text,r.text)


#5 Keep significant p-values
#Compute p-values
t.p=2*(1-pt(abs(t.value),df=t.df))
f.p=1-pf(abs(f.value),df1=f.df1,df2=f.df2)
z.p=2*(1-pnorm(abs(z.value)))
c.p=1-pchisq(abs(c.value),df=c.df)

#Subset statistics and d.f.
#ts
t.value.sig=subset(t.value,t.p<.05)
t.df.sig   =subset(t.df,   t.p<.05)
t.text.sig =subset(t.text, t.p<.05)
t.p.sig    =subset(t.p,    t.p<.05)
#fs
f.value.sig=subset(f.value,f.p<.05)
f.df1.sig  =subset(f.df1,  f.p<.05)
f.df2.sig  =subset(f.df2,  f.p<.05)
f.text.sig =subset(f.text, f.p<.05)
f.p.sig    =subset(f.p,    f.p<.05)
#chis
c.value.sig=subset(c.value,c.p<.05)
c.df.sig   =subset(c.df,   c.p<.05)
c.text.sig =subset(c.text, c.p<.05)
c.p.sig    =subset(c.p,    c.p<.05)
#zs
z.value.sig=subset(z.value,z.p<.05)
z.text.sig =subset(z.text, z.p<.05)
z.p.sig    =subset(z.p,    z.p<.05)

#All significant p-values (used for binomial)
all.p.sig=c(t.p.sig, f.p.sig, c.p.sig, z.p.sig)
#Number of significant results
ktot=length(all.p.sig)    
#Number of non-signifcant results in p-curve
kns=length(tests)-ktot    

#6 Compute pp-values 
#6.1 For t-values
if (length(t.value.sig)>0)  #if nonempty compute pp-values
{
  #skew
  t.ppr=t.p.sig*(1/.05)               #pp-value for right-skew 
  t.ppl=1-t.ppr                         #pp-value for left-skew
  #33%power
  #Find the ncp (uses function from top)
  t.ncp33=mapply(ncp33t,t.df.sig)
  #Using the ncp33 compute pp33.
  t.pp33=3*(pt(t.value.sig,  df=t.df.sig,  ncp=t.ncp33)-2/3)
}

#6.2 For F-values
if (length(f.value.sig)>0)  #if nonempty compute pp-values
{
  f.ppr=f.p.sig*(1/.05)             #pp-value for right-skew 
  f.ppl=1-f.ppr                     #pp-value for left-skew
  f.ncp33=mapply(ncp33f, f.df1.sig,  f.df2.sig)
  f.pp33 =3*(pf(f.value.sig,  df1=f.df1.sig, df2=f.df2.sig,  ncp=f.ncp33)-2/3)
}

#6.3 z-values
if (length(z.value.sig)>0)  #if nonempty compute pp-values
{
  z.ppr=z.p.sig*(1/.05)
  z.ppl=1-z.ppr
  z.pp33=3*(pnorm(z.value.sig,mean=1.5285687,sd=1)-2/3)   #Compute pp33-values using the 'ncp' 1.5285687 which gives the normal 33% power
  
}

#6.4 chi-values
if (length(c.value.sig)>0)  #if nonempty compute pp-values
{
  c.ppr=c.p.sig*(1/.05)
  c.ppl=1-c.ppr
  c.ncp33=mapply(ncp33chi, c.df.sig)
  c.pp33=3*(pchisq(c.value.sig,  df=c.df.sig, ncp=c.ncp33)-2/3)
}


#7 STOUFFER: Overall tests aggregating pp-values (using Fisher's method to aggregate uniform distributions of (p)p-values)
#7.1 Convert pp-values to Z scores, aggregate them and divide by sqrt(ktot)
Zppr =sum(qnorm(c(t.ppr,  f.ppr  ,c.ppr,  z.ppr )))/sqrt(ktot)          #right skew
Zppl =sum(qnorm(c(t.ppl,  f.ppl  ,c.ppl,  z.ppl )))/sqrt(ktot)          #left skew
Zpp33=sum(qnorm(c(t.pp33, f.pp33 ,c.pp33, z.pp33)))/sqrt(ktot)          #33%

#7.2 Compute overall p-values
p.Zppr =pnorm(Zppr)	
p.Zppl =pnorm(Zppl)	
p.Zpp33=pnorm(Zpp33)	


#8 Green line (Expected p-curve for 33% power)
#8.1 t-tests 
if (length(t.value.sig)>0)  #if nonempty compute pp-values
{
  #Critical values,xc, for p=.05, .04, .03, .02 and ,01
  t.x5=qt(.975,df=t.df.sig); t.x4=qt(.98, df=t.df.sig); t.x3=qt(.985,df=t.df.sig); 
  t.x2=qt(.99, df=t.df.sig); t.x1=qt(.995,df=t.df.sig)
  #For Binomial test 
  t.x25=qt(.9875,df=t.df.sig)                            #critical value for t-tests to get p=.025
  t.plow=1- 3*(pt(t.x25,df=t.df.sig, ncp=t.ncp33)-2/3)   #prob(p<.025 | ncp33% & p<.05)
  
  #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
  t.pp4=3*(pt(t.x4,df=t.df.sig, ncp=t.ncp33)-2/3)
  t.pp3=3*(pt(t.x3,df=t.df.sig, ncp=t.ncp33)-2/3)
  t.pp2=3*(pt(t.x2,df=t.df.sig, ncp=t.ncp33)-2/3)
  t.pp1=3*(pt(t.x1,df=t.df.sig, ncp=t.ncp33)-2/3)
  #within bins proportions
  t.prop5=mean(t.pp4); 
  t.prop4=mean(t.pp3-t.pp4); 
  t.prop3=mean(t.pp2-t.pp3); 
  t.prop2=mean(t.pp1-t.pp2); 
  t.prop1=mean(1-t.pp1)                       
}
#8.2 f-tests 
if (length(f.value.sig)>0)  #if nonempty compute pp-values
{
  #Critical values,xc, for p=.05, .04, .03, .02 and ,01
  f.x5=qf(.95,df1=f.df1.sig, df2=f.df2.sig);  f.x4=qf(.96,df1=f.df1.sig, df2=f.df2.sig); f.x3=qf(.97,,df1=f.df1.sig, df2=f.df2.sig); 
  f.x2=qf(.98, df1=f.df1.sig, df2=f.df2.sig); f.x1=qf(.99,df1=f.df1.sig, df2=f.df2.sig) 
  #For binomial test
  f.x25 =qf(.975,df1=f.df1.sig, df2=f.df2.sig)                          #Critical F value for p=.025  
  f.plow=1-3*(pf(f.x25,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)  #Prob(p<.025|ncp33% & p<.05)
  
  
  #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
  f.pp4=3*(pf(f.x4,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
  f.pp3=3*(pf(f.x3,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
  f.pp2=3*(pf(f.x2,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
  f.pp1=3*(pf(f.x1,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
  
  #within bins proportions
  f.prop5=mean(f.pp4); 
  f.prop4=mean(f.pp3-f.pp4); 
  f.prop3=mean(f.pp2-f.pp3); 
  f.prop2=mean(f.pp1-f.pp2); 
  f.prop1=mean(1-f.pp1)
}
#8.3 chi-tests 
if (length(c.value.sig)>0)  #if nonempty compute pp-values
{
  
  #Critical values,xc, for p=.05, .04, .03, .02 and ,01
  c.x5=qchisq(.95,df=c.df.sig); c.x4=qchisq(.96, df=c.df.sig); c.x3=qchisq(.97,df=c.df.sig); 
  c.x2=qchisq(.98, df=c.df.sig); c.x1=qchisq(.99,df=c.df.sig) 
  
  #For binomial test
  c.x25 =qchisq(.975,df=c.df.sig)                                      #Critical x2 value for p=.025
  c.plow=1-3*(pchisq(c.x25,df=c.df.sig, ncp=c.ncp33)-2/3)              #Prob(p<.025|ncp33% & p<.05)
  
  #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
  c.pp4=3*(pchisq(c.x4,df=c.df.sig, ncp=c.ncp33)-2/3)
  c.pp3=3*(pchisq(c.x3,df=c.df.sig, ncp=c.ncp33)-2/3)
  c.pp2=3*(pchisq(c.x2,df=c.df.sig, ncp=c.ncp33)-2/3)
  c.pp1=3*(pchisq(c.x1,df=c.df.sig, ncp=c.ncp33)-2/3)
  
  #within bins proportions
  c.prop5=mean(c.pp4); c.prop4=mean(c.pp3-c.pp4); c.prop3=mean(c.pp2-c.pp3); 
  c.prop2=mean(c.pp1-c.pp2); c.prop1=mean(1-c.pp1)
}
#8.4 z-tests 
if (length(z.value.sig)>0)  #if nonempty compute pp-values
{
  #Critical values,xc, for p=.05, .04, .03, .02 and ,01
  z.x5=qnorm(.975); z.x4=qnorm(.98); z.x3=qnorm(.985); z.x2=qnorm(.99); z.x1=qnorm(.995) 
  # For Binomial test	
  z.x25 =qnorm(.9825)                                         #Critical x2 value for p=.025
  z.plow=1-3*(pnorm(z.x25,mean=1.5285687,sd=1)-2/3)           #Prob(p<.025|ncp33% & p<.05)
  #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01, given p<.05 and ncp=ncp33
  z.pp4=3*(pnorm(z.x4,mean=1.5285687,sd=1)-2/3)
  z.pp3=3*(pnorm(z.x3,mean=1.5285687,sd=1)-2/3)
  z.pp2=3*(pnorm(z.x2,mean=1.5285687,sd=1)-2/3)
  z.pp1=3*(pnorm(z.x1,mean=1.5285687,sd=1)-2/3)
  #within bins proportions
  z.prop5=z.pp4; z.prop4=z.pp3-z.pp4; z.prop3=z.pp2-z.pp3; z.prop2=z.pp1-z.pp2; z.prop1=1-z.pp1
}


#9 combine t,F,chi,Z
#proportion of all tests that are of each type
t.share=length(t.value.sig)/ktot
f.share=length(f.value.sig)/ktot
c.share=length(c.value.sig)/ktot
z.share=length(z.value.sig)/ktot

#Average proportions within the 4 types of tests
t.props=c(t.prop1, t.prop2, t.prop3, t.prop4, t.prop5)
f.props=c(f.prop1, f.prop2, f.prop3, f.prop4, f.prop5)
c.props=c(c.prop1, c.prop2, c.prop3, c.prop4, c.prop5)
z.props=c(z.prop1, z.prop2, z.prop3, z.prop4, z.prop5)

#overall proportions (i.e.., THE GREEN LINE)
green=100*(t.props*t.share + f.props*f.share + c.props*c.share + z.props*z.share)


#10 The blue line (observed p-curve)   

#Put each p-value in a bin between 0 and .05
ps=ceiling(c(all.p.sig)*100)/100
#Count them
blue01=sum(ps<=.01)/ktot; blue02=sum(ps==.02)/ktot; blue03=sum(ps==.03)/ktot; 
blue04=sum(ps==.04)/ktot; blue05=sum(ps==.05)/ktot; 
#combine
blue=c(blue01,blue02,blue03,blue04,blue05)*100
#Note: i could have used the Table command, but it is a pain if there are no p-value in a given range

#11 Red line
red=c(20,20,20,20,20)

#12 Carry out binomial test
#Note: for t and Z test, the critical value is for p=.0125 one sided, for Z and Chi2 it is for .025 two-sided


#12.1 Combine the prob(p<.025) for each set of tests
plows=c(t.plow, f.plow, c.plow, z.plow)  
#12.2 Compute observed shared of p<.025 results
low.obs=sum(all.p.sig<=.025)
#12.3 Right skew: Compare observed share p<.025 with null of 50:50 and altenrative of more p<.025 than expected
binom.r=1-pbinom(q=low.obs-1, p=.5, size=ktot)     #The binomial in R computes the probability of x<=xo. We want prob(x>=x0) so we subtract one from x, and 1-prob()
#12.4 Left skew: Compare observed share p<.025 with null of 50:50 and altenrative of fewer p<.025 than expected
binom.l=pbinom(q=low.obs, p=.5, size=ktot)         #Here the default x<=x0 is what we want
#12.5 33% power: Compare observed share p<.025 with expected share ~72% based on the combination of expected shares for ncp33%
#              The probability of p<.025|ncp33 is slightly different for each test, hence I use the poisson binomial distribtuion (see reference top of this document)
binom.33=ppoibin(kk=low.obs,pp=plows)              


#13.POWER ESTIMATION

#13.1 SET OF FUNCTIONS 1. COMPUTE GAP BETWEEN POWER AND DESIRED POWER FOR A GIVEN NCP 
# (minimize these in the next step to solve for the ncp that gives the desired power)
ncp_error.t = function(delta, power, x, df)      pt(x, df = df, ncp = delta) - (1-power)   #if this equals 0, we found the ncp.
ncp_error.f = function(delta, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)   
ncp_error.c = function(delta, power, x, df)      pchisq(x, df = df, ncp = delta) - (1-power)   
ncp_error.z = function(delta, power, x)          pnorm(x, mean = delta,sd=1) - (1-power)   

#13.2 SET OF FUNCTIONS 2: MINIMIZE FUNCTIONS ABOVE
#t-test
getncp.t =function(df, power)   {      
  xc=qt(p=.975, df=df) # critical t-value
  return(uniroot(ncp_error.t, c(0, 37.62), x = xc, df =df, power=power)$root)   }  

#F-test
getncp.f =function(df1,df2, power)   {      
  xc=qf(p=.95, df1=df1,df2=df2) # critical F-value
  return(uniroot(ncp_error.f, c(0, 37.62), x = xc, df1 = df1,df2=df2, power=power)$root)  }


#chisq-test
getncp.c =function(df, power)   {      
  xc=qchisq(p=.95, df=df) # critical c-value
  return(uniroot(ncp_error.c, c(0, 37.62), x = xc, df = df, power=power)$root)   }

#Normal
getncp.z =function(power)   {      
  xc=qnorm(p=.975) # critical Z-value with df=1
  return(uniroot(ncp_error.z, c(0, 37.62), x = xc, power=power)$root)   }             

# 13.3 CREATE PP-VALUES FOR EACH OF THE FOUR DISTRIBUTIONS FOR HOW WELL A GIVEN POWER_EST FITS 
powerfit.t=function(t_obs, df_obs, power_est)    {
  ncp_est=mapply(getncp.t,df=df_obs,power=power_est)  #find ncp for each  that gives each test power.k
  p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)            #prob t>tobs given ncp_est
  ppr=(p_larger-(1-power_est))/power_est              #condition on p<.05
  return(ppr)   }

powerfit.f=function(f_obs, df1_obs, df2_obs, power_est)    {
  ncp_est=mapply(getncp.f,df1=df1_obs, df2=df2_obs,power=power_est)  #find ncp for each  that gives each test power.k
  p_larger=pf(f_obs,df1=df1_obs,df2=df2_obs, ncp=ncp_est)        #prob t>tobs given ncp_est
  ppr=(p_larger-(1-power_est))/power_est          #condition on p<.05
  return(ppr)   }

powerfit.z=function(z_obs, power_est)    {
  ncp_est=mapply(getncp.z,power=power_est)  
  p_larger=pnorm(z_obs,mean=ncp_est)        
  ppr=(p_larger-(1-power_est))/power_est          
  return(ppr)     }


powerfit.c=function(c_obs, df_obs, power_est)    {
  ncp_est=mapply(getncp.c,df=df_obs,power=power_est)  
  p_larger=pchisq(c_obs,df=df_obs,ncp=ncp_est)        
  ppr=(p_larger-(1-power_est))/power_est          
  return(ppr)   }

#13.4  STACK-UP ALL THE PP-VALUES INTO A VECTOR AND COMPARE THEM TO UNIFORM DISTRIBUTION USING KOLMOGOROV-SMIRNOV TEST

powerfit.all=function(power_est)
{
  ppr.all=c()
  #for each kind of test, check if there are any significant values, if there are, add ppr to overall ppr
  if (length(t.value.sig)>0) ppr.all=c(ppr.all, powerfit.t(t_obs=t.value.sig, df_obs=t.df.sig, power_est=power_est))
  if (length(f.value.sig)>0) ppr.all=c(ppr.all, powerfit.f(f_obs=f.value.sig, df1_obs=f.df1.sig, df2_obs=f.df2.sig, power_est=power_est))
  if (length(z.value.sig)>0) ppr.all=c(ppr.all, powerfit.z(z_obs=z.value.sig, power_est=power_est))
  if (length(c.value.sig)>0) ppr.all=c(ppr.all, powerfit.c(c_obs=c.value.sig, df_obs=c.df.sig, power_est=power_est))
  KSD=ks.test(ppr.all,punif)$statistic                #KS test on the resulting pprs
  return(KSD)
}

#13.5 FUNCTION THAT COMPUTES FIT FOR EACH LEVEL OF POWER, AND PLOT IT


plotfit=function()
{
  # Fit will be evaluated at every possible value of power between 5.1% and 99% in steps of 1%, stored in fit()
  fit=c()                                          #Create empty vector
  fit=powerfit.all(.051)                           #First evaluate fit for power of 5.1%, the lowest one can get for non-directional tests like x2 and F
  for (i in 6:99)   fit=c(fit,powerfit.all(i/100)) #Now do 6% to 99%
  # Find the minimum
  mini=match(min(fit),fit)       #which ith power level considered leads to best estimate
  hat=(mini+4)/100               #convert that into the power level, the ith value considered is (5+ith)/100
  #Plot results
  #create the x-axis
  x.power=seq(from=5,to=99)/100 
  #Draw the line
  par(mar=c(5.1,8.1,4.1,2.1))
  plot(x.power,fit,xlab="Underlying Power", ylab="",ylim=c(0,1), main="")  
  #Make red dot at the estimate
  points(hat,min(fit),pch=19,col="red",cex=2)    
  #Put a label with the estimate value
  sign="="
  if (hat<.06) sign="<"
  text(min(.7,max(.28,hat)),min(fit)-.1,paste0("Estimated Power ",sign," ",hat*100,"%"))
  mtext(c("Perfect","Terrible"),side=2,line=3,at=c(0,1),las=1,cex=1.25,col=c("blue","red"))
  mtext("How Good Is the Fit?",side=2,line=6.5,cex=1.5)
  mtext("(Kolmogorov-Smirnov D Stat)",side=2,line=5.5,col="gray")
  mtext("Do we have a good estimate of power?",side=3,line=1.75,cex=1.5,at=0.4)
  mtext("If you see a V-Shape with a low minimum-->yes",side=3,line=0.5,cex=1.25,at=0.4)
  
}

#Create two graphs in a single chart
#par(mfrow=c(2,1)) 
par(mfrow=c(1,1)) 

#14 Firest the p-curve itself
#Define x-axis as p-values (.01, .02..)
x = c(.01,.02,.03,.04,.05)

#Plot the observed p-curve

plot(x,blue,   type='l', col='dodgerblue2',  main="",lwd=2, xlab="", ylab="", 
     xaxt="n",yaxt="n", xlim=c(0.01,0.055), ylim=c(0,105), bty='L', las=1);  	

#x-axis value labels
x_=c(".01",".02",".03",".04",".05")
axis(1,at=x,labels=x_)
#y-axis value labels
y_=c("0%","25%","50%","75%","100%")
y=c(0,25,50,75,100)
axis(2,at=y,labels=y_,las=1,cex.axis=.75)

#Add y-axis label
mtext("Percentage of test results",font=2,side=2,line=2.75,cex=1.25)
#Add y-axis label
mtext("p            ",font=4,side=1,line=2.5,cex=1.25)
mtext(" -value",font=2,side=1,line=2.5,cex=1.25)


#Add little point in actual frequencies
points(x,blue,type="p",pch=20,bg="dodgerblue2",col="dodgerblue2")
#Add value-labels
text(x+.00075,blue+5,percent(round(blue)/100),col='black', cex=.75)
#Add red and green lines
lines(x,red,   type='l', col='firebrick2',    lwd=1.5, lty=3)
lines(x,green, type='l', col='springgreen4',  lwd=1.5, lty=5)

#Legend
#By default its x-position, legendx, is in the middle
legendx=.035 ; 
#Move left for p-curves that have more 80% of p-values =.02 or =.03 so that the legend does not touch blue line
if (blue04>.80 | blue05>.80) legendx=.02
#Print legend
legend(legendx, 100, c('Observed p-curve','Null of 33% power', 'Null of zero effect'), 
       box.col="white",lty=c(1,5,3), cex=.75,lwd=c(1,1),col=c('dodgerblue2','springgreen4', 'firebrick2'));

#ADD THE POWER FIT CHART
#plotfit()

#PRINT OUT RESULTS
printout=function()
{cat("\nTest for right-skew....Binomial: ",binom.r,"   Continuous: Z=",Zppr,"  p=",p.Zppr)
  cat("\nTest for 33%....Binomial: ",binom.33,"   Continuous: Z=",Zpp33,"  p=",p.Zpp33)
  cat("\nTest for left-skew....Binomial: ",binom.l,"   Continuous: Z=",Zppl,"  p=",p.Zppl)
}
printout()


}


# 
# ################################################
# #15 Cumulative test
# #MAKE CUMULATIVE CONDITIONAL ON SIGNIFICNCE SO IF RIGHT-SKEW IS SIGNIFICANT, EXCLUDE LOWEST P-VALUES
# #BUT IF RIGHT SKEW IS N.S., EXCLUEDE HIGHEST P-VALUES
# #OR MAYBE DO IT BOTH WAYS ALWAYS
# 
# #If right skew is significant, assess robustness to excluding most significnat finding, one at a time
# all.zppr = sort(qnorm(c(t.ppr,  f.ppr  ,c.ppr,  z.ppr )))
# all.zppl = sort(qnorm(c(t.ppl,  f.ppl  ,c.ppl,  z.ppl )))
# all.zpp33= sort(qnorm(c(t.pp33,f.pp33 ,c.pp33, z.pp33 )))
# 
# droplow.zr=droplow.zl=droplow.z33=drophigh.zr=drophigh.zl=drophigh.z33=c()
# for (i in 1:(ktot))
# {
#   droplow.zr[i] =sum(all.zppr[i:ktot]/sqrt(ktot-i+1))
#   droplow.zl[i] =sum(all.zppl[i:ktot]/sqrt(ktot-i+1))
#   droplow.z33[i]=sum(all.zpp33[i:ktot]/sqrt(ktot-i+1))
#   
#   drophigh.zr[i] =sum(all.zppr[1:(ktot-i+1)]/sqrt(ktot-i+1))
#   drophigh.zl[i] =sum(all.zppl[1:(ktot-i+1)]/sqrt(ktot-i+1))
#   drophigh.z33[i]=sum(all.zpp33[1:(ktot-i+1)]/sqrt(ktot-i+1))
# }
# 
# 
# 
# #19.2 FUNCTION THAT PLOTS RESULTS
# plotdrop=function(var)
# {
#   #Plot the dots
#   plot(0:(ktot-1),pnorm(var),xlab="",ylab="",type="b",yaxt="n",main="",cex.main=1.15,ylim=c(0,1))
#   #Add marker in results with 0 drops
#   points(0,pnorm(var[1]),pch=19,cex=1.6)
#   #Red line at p=.05
#   abline(h=.05,col="red")  #Red line at p=.05
#   #Y-axis value labels
#   axis(2,c(.01,.05,.1,seq(from=.2,to=.9,by=.10)),las=1,cex.axis=.95)
# }
# 
# #19.3 RUN PLOT FUNCTION 6 TIMES
# #Put all graphs together
# dev.off()  
# par(mfrow=c(3,2),mar=c(4,4,1,2),mgp=c(2.5,1,0),oma=c(5,14,5,1)) 
# #Plot(1)
# plotdrop(droplow.zr)
# mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
# mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
# #Rigt Skew label
# mtext("Right Skew",line=8,side = 2,cex=1.2,las=1,col="Blue") 
# #Low to high label
# mtext(bquote("Drop"~italic(k)~bold("lowest")~"original p-values"),line=1,side = 3,cex=1.5,las=1) 
# #Plot(2)
# plotdrop(drophigh.zr)
# mtext(bquote("Drop"~italic(k)~bold("highest")~"original p-values"),line=1,side = 3,cex=1.5,las=1) 
# #Plot (3)
# plotdrop(drophigh.z33)
# mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
# mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
# #33%  Skew label
# mtext("33% Power%",line=8,side = 2,cex=1.2,las=1,col="springgreen4") 
# #Plot (4)
# plotdrop(droplow.z33)
# #Plot (5)
# plotdrop(drophigh.zl)
# mtext(side=2,line=4,"P-value Overall Test",font=2,cex=.85)
# mtext(side=2,line=3,"(Stouffer's Method)",font=3,cex=.75)
# mtext(side=1,line=2.5,"K Tests Dropped From p-curve",cex=1.15,font=2)
# #33%  Skew label
# mtext("Left Skew",line=8,side = 2,cex=1.2,las=1,col="red") 
# #Plot (6)
# plotdrop(droplow.zl)
# #x-axis label
# mtext(side=1,line=2.5,"K Tests Dropped From p-curve",cex=1.15,font=2)
# #Legend (winging it for location)
# op=par(usr=c(0,1,0,1),xpd=NA)   #allow goin outsie of plot 6 and recalibrate dimensions to be 0-1 in x and y
# legend(-.6,-.25,horiz=TRUE,pch=c(19,1),cex=1.4, legend=c("Including all p-values","Dropping p-values"))   
# #so the legend is placed 60% of a chart to the left of 0 of #6, an 25% of a chart below it.
# 
# 


