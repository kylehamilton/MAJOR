### R code from vignette source 'flup'
### Encoding: UTF-8

###################################################
### code chunk number 1: flup.rnw:22-25
###################################################
options( width=90,
         SweaveHooks=list( fig=function()
         par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n") ) )


###################################################
### code chunk number 2: flup.rnw:137-139
###################################################
library(Epi)
print( sessionInfo(), l=F )


###################################################
### code chunk number 3: flup.rnw:148-157
###################################################
data( DMlate )
head( DMlate )
dmL <- Lexis( entry = list( per=dodm,
                            age=dodm-dobth,
                            tfD=0 ),
               exit = list( per=dox ),
        exit.status = factor( !is.na(dodth), labels=c("DM","Dead") ),
               data = DMlate )
timeScales(dmL)


###################################################
### code chunk number 4: flup.rnw:180-182
###################################################
str( dmL )
head( dmL )[,1:10]


###################################################
### code chunk number 5: flup.rnw:198-199
###################################################
summary.Lexis( dmL, timeScales=TRUE )


###################################################
### code chunk number 6: dmL1
###################################################
plot( dmL )


###################################################
### code chunk number 7: dmL2
###################################################
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6 )
plot( dmL, 1:2, lwd=1, col=c("blue","red")[dmL$sex],
      grid=TRUE, lty.grid=1, col.grid=gray(0.7),
      xlim=1960+c(0,60), xaxs="i",
      ylim=  40+c(0,60), yaxs="i", las=1 )
points( dmL, 1:2, pch=c(NA,3)[dmL$lex.Xst],
        col="lightgray", lwd=3, cex=0.3 )
points( dmL, 1:2, pch=c(NA,3)[dmL$lex.Xst],
        col=c("blue","red")[dmL$sex], lwd=1, cex=0.3 )
box(bty='o')


###################################################
### code chunk number 8: flup.rnw:255-258
###################################################
dmS1 <- splitLexis( dmL, "age", breaks=seq(0,100,5) )
summary( dmL )
summary( dmS1 )


###################################################
### code chunk number 9: flup.rnw:268-271
###################################################
wh.id <- c(9,27,52,484)
subset( dmL , lex.id %in% wh.id )[,1:10]
subset( dmS1, lex.id %in% wh.id )[,1:10]


###################################################
### code chunk number 10: flup.rnw:277-279
###################################################
dmS2 <- splitLexis( dmS1, "tfD", breaks=c(0,1,2,5,10,20,30,40) )
subset( dmS2, lex.id %in% wh.id )[,1:10]


###################################################
### code chunk number 11: flup.rnw:284-290
###################################################
library( popEpi )
dmM <- splitMulti( dmL, age = seq(0,100,5), 
                        tfD = c(0,1,2,5,10,20,30,40),
                   drop = FALSE )
summary( dmS2 )
summary( dmM )


###################################################
### code chunk number 12: flup.rnw:301-304
###################################################
identical( dmS2, dmM )
class( dmS2 )
class( dmM )


###################################################
### code chunk number 13: flup.rnw:334-344
###################################################
whc <- c(names(dmL)[1:7],"dodm","doins") # WHich Columns do we want to see?
subset( dmL, lex.id %in% wh.id )[,whc]
dmC <- cutLexis( data = dmL, 
                  cut = dmL$doins, 
            timescale = "per",
            new.state = "Ins",
            new.scale = "tfI",
     precursor.states = "DM" )
whc <- c(names(dmL)[1:8],"doins") # WHich Columns do we want to see?
subset( dmC, lex.id %in% wh.id )[,whc]


###################################################
### code chunk number 14: flup.rnw:362-369
###################################################
dmS2C <- cutLexis( data = dmS2, 
                    cut = dmS2$doins,
              timescale = "per",
              new.state = "Ins",
              new.scale = "tfI",
       precursor.states = "DM" )
subset( dmS2C, lex.id %in% wh.id )[,whc]


###################################################
### code chunk number 15: flup.rnw:393-394
###################################################
summary( dmS2C, timeScales=TRUE )


###################################################
### code chunk number 16: box1
###################################################
boxes( dmC, boxpos=TRUE, scale.R=1000, show.BE=TRUE )


###################################################
### code chunk number 17: flup.rnw:437-445
###################################################
timeBand( dmS2C, "age", "middle" )[1:10]
# For nice printing and column labelling we use the data.frame() function:
data.frame( dmS2C[,c("per","age","tfD","lex.dur")],
            mid.age=timeBand( dmS2C, "age", "middle" ),
              mid.t=timeBand( dmS2C, "tfD", "middle" ),
             left.t=timeBand( dmS2C, "tfD", "left"   ),
            right.t=timeBand( dmS2C, "tfD", "right"  ),
             fact.t=timeBand( dmS2C, "tfD", "factor" ) )[1:15,]


###################################################
### code chunk number 18: flup.rnw:481-482
###################################################
summary( (dmS2$age-dmS2$tfD) - (dmS2$dodm-dmS2$dobth) ) 


###################################################
### code chunk number 19: flup.rnw:487-489
###################################################
summary( timeBand( dmS2, "age", "middle" ) -
         timeBand( dmS2, "tfD", "middle" ) - (dmS2$dodm-dmS2$dobth) )


###################################################
### code chunk number 20: flup.rnw:594-596
###################################################
dmCs <- splitMulti( dmC, age = seq(0,110,1/4) )
summary( dmCs, t=T )


###################################################
### code chunk number 21: flup.rnw:618-623
###################################################
( a.kn <- with( subset( dmCs, lex.Xst=="Dead" ), 
                quantile( age+lex.dur, (1:5-0.5)/5 ) ) )
( i.kn <- c( 0, 
          with( subset( dmCs, lex.Xst=="Dead" & lex.Cst=="Ins" ), 
                quantile( tfI+lex.dur, (1:4)/5 ) ) ) )


###################################################
### code chunk number 22: flup.rnw:639-644
###################################################
ma <- glm( (lex.Xst=="Dead") ~ Ns(age,knots=a.kn),
            family = poisson,
            offset = log(lex.dur),
              data = dmCs )
summary( ma )


###################################################
### code chunk number 23: flup.rnw:663-666
###################################################
Ma <- glm( cbind(lex.Xst=="Dead",lex.dur) ~ Ns(age,knots=a.kn),
           family = poisreg, data = dmCs )
summary( Ma )


###################################################
### code chunk number 24: flup.rnw:674-676
###################################################
Xa <- glm.Lexis( dmCs, from="DM", to="Dead", 
                 formula = ~ Ns(age,knots=a.kn) )


###################################################
### code chunk number 25: flup.rnw:679-680
###################################################
attr( Xa, "Lexis" )


###################################################
### code chunk number 26: flup.rnw:689-690
###################################################
xa <- glm.Lexis( dmCs, formula = ~ Ns(age,knots=a.kn) )


###################################################
### code chunk number 27: flup.rnw:693-694
###################################################
c( deviance(ma), deviance(Ma), deviance(Xa), deviance(xa) )


###################################################
### code chunk number 28: pr-a
###################################################
nd <- data.frame( age=40:85, lex.dur=1000 )
pr.0 <- ci.pred( ma, newdata = nd )      # mortality per 100 PY
pr.a <- ci.pred( Ma, newdata = nd )*1000 # mortality per 100 PY
summary(pr.0/pr.a)
matshade( nd$age, pr.a, plot=TRUE,
          type="l", lty=1,
          log="y", xlab="Age (years)",
          ylab="DM mortality per 1000 PY")


###################################################
### code chunk number 29: flup.rnw:741-745
###################################################
pm <- glm( cbind(lex.Xst=="Dead",lex.dur) ~ Ns(age,knots=a.kn) 
                                          + lex.Cst + sex,
           family=poisreg, data = dmCs )
round( ci.exp( pm ), 3 )


###################################################
### code chunk number 30: flup.rnw:759-763
###################################################
pm <- glm( cbind(lex.Xst=="Dead",lex.dur) ~ Ns(age,knots=a.kn) 
                                          + Ns(tfI,knots=i.kn) 
                                          + lex.Cst + sex,
           family=poisreg, data = tsNA20(dmCs) )


###################################################
### code chunk number 31: flup.rnw:769-775
###################################################
Pm <- glm.Lexis( tsNA20(dmCs), 
                 form = ~ Ns(age,knots=a.kn) 
                        + Ns(tfI,knots=i.kn) 
                        + lex.Cst + sex )
c( deviance(Pm), deviance(pm) )
identical( model.matrix(Pm), model.matrix(pm) )


###################################################
### code chunk number 32: flup.rnw:781-782
###################################################
round( ci.exp( Pm, subset="ex" ), 3 )


###################################################
### code chunk number 33: ins-time
###################################################
ndI <- data.frame( expand.grid( tfI=c(NA,seq(0,15,0.1)),
                                ai=seq(40,80,10) ),
                   sex="M",
                   lex.Cst="Ins" )
ndI <- transform( ndI, age=ai+tfI )
head( ndI )
ndA <- data.frame( age= seq(40,100,0.1), tfI=0,  lex.Cst="DM", sex="M" )
pri <- ci.pred( Pm, ndI ) * 1000
pra <- ci.pred( Pm, ndA ) * 1000
matshade( ndI$age, pri, plot=TRUE, las=1,
          xlab="Age (years)", ylab="DM mortality per 1000 PY",
          log="y", lty=1, col="blue" )
matshade( ndA$age, pra )


###################################################
### code chunk number 34: flup.rnw:819-823
###################################################
library( survival )
cm <- coxph( Surv(age,age+lex.dur,lex.Xst=="Dead") ~
             Ns(tfI,knots=i.kn) + lex.Cst + sex,
             data = tsNA20(dmCs) )


###################################################
### code chunk number 35: flup.rnw:827-830
###################################################
Cm <- coxph.Lexis( tsNA20(dmCs), 
                   form= age ~ Ns(tfI,knots=i.kn) + lex.Cst + sex )
cbind( ci.exp( cm ), ci.exp( Cm ) )


###################################################
### code chunk number 36: flup.rnw:839-842
###################################################
round( cbind( ci.exp( Pm ),
       rbind( matrix(NA,5,3),
              ci.exp( cm )[-6,] ) ), 3 )


###################################################
### code chunk number 37: Ieff
###################################################
nd <- data.frame( tfI=seq(0,15,,151), lex.Cst="Ins", sex="M" )
nr <- data.frame( tfI=    2         , lex.Cst="Ins", sex="M" )
ppr <- ci.exp( pm, list(nd,nr), xvars="age" )
cpr <- ci.exp( cm, list(nd,nr) )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( nd$tfI, cbind(ppr,cpr), plot=T, 
          lty=c(1,2), log="y",
          xlab="Time since insulin (years)", ylab="Rate ratio")
abline( h=1, lty=3 )


###################################################
### code chunk number 38: IeffR
###################################################
nd <- data.frame( tfI=seq(0,15,,151), lex.Cst="Ins", sex="M" )
nr <- data.frame( tfI=    0         , lex.Cst="DM" , sex="M" )
ppr <- ci.exp( pm, list(nd,nr), xvars="age" )
cpr <- ci.exp( cm, list(nd,nr) )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( nd$tfI, cbind(ppr,cpr), 
          xlab="Time since insulin (years)",
          ylab="Rate ratio relative to non-Insulin",
          lty=c(1,2), log="y", plot=T )


###################################################
### code chunk number 39: flup.rnw:948-953
###################################################
imx <- glm.Lexis( tsNA20(dmCs), 
                 formula = ~ Ns(age    ,knots=a.kn) 
                           + Ns(    tfI,knots=i.kn)
                           + Ns(age-tfI,knots=a.kn)
                           + lex.Cst + sex )


###################################################
### code chunk number 40: flup.rnw:963-973
###################################################
Im <- glm.Lexis( tsNA20(dmCs), 
                 formula = ~ Ns(age    ,knots=a.kn) 
                           + Ns(    tfI,knots=i.kn)
                           + Ns((age-tfI)*(lex.Cst=="Ins"),knots=a.kn)
                           + lex.Cst + sex )
im <- glm.Lexis( tsNA20(dmCs), 
                 formula = ~ Ns(age    ,knots=a.kn) 
                           + Ns(    tfI,knots=i.kn)
                           + lex.Cst:Ns(age-tfI,knots=a.kn)
                           + lex.Cst + sex )


###################################################
### code chunk number 41: flup.rnw:988-989
###################################################
anova( imx, Im, im, test='Chisq')


###################################################
### code chunk number 42: dur-int
###################################################
pxi <- ci.pred( imx, ndI )
pxa <- ci.pred( imx, ndA )
pIi <- ci.pred( Im , ndI )
pIa <- ci.pred( Im , ndA )
pii <- ci.pred( im , ndI )
pia <- ci.pred( im , ndA )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( ndI$age, cbind( pxi, pIi, pii)*1000, plot=T, log="y",
          xlab="Age", ylab="Mortality per 1000 PY",
          lty=1, lwd=2, col=c("blue","forestgreen","red"), alpha=0.1 )
matshade( ndA$age, cbind( pxa, pIa, pia)*1000, 
          lty=1, lwd=2, col=c("blue","forestgreen","red"), alpha=0.1 )


###################################################
### code chunk number 43: dur-int-RR
###################################################
ndR <- transform( ndI, tfI=0, lex.Cst="DM" )
cbind( head(ndI), head(ndR) )
Rxi <- ci.exp( imx, list(ndI,ndR) )
Rii <- ci.exp( im , list(ndI,ndR) )
RIi <- ci.exp( Im , list(ndI,ndR) )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( ndI$age, cbind( Rxi, RIi, Rii), plot=T, log="y",
          xlab="Age (years)", ylab="Rate ratio vs, non-Insulin",
          lty=1, lwd=2, col=c("blue","forestgreen","red"), alpha=0.1 )
abline( h=1 )
abline( h=ci.exp(imx,subset="lex.Cst")[,1], lty="25", col="blue" )


###################################################
### code chunk number 44: splint
###################################################
gm <- glm.Lexis( tsNA20(dmCs), 
                 formula = ~ Ns(age,knots=a.kn) 
                           + Ns(tfI,knots=i.kn)
                           + lex.Cst:Ns(age,knots=a.kn):Ns(tfI,knots=i.kn)
                           + lex.Cst + sex )
pgi <- ci.pred( gm, ndI )
pga <- ci.pred( gm, ndA )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( ndI$age, cbind( pgi, pii )*1000,  plot=T,
          lty=c("solid","21"), lend="butt", lwd=2, log="y",
          xlab="Age (years)", ylab="Mortality rates per 1000 PY",
          alpha=c(0.2,0.1), col=c("black","red") )
matshade( ndA$age, cbind( pga, pia )*1000,
          lty=c("solid","21"), lend="butt", lwd=2,
          alpha=c(0.2,0.1), col=c("black","red") )


###################################################
### code chunk number 45: RR-int
###################################################
ndR <- transform( ndI, lex.Cst="DM", tfI=0 )
iRR <- ci.exp( im, ctr.mat=list(ndI,ndR) )
gRR <- ci.exp( gm, ctr.mat=list(ndI,ndR) )
par( mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, las=1, bty="n" )
matshade( ndI$age, cbind(gRR,iRR), lty=1, log="y", plot=TRUE, 
          xlab="Age (years)", ylab="Rate ratio: Ins vs. non-Ins",
          col=c("black","red") )
abline( h=1 )


###################################################
### code chunk number 46: flup.rnw:1112-1125
###################################################
dmd <- glm.Lexis( dmCs,
                  from="DM", to="Dead",
                  formula = ~ Ns(age,knots=a.kn) 
                            + sex )
ind <- glm.Lexis( dmCs,
                  from="Ins", to="Dead",
                  formula = ~ Ns(age,knots=a.kn) 
                            + Ns(tfI,knots=i.kn)
                            + Ns(age-tfI,knots=a.kn)
                            + sex )
ini <- ci.pred( ind, ndI )
dmi <- ci.pred( dmd, ndI )
dma <- ci.pred( dmd, ndA )


###################################################
### code chunk number 47: sep-mort
###################################################
par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n")
matshade( ndI$age, ini*1000, plot=TRUE, log="y",
          xlab="Age (years)", ylab="Mortality rates per 1000 PY",
          lwd=2, col="red" )
matshade( ndA$age, dma*1000,
          lwd=2, col="black" )


###################################################
### code chunk number 48: sep-HR
###################################################
par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n")
matshade( ndI$age, ci.ratio(ini,dmi), plot=TRUE, log="y",
          xlab="Age (years)", ylab="RR insulin vs. no insulin",
          lwd=2, col="red" )
abline( h=1 )


###################################################
### code chunk number 49: flup.rnw:1171-1179
###################################################
dmCs <- cutLexis( data = dmS2, 
                    cut = dmS2$doins,
              timescale = "per",
              new.state = "Ins",
              new.scale = "tfI",
       precursor.states = "DM",
           split.states = TRUE )
summary( dmCs )


###################################################
### code chunk number 50: box4
###################################################
boxes( dmCs, boxpos=list(x=c(15,15,85,85),
                         y=c(85,15,85,15)),
       scale.R=1000, show.BE=TRUE )


###################################################
### code chunk number 51: flup.rnw:1209-1217
###################################################
dmM <- mcutLexis( dmL,
             timescale = "per",
                    wh = c("doins","dooad"), 
            new.states = c("Ins","OAD"),
            new.scales = c("tfI","tfO"),
      precursor.states = "DM",
          ties.resolve = TRUE )
summary( dmM, t=T )


###################################################
### code chunk number 52: flup.rnw:1221-1226
###################################################
wh <- c(subset(dmM,lex.Cst=="Ins-OAD")$lex.id[1:2],
        subset(dmM,lex.Cst=="OAD-Ins")$lex.id[1:2])
options( width=110 )
print( subset( dmM, lex.id %in% wh )[,c('lex.id',names(dmM[1:8]),c("doins","dooad"))],
       digits=6, row.names=FALSE )


###################################################
### code chunk number 53: mbox
###################################################
boxes( dmM, boxpos=list(x=c(15,80,40,40,85,85),
                        y=c(50,50,90,10,90,10)),
            scale.R=1000, show.BE=TRUE )


###################################################
### code chunk number 54: mboxr
###################################################
summary( dmMr <- Relevel( dmM, list('OAD+Ins'=5:6), first=FALSE) )
boxes( dmMr, boxpos=list(x=c(15,50,15,85,85),
                         y=c(85,50,15,85,15)),
             scale.R=1000, show.BE=TRUE )


