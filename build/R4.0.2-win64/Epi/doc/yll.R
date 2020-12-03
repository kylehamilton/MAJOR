### R code from vignette source 'yll'
### Encoding: UTF-8

###################################################
### code chunk number 1: yll.rnw:21-24
###################################################
options( width=90,
         SweaveHooks=list( fig=function()
         par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n") ) )


###################################################
### code chunk number 2: states
###################################################
library( Epi )
TM <- matrix(NA,4,4)
rownames(TM) <-
colnames(TM) <- c("Well","DM","Dead","Dead(DM)") 
TM[1,2:3] <- TM[2,4] <- 1
zz <- boxes( TM, boxpos=list(x=c(20,80,20,80),y=c(80,80,20,20)), wm=1.5, hm=4 )


###################################################
### code chunk number 3: states
###################################################
zz$Arrowtext <- c( expression(lambda),   
                   expression(mu[W]),    
                   expression(mu[D][M]) )
boxes( zz )


###################################################
### code chunk number 4: yll.rnw:265-266
###################################################
data( DMepi )


###################################################
### code chunk number 5: yll.rnw:271-273
###################################################
str( DMepi )
head( DMepi )


###################################################
### code chunk number 6: yll.rnw:293-297
###################################################
DMepi <- transform( subset( DMepi, A>30 ),
                    D.T = D.nD + D.DM, 
                    Y.T = Y.nD + Y.DM )
head(DMepi)


###################################################
### code chunk number 7: yll.rnw:303-329
###################################################
# Knots used in all models
( a.kn <- seq(40,95,,6) )
( p.kn <- seq(1997,2015,,4) )
( c.kn <- seq(1910,1976,,6) )
# Check the number of events between knots
ae <- xtabs( cbind(D.nD,D.DM,X) ~ cut(A,c(30,a.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(ae,1), col.vars=3:2 )
pe <- xtabs( cbind(D.nD,D.DM,X) ~ cut(P,c(1990,p.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(pe,1), col.vars=3:2 )
ce <- xtabs( cbind(D.nD,D.DM,X) ~ cut(P-A,c(-Inf,c.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(ce,1), col.vars=3:2 )
# Fit an APC-model for all transitions, seperately for men and women
mW.m <- glm( D.nD ~ -1 + Ns(A  ,knots=a.kn,int=TRUE) +
                         Ns(  P,knots=p.kn,ref=2005) +
                         Ns(P-A,knots=c.kn,ref=1950), 
           offset = log(Y.nD),
           family = poisson,
             data = subset( DMepi, sex=="M" ) )
mD.m <- update( mW.m,  D.DM ~ . , offset=log(Y.DM) )
mT.m <- update( mW.m,  D.T  ~ . , offset=log(Y.T ) )
lW.m <- update( mW.m,  X ~ . )
# Model for women
mW.f <- update( mW.m, data = subset( DMepi, sex=="F" ) )
mD.f <- update( mD.m, data = subset( DMepi, sex=="F" ) )
mT.f <- update( mT.m, data = subset( DMepi, sex=="F" ) )
lW.f <- update( lW.m, data = subset( DMepi, sex=="F" ) )


###################################################
### code chunk number 8: yll.rnw:336-373
###################################################
a.ref <- 30:90
p.ref <- 1996:2016
aYLL <- NArray( list( type = c("Imm","Tot","Sus"),
                       sex = levels( DMepi$sex ),
                       age = a.ref,
                      date = p.ref ) )
str( aYLL )
system.time(
for( ip in p.ref )
   {
   nd <- data.frame( A = seq(30,90,0.2)+0.1,
                     P = ip,
                  Y.nD = 1,
                  Y.DM = 1,
                  Y.T  = 1 )
   muW.m <- ci.pred( mW.m, nd )[,1]
   muD.m <- ci.pred( mD.m, nd )[,1]
   muT.m <- ci.pred( mT.m, nd )[,1]
   lam.m <- ci.pred( lW.m, nd )[,1]
   muW.f <- ci.pred( mW.f, nd )[,1]
   muD.f <- ci.pred( mD.f, nd )[,1]
   muT.f <- ci.pred( mT.f, nd )[,1]
   lam.f <- ci.pred( lW.f, nd )[,1]
   aYLL["Imm","M",,paste(ip)] <- yll( int=0.2, muW.m, muD.m, lam=NULL, 
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   aYLL["Imm","F",,paste(ip)] <- yll( int=0.2, muW.f, muD.f, lam=NULL,  
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   aYLL["Tot","M",,paste(ip)] <- yll( int=0.2, muT.m, muD.m, lam=NULL,  
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   aYLL["Tot","F",,paste(ip)] <- yll( int=0.2, muT.f, muD.f, lam=NULL,  
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   aYLL["Sus","M",,paste(ip)] <- yll( int=0.2, muW.m, muD.m, lam=lam.m, 
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   aYLL["Sus","F",,paste(ip)] <- yll( int=0.2, muW.f, muD.f, lam=lam.f, 
                                      A=a.ref, age.in=30, note=FALSE )[-1]
   } )
round( ftable( aYLL[,,seq(1,61,10),], col.vars=c(3,2) ), 1 )


###################################################
### code chunk number 9: imm
###################################################
plyll <- function(wh){
par( mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, bty="n", las=1 )

matplot( a.ref, aYLL[wh,"M",,],
         type="l", lty=1, col="blue", lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:10,col=gray(0.7))
text( 90, 11, "Men", col="blue", adj=1 )
text( 40, aYLL[wh,"M","40","1996"], "1996", adj=c(0,0), col="blue" )
text( 43, aYLL[wh,"M","44","2016"], "2016", adj=c(1,1), col="blue" )

matplot( a.ref, aYLL[wh,"F",,],
         type="l", lty=1, col="red", lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:10,col=gray(0.7))
text( 90, 11, "Women", col="red", adj=1 )
text( 40, aYLL[wh,"F","40","1996"], "1996", adj=c(0,0), col="red" )
text( 43, aYLL[wh,"F","44","2016"], "2016", adj=c(1,1), col="red" )
}
plyll("Imm")


###################################################
### code chunk number 10: tot
###################################################
plyll("Tot")


###################################################
### code chunk number 11: sus
###################################################
plyll("Sus")


###################################################
### code chunk number 12: CHANGE1 (eval = FALSE)
###################################################
## source( "../R/erl.R", keep.source=TRUE )


###################################################
### code chunk number 13: CHANGE2
###################################################
surv1 <- Epi::surv1
surv2 <- Epi::surv2
erl1 <- Epi::erl1
erl <- Epi::erl
yll <- Epi::yll


###################################################
### code chunk number 14: yll.rnw:484-485
###################################################
surv1


###################################################
### code chunk number 15: yll.rnw:489-490
###################################################
erl1


###################################################
### code chunk number 16: yll.rnw:497-498
###################################################
surv2


###################################################
### code chunk number 17: yll.rnw:502-503
###################################################
erl


###################################################
### code chunk number 18: yll.rnw:507-508
###################################################
yll


