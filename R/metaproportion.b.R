
# This file is a generated template, your changes will not be overwritten

metaProportionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "metaProportionClass",
    inherit = metaProportionBase,
    private = list(
      .run = function() {
        xi <- self$options$xi
        ni <- self$options$ni
        mods <- self$options$moderatorcor
        fsntype <- self$options$fsntype
        method2 <- self$options$methodmetacor
        cormeasure <- self$options$cormeasure
        slab <- self$options$slab
        #includemods <- self$options$includemods
        addcred <- self$options$addcred
        addfit <- self$options$addfit
        showweights <- self$options$showweights
        level <- self$options$level
        #yaxis <- self$options$yaxis
        #data <- self$data
        steps <- self$options$steps
        pchForest <- self$options$pchForest
        table <- self$results$textRICH
        
        
        ready <- TRUE
        if (is.null(self$options$xi) || is.null(self$options$ni) == TRUE){
          
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject("Number of indivduals with the experiencing the event, Sample Sizes, and Study Label fields must be populated to run analysis", code='')
        }
        if (is.null(self$options$slab) == TRUE){
          
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject("Study Label fields must be populated to run analysis", code='')
        }
        if (ready == TRUE) {
          
          if (is.null(self$options$moderatorcor) == FALSE){
            data <- data.frame(xi = self$data[[self$options$xi]], ni = self$data[[self$options$ni]], mods = self$data[[self$options$moderatorcor]], slab = self$data[[self$options$slab]])
            data[[xi]] <- jmvcore::toNumeric(data[[xi]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data[[mods]] <- jmvcore::toNumeric(data[[mods]])
            data$checkG1 <- 0
            data$checkG1 <- data$ni - data$xi
            if (data$checkG1 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents is higher than the sample size , check your data ", code='')
            }

          } else {
            data <- data.frame(xi = self$data[[self$options$xi]], ni = self$data[[self$options$ni]], slab = self$data[[self$options$slab]])
            data[[xi]] <- jmvcore::toNumeric(data[[xi]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data$checkG1 <- 0
            data$checkG1 <- data$ni - data$xi
            if (data$checkG1 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents is higher than the sample size , check your data ", code='')
            }

          }
          
          if (is.null(self$options$moderatorcor) == FALSE){
            res <- metafor::rma(xi=xi, ni=ni, method=method2, measure=cormeasure, mods=mods, data=data, slab=slab, level=level)
          } else {
            res <- metafor::rma(xi=xi, ni=ni, method=method2, measure=cormeasure, data=data, slab=slab, level=level)
          }
          
          #}
          
          # if (self$options$includemods == TRUE) {
          #   data <- data.frame(ri = self$data[[self$options$rcor]], ni = self$data[[self$options$samplesize]], mods = self$data[[self$options$moderatorcor]], slab = self$data[[self$options$slab]])
          #   data[[ri]] <- jmvcore::toNumeric(data[[ri]])
          #   data[[ni]] <- jmvcore::toNumeric(data[[ni]])
          #   data[[mods]] <- jmvcore::toNumeric(data[[mods]])
          # } else {
          #   data <- data.frame(ri = self$data[[self$options$rcor]], ni = self$data[[self$options$samplesize]], slab = self$data[[self$options$slab]])
          #   data[[ri]] <- jmvcore::toNumeric(data[[ri]])
          #   data[[ni]] <- jmvcore::toNumeric(data[[ni]])
          # }
          # 
          # if (self$options$includemods == TRUE) {
          #   res <- metafor::rma(ri=ri, ni=ni, method=method2, measure=cormeasure, mods=mods, data=data, slab=slab, level=level)
          # } else {
          #   res <- metafor::rma(ri=ri, ni=ni, method=method2, measure=cormeasure, data=data, slab=slab, level=level)
          # }
          
          
          #Pub Bias
          failsafePB <- metafor::fsn(yi=res$yi, vi=res$vi, type=fsntype)
          ranktestPB <- metafor::ranktest(res)
          regtestPB <- metafor::regtest(res)
          
          
          fsnRICH <- self$results$pubBias$fsnRICH
          
          fsnRICH$setRow(rowNo=1, values=list(
            failSafeNumber=failsafePB$fsnum[1],
            p=failsafePB$pval[1]
          ))
          fsnTitle <- paste("Fail-Safe N Analysis (File Drawer Analysis)")
          fsnNote <- paste("Fail-safe N Calculation Using the ",fsntype," Approach", sep="")
          fsnRICH$setTitle(title=fsnTitle)
          fsnRICH$setNote("fsnNoteTable",fsnNote)
          
          rankRICH <- self$results$pubBias$rankRICH
          rankRICH$setRow(rowNo=1, values=list(
            rankTau=ranktestPB$tau[1],
            p=ranktestPB$pval[1]
          ))
          
          regRICH <- self$results$pubBias$regRICH
          regRICH$setRow(rowNo=1, values=list(
            Z=regtestPB$zval[1],
            p=regtestPB$pval[1]
          ))
          
          # # Extracting the effect sizes and sampling variances:
          # effect <- res$yi
          # v <- res$vi
          # 
          # # The weight-function model with no mean model:
          # wfRES <- weightr::weightfunct(effect, v)
          # 
          # 
          # self$results$weightFunctionModel$setContent(wfRES)
          #Model Fit 
          modelFitRICH <- self$results$modelFitRICH
          modelFitRICH$setRow(rowNo=1, values=list(
            label="Maximum-Likelihood",
            loglikelihood=res$fit.stats[1,1],
            deviance=res$fit.stats[2,1],
            AIC=res$fit.stats[3,1],
            BIC=res$fit.stats[4,1],
            AICc=res$fit.stats[5,1]
          ))        
          
          
          modelFitRICH$setRow(rowNo=2, values=list(
            label="Restricted Maximum-Likelihood",
            loglikelihood=res$fit.stats[1,2],
            deviance=res$fit.stats[2,2],
            AIC=res$fit.stats[3,2],
            BIC=res$fit.stats[4,2],
            AICc=res$fit.stats[5,2]
          ))
          
          #fit statistics and information criteria
          #Show if checked, hide if unchecked
          if (self$options$showModelFit == TRUE) {
            modelFitRICH$setVisible(visible=TRUE)
          } else {
            modelFitRICH$setVisible(visible=FALSE)
          }
          
          #Pub Bias Connections
          #self$results$pubBias$fsn$setContent(failsafePB)
          #self$results$pubBias$rank$setContent(ranktestPB)
          #self$results$pubBias$reg$setContent(regtestPB)
          
          #Data Prep: Results Table
          CILB <- round(res$ci.lb[1], 3)
          CIUB <- round(res$ci.ub[1], 3)
          ciLBUB <- paste(CILB,"-",CIUB)
          
          
          # Results Table
          #table <- self$results$textRICH
          # esDataNULL <- is.null(self$options$rcor)
          # if (esDataNULL == TRUE){
          #   table$setError("error")
          # }
          
          table$setRow(rowNo=1, values=list(
            Intercept="Intercept",
            Estimate=as.numeric(res$b[1]),
            se=res$se[1],
            CILow=res$ci.lb[1],
            CIHigh=res$ci.ub[1],
            p=res$pval[1],
            Z=res$zval[1],
            k=res$k
          ))
          
          if (self$options$methodmetacor == "DL"){
            tau2EstimatorName = "DerSimonian-Laird"
          } else if (self$options$methodmetacor == "HE"){
            tau2EstimatorName = "Hedges"
          } else if (self$options$methodmetacor == "HS"){
            tau2EstimatorName = "Hunter-Schmidt"
          } else if (self$options$methodmetacor == "SJ"){
            tau2EstimatorName = "Sidik-Jonkman"
          } else if (self$options$methodmetacor == "ML"){
            tau2EstimatorName = "Maximum-Likelihood"
          } else if (self$options$methodmetacor == "REML"){
            tau2EstimatorName = "Restricted Maximum-Likelihood"
          } else if (self$options$methodmetacor == "EB"){
            tau2EstimatorName = "Empirical Bayes"
          } else if (self$options$methodmetacor == "PM"){
            tau2EstimatorName = "Paule-Mandel"
          }
          
          if (is.null(self$options$moderatorcor) == FALSE){
            titleMix <- paste("Mixed-Effects Model (k = ",res$k,")",sep="")
            titleMixNote <- paste("Tau\u00B2 Estimator: ",tau2EstimatorName, sep="")
            table$setTitle(title=titleMix)
            table$setNote("mixnote",titleMixNote)
          } else if (self$options$methodmetacor == "FE"){
            titleFix <- paste("Fixed-Effects Model (k = ",res$k,")",sep="")
            table$setTitle(title=titleFix)
            
          } else {
            titleRan <- paste("Random-Effects Model (k = ",res$k,")",sep="")
            titleRanNote <- paste("Tau\u00B2 Estimator: ",tau2EstimatorName, sep="")
            table$setTitle(title=titleRan)
            table$setNote("rannote",titleRanNote)
          }
          
          if (is.null(self$options$moderatorcor) == FALSE){
            
            modCILB <- round(res$ci.lb[2], 3)
            modCIUB <- round(res$ci.ub[2], 3)
            
            table$setRow(rowNo=2, values=list(
              Intercept="Moderator",
              Estimate=as.numeric(res$b[2]),
              se=res$se[2],
              CILow=res$ci.lb[2],
              CIHigh=res$ci.ub[2],
              p=res$pval[2],
              Z=res$zval[2],
              k=res$k
            ))
            
          } else {
            table$setRow(rowNo=2, values=list(
              Intercept=" ",
              Estimate=NULL,
              se=NULL,
              CILow=NULL,
              CIHigh=NULL,
              p=NULL,
              Z=NULL,
              k=NULL   
            ))
          }
          
          #Data Prep: Heterogeneity Stats
          tauSquared <- round(res$tau2, 4)
          tauSquaredSE <- round(res$se.tau2, 4)
          tauSqCombind <- paste(tauSquared,"(SE=",tauSquaredSE,")")
          tauOnly <- round(sqrt(res$tau2), 4)
          ISquStat <- paste(round(res$I2, 2),"%",sep="")
          HSquStat <- round(res$H2, 4)
          
          if (is.null(self$options$moderatorcor) == FALSE){
            RSquStat <- paste(round(res$R2, 2),"%",sep="")
          } else {
            RSquStat <- NULL
          }
          
          #Data Prep: Heterogeneity Test
          QTestStatDF <- round(res$k - 1, 4)
          
          #Heterogeneity Stats annd Test Table
          tableTauSqaured <- self$results$tableTauSqaured
          tableTauSqaured$setRow(rowNo=1, values=list(
            tauSqComb=tauSqCombind,
            tauSQRT=tauOnly,
            ISqu=ISquStat,
            HSqu=HSquStat,
            RSqu=RSquStat,
            QallDF=QTestStatDF,
            Qall=res$QE,
            QallPval=res$QEp
          )) 
          
          
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          image <- self$results$plot
          imageFUN <- self$results$funplot
          
          image$setState(res)
          imageFUN$setState(res)
          
          # }}))
        }},
      #Forest Plot Function
      .plot=function(image, ...) {  # <-- the plot function
        plotData <- image$state
        #StudyID <- self$options$studylabels
        #yi <- self$options$yi
        #vi <- self$options$vi
        #res <- metafor::rma(yi=yi, vi=vi, data=self$data)
        addcred <- self$options$addcred
        addfit <- self$options$addfit
        level <- self$options$level
        showweights <- self$options$showweights
        xlab <- self$options$xAxisTitle
        order <- self$options$forestOrder
        steps <- self$options$steps
        pchForest <- self$options$pchForest
        pch <- as.numeric(pchForest)
        ready <- TRUE
        if (is.null(self$options$xi) || is.null(self$options$ni) == TRUE){
          #if (is.null(self$options$rcor) == TRUE){
          
          ready <- FALSE
        }
        if (is.null(image$state$yi) || is.null(image$state$vi) == TRUE){
          ready <- FALSE
        }
        if (ready == TRUE) {
          
          #plot <- metafor::forest(plotData$yi, plotData$vi, addcred=addcred, addfit=addfit)
          plot <- metafor::forest(plotData, addcred=addcred, addfit=addfit, level=level, showweights=showweights, xlab=xlab, order=order, steps=steps, pch=pch)
          print(plot)
          TRUE}
      },
      #Funnel Plot Function
      .funplot=function(imageFUN, ...) {  # <-- the plot function
        plotDataFUN <- imageFUN$state
        yaxis <- self$options$yaxis
        yaxisInv <- self$options$yaxisInv
        enhancePlot <- self$options$enhanceFunnel
        ready <- TRUE
        if (is.null(self$options$xi) || is.null(self$options$ni) == TRUE){
          #if (is.null(self$options$rcor) == TRUE){
          
          ready <- FALSE
        }
        if (is.null(imageFUN$state$yi) || is.null(imageFUN$state$vi) == TRUE){
          ready <- FALSE
        }
        if (ready == TRUE) {
          
          if (self$options$yaxisInv == TRUE) {
            if (self$options$enhanceFunnel == TRUE) {
              yaxisTrans <- paste(yaxis,"nv",sep="")
              plotFUN <- metafor::funnel(plotDataFUN,yaxis=yaxisTrans,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"))
            } else {
              yaxisTrans <- paste(yaxis,"nv",sep="")
              plotFUN <- metafor::funnel(plotDataFUN,yaxis=yaxisTrans)
            }
            
          } else {
            if (self$options$enhanceFunnel == TRUE) {
              plotFUN <- metafor::funnel(plotDataFUN,yaxis=yaxis,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"))
            } else {
              plotFUN <- metafor::funnel(plotDataFUN,yaxis=yaxis)
            }
          }
          print(plotFUN)
          TRUE}
      })
)