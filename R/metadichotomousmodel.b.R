
# This file is a generated template, your changes will not be overwritten

metaDichotomousModelClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "metaDichotomousModelClass",
    inherit = metaDichotomousModelBase,
    private = list(
      .run = function() {
        ai <- self$options$ai
        n1i <- self$options$n1i
        ci <- self$options$ci
        n2i <- self$options$n2i
        mods <- self$options$moderatorcor
        slab <- self$options$slab
        #includemods <- self$options$includemods
        addcred <- self$options$addcred
        addfit <- self$options$addfit
        showweights <- self$options$showweights
        level <- self$options$level
        fsntype <- self$options$fsntype
        method2 <- self$options$methodmetamdms
        mdmseasure <- self$options$mdmsmeasure
        yaxis <- self$options$yaxis
        steps <- self$options$steps
        pchForest <- self$options$pchForest
        table <- self$results$textRICH
        
        
        ready <- TRUE
        if (is.null(self$options$ai) || is.null(self$options$n1i) || is.null(self$options$ci) || is.null(self$options$n2i) == TRUE){
          
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject("Incidents, Sample Size, and Study Label fields must be populated to run analysis", code='')
        }
        if (is.null(self$options$slab) == TRUE){
          
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject("Study Label fields must be populated to run analysis", code='')
        }

        if (ready == TRUE) {
          
          if (is.null(self$options$moderatorcor) == FALSE){
            data <- data.frame(ai = self$data[[self$options$ai]], n1i = self$data[[self$options$n1i]], ci = self$data[[self$options$ci]], n2i = self$data[[self$options$n2i]], mods = self$data[[self$options$moderatorcor]], slab = self$data[[self$options$slab]])
            data[[ai]] <- jmvcore::toNumeric(data[[ai]])
            data[[n1i]] <- jmvcore::toNumeric(data[[n1i]])
            data[[ci]] <- jmvcore::toNumeric(data[[ci]])
            data[[n2i]] <- jmvcore::toNumeric(data[[n2i]])
            data[[mods]] <- jmvcore::toNumeric(data[[mods]])
            data$checkG1 <- 0
            data$checkG2 <- 0
            data$checkG1 <- data$n1i - data$ai
            data$checkG2 <- data$n2i - data$ci
            if (data$checkG1 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents in the experimental group is higher than the associated sample size group, check your data ", code='')
            }
            if (data$checkG2 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents in the control group is higher than the associated sample size group, check your data ", code='')
            }
          } else {
            data <- data.frame(ai = self$data[[self$options$ai]], n1i = self$data[[self$options$n1i]], ci = self$data[[self$options$ci]], n2i = self$data[[self$options$n2i]], slab = self$data[[self$options$slab]])
            data[[ai]] <- jmvcore::toNumeric(data[[ai]])
            data[[n1i]] <- jmvcore::toNumeric(data[[n1i]])
            data[[ci]] <- jmvcore::toNumeric(data[[ci]])
            data[[n2i]] <- jmvcore::toNumeric(data[[n2i]])
            data$checkG1 <- 0
            data$checkG2 <- 0
            data$checkG1 <- data$n1i - data$ai
            data$checkG2 <- data$n2i - data$ci
            if (data$checkG1 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents in the experimental group is higher than the associated sample size group, check your data ", code='')
            }
            if (data$checkG2 < 0){
              #ready <- FALSE
              jmvcore::reject("Number of incidents in the control group is higher than the associated sample size group, check your data ", code='')
            }
          }
          
          if (is.null(self$options$moderatorcor) == FALSE){
            res <- metafor::rma(ai=ai, n1i=n1i, ci=ci, n2i=n2i, mods=mods, method=method2, measure=mdmseasure, data=data, slab=slab, level=level)
            if (is.list(res) == FALSE){
              jmvcore::reject("Check sample size and incident data", code='')
            }
            
            } else {

            res <- try(metafor::rma(ai=ai, n1i=n1i, ci=ci, n2i=n2i, method=method2, measure=mdmseasure, data=data, slab=slab, level=level))
            if (is.list(res) == FALSE){
              jmvcore::reject("Check sample size and incident data", code='')
            }
          }
          
            # else if (message == 'missing value where TRUE/FALSE needed')
            #   message <- 'One or both variables contain infinite values'
            # if (is.character(res)== TRUE)
            #   jmvcore::reject("Check sample size and incident data", code='')
          
          
          
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
          
          if (self$options$methodmetamdms == "DL"){
            tau2EstimatorName = "DerSimonian-Laird"
          } else if (self$options$methodmetamdms == "HE"){
            tau2EstimatorName = "Hedges"
          } else if (self$options$methodmetamdms == "HS"){
            tau2EstimatorName = "Hunter-Schmidt"
          } else if (self$options$methodmetamdms == "SJ"){
            tau2EstimatorName = "Sidik-Jonkman"
          } else if (self$options$methodmetamdms == "ML"){
            tau2EstimatorName = "Maximum-Likelihood"
          } else if (self$options$methodmetamdms == "REML"){
            tau2EstimatorName = "Restricted Maximum-Likelihood"
          } else if (self$options$methodmetamdms == "EB"){
            tau2EstimatorName = "Empirical Bayes"
          } else if (self$options$methodmetamdms == "PM"){
            tau2EstimatorName = "Paule-Mandel"
          }
          
          if (is.null(self$options$moderatorcor) == FALSE){
            titleMix <- paste("Mixed-Effects Model (k = ",res$k,")",sep="")
            titleMixNote <- paste("Tau\u00B2 Estimator: ",tau2EstimatorName, sep="")
            table$setTitle(title=titleMix)
            table$setNote("mixnote",titleMixNote)
          } else if (self$options$methodmetamdms == "FE"){
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
        if (is.null(self$options$ai) || is.null(self$options$n1i) || is.null(self$options$ci) || is.null(self$options$n2i) == TRUE){
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
     # Funnel Plot Function
      .funplot=function(imageFUN, ...) {  # <-- the plot function
        plotDataFUN <- imageFUN$state
        yaxis <- self$options$yaxis
        yaxisInv <- self$options$yaxisInv
        enhancePlot <- self$options$enhanceFunnel
        ready <- TRUE
        if (is.null(self$options$ai) || is.null(self$options$n1i) || is.null(self$options$ci) || is.null(self$options$n2i) == TRUE){
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
      }
      )
)