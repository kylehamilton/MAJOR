

# This file is a generated template, your changes will not be overwritten

metaDVClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "metaDVClass",
    inherit = metaDVBase,
    private = list(
      .run = function() {
        yi <- self$options$effectSize
        vi <- self$options$samplingVariances
        moderator <- self$options$moderatorcor
        fsntype <- self$options$fsntype
        method2 <- self$options$methodmetacor
        #cormeasure <- self$options$cormeasure
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
        moderatorType <- self$options$moderatorType
        lowerTOST <- self$options$lowerTOST
        upperTOST <- self$options$upperTOST
        alphaTOST <- self$options$alphaTOST
        
        
        ready <- TRUE
        if (is.null(self$options$effectSize) ||
            is.null(self$options$samplingVariances) ||
            is.null(self$options$slab) == TRUE) {

            ready <- FALSE
            # I really need to think of a better error message this is a place holder until I figure something out
            jmvcore::reject(
              "Effect Sizes, Sampling Variances, and Study Label fields must be populated to run analysis",
              code = ''
            )
          }
          if (is.null(self$options$slab) == TRUE) {
            ready <- FALSE
            # I really need to think of a better error message this is a place holder until I figure something out
            jmvcore::reject("Study Label fields must be populated to run analysis", code =
                              '')
          }
          
          if (ready == TRUE) {
            if (self$options$moderatorType == "NON") {
              if (is.null(self$options$moderatorcor) == FALSE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject("Must Remove Moderator Variable", code =
                                  '')
              }
              data <-
                data.frame(yi = self$data[[self$options$effectSize]],
                           vi = self$data[[self$options$samplingVariances]],
                           slab = self$data[[self$options$slab]])
              data[[yi]] <- jmvcore::toNumeric(data[[yi]])
              data[[vi]] <- jmvcore::toNumeric(data[[vi]])
              
              res <-
                metafor::rma(
                  yi = yi,
                  vi = vi,
                  method = method2,
                  data = data,
                  slab = slab,
                  level = level
                )
              
              resTOST <-
                TOSTER::TOSTmeta(
                  ES = res$beta,
                  se = res$se,
                  low_eqbound_d = lowerTOST,
                  high_eqbound_d = upperTOST,
                  alpha = alphaTOST,
                  plot = FALSE,
                  verbose = FALSE
                )
              resTOST$se <- res$se
              
              resTOSTText <- capture.output(TOSTER::TOSTmeta(
                ES = res$beta,
                se = res$se,
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                verbose = TRUE,
                plot = FALSE
              ))
              
            }
            
            if (self$options$moderatorType == "CON") {
              if (is.null(self$options$moderatorcor) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject("Must Supply a Moderator Variable", code =
                                  '')
              }
              
              data <-
                data.frame(
                  yi = self$data[[self$options$effectSize]],
                  vi = self$data[[self$options$samplingVariances]],
                  moderator = self$data[[self$options$moderatorcor]],
                  slab = self$data[[self$options$slab]]
                )
              data[[yi]] <- jmvcore::toNumeric(data[[yi]])
              data[[vi]] <- jmvcore::toNumeric(data[[vi]])
              data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
              
              res <-
                metafor::rma(
                  yi = yi,
                  vi = vi,
                  method = method2,
                  mods = moderator,
                  data = data,
                  slab = slab,
                  level = level
                )
              
              resTOST <-
                TOSTER::TOSTmeta(
                  ES = res$beta,
                  se = res$se,
                  low_eqbound_d = lowerTOST,
                  high_eqbound_d = upperTOST,
                  alpha = alphaTOST,
                  plot = FALSE,
                  verbose = FALSE
                )
              resTOST$se <- res$se
              
              resTOSTText <- capture.output(TOSTER::TOSTmeta(
                ES = res$beta,
                se = res$se,
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                verbose = TRUE,
                plot = FALSE
              ))
              
              }
            
            if ((self$options$moderatorType) == "CAT") {
              if (is.null(self$options$moderatorcor) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject("Must Supply a Moderator Variable", code =
                                  '')
              }
              
              data <-
                data.frame(
                  yi = self$data[[self$options$effectSize]],
                  vi = self$data[[self$options$samplingVariances]],
                  moderator = self$data[[self$options$moderatorcor]],
                  slab = self$data[[self$options$slab]]
                )
              data[[yi]] <- jmvcore::toNumeric(data[[yi]])
              data[[vi]] <- jmvcore::toNumeric(data[[vi]])
              data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
              
              res <-
                metafor::rma(
                  yi = yi,
                  vi = vi,
                  method = method2,
                  mods = ~ factor(moderator),
                  data = data,
                  slab = slab,
                  level = level
                )
              
              resTOST <-
                TOSTER::TOSTmeta(
                  ES = res$beta,
                  se = res$se,
                  low_eqbound_d = lowerTOST,
                  high_eqbound_d = upperTOST,
                  alpha = alphaTOST,
                  verbose = FALSE,
                  plot = FALSE
                )
              resTOST$se <- res$se
              
              resTOSTText <- capture.output(TOSTER::TOSTmeta(
                ES = res$beta,
                se = res$se,
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                verbose = TRUE,
                plot = FALSE
              ))
              
              }
          }
          
        #TOST Output 
        
        #TOST Table
        TOSToutput <- self$results$pubBias$TOSToutput
        TOSToutput$setRow(rowNo = 1,
                       values = list(TOST_Z1 = resTOST$TOST_Z1[1],
                                     TOST_p1 = resTOST$TOST_p1[1],
                                     TOST_Z2 = resTOST$TOST_Z2[1],
                                     TOST_p2 = resTOST$TOST_p2[1],
                                     LL_CI_TOST = resTOST$LL_CI_TOST[1],
                                     UL_CI_TOST = resTOST$UL_CI_TOST[1],
                                     LL_CI_ZTEST = resTOST$LL_CI_ZTEST[1],
                                     UL_CI_ZTEST = resTOST$UL_CI_ZTEST[1]))
        #TOST Text Output
        TOSToutputtext <- self$results$pubBias$TOSToutputtext
        
        outputTextTOST <-
          paste(resTOSTText[18], 
                resTOSTText[20], 
                resTOSTText[21], sep = "\n")
        
        TOSToutputtext$setContent(outputTextTOST)
        
        #Visability Option TOST
        if (self$options$showTestTOST == TRUE) {
          TOSToutputtext$setVisible(visible = TRUE)
        } else {
          TOSToutputtext$setVisible(visible = FALSE)
        }
        
          #Pub Bias
          failsafePB <-
            metafor::fsn(yi = res$yi,
                         vi = res$vi,
                         type = fsntype)
          ranktestPB <- metafor::ranktest(res)
          regtestPB <- metafor::regtest(res)
          
          
          fsnRICH <- self$results$pubBias$fsnRICH
          
          fsnRICH$setRow(
            rowNo = 1,
            values = list(failSafeNumber = failsafePB$fsnum[1],
                          p = failsafePB$pval[1])
          )
          fsnTitle <-
            paste("Fail-Safe N Analysis (File Drawer Analysis)")
          fsnNote <-
            paste("Fail-safe N Calculation Using the ",
                  fsntype,
                  " Approach",
                  sep = "")
          fsnRICH$setTitle(title = fsnTitle)
          fsnRICH$setNote("fsnNoteTable", fsnNote)
          
          rankRICH <- self$results$pubBias$rankRICH
          rankRICH$setRow(
            rowNo = 1,
            values = list(rankTau = ranktestPB$tau[1],
                          p = ranktestPB$pval[1])
          )
          
          regRICH <- self$results$pubBias$regRICH
          regRICH$setRow(rowNo = 1,
                         values = list(Z = regtestPB$zval[1],
                                       p = regtestPB$pval[1]))
          
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
          modelFitRICH$setRow(
            rowNo = 1,
            values = list(
              label = "Maximum-Likelihood",
              loglikelihood = res$fit.stats[1, 1],
              deviance = res$fit.stats[2, 1],
              AIC = res$fit.stats[3, 1],
              BIC = res$fit.stats[4, 1],
              AICc = res$fit.stats[5, 1]
            )
          )
          
          
          modelFitRICH$setRow(
            rowNo = 2,
            values = list(
              label = "Restricted Maximum-Likelihood",
              loglikelihood = res$fit.stats[1, 2],
              deviance = res$fit.stats[2, 2],
              AIC = res$fit.stats[3, 2],
              BIC = res$fit.stats[4, 2],
              AICc = res$fit.stats[5, 2]
            )
          )
          
          #fit statistics and information criteria
          #Show if checked, hide if unchecked
          if (self$options$showModelFit == TRUE) {
            modelFitRICH$setVisible(visible = TRUE)
          } else {
            modelFitRICH$setVisible(visible = FALSE)
          }
          
          #Pub Bias Connections
          #self$results$pubBias$fsn$setContent(failsafePB)
          #self$results$pubBias$rank$setContent(ranktestPB)
          #self$results$pubBias$reg$setContent(regtestPB)
          
          #Data Prep: Results Table
          CILB <- round(res$ci.lb[1], 3)
          CIUB <- round(res$ci.ub[1], 3)
          ciLBUB <- paste(CILB, "-", CIUB)
          
          
          # Results Table
          #table <- self$results$textRICH
          # esDataNULL <- is.null(self$options$effectSize)
          # if (esDataNULL == TRUE){
          #   table$setError("error")
          # }
          
          table$setRow(
            rowNo = 1,
            values = list(
              Intercept = "Intercept",
              Estimate = as.numeric(res$b[1]),
              se = res$se[1],
              CILow = res$ci.lb[1],
              CIHigh = res$ci.ub[1],
              p = res$pval[1],
              Z = res$zval[1],
              k = res$k
            )
          )
          
          if (self$options$methodmetacor == "DL") {
            tau2EstimatorName = "DerSimonian-Laird"
          } else if (self$options$methodmetacor == "HE") {
            tau2EstimatorName = "Hedges"
          } else if (self$options$methodmetacor == "HS") {
            tau2EstimatorName = "Hunter-Schmidt"
          } else if (self$options$methodmetacor == "SJ") {
            tau2EstimatorName = "Sidik-Jonkman"
          } else if (self$options$methodmetacor == "ML") {
            tau2EstimatorName = "Maximum-Likelihood"
          } else if (self$options$methodmetacor == "REML") {
            tau2EstimatorName = "Restricted Maximum-Likelihood"
          } else if (self$options$methodmetacor == "EB") {
            tau2EstimatorName = "Empirical Bayes"
          } else if (self$options$methodmetacor == "PM") {
            tau2EstimatorName = "Paule-Mandel"
          }
          
          if (is.null(self$options$moderatorcor) == FALSE) {
            titleMix <- paste("Mixed-Effects Model (k = ", res$k, ")", sep = "")
            titleMixNote <-
              paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            table$setTitle(title = titleMix)
            table$setNote("mixnote", titleMixNote)
          } else if (self$options$methodmetacor == "FE") {
            titleFix <- paste("Fixed-Effects Model (k = ", res$k, ")", sep = "")
            table$setTitle(title = titleFix)
            
          } else {
            titleRan <- paste("Random-Effects Model (k = ", res$k, ")", sep = "")
            titleRanNote <-
              paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            table$setTitle(title = titleRan)
            table$setNote("rannote", titleRanNote)
          }
          
          if (is.null(self$options$moderatorcor) == FALSE) {
            modCILB <- round(res$ci.lb[2], 3)
            modCIUB <- round(res$ci.ub[2], 3)
            
            table$setRow(
              rowNo = 2,
              values = list(
                Intercept = "Moderator",
                Estimate = as.numeric(res$b[2]),
                se = res$se[2],
                CILow = res$ci.lb[2],
                CIHigh = res$ci.ub[2],
                p = res$pval[2],
                Z = res$zval[2],
                k = res$k
              )
            )
            
          } else {
            table$setRow(
              rowNo = 2,
              values = list(
                Intercept = " ",
                Estimate = NULL,
                se = NULL,
                CILow = NULL,
                CIHigh = NULL,
                p = NULL,
                Z = NULL,
                k = NULL
              )
            )
          }
          
          #Data Prep: Heterogeneity Stats
          tauSquared <- round(res$tau2, 4)
          tauSquaredSE <- round(res$se.tau2, 4)
          tauSqCombind <- paste(tauSquared, "(SE=", tauSquaredSE, ")")
          tauOnly <- round(sqrt(res$tau2), 4)
          ISquStat <- paste(round(res$I2, 2), "%", sep = "")
          HSquStat <- round(res$H2, 4)
          
          if (is.null(self$options$moderatorcor) == FALSE) {
            RSquStat <- paste(round(res$R2, 2), "%", sep = "")
          } else {
            RSquStat <- NULL
          }
          
          #Data Prep: Heterogeneity Test
          QTestStatDF <- round(res$k - 1, 4)
          
          #Heterogeneity Stats annd Test Table
          tableTauSqaured <- self$results$tableTauSqaured
          tableTauSqaured$setRow(
            rowNo = 1,
            values = list(
              tauSqComb = tauSqCombind,
              tauSQRT = tauOnly,
              ISqu = ISquStat,
              HSqu = HSquStat,
              RSqu = RSquStat,
              QallDF = QTestStatDF,
              Qall = res$QE,
              QallPval = res$QEp
            )
          )
          
          
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          image <- self$results$plot
          imageFUN <- self$results$funplot
          imageTOST <- self$results$tostplot
          
          image$setState(res)
          imageFUN$setState(res)
          imageTOST$setState(resTOST)
          
          # }}))
        #}
      },
      
      .tostplot = function(imageTOST, ...) {
        # <-- the plot function
        plotDataTOST <- imageTOST$state

        ready <- TRUE
        if (is.null(self$options$effectSize) ||
            is.null(self$options$samplingVariances) ||
            is.null(self$options$slab) == TRUE) {
          
          
          ready <- FALSE
        }
        if (is.null(imageTOST$state$ES) ||
            is.null(imageTOST$state$alpha) == TRUE) {
          ready <- FALSE
        }
        if (ready == TRUE) {
          #I couldn't make jamovi put the plot from TOSTER into the results without
          #it popping a new window and crashing so I'm trying this ugly work around
          #I just copied the function from TOSTER and then put the new variable names
          #in place of the ones that the author Daniel Laken wrote. I should come back 
          #to this and clean it up slash do it correctly.
          plotTOSTfunction <- function(ES, se, low_eqbound_d, high_eqbound_d, alpha) {
            Z1<-(ES-low_eqbound_d)/se
            p1<-pnorm(Z1, lower.tail=FALSE)
            Z2<-(ES-high_eqbound_d)/se
            p2<-pnorm(Z2, lower.tail=TRUE)
            Z<-(ES/se)
            pttest<-2*pnorm(-abs(Z))
            LL90<-ES-qnorm(1-alpha)*(se)
            UL90<-ES+qnorm(1-alpha)*(se)
            LL95<-ES-qnorm(1-alpha/2)*(se)
            UL95<-ES+qnorm(1-alpha/2)*(se)
            ptost<-max(p1,p2) #Get highest p-value for summary TOST result
            Ztost<-ifelse(abs(Z1) < abs(Z2), Z1, Z2) #Get lowest t-value for summary TOST result
            results<-data.frame(Z1,p1,Z2,p2,LL90,UL90)
            colnames(results) <- c("Z-value 1","p-value 1","Z-value 2","p-value 2", paste("Lower Limit ",100*(1-alpha*2),"% CI",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI",sep=""))
            testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
            TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
            
            # Plot results
            plot(NA, ylim=c(0,1), xlim=c(min(LL95,low_eqbound_d,ES)-max(UL95-LL95, high_eqbound_d-low_eqbound_d,ES)/10, max(UL95,high_eqbound_d,ES)+max(UL95-LL95, high_eqbound_d-low_eqbound_d, ES)/10), bty="l", yaxt="n", ylab="",xlab="Effect size")
            points(x=ES, y=0.5, pch=15, cex=2)
            abline(v=high_eqbound_d, lty=2)
            abline(v=low_eqbound_d, lty=2)
            abline(v=0, lty=2, col="grey")
            segments(LL90,0.5,UL90,0.5, lwd=3)
            segments(LL95,0.5,UL95,0.5, lwd=1)
            title(main=paste("Equivalence bounds ",round(low_eqbound_d,digits=3)," and ",round(high_eqbound_d,digits=3),"\nEffect size = ",round(ES,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
            }
          tostPlotFUN <-
            plotTOSTfunction(
              ES = plotDataTOST$ES,
              se = plotDataTOST$se,
              low_eqbound_d = plotDataTOST$low_eqbound_d,
              high_eqbound_d = plotDataTOST$high_eqbound_d,
              alpha = plotDataTOST$alpha
            )
          print(tostPlotFUN)
          TRUE
        }
      },
      #Forest Plot Function
      .plot = function(image, ...) {
        # <-- the plot function
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
        if (is.null(self$options$effectSize) ||
            is.null(self$options$samplingVariances) ||
            is.null(self$options$slab) == TRUE) {
          #if (is.null(self$options$effectSize) == TRUE){
          
          ready <- FALSE
        }
        if (is.null(image$state$yi) ||
            is.null(image$state$vi) == TRUE) {
          ready <- FALSE
        }
        if (ready == TRUE) {
          #plot <- metafor::forest(plotData$yi, plotData$vi, addcred=addcred, addfit=addfit)
          plot <-
            metafor::forest(
              plotData,
              addcred = addcred,
              addfit = addfit,
              level = level,
              showweights = showweights,
              xlab = xlab,
              order = order,
              steps = steps,
              pch = pch
            )
          print(plot)
          TRUE
        }
      },
      #Funnel Plot Function
      .funplot = function(imageFUN, ...) {
        # <-- the plot function
        plotDataFUN <- imageFUN$state
        yaxis <- self$options$yaxis
        yaxisInv <- self$options$yaxisInv
        enhancePlot <- self$options$enhanceFunnel
        ready <- TRUE
        if (is.null(self$options$effectSize) ||
            is.null(self$options$samplingVariances) ||
            is.null(self$options$slab) == TRUE) {
          #if (is.null(self$options$effectSize) == TRUE){
          
          ready <- FALSE
        }
        if (is.null(imageFUN$state$yi) ||
            is.null(imageFUN$state$vi) == TRUE) {
          ready <- FALSE
        }
        if (ready == TRUE) {
          if (self$options$yaxisInv == TRUE) {
            if (self$options$enhanceFunnel == TRUE) {
              yaxisTrans <- paste(yaxis, "nv", sep = "")
              plotFUN <-
                metafor::funnel(
                  plotDataFUN,
                  yaxis = yaxisTrans,
                  level = c(90, 95, 99),
                  shade = c("white", "gray", "darkgray")
                )
            } else {
              yaxisTrans <- paste(yaxis, "nv", sep = "")
              plotFUN <-
                metafor::funnel(plotDataFUN, yaxis = yaxisTrans)
            }
            
          } else {
            if (self$options$enhanceFunnel == TRUE) {
              plotFUN <-
                metafor::funnel(
                  plotDataFUN,
                  yaxis = yaxis,
                  level = c(90, 95, 99),
                  shade = c("white", "gray", "darkgray")
                )
            } else {
              plotFUN <- metafor::funnel(plotDataFUN, yaxis = yaxis)
            }
          }
          print(plotFUN)
          TRUE
        }
      }
    )
  )