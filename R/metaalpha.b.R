

# This file is a generated template, your changes will not be overwritten

metaAlphaClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "metaAlphaClass",
    inherit = metaAlphaBase,
    private = list(
      .run = function() {
        ai <- self$options$ai
        mi <- self$options$mi
        ni <- self$options$ni
        moderator <- self$options$moderatorcor
        slab <- self$options$slab
        #includemods <- self$options$includemods
        addcred <- self$options$addcred
        addfit <- self$options$addfit
        showweights <- self$options$showweights
        level <- self$options$level
        fsntype <- self$options$fsntype
        method2 <- self$options$methodmetacor
        mdmseasure <- self$options$cormeasure
        yaxis <- self$options$yaxis
        steps <- self$options$steps
        pchForest <- self$options$pchForest
        table <- self$results$textRICH
        moderatorType <- self$options$moderatorType
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) || is.null(self$options$ni) == TRUE) {
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject(
            "Cronbach's Alpha, Number of Items, Sample Size and Study Label fields must be populated to run analysis",
            code = ''
          )
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
              data.frame(
                ai = self$data[[self$options$ai]],
                mi = self$data[[self$options$mi]],
                ni = self$data[[self$options$ni]],
                slab = self$data[[self$options$slab]]
              )
            data[[ai]] <- jmvcore::toNumeric(data[[ai]])
            data[[mi]] <- jmvcore::toNumeric(data[[mi]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            
            res <-
              metafor::rma(
                ai = ai,
                ni = ni,
                mi = mi,
                method = method2,
                measure = mdmseasure,
                data = data,
                slab = slab,
                level = level
              )
          }
          
          if (self$options$moderatorType == "CON") {
            if (is.null(self$options$moderatorcor) == TRUE) {
              ready <- FALSE
              # I really need to think of a better error message this is a place holder until I figure something out
              jmvcore::reject("Must Supply a Moderator Variable", code =
                                '')
            }
            #data <- data[!is.na(data$moderator),]
            #rownames(data) <- NULL
            #data <- data %>% drop_na(moderator)
            data <-
              data.frame(
                ai = self$data[[self$options$ai]],
                mi = self$data[[self$options$mi]],
                ni = self$data[[self$options$ni]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[ai]] <- jmvcore::toNumeric(data[[ai]])
            data[[mi]] <- jmvcore::toNumeric(data[[mi]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            res <-
              metafor::rma(
                ai = ai,
                ni = ni,
                mi = mi,
                mods = moderator,
                method = method2,
                measure = mdmseasure,
                data = data,
                slab = slab,
                level = level
              )}
          
          if ((self$options$moderatorType) == "CAT") {
            if (is.null(self$options$moderatorcor) == TRUE) {
              ready <- FALSE
              # I really need to think of a better error message this is a place holder until I figure something out
              jmvcore::reject("Must Supply a Moderator Variable", code =
                                '')
            }
            #data <- data[!is.na(data$moderator),]
            #rownames(data) <- NULL
            #data <- data %>% drop_na(moderator)
            data <-
              data.frame(
                ai = self$data[[self$options$ai]],
                mi = self$data[[self$options$mi]],
                ni = self$data[[self$options$ni]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[ai]] <- jmvcore::toNumeric(data[[ai]])
            data[[mi]] <- jmvcore::toNumeric(data[[mi]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            res <-
              metafor::rma(
                ai = ai,
                ni = ni,
                mi = mi,
                mods = ~ factor(moderator),
                method = method2,
                measure = mdmseasure,
                data = data,
                slab = slab,
                level = level
              )}
        }
          
          
        #Pub Bias
        failsafePB <-
          metafor::fsn(yi = res$yi,
                       vi = res$vi,
                       type = fsntype)
        ranktestPB <- metafor::ranktest(res)
        regtestPB <- metafor::regtest(res)
        
        
        #fsnRICH <- self$results$pubBias$fsnRICH
        fsnRICH <- self$results$fsnRICH
        # fsnRICH$setRow(
        #   rowNo = 1,
        #   values = list(failSafeNumber = failsafePB$fsnum[1],
        #                 p = failsafePB$pval[1])
        # )
        # fsnTitle <-
        #   paste("Fail-Safe N Analysis (File Drawer Analysis)")
        # fsnNote <-
        #   paste("Fail-safe N Calculation Using the ",
        #         fsntype,
        #         " Approach",
        #         sep = "")
        # fsnRICH$setTitle(title = fsnTitle)
        # fsnRICH$setNote("fsnNoteTable", fsnNote)
        #fsnRICH <- self$results$fsnRICH
        fsnRICH$setRow(
          rowNo = 1,
          values = list(
            label = "Fail-Safe N",
            failSafeNumber = failsafePB$fsnum[1],
            p = failsafePB$pval[1])
        )
        
        
        fsnRICH$setRow(
          rowNo = 2,
          values = list(
            label = "Kendalls Tau",
            failSafeNumber = ranktestPB$tau[1],
            p = ranktestPB$pval[1])
        )
        
        
        fsnRICH$setRow(
          rowNo = 3,
          values = list(
            label = "Egger's Regression",
            failSafeNumber = regtestPB[["zval"]],
            p = regtestPB[["pval"]])
        )
        
        
        fsnTitle <-
          paste("Publication Bias Assessment")
        fsnNote <-
          paste("Fail-safe N Calculation Using the ",
                fsntype,
                " Approach",
                sep = "")
        fsnRICH$setTitle(title = fsnTitle)
        fsnRICH$setNote("fsnNoteTable", fsnNote)
        fsnRICH <- self$results$fsnRICH
          
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
          
          # Influence Diagnostics
          
          inf <- influence(res)
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          image <- self$results$plot
          imageFUN <- self$results$funplot
          #imageTOST <- self$results$tostplot
          imageDiagPlot1 <- self$results$diagPlotAll$diagplot1
          imageDiagPlot2 <- self$results$diagPlotAll$diagplot2
          imageDiagPlot3 <- self$results$diagPlotAll$diagplot3
          imageDiagPlot4 <- self$results$diagPlotAll$diagplot4
          imageDiagPlot5 <- self$results$diagPlotAll$diagplot5
          imageDiagPlot6 <- self$results$diagPlotAll$diagplot6
          imageDiagPlot7 <- self$results$diagPlotAll$diagplot7
          imageDiagPlot8 <- self$results$diagPlotAll$diagplot8
          imageDiagPlot9 <- self$results$diagPlotAll$diagplot9
          
          image$setState(res)
          imageFUN$setState(res)
          #imageTOST$setState(resTOST)
          imageDiagPlot1$setState(inf)
          imageDiagPlot2$setState(inf)
          imageDiagPlot3$setState(inf)
          imageDiagPlot4$setState(inf)
          imageDiagPlot5$setState(inf)
          imageDiagPlot6$setState(inf)
          imageDiagPlot7$setState(inf)
          imageDiagPlot8$setState(inf)
          imageDiagPlot9$setState(res)
          
          if (self$options$showInfPlot == TRUE) {
            imageDiagPlot1$setVisible(visible = TRUE)
            imageDiagPlot2$setVisible(visible = TRUE)
            imageDiagPlot3$setVisible(visible = TRUE)
            imageDiagPlot4$setVisible(visible = TRUE)
            imageDiagPlot5$setVisible(visible = TRUE)
            imageDiagPlot6$setVisible(visible = TRUE)
            imageDiagPlot7$setVisible(visible = TRUE)
            imageDiagPlot8$setVisible(visible = TRUE)
            imageDiagPlot9$setVisible(visible = TRUE)
          } else {
            imageDiagPlot1$setVisible(visible = FALSE)
            imageDiagPlot2$setVisible(visible = FALSE)
            imageDiagPlot3$setVisible(visible = FALSE)
            imageDiagPlot4$setVisible(visible = FALSE)
            imageDiagPlot5$setVisible(visible = FALSE)
            imageDiagPlot6$setVisible(visible = FALSE)
            imageDiagPlot7$setVisible(visible = FALSE)
            imageDiagPlot8$setVisible(visible = FALSE)
            imageDiagPlot9$setVisible(visible = FALSE)
          }
          # }}))
        #}
      },
      
      .influDiagPlot1 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot1 <- plot(plotDataInfluence, plotinf=1)
        print(influDiagPlot1)
        TRUE
      },
      .influDiagPlot2 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot2 <- plot(plotDataInfluence, plotinf=2)
        print(influDiagPlot2)
        TRUE
      },
      
      .influDiagPlot3 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot3 <- plot(plotDataInfluence, plotinf=3)
        print(influDiagPlot3)
        TRUE
      },
      
      .influDiagPlot4 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot4 <- plot(plotDataInfluence, plotinf=4)
        print(influDiagPlot4)
        TRUE
      },
      
      .influDiagPlot5 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot5 <- plot(plotDataInfluence, plotinf=5)
        print(influDiagPlot5)
        TRUE
      },
      
      .influDiagPlot6 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot6 <- plot(plotDataInfluence, plotinf=6)
        print(influDiagPlot6)
        TRUE
      },
      
      .influDiagPlot7 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot7 <- plot(plotDataInfluence, plotinf=7)
        print(influDiagPlot7)
        TRUE
      },
      
      .influDiagPlot8 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
        }
        
        influDiagPlot8 <- plot(plotDataInfluence, plotinf=8)
        print(influDiagPlot8)
        TRUE
      },
      
      .influDiagPlot9 = function(imageDiagPlot9, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot9$state
        
        ready <- TRUE
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) ||
            is.null(self$options$ni) ||
            is.null(self$options$slab) == TRUE) {
          ready <- FALSE
          jmvcore::reject(
            "Cronbach's Alpha, Number of Items, Sample Size and Study Label fields must be populated to run analysis",
            code = ''
          )
        } else {
          influDiagPlot9 <- qqnorm(plotDataInfluence)
          print(influDiagPlot9)
        }
        TRUE
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
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) || is.null(self$options$ni) == TRUE) {
          #if (is.null(self$options$rcor) == TRUE){
          
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
        if (is.null(self$options$ai) ||
            is.null(self$options$mi) || is.null(self$options$ni) == TRUE) {
          #if (is.null(self$options$rcor) == TRUE){
          
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