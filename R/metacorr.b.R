

# This file is a generated template, your changes will not be overwritten

MetaCorrClass <- R6::R6Class(
  "MetaCorrClass",
  inherit = MetaCorrBase,
  private = list(
    .run = function() {
      ri <- self$options$rcor
      ni <- self$options$samplesize
      moderator <- self$options$moderatorcor
      fsntype <- self$options$fsntype
      method2 <- self$options$methodmetacor
      cormeasure <- self$options$cormeasure
      moderatorType <- self$options$moderatorType
      slab <- self$options$slab
      #includemods <- self$options$includemods
      addcred <- self$options$addcred
      addfit <- self$options$addfit
      showweights <- self$options$showweights
      #New 03032018
      steps <- self$options$steps
      pchForest <- self$options$pchForest
      #End new 03032018
      level <- self$options$level
      #yaxis <- self$options$yaxis
      #data <- self$data
      table <- self$results$textRICH
      # lowerTOST <- self$options$lowerTOST
      # upperTOST <- self$options$upperTOST
      # alphaTOST <- self$options$alphaTOST
      
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        ready <- FALSE
        # I really need to think of a better error message this is a place holder until I figure something out
        jmvcore::reject(
          "Correlations, Sample Sizes, and Study Label fields must be populated to run analysis",
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
              data.frame(ri = self$data[[self$options$rcor]],
                         ni = self$data[[self$options$samplesize]],
                         slab = self$data[[self$options$slab]])
            data[[ri]] <- jmvcore::toNumeric(data[[ri]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            
           res <-
            metafor::rma(
              ri = ri,
              ni = ni,
              method = method2,
              measure = cormeasure,
              data = data,
              slab = slab,
              level = level
              )
           
           # resTemp <-
           #   metafor::rma(
           #     ri = ri,
           #     ni = ni,
           #     method = method2,
           #     measure = "COR",
           #     data = data,
           #     slab = slab,
           #     level = level
           #   )
           
           # resTOST <-
           #   TOSTER::TOSTmeta(
           #     ES = resTemp$beta,
           #     se = resTemp$se,
           #     low_eqbound_d = lowerTOST,
           #     high_eqbound_d = upperTOST,
           #     alpha = alphaTOST,
           #     plot = FALSE,
           #     verbose = FALSE
           #   )
           # 
           # resTOST$se <- resTemp$se
           # resTOST$rcor <- resTemp$yi
           # resTOST$samplesize <- resTemp$vi
           # resTOST$slab <- resTemp$slab
           # resTOST$ni <- resTemp$ni
           # resTOST$ri2 <- data$ri
           # resTOST$ni2 <- data$ni
           # resTOSTText <- capture.output(TOSTER::TOSTmeta(
           #   ES = resTemp$beta,
           #   se = resTemp$se,
           #   low_eqbound_d = lowerTOST,
           #   high_eqbound_d = upperTOST,
           #   alpha = alphaTOST,
           #   verbose = TRUE,
           #   plot = FALSE
           # ))
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
              ri = self$data[[self$options$rcor]],
              ni = self$data[[self$options$samplesize]],
              moderator = self$data[[self$options$moderatorcor]],
              slab = self$data[[self$options$slab]]
            )
          data[[ri]] <- jmvcore::toNumeric(data[[ri]])
          data[[ni]] <- jmvcore::toNumeric(data[[ni]])
          data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
          
          res <-
            metafor::rma(
              ri = ri,
              ni = ni,
              method = method2,
              measure = cormeasure,
              mods = moderator,
              data = data,
              slab = slab,
              level = level
            )
          # resTOST <-
          #   TOSTER::TOSTmeta(
          #     ES = res$beta,
          #     se = res$se,
          #     low_eqbound_d = lowerTOST,
          #     high_eqbound_d = upperTOST,
          #     alpha = alphaTOST,
          #     verbose = FALSE,
          #     plot = FALSE
          #   )
          # resTOST$se <- res$se
          # resTOST$rcor <- res$yi
          # resTOST$samplesize <- res$vi
          # resTOST$slab <- res$slab
          # resTOST$ni <- res$ni
          # resTOST$ri <- data$ri
          # resTOSTText <- capture.output(TOSTER::TOSTmeta(
          #   ES = res$beta,
          #   se = res$se,
          #   low_eqbound_d = lowerTOST,
          #   high_eqbound_d = upperTOST,
          #   alpha = alphaTOST,
          #   verbose = TRUE,
          #   plot = FALSE
          # ))
          }
          
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
                ri = self$data[[self$options$rcor]],
                ni = self$data[[self$options$samplesize]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[ri]] <- jmvcore::toNumeric(data[[ri]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            res <-
              metafor::rma(
                ri = ri,
                ni = ni,
                method = method2,
                measure = cormeasure,
                mods = ~ factor(moderator),
                data = data,
                slab = slab,
                level = level
              )
            # resTOST <-
            #   TOSTER::TOSTmeta(
            #     ES = res$beta,
            #     se = res$se,
            #     low_eqbound_d = lowerTOST,
            #     high_eqbound_d = upperTOST,
            #     alpha = alphaTOST,
            #     verbose = FALSE,
            #     plot = FALSE
            #   )
            # resTOST$se <- res$se
            # resTOST$rcor <- res$yi
            # resTOST$samplesize <- res$vi
            # resTOST$slab <- res$slab
            # resTOST$ni <- res$ni
            # resTOST$ri <- data$ri
            # resTOSTText <- capture.output(TOSTER::TOSTmeta(
            #   ES = res$beta,
            #   se = res$se,
            #   low_eqbound_d = lowerTOST,
            #   high_eqbound_d = upperTOST,
            #   alpha = alphaTOST,
            #   verbose = TRUE,
            #   plot = FALSE
            # ))
            }
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
      
        
        # rankRICH <- self$results$pubBias$rankRICH
        # rankRICH$setRow(
        #   rowNo = 1,
        #   values = list(rankTau = ranktestPB$tau[1],
        #                 p = ranktestPB$pval[1])
        # )
        # 
        # regRICH <- self$results$pubBias$regRICH
        # regRICH$setRow(rowNo = 1,
        #                values = list(Z = regtestPB$zval[1],
        #                              p = regtestPB$pval[1]))
        
        # Extracting the effect sizes and sampling variances:
        # effect <- res$yi
        # v <- res$vi
        
        # The weight-function model with no mean model:
        # wfRES <- weightr::weightfunct(effect, v)
        #
        #
        # self$results$weightFunctionModel$setContent(wfRES)
        #
        # #Show if checked, hide if unchecked
        # if (self$options$showWF == TRUE) {
        #   weightFunctionModel$setVisible(visible=TRUE)
        # } else {
        #   weightFunctionModel$setVisible(visible=FALSE)
        # }
        #
        
        
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
        # esDataNULL <- is.null(self$options$rcor)
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
        
        # if (self$options$includemods == TRUE){
        # titleMix <- paste("Mixed-Effects Model (k = ",res$k,")",sep="")
        # titleMixNote <- paste("Tau\u00B2 Estimator: ",tau2EstimatorName, sep="")
        # table$setTitle(title=titleMix)
        # table$setNote("mixnote",titleMixNote)
        
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        ready <- FALSE
        jmvcore::reject(
          "Correlations, Sample Sizes, and Study Label fields must be populated to run analysis",
          code = ''
        )
      } else {
        influDiagPlot9 <- qqnorm(plotDataInfluence)
        print(influDiagPlot9)
      }
      TRUE
    },
    
    # .tostplot = function(imageTOST, ...) {
    #   # <-- the plot function
    #   plotDataTOST <- imageTOST$state
    #   
    #   ready <- TRUE
    #   if (is.null(plotDataTOST$rcor) ||
    #       is.null(plotDataTOST$samplesize) ||
    #       is.null(plotDataTOST$slab) == TRUE) {
    #     
    #     
    #     ready <- FALSE
    #   }
    #   if (is.null(imageTOST$state$ES) ||
    #       is.null(imageTOST$state$alpha) == TRUE) {
    #     ready <- FALSE
    #   }
    #   if (ready == TRUE) {
    #     #I couldn't make jamovi put the plot from TOSTER into the results without
    #     #it popping a new window and crashing so I'm trying this ugly work around
    #     #I just copied the function from TOSTER and then put the new variable names
    #     #in place of the ones that the author Daniel Laken wrote. I should come back 
    #     #to this and clean it up slash do it correctly.
    #     plotTOSTfunction <- function(ES, se, low_eqbound_d, high_eqbound_d, alpha) {
    #       Z1<-(ES-low_eqbound_d)/se
    #       p1<-pnorm(Z1, lower.tail=FALSE)
    #       Z2<-(ES-high_eqbound_d)/se
    #       p2<-pnorm(Z2, lower.tail=TRUE)
    #       Z<-(ES/se)
    #       pttest<-2*pnorm(-abs(Z))
    #       LL90<-ES-qnorm(1-alpha)*(se)
    #       UL90<-ES+qnorm(1-alpha)*(se)
    #       LL95<-ES-qnorm(1-alpha/2)*(se)
    #       UL95<-ES+qnorm(1-alpha/2)*(se)
    #       ptost<-max(p1,p2) #Get highest p-value for summary TOST result
    #       Ztost<-ifelse(abs(Z1) < abs(Z2), Z1, Z2) #Get lowest t-value for summary TOST result
    #       results<-data.frame(Z1,p1,Z2,p2,LL90,UL90)
    #       colnames(results) <- c("Z-value 1","p-value 1","Z-value 2","p-value 2", paste("Lower Limit ",100*(1-alpha*2),"% CI",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI",sep=""))
    #       testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
    #       TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
    #       
    #       # Plot results
    #       plot(NA, ylim=c(0,1), xlim=c(min(LL95,low_eqbound_d,ES)-max(UL95-LL95, high_eqbound_d-low_eqbound_d,ES)/10, max(UL95,high_eqbound_d,ES)+max(UL95-LL95, high_eqbound_d-low_eqbound_d, ES)/10), bty="l", yaxt="n", ylab="",xlab="Effect size")
    #       points(x=ES, y=0.5, pch=15, cex=2)
    #       abline(v=high_eqbound_d, lty=2)
    #       abline(v=low_eqbound_d, lty=2)
    #       abline(v=0, lty=2, col="grey")
    #       segments(LL90,0.5,UL90,0.5, lwd=3)
    #       segments(LL95,0.5,UL95,0.5, lwd=1)
    #       title(main=paste("Equivalence bounds ",round(low_eqbound_d,digits=3)," and ",round(high_eqbound_d,digits=3),"\nEffect size = ",round(ES,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
    #     }
    #     rTemp <- plotDataTOST$b
    #     nTemp <- sum(plotDataTOST$ni)
    #     nTemp <- as.numeric(nTemp)       
    #                  
    #     rd <- compute.es::res(r=rTemp, n=nTemp)
    #     plotDataTOST$ESd <- rd$d 
    #     tostPlotFUN <-
    #       plotTOSTfunction(
    #         ES = plotDataTOST$ESd,
    #         se = plotDataTOST$se,
    #         low_eqbound_d = plotDataTOST$low_eqbound_d,
    #         high_eqbound_d = plotDataTOST$high_eqbound_d,
    #         alpha = plotDataTOST$alpha
    #       )
    #     print(tostPlotFUN)
    #     TRUE
    #   }
    # },
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
            plotFUN <- metafor::funnel(plotDataFUN, yaxis = yaxisTrans)
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