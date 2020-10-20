

# This file is a generated template, your changes will not be overwritten

metaMeanDiffClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "metaMeanDiffClass",
    inherit = metaMeanDiffBase,
    private = list(
      .run = function() {
        n1i <- self$options$n1i
        m1i <- self$options$m1i
        sd1i <- self$options$sd1i
        n2i <- self$options$n2i
        m2i <- self$options$m2i
        sd2i <- self$options$sd2i
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
        lowerTOST <- self$options$lowerTOST
        upperTOST <- self$options$upperTOST
        alphaTOST <- self$options$alphaTOST
        tesAlternative <- self$options$tesAlternative
        tesAlpha <- self$options$tesAlpha
        tesH0 <- self$options$tesH0

        ready <- TRUE
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
          ready <- FALSE
          # I really need to think of a better error message this is a place holder until I figure something out
          jmvcore::reject(
            "Sample Size, Mean, Standard Deviation and Study Label fields must be populated to run analysis",
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
              data.frame(
                n1i = self$data[[self$options$n1i]],
                m1i = self$data[[self$options$m1i]],
                sd1i = self$data[[self$options$sd1i]],
                n2i = self$data[[self$options$n2i]],
                m2i = self$data[[self$options$m2i]],
                sd2i = self$data[[self$options$sd2i]],
                slab = self$data[[self$options$slab]]
              )
            data[[n1i]] <- jmvcore::toNumeric(data[[n1i]])
            data[[m1i]] <- jmvcore::toNumeric(data[[m1i]])
            data[[sd1i]] <- jmvcore::toNumeric(data[[sd1i]])
            data[[n2i]] <- jmvcore::toNumeric(data[[n2i]])
            data[[m2i]] <- jmvcore::toNumeric(data[[m2i]])
            data[[sd2i]] <- jmvcore::toNumeric(data[[sd2i]])
            
            if (self$options$testType == FALSE) {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  test="z",
                  data = data,
                  slab = slab,
                  level = level
                )
            } else {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  test="knha",
                  data = data,
                  slab = slab,
                  level = level
                )
            }
            
            resTOST <-
              TOSTER::TOSTmeta(
                ES = res$beta[1],
                se = res$se[1],
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                plot = FALSE,
                verbose = FALSE
              )
            resTOST$se <- res$se
            
            resTOSTText <- capture.output(TOSTER::TOSTmeta(
              ES = res$beta[1],
              se = res$se[1],
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
                n1i = self$data[[self$options$n1i]],
                m1i = self$data[[self$options$m1i]],
                sd1i = self$data[[self$options$sd1i]],
                n2i = self$data[[self$options$n2i]],
                m2i = self$data[[self$options$m2i]],
                sd2i = self$data[[self$options$sd2i]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[n1i]] <- jmvcore::toNumeric(data[[n1i]])
            data[[m1i]] <- jmvcore::toNumeric(data[[m1i]])
            data[[sd1i]] <- jmvcore::toNumeric(data[[sd1i]])
            data[[n2i]] <- jmvcore::toNumeric(data[[n2i]])
            data[[m2i]] <- jmvcore::toNumeric(data[[m2i]])
            data[[sd2i]] <- jmvcore::toNumeric(data[[sd2i]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            if (self$options$testType == FALSE) {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  mods = moderator,
                  test="z",
                  data = data,
                  slab = slab,
                  level = level
                )
            } else {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  mods = moderator,
                  test="knha",
                  data = data,
                  slab = slab,
                  level = level
                )
            }
            
            resTOST <-
              TOSTER::TOSTmeta(
                ES = res$beta[1],
                se = res$se[1],
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                plot = FALSE,
                verbose = FALSE
              )
            resTOST$se <- res$se
            
            resTOSTText <- capture.output(TOSTER::TOSTmeta(
              ES = res$beta[1],
              se = res$se[1],
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
                n1i = self$data[[self$options$n1i]],
                m1i = self$data[[self$options$m1i]],
                sd1i = self$data[[self$options$sd1i]],
                n2i = self$data[[self$options$n2i]],
                m2i = self$data[[self$options$m2i]],
                sd2i = self$data[[self$options$sd2i]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[n1i]] <- jmvcore::toNumeric(data[[n1i]])
            data[[m1i]] <- jmvcore::toNumeric(data[[m1i]])
            data[[sd1i]] <- jmvcore::toNumeric(data[[sd1i]])
            data[[n2i]] <- jmvcore::toNumeric(data[[n2i]])
            data[[m2i]] <- jmvcore::toNumeric(data[[m2i]])
            data[[sd2i]] <- jmvcore::toNumeric(data[[sd2i]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            if (self$options$testType == FALSE) {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  mods = moderator,
                  test="z",
                  data = data,
                  slab = slab,
                  level = level
                )
            } else {
              res <-
                metafor::rma(
                  n1i = n1i,
                  n2i = n2i,
                  m1i = m1i,
                  m2i = m2i,
                  sd1i = sd1i,
                  sd2i = sd2i,
                  method = method2,
                  measure = mdmseasure,
                  mods = moderator,
                  test="knha",
                  data = data,
                  slab = slab,
                  level = level
                )
            }
            
            resTOST <-
              TOSTER::TOSTmeta(
                ES = res$beta[1],
                se = res$se[1],
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                verbose = FALSE,
                plot = FALSE
              )
            resTOST$se <- res$se
            
            resTOSTText <- capture.output(TOSTER::TOSTmeta(
              ES = res$beta[1],
              se = res$se[1],
              low_eqbound_d = lowerTOST,
              high_eqbound_d = upperTOST,
              alpha = alphaTOST,
              verbose = TRUE,
              plot = FALSE
            ))
            }
        }
        
        
        #summary
        #I took this entire bit of code from Emily Kothe and her amazing meta-analysis templates
        #https://osf.io/6bk7b/
        
        res_back<-predict(res)
        
        summaryOutputText <- self$results$summaryOutputText
        #if (self$options$moderatorType == "NON") {
        outputTextSummary <-
          paste(
            "A meta-analysis was conducted (k=",
            res$k,
            "). The average difference between the two groups was g = ",
            round(res$b[1], 2),
            ", (p = ",
            round(res$pval[1], 3),
            ", 95% CI [",
            round(res$ci.lb[1], 2),
            ", ",
            round(res$ci.ub[1], 2),
            "]).",
            ifelse(
              res$pval[1] > 0.05,
              " It is important to note that a p>.05 indicates lack of evidence of an effect (i.e. uncertainty) rather than evidence of no effect unless confidence intervals are sufficently narrow to rule out a clinically meaningful effect.",
              ""
            ),
            sep = ""
          )
        #}
        # if (self$options$moderatorType == "CAT") {
        #   outputTextSummary <-
        #     paste(
        #       "A meta-analysis was conducted (k=",
        #       res$k,
        #       "). The average difference between the two groups was g = ",
        #       round(res$b[1], 2),
        #       ", (p = ",
        #       round(res$pval[1], 3),
        #       ", 95% CI [",
        #       round(res$ci.lb[1], 2),
        #       ", ",
        #       round(res$ci.ub[1], 2),
        #       "]).",
        #       ifelse(
        #         res$pval[1] > 0.05,
        #         " It is important to note that a p>.05 indicates lack of evidence of an effect (i.e. uncertainty) rather than evidence of no effect unless confidence intervals are sufficently narrow to rule out a clinically meaningful effect.",
        #         ""
        #       ),
        #       sep = ""
        #     )
        # }
        # 
        # if (self$options$moderatorType == "CON") {
        #   outputTextSummary <-
        #     paste(
        #       "A meta-analysis was conducted (k=",
        #       res$k,
        #       "). The average difference between the two groups was g = ",
        #       round(res$b[1], 2),
        #       ", (p = ",
        #       round(res$pval[1], 3),
        #       ", 95% CI [",
        #       round(res$ci.lb[1], 2),
        #       ", ",
        #       round(res$ci.ub[1], 2),
        #       "]).",
        #       ifelse(
        #         res$pval[1] > 0.05,
        #         " It is important to note that a p>.05 indicates lack of evidence of an effect (i.e. uncertainty) rather than evidence of no effect unless confidence intervals are sufficently narrow to rule out a clinically meaningful effect.",
        #         ""
        #       ),
        #       sep = ""
        #     )
        # }        
        
        
        summaryOutputText$setContent(outputTextSummary)
        
        ### Second part
        summaryOutputText2 <- self$results$summaryOutputText2
        
        if (self$options$moderatorType == "NON") {
        outputTextSummary2 <-
          paste(
            "A Cochran's Q test was conducted to examine whether variations in the observed effect are likely to be attributable soley to sampling error (Q~(df=",
            res$k[1]-1,
            ")~=",
            round(res$QE,2),
            ", p=",
            ifelse(res$QEp[1] < 0.001,
                   "<.001",
                   round(res$QEp[1],3)),
            ".", 
            ifelse(res$QEp[1] < 0.05, 
                   " The variation in the effect is greater than would be expected from sampling error alone. It appears that the true effect varies betweeen studies.",
                   "There is no evidence that the true effect size varies between studies."),
            "The I^2^ statistics indicates the proportion of variance in the observed effect attributable to sampling error. In this instance, the I^2^ =",
            round(res$I2[1],2),
            " %.",
            "Note, this statistic is not an absolute measure of heterogeneity (although it is often interpreted as such). It is strongly advise against using rules of thumb such as small, medium, or large when interpreting I^2^ values. Instead, researchers increasingly argue that the information provided credibility or prediction intervals is more useful in understanding the heterogeneity of true effect sizes in meta-analysis.",
            "In this instance the 95% credibility intervals are",
            round(res_back$cr.lb,2), ",", round(res_back$cr.ub,2),
            ". That is, it is estimated that 95% of true effect sizes fall between g=",
            round(res_back$cr.lb,2)," and g=", round(res_back$cr.ub,2), ".",
            sep = ""
          )
        }
        if (self$options$moderatorType == "CAT" ||
            self$options$moderatorType == "CON") {
          outputTextSummary2 <-
            paste(
              "A Cochran's Q test was conducted to examine whether variations in the observed effect are likely to be attributable soley to sampling error (Q~(df=",
              res$k[1]-1,
              ")~=",
              round(res$QE,2),
              ", p=",
              ifelse(res$QEp[1] < 0.001,
                     "<.001",
                     round(res$QEp[1],3)),
              ".", 
              ifelse(res$QEp[1] < 0.05, 
                     " The variation in the effect is greater than would be expected from sampling error alone. It appears that the true effect varies betweeen studies.",
                     "There is no evidence that the true effect size varies between studies."),
              "The I^2^ statistics indicates the proportion of variance in the observed effect attributable to sampling error. In this instance, the I^2^ =",
              round(res$I2[1],2),
              " %.",
              sep = ""
            ) 
        }
        
        
        summaryOutputText2$setContent(outputTextSummary2)
        #TOST Output 
        
        #TOST Table
        TOSToutput <- self$results$TOSToutput
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
        TOSToutputtext <- self$results$TOSToutputtext
        
        outputTextTOST <-
          paste(resTOSTText[18], 
                resTOSTText[20], 
                resTOSTText[21], sep = "\n")
        
        TOSToutputtext$setContent(outputTextTOST)
        
        #Display TOST
        if (self$options$showTOST == TRUE) {
          TOSToutput$setVisible(visible = TRUE)
          TOSToutputtext$setVisible(visible = TRUE)
        } else {
          TOSToutput$setVisible(visible = FALSE)
          TOSToutputtext$setVisible(visible = FALSE)
        }  
        
        # #Visability Option TOST
        # if (self$options$showTestTOST == TRUE) {
        #   TOSToutputtext$setVisible(visible = TRUE)
        # } else {
        #   TOSToutputtext$setVisible(visible = FALSE)
        # }
        
        
          #Pub Bias
        failsafePB <-
          metafor::fsn(yi = res$yi,
                       vi = res$vi,
                       type = fsntype)
        ranktestPB <- metafor::ranktest(res)
        regtestPB <- metafor::regtest(res)
        fsnRICH <- self$results$fsnRICH
        
        if (self$options$moderatorType == "NON") {
          trimfillPB <- metafor::trimfill(res)
          fsnRICH$setRow(
            rowNo = 4,
            values = list(
              label = "Trim and Fill Number of Studies",
              failSafeNumber = trimfillPB[["k0"]])
          )
        }
        
        if (self$options$moderatorType == "CAT") {
          fsnRICH$setRow(
            rowNo = 4,
            values = list(
              label = "Trim and Fill Number of Studies")
          )
        }
        if (self$options$moderatorType == "CON") {
          fsnRICH$setRow(
            rowNo = 4,
            values = list(
              label = "Trim and Fill Number of Studies")
          )
        }
          
        
        
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
            label = "Begg and Mazumdar Rank Correlation",
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
        
        # fsnRICH$setRow(
        #   rowNo = 4,
        #   values = list(
        #     label = "Trim and Fill Number of Studies",
        #     failSafeNumber = trimfillPB[["k0"]])
        # )
        
        fsnTitle <-
          paste("Publication Bias Assessment")
        fsnNote <-
          paste("Fail-safe N Calculation Using the ",
                fsntype,
                " Approach",
                sep = "")
        fsnRICH$setTitle(title = fsnTitle)
        fsnRICH$setNote("fsnNoteTable", fsnNote)
        #fsnRICH <- self$results$fsnRICH
          # failsafePB <-
          #   metafor::fsn(yi = res$yi,
          #                vi = res$vi,
          #                type = fsntype)
          # ranktestPB <- metafor::ranktest(res)
          # regtestPB <- metafor::regtest(res)
          # 
          # 
          # fsnRICH <- self$results$pubBias$fsnRICH
          # 
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
          # 
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
          
          # # Extracting the effect sizes and sampling variances:
          # effect <- res$yi
          # v <- res$vi
          #
          # # The weight-function model with no mean model:
          # wfRES <- weightr::weightfunct(effect, v)
          #
          #
          # self$results$weightFunctionModel$setContent(wfRES)
        
        # Test of Excess Significance
        
        TES <- tes(res, H0 = tesH0, alternative = tesAlternative, alpha = tesAlpha)
        resultsTES <- self$results$resultsTES
        resultsTES$setRow(
        rowNo = 1,
        values = list(
          label = "Observed Number of Significant Findings",
          tesNumberOutput = TES[["O"]]
        )
    )
        resultsTES$setRow(
          rowNo = 2,
          values = list(
            label = "Expected Number of Significant Findings",
            tesNumberOutput = TES[["k"]]
          )
        )
        resultsTES$setRow(
          rowNo = 3,
          values = list(
            label = "Observed Number / Expected Number",
            tesNumberOutput = TES[["OEratio"]]
          )
        )
        
        tesQuantile <- quantile(TES[["power"]])
        tesQuantile25 <- as.numeric(tesQuantile[2])
        tesQuantile75 <- as.numeric(tesQuantile[4])
        
        resultsTES2 <- self$results$resultsTES2
        resultsTES2$setRow(
          rowNo = 1,
          values = list(
            tesOutputMin = min(TES[["power"]]),
            tesOutputQ1 = tesQuantile25,
            tesOutputMed = median(TES[["power"]]),
            tesOutputQ3 = tesQuantile75,
            tesOutputMax = max(TES[["power"]])
          )
        )
        
        
        tesNote2 <-
          paste("Estimated Power of Tests (based on theta = ",
                round(TES[["theta"]], 4),
                ")",
                sep = "")

        resultsTES2$setNote("tesNoteTable", tesNote2)
        
        tesOutput3 <- self$results$tesOutput3
        tesResults3 <-
          paste(
            "Test of Excess Significance: p = ",
            round(TES[["pval"]], 4),
            " ( X^2 = ",
            round(TES[["X2"]], 4),
            ", df = 1). Limit Estimate: ",
            round(TES[["theta.lim"]], 4),
            " (where p = ",
            round(TES[["tes.alpha"]], 4),
            ")",
            sep = ""
          )
        
        tesOutput3$setContent(tesResults3)

        
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
            if (self$options$testType == FALSE) {
            titleMixNote <-
              paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            } else {
              titleMixNote <-
                paste("Tau\u00B2 Estimator: ", tau2EstimatorName, ". Knapp and Hartung (2003) adjustment used.", sep = "")
            }
            table$setTitle(title = titleMix)
            table$setNote("mixnote", titleMixNote)
          } else if (self$options$methodmetacor == "FE") {
            titleFix <- paste("Fixed-Effects Model (k = ", res$k, ")", sep = "")
            table$setTitle(title = titleFix)
            
          } else {
            titleRan <- paste("Random-Effects Model (k = ", res$k, ")", sep = "")
            if (self$options$testType == FALSE) {
            titleRanNote <-
              paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            } else {
              titleRanNote <-
                paste("Tau\u00B2 Estimator: ", tau2EstimatorName, ". Knapp and Hartung (2003) adjustment used.", sep = "")
            }
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
          tauSquared <- round(res$tau2[1], 4)
          tauSquaredSE <- round(res$se.tau2[1], 4)
          tauSqCombind <-
            paste(tauSquared, "(SE=", tauSquaredSE, ")")
          tauOnly <- round(sqrt(res$tau2[1]), 4)
          ISquStat <- paste(round(res$I2[1], 2), "%", sep = "")
          HSquStat <- round(res$H2[1], 4)
          
          if (is.null(self$options$moderatorcor) == FALSE) {
            RSquStat <- paste(round(res$R2, 2), "%", sep = "")
          } else {
            RSquStat <- NULL
          }
          
          #Data Prep: Heterogeneity Test
          QTestStatDF <- round(res$k[1] - 1, 4)
          
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
              Qall = res$QE[1],
              QallPval = res$QEp[1]
            )
          )
          
          # Influence Diagnostics
          
          inf <- influence(res)
          
          
          # 
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          image <- self$results$plot
          imageFUN <- self$results$funplot
          # imageFUNTRIM <- self$results$funplotTrimGroup$funplotTrim
          imageTOST <- self$results$tostplot
          imageDiagPlot1 <- self$results$diagPlotAll$diagplot1
          imageDiagPlot2 <- self$results$diagPlotAll$diagplot2
          imageDiagPlot3 <- self$results$diagPlotAll$diagplot3
          imageDiagPlot4 <- self$results$diagPlotAll$diagplot4
          imageDiagPlot5 <- self$results$diagPlotAll$diagplot5
          imageDiagPlot6 <- self$results$diagPlotAll$diagplot6
          imageDiagPlot7 <- self$results$diagPlotAll$diagplot7
          imageDiagPlot8 <- self$results$diagPlotAll$diagplot8
          imageDiagPlot9 <- self$results$diagPlotAll$diagplot9
          # new plots from metafor 10/20/2020 wkh
          imageLLPlot <- self$results$likelihoodPlot
          
          image$setState(res)
          imageFUN$setState(res)
          #imageFUNTRIM$setState(res)
          imageTOST$setState(resTOST)
          imageDiagPlot1$setState(inf)
          imageDiagPlot2$setState(inf)
          imageDiagPlot3$setState(inf)
          imageDiagPlot4$setState(inf)
          imageDiagPlot5$setState(inf)
          imageDiagPlot6$setState(inf)
          imageDiagPlot7$setState(inf)
          imageDiagPlot8$setState(inf)
          imageDiagPlot9$setState(res)
          imageLLPlot$setState(res)
          
          #Display TOST Image
          if (self$options$showTOST == TRUE) {
            imageTOST$setVisible(visible = TRUE)
          } else {
            imageTOST$setVisible(visible = FALSE)
          }  
          
          #Display LL Plot Image
          if (self$options$showLL== TRUE) {
            imageLLPlot$setVisible(visible = TRUE)
          } else {
            imageLLPlot$setVisible(visible = FALSE)
          }  
          
          #Display Diagnostic Plots
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
          #Display Trim and Fill Funnel Plot
          # if (self$options$showFunTrimPlot == TRUE) {
          #   imageFUNTRIM$setVisible(visible = TRUE)
          # } else {
          #   imageFUNTRIM$setVisible(visible = FALSE)
          # }
          # }}))
        #}
      },
      .influDiagPlot1 = function(imageDiagPlot1, ...) {
        # <-- the plot function
        plotDataInfluence <- imageDiagPlot1$state
        
        ready <- TRUE
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
          ready <- FALSE
          jmvcore::reject(
            "Sample Size, Mean, Standard Deviation and Study Label fields must be populated to run analysis",
            code = ''
          )
        } else {
          influDiagPlot9 <- qqnorm(plotDataInfluence)
          print(influDiagPlot9)
        }
        TRUE
      },
    .likelihoodPlot = function(imageLLPlot, ...) {
      # <-- the plot function
      plotLL<- imageLLPlot$state
      
      ready <- TRUE
      if (is.null(self$options$n1i) ||
          is.null(self$options$m1i) ||
          is.null(self$options$sd1i) ||
          is.null(self$options$n2i) ||
          is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
        ready <- FALSE
        jmvcore::reject(
          "Sample Size, Mean, and Standard Deviation fields must be populated to generate this plot",
          code = ''
        )
      } else {
        data_test <- NA
        data_test$yi <- plotLL$yi
        data_test$vi <- plotLL$vi
        
        llplot_output <- llplot(measure="GEN", yi=yi, vi=vi, data=data_test, lwd=1, refline=NA, xlim=c(-3,3))
        print(llplot_output)
      }
      TRUE
    },      
      .tostplot = function(imageTOST, ...) {
        # <-- the plot function
        plotDataTOST <- imageTOST$state
        
        ready <- TRUE
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
          
          
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
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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
      # #Funnel Plot Function for Trim and Fill
      # .funplotTrim = function(imageFUNTRIM, ...) {
      #   # <-- the plot function
      #   plotDataFUN <- imageFUNTRIM$state
      #   yaxis <- self$options$yaxis
      #   yaxisInv <- self$options$yaxisInv
      #   enhancePlot <- self$options$enhanceFunnel
      #   
      #   
      #   ready <- TRUE
      #   if (is.null(self$options$n1i) ||
      #       is.null(self$options$m1i) ||
      #       is.null(self$options$sd1i) ||
      #       is.null(self$options$n2i) ||
      #       is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
      #     #if (is.null(self$options$rcor) == TRUE){
      #     
      #     ready <- FALSE
      #   }
      #   if (is.null(imageFUNTRIM$state$yi) ||
      #       self$options$moderatorType == "CAT" ||
      #       self$options$moderatorType == "CON" ||
      #       is.null(imageFUNTRIM$state$vi) == TRUE) {
      #     ready <- FALSE
      #   }
      #   if (ready == TRUE) {
      #     if (self$options$yaxisInv == TRUE) {
      #       if (self$options$enhanceFunnel == TRUE) {
      #         yaxisTrans <- paste(yaxis, "nv", sep = "")
      #         plotFUNTRIM <-
      #           metafor::funnel(
      #             trimfill(plotDataFUN),
      #             yaxis = yaxisTrans,
      #             level = c(90, 95, 99),
      #             shade = c("white", "gray", "darkgray")
      #           )
      #       } else {
      #         yaxisTrans <- paste(yaxis, "nv", sep = "")
      #         plotFUNTRIM <-
      #           metafor::funnel(trimfill(plotDataFUN), yaxis = yaxisTrans)
      #       }
      #       
      #     } else {
      #       if (self$options$enhanceFunnel == TRUE) {
      #         plotFUNTRIM <-
      #           metafor::funnel(
      #             trimfill(plotDataFUN),
      #             yaxis = yaxis,
      #             level = c(90, 95, 99),
      #             shade = c("white", "gray", "darkgray")
      #           )
      #       } else {
      #         plotFUNTRIM <- metafor::funnel(trimfill(plotDataFUN), yaxis = yaxis)
      #       }
      #     }
      #     print(plotFUNTRIM)
      #     TRUE
      #   }
      # },
      #Funnel Plot Function
      .funplot = function(imageFUN, ...) {
        # <-- the plot function
        plotDataFUN <- imageFUN$state
        yaxis <- self$options$yaxis
        yaxisInv <- self$options$yaxisInv
        enhancePlot <- self$options$enhanceFunnel
        ready <- TRUE
        if (is.null(self$options$n1i) ||
            is.null(self$options$m1i) ||
            is.null(self$options$sd1i) ||
            is.null(self$options$n2i) ||
            is.null(self$options$m2i) || is.null(self$options$sd2i) == TRUE) {
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