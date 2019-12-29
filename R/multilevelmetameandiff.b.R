
# This file is a generated template, your changes will not be overwritten

multiLevelMetaMeanDiffClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multiLevelMetaMeanDiffClass",
    inherit = multiLevelMetaMeanDiffBase,
    private = list(
        .run = function() {
            n1i <- self$options$n1i
            m1i <- self$options$m1i
            sd1i <- self$options$sd1i
            n2i <- self$options$n2i
            m2i <- self$options$m2i
            sd2i <- self$options$sd2i
            slab <- self$options$slab
            clusterOne <- self$options$clusterOne
            clusterTwo <- self$options$clusterTwo
            method2 <- self$options$methodmetacor
            cormeasure <- self$options$cormeasure
            
            table <- self$results$textRICH
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
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
                
                dataMeta <-
                    data.frame(n1i = self$data[[self$options$n1i]],
                               m1i = self$data[[self$options$m1i]],
                               sd1i = self$data[[self$options$sd1i]],
                               n2i = self$data[[self$options$n2i]],
                               m2i = self$data[[self$options$m2i]],
                               sd2i = self$data[[self$options$sd2i]],
                               slab = self$data[[self$options$slab]],
                               clusterOne = self$data[[self$options$clusterOne]],
                               clusterTwo = self$data[[self$options$clusterTwo]])
                dataMeta[[n1i]] <- jmvcore::toNumeric(dataMeta[[n1i]])
                dataMeta[[m1i]] <- jmvcore::toNumeric(dataMeta[[m1i]])
                dataMeta[[sd1i]] <- jmvcore::toNumeric(dataMeta[[sd1i]])
                dataMeta[[n2i]] <- jmvcore::toNumeric(dataMeta[[n2i]])
                dataMeta[[m2i]] <- jmvcore::toNumeric(dataMeta[[m2i]])
                dataMeta[[sd2i]] <- jmvcore::toNumeric(dataMeta[[sd2i]])                
                #dataMeta[[clusterOne]] <- jmvcore::toNumeric(dataMeta[[clusterOne]])
                #dataMeta[[clusterTwo]] <- jmvcore::toNumeric(dataMeta[[clusterTwo]])
                dat <- dataMeta
                dat <-
                    metafor::escalc(
                        measure = cormeasure,
                        n1i = n1i,
                        m1i = m1i,
                        sd1i = sd1i,
                        n2i = n2i,
                        m2i = m2i,
                        sd2i = sd2i,
                        data = dataMeta,
                        slab = slab
                    )
                
                res.ml <-
                    metafor::rma.mv(
                        yi = yi,
                        V = vi,
                        method = method2,
                        slab = slab,
                        random = ~ 1 | clusterOne/clusterTwo,
                        data = dat
                    )
                
                
                
            }
            
            
            #Results
            table$setRow(
                rowNo = 1,
                values = list(
                    Intercept = "Intercept",
                    Estimate = as.numeric(res.ml$b[1]),
                    se = res.ml$se[1],
                    CILow = res.ml$ci.lb[1],
                    CIHigh = res.ml$ci.ub[1],
                    p = res.ml$pval[1],
                    Z = res.ml$zval[1],
                    k = res.ml$k
                )
            )
            
            
            ### Heterogeneity ###
            tableHeterogeneity <- self$results$tableHeterogeneity
            #Data Prep: Heterogeneity Test
            QTestStatDF <- round(res.ml$k - 1, 4)
            
            #Heterogeneity Stats annd Test Table
            
            tableHeterogeneity$setRow(
                rowNo = 1,
                values = list(
                    QallDF = QTestStatDF,
                    Qall = res.ml$QE,
                    QallPval = res.ml$QEp
                )
            )
            
            
            ### Variance Components ###
            tableVariance <- self$results$tableVariance
            
            
            
            #Variance Components Table
            
            tableVariance$setRow(
                rowNo = 1,
                values = list(
                    label = "Sigma Cluster One",
                    estimate = res.ml$sigma2[1],
                    squareroot = sqrt(res.ml$sigma2[1]),
                    numLevel = res.ml$s.nlevels[1]
                )
            )
            
            tableVariance$setRow(
                rowNo = 2,
                values = list(
                    label = "Sigma Cluster Two",
                    estimate = res.ml$sigma2[2],
                    squareroot = sqrt(res.ml$sigma2[2]),
                    numLevel = res.ml$s.nlevels[2]
                )
            )
            
            #Data Prep: Variance Components
            interClassCorr <- round(res.ml$sigma2[1] / sum(res.ml$sigma2), 3)
            
            tableVarianceNote <-
                paste("Intraclass correlation coefficient = ",
                      interClassCorr,
                      sep = "")
            tableVariance$setNote("tableVarianceTable", tableVarianceNote)
            
            #Model Fit
            modelFitRICH <- self$results$modelFitRICH
            modelFitRICH$setRow(
                rowNo = 1,
                values = list(
                    label = "Maximum-Likelihood",
                    loglikelihood = res.ml$fit.stats[1, 1],
                    deviance = res.ml$fit.stats[2, 1],
                    AIC = res.ml$fit.stats[3, 1],
                    BIC = res.ml$fit.stats[4, 1],
                    AICc = res.ml$fit.stats[5, 1]
                )
            )
            
            modelFitRICH$setRow(
                rowNo = 2,
                values = list(
                    label = "Restricted Maximum-Likelihood",
                    loglikelihood = res.ml$fit.stats[1, 2],
                    deviance = res.ml$fit.stats[2, 2],
                    AIC = res.ml$fit.stats[3, 2],
                    BIC = res.ml$fit.stats[4, 2],
                    AICc = res.ml$fit.stats[5, 2]
                )
            )
            
            #fit statistics and information criteria
            #Show if checked, hide if unchecked
            if (self$options$showModelFit == TRUE) {
                modelFitRICH$setVisible(visible = TRUE)
            } else {
                modelFitRICH$setVisible(visible = FALSE)
            }
            
        })
)
