
changeScoreClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "changeScoreClass",
    inherit = changeScoreBase,
    private = list(
        .run = function() {
            #Treatment Group
            n1i_pre <- self$options$n1i_pre
            m1i_pre <- self$options$m1i_pre
            sd1i_pre <- self$options$sd1i_pre
            m1i_post <- self$options$m1i_post
            sd1i_post <- self$options$sd1i_post
            #Control Group
            n2i_pre <- self$options$n2i_pre
            m2i_pre <- self$options$m2i_pre
            sd2i_pre <- self$options$sd2i_pre
            m2i_post <- self$options$m2i_post
            sd2i_post <- self$options$sd2i_post
            
            #n2i_pre <- self$options$n2i_pre
            #n2i_post <- self$options$n2i_post
            
            #Corrlations between time points
            ri <- self$options$ri
            
            #Study Labels
            slab <- self$options$slab
        # I'll come back later and add this, I have a meeting soon and I just want to get this to work before I go
           # moderator <- self$options$moderatorcor
           # moderatorType <- self$options$moderatorType
            
            
            method2 <- self$options$methodmetacor
            mdmseasure <- self$options$cormeasure
            table <- self$results$textRICH

            ready <- TRUE
            if (is.null(self$options$n1i_pre) ||
                is.null(self$options$m1i_pre) ||
                is.null(self$options$sd1i_pre) ||
                is.null(self$options$m1i_post) ||
                is.null(self$options$sd1i_post) ||
                is.null(self$options$n2i_pre) ||
                is.null(self$options$m2i_pre) ||
                is.null(self$options$sd2i_pre) ||
                is.null(self$options$m2i_post) ||
                is.null(self$options$sd2i_post) || is.null(self$options$ri) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject(
                    "Correlations, Sample Size, Mean, Standard Deviation and Study Label fields must be populated to run analysis",
                    code = ''
                )
            }
            if (is.null(self$options$slab) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject("Study Label fields must be populated to run analysis", code =
                                    '')
            }
            
            # if (ready == TRUE) {
            #     if (self$options$moderatorType == "NON") {
            #         if (is.null(self$options$moderatorcor) == FALSE) {
            #             ready <- FALSE
            #             # I really need to think of a better error message this is a place holder until I figure something out
            #             jmvcore::reject("Must Remove Moderator Variable", code =
            #                                 '')
            #         }
                    dataTreatment <-
                        data.frame(
                            n1i_pre = self$data[[self$options$n1i_pre]],
                            m1i_pre = self$data[[self$options$m1i_pre]],
                            sd1i_pre = self$data[[self$options$sd1i_pre]],
                            m1i_post = self$data[[self$options$m1i_post]],
                            sd1i_post = self$data[[self$options$sd1i_post]],
                            ri = self$data[[self$options$ri]],
                            slab = self$data[[self$options$slab]]
                        )
                    
                    
                    dataTreatment[[n1i_pre]] <- jmvcore::toNumeric(dataTreatment[[n1i_pre]])
                    dataTreatment[[m1i_pre]] <- jmvcore::toNumeric(dataTreatment[[m1i_pre]])
                    dataTreatment[[sd1i_pre]] <- jmvcore::toNumeric(dataTreatment[[sd1i_pre]])
                    dataTreatment[[m1i_post]] <- jmvcore::toNumeric(dataTreatment[[m1i_post]])
                    dataTreatment[[sd1i_post]] <- jmvcore::toNumeric(dataTreatment[[sd1i_post]])
                    # dataTreatment[[n2i_pre]] <- jmvcore::toNumeric(dataTreatment[[n2i_pre]])
                    # dataTreatment[[m2i_pre]] <- jmvcore::toNumeric(dataTreatment[[m2i_pre]])
                    # dataTreatment[[sd2i_pre]] <- jmvcore::toNumeric(dataTreatment[[sd2i_pre]])
                    # dataTreatment[[m2i_post]] <- jmvcore::toNumeric(dataTreatment[[m2i_post]])
                    # dataTreatment[[sd2i_post]] <- jmvcore::toNumeric(dataTreatment[[sd2i_post]])
                    dataTreatment[[ri]] <- jmvcore::toNumeric(dataTreatment[[ri]])
                    
                    dataControl <-
                        data.frame(
                            n2i_pre = self$data[[self$options$n2i_pre]],
                            m2i_pre = self$data[[self$options$m2i_pre]],
                            sd2i_pre = self$data[[self$options$sd2i_pre]],
                            m2i_post = self$data[[self$options$m2i_post]],
                            sd2i_post = self$data[[self$options$sd2i_post]],
                            ri = self$data[[self$options$ri]],
                            slab = self$data[[self$options$slab]]
                        )
                    
                    dataControl[[n2i_pre]] <- jmvcore::toNumeric(dataControl[[n2i_pre]])
                    dataControl[[m2i_pre]] <- jmvcore::toNumeric(dataControl[[m2i_pre]])
                    dataControl[[sd2i_pre]] <- jmvcore::toNumeric(dataControl[[sd2i_pre]])
                    dataControl[[m2i_post]] <- jmvcore::toNumeric(dataControl[[m2i_post]])
                    dataControl[[sd2i_post]] <- jmvcore::toNumeric(dataControl[[sd2i_post]])
                    
                    
                    if (self$options$testType == FALSE) {
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        
                        datT <- escalc(measure = mdmseasure, m1i=m1i_post, m2i=m1i_pre, sd1i=sd1i_pre, sd2i=sd1i_post, ni=n1i_pre, ri=ri, data=dataTreatment)
                        datC <- escalc(measure = mdmseasure, m1i=m2i_post, m2i=m2i_pre, sd1i=sd2i_pre, sd2i=sd2i_post, ni=n2i_pre, ri=ri, data=dataControl)
                        dat <- data.frame(yi = datT$yi - datC$yi, vi = datT$vi + datC$vi)
                        
                        res <-
                            metafor::rma(
                                yi,
                                vi,
                                method = method2,
                                test="z",
                                data = dat,
                                #slab = slab
                            )
                    } else {
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        # LOOK INTO THIS AND MAKE SURE YOU DIDNT MESS THIS UP
                        
                        datT <- escalc(measure = mdmseasure, m1i=m1i_post, m2i=m1i_pre, sd1i=sd1i_pre, sd2i=sd1i_post, ni=n1i_pre, ri=ri, data=dataTreatment)
                        datC <- escalc(measure = mdmseasure, m1i=m2i_post, m2i=m2i_pre, sd1i=sd2i_pre, sd2i=sd2i_post, ni=n2i_pre, ri=ri, data=dataControl)
                        dat <- data.frame(yi = datT$yi - datC$yi, vi = datT$vi + datC$vi)
                        
                        res <-
                            metafor::rma(
                                yi,
                                vi,
                                method = method2,
                                measure = mdmseasure,
                                test="knha",
                                data = dat,
                                #slab = slab
                            )
                    }
                   
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
                    
                    # if (is.null(self$options$moderatorcor) == FALSE) {
                    #     titleMix <- paste("Mixed-Effects Model (k = ", res$k, ")", sep = "")
                    #     if (self$options$testType == FALSE) {
                    #         titleMixNote <-
                    #             paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
                    #     } else {
                    #         titleMixNote <-
                    #             paste("Tau\u00B2 Estimator: ", tau2EstimatorName, ". Knapp and Hartung (2003) adjustment used.", sep = "")
                    #     }
                    #     table$setTitle(title = titleMix)
                    #     table$setNote("mixnote", titleMixNote)
                    #} else if (self$options$methodmetacor == "FE") {
                     if (self$options$methodmetacor == "FE") {
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
                    
                    # if (is.null(self$options$moderatorcor) == FALSE) {
                    #     modCILB <- round(res$ci.lb[2], 3)
                    #     modCIUB <- round(res$ci.ub[2], 3)
                    #     
                    #     table$setRow(
                    #         rowNo = 2,
                    #         values = list(
                    #             Intercept = "Moderator",
                    #             Estimate = as.numeric(res$b[2]),
                    #             se = res$se[2],
                    #             CILow = res$ci.lb[2],
                    #             CIHigh = res$ci.ub[2],
                    #             p = res$pval[2],
                    #             Z = res$zval[2],
                    #             k = res$k
                    #         )
                    #     )
                    #     
                    # } else {
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
                    #}
                    
                    #Data Prep: Heterogeneity Stats
                    tauSquared <- round(res$tau2[1], 4)
                    tauSquaredSE <- round(res$se.tau2[1], 4)
                    tauSqCombind <-
                        paste(tauSquared, "(SE=", tauSquaredSE, ")")
                    tauOnly <- round(sqrt(res$tau2[1]), 4)
                    ISquStat <- paste(round(res$I2[1], 2), "%", sep = "")
                    HSquStat <- round(res$H2[1], 4)
                    
                    # if (is.null(self$options$moderatorcor) == FALSE) {
                    #     RSquStat <- paste(round(res$R2, 2), "%", sep = "")
                    # } else {
                        RSquStat <- NULL
                    #}
                    
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
                    
                }

            #}
            

        #})
    )
)
