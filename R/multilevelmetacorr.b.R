
# This file is a generated template, your changes will not be overwritten

multiLevelMetaCorrClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multiLevelMetaCorrClass",
    inherit = multiLevelMetaCorrBase,
    private = list(
        .run = function() {
            ri <- self$options$rcor
            ni <- self$options$samplesize
            #cormeasure <- self$options$cormeasure
            slab <- self$options$slab
            clusterOne <- self$options$clusterOne
            clusterTwo <- self$options$clusterTwo
            
            table <- self$results$textRICH
            
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
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
                
                    dataMeta <-
                        data.frame(ri = self$data[[self$options$rcor]],
                                   ni = self$data[[self$options$samplesize]],
                                   slab = self$data[[self$options$slab]],
                                   clusterOne = self$data[[self$options$clusterOne]],
                                   clusterTwo = self$data[[self$options$clusterTwo]])
                    dataMeta[[ri]] <- jmvcore::toNumeric(dataMeta[[ri]])
                    dataMeta[[ni]] <- jmvcore::toNumeric(dataMeta[[ni]])
                    #dataMeta[[clusterOne]] <- jmvcore::toNumeric(dataMeta[[clusterOne]])
                    #dataMeta[[clusterTwo]] <- jmvcore::toNumeric(dataMeta[[clusterTwo]])
                    dat <- dataMeta
                    dat <- metafor::escalc(measure="ZCOR", ri=ri, ni=ni, data=dataMeta, slab=slab)

                    res.ml <-
                        metafor::rma.mv(yi=yi, V=vi, slab=slab, random = ~ 1 | clusterOne/clusterTwo, data = dat)
                   
                    
                    
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
            
            # table$setRow(
            #     rowNo = 2,
            #     values = list(
            #         Intercept = "Moderator",
            #         Estimate = as.numeric(res$b[2]),
            #         se = res$se[2],
            #         CILow = res$ci.lb[2],
            #         CIHigh = res$ci.ub[2],
            #         p = res$pval[2],
            #         Z = res$zval[2],
            #         k = res$k
            #     )
            # )

            ### Heterogeneity ###
            tableHeterogeneity <- self$results$tableHeterogeneity
            #Data Prep: Heterogeneity Test
            # QTestStatDF <- round(res.ml$k - 1, 4)
            # 
            # #Heterogeneity Stats annd Test Table
            # 
            # tableHeterogeneity$setRow(
            #     rowNo = 1,
            #     values = list(
            #         QallDF = QTestStatDF,
            #         Qall = res.ml$QE,
            #         QallPval = res.ml$QEp
            #     )
            # )                

            #Heterogeneity Stats annd Test Table
            
            tableHeterogeneity$setRow(
                rowNo = 1,
                values = list(
                    QallDF = NULL,
                    Qall = NULL,
                    QallPval = NULL
                )
            ) 
            ### Variance Components ###
            tableVariance <- self$results$tableVariance
            
            #Data Prep: Variance Components
            #interClassCorr <- round(res.ml$sigma2[1] / sum(res.ml$sigma2), 3)
            
            
            
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
            # tableVariance$setRow(
            #     rowNo = 1,
            #     values = list(
            #         label = "Sigma Cluster One",
            #         estimate = NULL,
            #         squareroot = NULL,
            #         numLevel = NULL
            #     )
            # )         
            # 
            # tableVariance$setRow(
            #     rowNo = 2,
            #     values = list(
            #         label = "Sigma Cluster Two",
            #         estimate = NULL,
            #         squareroot = NULL,
            #         numLevel = NULL
            #     )
            # ) 
                        
        })
)
