
# This file is a generated template, your changes will not be overwritten

bayesmetasmdClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "bayesmetasmdClass",
    inherit = bayesmetasmdBase,
    private = list(
        .run = function() {
            n1i <- self$options$n1i
            m1i <- self$options$m1i
            sd1i <- self$options$sd1i
            n2i <- self$options$n2i
            m2i <- self$options$m2i
            sd2i <- self$options$sd2i
            cormeasure <- self$options$cormeasure
            slab <- self$options$slab
            scalePrior <- self$options$scalePrior
            tauPrior <- self$options$tauPrior
            muPrior <- self$options$muPrior
            muMeanPrior <- self$options$muMeanPrior
            muStandardDeviationPrior <- self$options$muStandardDeviationPrior
            #method2 <- self$options$methodmetacor
            mdmseasure <- self$options$cormeasure
            #level <- self$options$level
            table <- self$results$textRICH
            
            ready <- TRUE
            if (is.null(self$options$n1i) ||
                is.null(self$options$m1i) ||
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
                
                res <-
                    metafor::rma(
                        n1i = n1i,
                        n2i = n2i,
                        m1i = m1i,
                        m2i = m2i,
                        sd1i = sd1i,
                        sd2i = sd2i,
                        #method = method2,
                        measure = cormeasure,
                        data = data,
                        slab = slab
                    )
                
                dat <- metafor::escalc(
                    measure = cormeasure,
                    ri = ri,
                    ni = ni,
                    data = data,
                    slab = slab
                )
                
                dat$sei <- NULL
                dat$sei <- sqrt(dat$vi)
                
                
                if (self$options$tauPrior == "halfCauchy") {
                    taupriordensity <- function(t){dhalfcauchy(t, scale=scalePrior)}
                } else if (self$options$tauPrior == "halfNormal") {
                    taupriordensity <- function(t){dhalfnormal(t, scale=scalePrior)}
                } else if (self$options$tauPrior == "uniform") {
                    taupriordensity <- "uniform"
                } else if (self$options$tauPrior == "sqrt") {
                    taupriordensity <- "sqrt"
                } else if (self$options$tauPrior == "Jeffreys") {
                    taupriordensity <- "Jeffreys"
                } else if (self$options$tauPrior == "conventional") {
                    taupriordensity <- "conventional"
                } else if (self$options$tauPrior == "DuMouchel") {
                    taupriordensity <- "DuMouchel"
                } else if (self$options$tauPrior == "shrinkage") {
                    taupriordensity <- "shrinkage"
                } else if (self$options$tauPrior == "I2") {
                    taupriordensity <- "I2"
                }
                
                
                if (self$options$muPrior == "normal") {
                    resBayes <- bayesmeta::bayesmeta(dat,
                                                     label = dat$slab,
                                                     mu.prior.mean=muMeanPrior,
                                                     mu.prior.sd=muStandardDeviationPrior,
                                                     tau.prior=taupriordensity)
                } else if (self$options$muPrior == "uniform") {
                    resBayes <- bayesmeta::bayesmeta(dat,
                                                     label = dat$slab,
                                                     tau.prior=taupriordensity)
                }
                
                res <- resBayes
                
                
                
                table$setRow(
                    rowNo = 1,
                    values = list(
                        Labels = "Mode",
                        Tau = resBayes$summary[1,1],
                        Mu = resBayes$summary[1,2],
                        Theta = resBayes$summary[1,3]
                    )
                )
                table$setRow(
                    rowNo = 2,
                    values = list(
                        Labels = "Median",
                        Tau = resBayes$summary[2,1],
                        Mu = resBayes$summary[2,2],
                        Theta = resBayes$summary[2,3]
                    )
                )
                table$setRow(
                    rowNo = 3,
                    values = list(
                        Labels = "Mean",
                        Tau = resBayes$summary[3,1],
                        Mu = resBayes$summary[3,2],
                        Theta = resBayes$summary[3,3]
                    )
                )
                table$setRow(
                    rowNo = 4,
                    values = list(
                        Labels = "SD",
                        Tau = resBayes$summary[4,1],
                        Mu = resBayes$summary[4,2],
                        Theta = resBayes$summary[4,3]
                    )
                )
                table$setRow(
                    rowNo = 5,
                    values = list(
                        Labels = "95% Lower",
                        Tau = resBayes$summary[5,1],
                        Mu = resBayes$summary[5,2],
                        Theta = resBayes$summary[5,3]
                    )
                )
                table$setRow(
                    rowNo = 6,
                    values = list(
                        Labels = "95% Upper",
                        Tau = resBayes$summary[6,1],
                        Mu = resBayes$summary[6,2],
                        Theta = resBayes$summary[6,3]
                    )
                )
                
                
                # plotPosteriorDensity <-
                #   plot(
                #     resBayes
                #     #which = "3",
                #   )
                res1 <- res
                
                image <- self$results$plot
                image$setState(resBayes)
                
                imagePDTau <- self$results$plotPDTau
                imagePDTau$setState(res)
                
                imagePDMu <- self$results$plotPDMu
                imagePDMu$setState(res1)
                
                imageJPD <- self$results$plotJPD
                imageJPD$setState(res1)
            }  
        },
        
        #Forest Plot Function
        .plot = function(image, ...) {
            # <-- the plot function
            plotData <- image$state
            
            ready <- TRUE
            if (is.null(self$options$n1i) ||
                 is.null(self$options$m1i) ||
                is.null(self$options$slab) == TRUE) {
                #if (is.null(self$options$rcor) == TRUE){
                
                ready <- FALSE
            }
            
            if (ready == TRUE) {
                plot <- (forestplot::forestplot(plotData))
                print(plot)
                TRUE
            }
        },
        #}
        .plotPDTau = function(imagePDTau, ...) {
            # <-- the plot function
            plotDataPD <- imagePDTau$state
            ready <- TRUE
            if (is.null(self$options$n1i) ||
                 is.null(self$options$m1i) ||
                is.null(self$options$slab) == TRUE) {
                ready <- FALSE
            }
            if (ready == TRUE) {
                plotPDTau <- plot(plotDataPD, which="3")
                print(plotPDTau)
                TRUE
            }
        },
        .plotPDMu = function(imagePDMu, ...) {
            # <-- the plot function
            plotDataPD1 <- imagePDMu$state
            ready <- TRUE
            if (is.null(self$options$n1i) ||
                 is.null(self$options$m1i) ||
                is.null(self$options$slab) == TRUE) {
                ready <- FALSE
            }
            if (ready == TRUE) {
                plotPDMu <- plot(plotDataPD1, which="4")
                print(plotPDMu)
                TRUE
            }
        },
        .plotJPD = function(imageJPD, ...) {
            # <-- the plot function
            plotDataJPD <- imageJPD$state
            ready <- TRUE
            if (is.null(self$options$n1i) ||
                 is.null(self$options$m1i) ||
                is.null(self$options$slab) == TRUE) {
                ready <- FALSE
            }
            if (ready == TRUE) {
                plotJPD <- plot(plotDataJPD, which="2")
                print(plotJPD)
                TRUE
            }
        }
    )
)
