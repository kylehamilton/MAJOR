
# This file is a generated template, your changes will not be overwritten

bayesmetacorrClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "bayesmetacorrClass",
    inherit = bayesmetacorrBase,
    private = list(
      .run = function() {
        ri <- self$options$rcor
        ni <- self$options$samplesize
        cormeasure <- self$options$cormeasure
        slab <- self$options$slab
        scalePrior <- self$options$scalePrior
        tauPrior <- self$options$tauPrior
        muPrior <- self$options$muPrior
        muMeanPrior <- self$options$muMeanPrior
        muStandardDeviationPrior <- self$options$muStandardDeviationPrior
        #level <- self$options$level
        table <- self$results$textRICH
        
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
            data <-
              data.frame(ri = self$data[[self$options$rcor]],
                         ni = self$data[[self$options$samplesize]],
                         slab = self$data[[self$options$slab]])
            data[[ri]] <- jmvcore::toNumeric(data[[ri]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
  
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
                                               label = dat$authors,
                                               mu.prior.mean=muMeanPrior,
                                               mu.prior.sd=muStandardDeviationPrior,
                                               tau.prior=taupriordensity)
            } else if (self$options$muPrior == "uniform") {
              resBayes <- bayesmeta::bayesmeta(dat,
                                               label = dat$authors,
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
        if (is.null(self$options$rcor) ||
            is.null(self$options$samplesize) ||
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
        if (is.null(self$options$rcor) ||
            is.null(self$options$samplesize) ||
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
        if (is.null(self$options$rcor) ||
            is.null(self$options$samplesize) ||
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
        if (is.null(self$options$rcor) ||
            is.null(self$options$samplesize) ||
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
