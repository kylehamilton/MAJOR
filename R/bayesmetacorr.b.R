
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
        addcred <- self$options$addcred
        addfit <- self$options$addfit
        showweights <- self$options$showweights
        steps <- self$options$steps
        pchForest <- self$options$pchForest
        level <- self$options$level
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
            
            resBayes <- bayesmeta::bayesmeta(dat,
                                        label = dat$authors,
                                        tau.prior = "uniform")
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
          

            # titleRan <- paste("Random-Effects Model (k = ", res$k, ")", sep = "")
            # titleRanNote <-
            #   paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            # table$setTitle(title = titleRan)
            # table$setNote("rannote", titleRanNote)
          
          image <- self$results$plot
          image$setState(resBayes)
          # imagePD <- self$results$plotPD
          # imagePD$setState(resBayes)
        
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
        # addcred <- self$options$addcred
        # addfit <- self$options$addfit
        # level <- self$options$level
        # showweights <- self$options$showweights
        # xlab <- self$options$xAxisTitle
        # order <- self$options$forestOrder
        # steps <- self$options$steps
        # pchForest <- self$options$pchForest
        # pch <- as.numeric(pchForest)
        ready <- TRUE
        if (is.null(self$options$rcor) ||
            is.null(self$options$samplesize) ||
            is.null(self$options$slab) == TRUE) {
          #if (is.null(self$options$rcor) == TRUE){
          
          ready <- FALSE
        }
        # if (is.null(image$state$yi) ||
        #     is.null(image$state$vi) == TRUE) {
        #   ready <- FALSE
        # }
        if (ready == TRUE) {
          plot <-
            metafor::forest(
              plotData
              # plotData,
              # addcred = addcred,
              # addfit = addfit,
              # level = level,
              # showweights = showweights,
              # xlab = xlab,
              # order = order,
              # steps = steps,
              # pch = pch
            )
          print(plot)
          TRUE
        }
      # },
      }
      # .plotPD = function(imagePD, ...) {
      #   # <-- the plot function
      #   plotDataPD <- imagePD$state
      #   #StudyID <- self$options$studylabels
      #   #yi <- self$options$yi
      #   #vi <- self$options$vi
      #   #res <- metafor::rma(yi=yi, vi=vi, data=self$data)
      #   # addcred <- self$options$addcred
      #   # addfit <- self$options$addfit
      #   # level <- self$options$level
      #   # showweights <- self$options$showweights
      #   # xlab <- self$options$xAxisTitle
      #   # order <- self$options$forestOrder
      #   # steps <- self$options$steps
      #   # pchForest <- self$options$pchForest
      #   # pch <- as.numeric(pchForest)
      #   ready <- TRUE
      #   if (is.null(self$options$rcor) ||
      #       is.null(self$options$samplesize) ||
      #       is.null(self$options$slab) == TRUE) {
      #     ready <- FALSE
      #   }
      #   if (ready == TRUE) {
      #     plotPosteriorDensity <-
      #       bayesmeta::plot.bayesmeta(
      #         plotDataPD,
      #         prior=TRUE,
      #         title = ("Posterior density"),
      #         xlim = c(0,1),
      #         ylim = c(0,1)
      #       )
      #     print(PDPlot)
      #     TRUE
      #   }
      # }
    )
)
