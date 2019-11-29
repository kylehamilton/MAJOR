
# This file is a generated template, your changes will not be overwritten

robvisClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "robvisClass",
    inherit = robvisBase,
    private = list(
        .run = function() {
            Study <- self$options$Study
            D1 <- self$options$D1
            D2 <- self$options$D2
            D3 <- self$options$D3
            D4 <- self$options$D4
            D5 <- self$options$D5
            Overall <- self$options$Overall
            Weight <- self$options$Weight
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            ready <- TRUE
            if (is.null(self$options$D1) ||
                is.null(self$options$D2) ||
                is.null(self$options$D3) ||
                is.null(self$options$D4) ||
                is.null(self$options$D5) ||
                is.null(self$options$Overall) ||
                is.null(self$options$Weight) ||
                is.null(self$options$Study) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject(
                    "All five bias domains and study label fields must be populated to run analysis",
                    code = ''
                )
            }
            if (is.null(self$options$Study) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject("Study label field must be populated to run analysis", code =
                                    '')
            }
            
            
            if (ready == TRUE) {
                plotData <-
                        data.frame(D1 = self$data[[self$options$D1]],
                                   D2 = self$data[[self$options$D2]],
                                   D3 = self$data[[self$options$D3]],
                                   D4 = self$data[[self$options$D4]],
                                   D5 = self$data[[self$options$D5]],
                                   Overall = self$data[[self$options$Overall]],
                                   Weight = self$data[[self$options$Weight]],
                                   Study = self$data[[self$options$Study]])
                    data[[Weight]] <- jmvcore::toNumeric(data[[Weight]])
            }

                    image <- self$results$plot
                    image$setState(plotData)     
 
            },
            .plot=function(image, ...) {
                plotData <- image$state
                summary_rob <- robvis::rob_summary(data = data_rob2, tool = "ROB2")
                summary_rob
                TRUE
            })
)
