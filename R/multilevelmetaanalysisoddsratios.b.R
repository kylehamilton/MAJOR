
# This file is a generated template, your changes will not be overwritten

multiLevelMetaAnalysisOddsRatiosClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multiLevelMetaAnalysisOddsRatiosClass",
    inherit = multiLevelMetaAnalysisOddsRatiosBase,
    private = list(
        .run = function() {
            
            ### results ###
            resultsTable <- self$results$resultsTable

            
            ready <- TRUE
            if (is.null(self$options$ai) ||
                is.null(self$options$bi) ||
                is.null(self$options$ci) || is.null(self$options$di) == TRUE) {
                ready <- FALSE
                # I really need to think of a better error message this is a place holder until I figure something out
                jmvcore::reject(
                    "Incidents, Sample Size, and Study Label fields must be populated to run analysis",
                    code = ''
                )
            }

            data <- data.frame(
                ai = jmvcore::toNumeric(self$data[[self$options$ai]]),
                bi = jmvcore::toNumeric(self$data[[self$options$bi]]),
                ci = jmvcore::toNumeric(self$data[[self$options$ci]]),
                di = jmvcore::toNumeric(self$data[[self$options$di]]))

            ### change data into long format
            dat.long <- to.long(measure = "OR", ai = ai, bi = bi, ci = ci, di = di, data = data)
            

            ### set levels of group variable ("exp" = experimental/vaccinated; "con" = control/non-vaccinated)
            levels(dat.long$group) <- c("experimental", "control")
            
            ### set "con" to reference level
            dat.long$group <- relevel(dat.long$group, ref = "control")
            
            dat.long <- escalc(measure = "PLO", xi = out1, mi = out2, data = dat.long)
            
            
            res <- rma.mv(yi, vi, mods = ~ group, random = ~ group | study, struct = "UN", data = dat.long)
            
            resultsTable$setRow(
                rowNo = 1,
                values = list(
                    Intercept = "Intercept",
                    Estimate = as.numeric(res$b[1]),
                    se = res$se[1],
                    CILow = res$ci.lb[1],
                    CIHigh = res$ci.ub[1],
                    p = res$pval[1],
                    Z = res$zval[1]
                )
            )
            
            resultsTable$setRow(
                rowNo = 2,
                values = list(
                    Intercept = "Group",
                    Estimate = as.numeric(res$b[2]),
                    se = res$se[2],
                    CILow = res$ci.lb[2],
                    CIHigh = res$ci.ub[2],
                    p = res$pval[2],
                    Z = res$zval[2]
                )
            )            

            resultsTableTitle <- paste("Multivariate Meta-Analysis Model (k = ", res$k, "; method:", res$method, ")", sep = "")
            # titleMixNote <-
            #     paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
            resultsTable$setTitle(title = resultsTableTitle)
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)
