
# This file is automatically generated, you probably don't want to edit this

metaDichotomousModelOptions <- if (requireNamespace('jmvcore')) R6::R6Class(
    "metaDichotomousModelOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            ai = NULL,
            n1i = NULL,
            ci = NULL,
            n2i = NULL,
            slab = NULL,
            moderatorcor = NULL,
            methodmetamdms = "REML",
            mdmsmeasure = "OR",
            showBackTransform = FALSE,
            moderatorType = "NON",
            level = 95,
            showModelFit = FALSE,
            addcred = FALSE,
            addfit = TRUE,
            showweights = FALSE,
            steps = 5,
            xAxisTitle = NULL,
            forestPlotSize = "SMALL",
            funnelPlotSize = "SMALL",
            pchForest = "15",
            forestOrder = "fit",
            fsntype = "Rosenthal",
            yaxis = "sei",
            yaxisInv = FALSE,
            enhanceFunnel = FALSE, ...) {

            super$initialize(
                package='MAJOR',
                name='metaDichotomousModel',
                requiresData=TRUE,
                ...)

            private$..ai <- jmvcore::OptionVariable$new(
                "ai",
                ai,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..n1i <- jmvcore::OptionVariable$new(
                "n1i",
                n1i,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..ci <- jmvcore::OptionVariable$new(
                "ci",
                ci,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..n2i <- jmvcore::OptionVariable$new(
                "n2i",
                n2i,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..slab <- jmvcore::OptionVariable$new(
                "slab",
                slab,
                suggested=list(
                    "nominal"))
            private$..moderatorcor <- jmvcore::OptionVariable$new(
                "moderatorcor",
                moderatorcor,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..methodmetamdms <- jmvcore::OptionList$new(
                "methodmetamdms",
                methodmetamdms,
                options=list(
                    "DL",
                    "HE",
                    "HS",
                    "SJ",
                    "ML",
                    "REML",
                    "EB",
                    "FE"),
                default="REML")
            private$..mdmsmeasure <- jmvcore::OptionList$new(
                "mdmsmeasure",
                mdmsmeasure,
                options=list(
                    "RR",
                    "OR",
                    "RD",
                    "AS",
                    "PETO"),
                default="OR")
            private$..showBackTransform <- jmvcore::OptionBool$new(
                "showBackTransform",
                showBackTransform,
                default=FALSE)
            private$..moderatorType <- jmvcore::OptionList$new(
                "moderatorType",
                moderatorType,
                options=list(
                    "NON",
                    "CAT",
                    "CON"),
                default="NON")
            private$..level <- jmvcore::OptionNumber$new(
                "level",
                level,
                min=50,
                max=99.9,
                default=95)
            private$..showModelFit <- jmvcore::OptionBool$new(
                "showModelFit",
                showModelFit,
                default=FALSE)
            private$..addcred <- jmvcore::OptionBool$new(
                "addcred",
                addcred,
                default=FALSE)
            private$..addfit <- jmvcore::OptionBool$new(
                "addfit",
                addfit,
                default=TRUE)
            private$..showweights <- jmvcore::OptionBool$new(
                "showweights",
                showweights,
                default=FALSE)
            private$..steps <- jmvcore::OptionNumber$new(
                "steps",
                steps,
                min=1,
                max=999,
                default=5)
            private$..xAxisTitle <- jmvcore::OptionString$new(
                "xAxisTitle",
                xAxisTitle)
            private$..forestPlotSize <- jmvcore::OptionList$new(
                "forestPlotSize",
                forestPlotSize,
                options=list(
                    "SMALL",
                    "MEDIUM",
                    "LARGE",
                    "SMALLWIDE",
                    "MEDIUMWIDE",
                    "LARGEWIDE"),
                default="SMALL")
            private$..funnelPlotSize <- jmvcore::OptionList$new(
                "funnelPlotSize",
                funnelPlotSize,
                options=list(
                    "SMALL",
                    "MEDIUM",
                    "LARGE"),
                default="SMALL")
            private$..pchForest <- jmvcore::OptionList$new(
                "pchForest",
                pchForest,
                options=list(
                    "16",
                    "18",
                    "15",
                    "17",
                    "1",
                    "5",
                    "0",
                    "2",
                    "8"),
                default="15")
            private$..forestOrder <- jmvcore::OptionList$new(
                "forestOrder",
                forestOrder,
                options=list(
                    "obs",
                    "fit",
                    "prec",
                    "resid",
                    "abs.resid"),
                default="fit")
            private$..fsntype <- jmvcore::OptionList$new(
                "fsntype",
                fsntype,
                options=list(
                    "Rosenthal",
                    "Orwin",
                    "Rosenberg"),
                default="Rosenthal")
            private$..yaxis <- jmvcore::OptionList$new(
                "yaxis",
                yaxis,
                options=list(
                    "sei",
                    "vi",
                    "ni",
                    "sqrtni",
                    "lni"),
                default="sei")
            private$..yaxisInv <- jmvcore::OptionBool$new(
                "yaxisInv",
                yaxisInv,
                default=FALSE)
            private$..enhanceFunnel <- jmvcore::OptionBool$new(
                "enhanceFunnel",
                enhanceFunnel,
                default=FALSE)

            self$.addOption(private$..ai)
            self$.addOption(private$..n1i)
            self$.addOption(private$..ci)
            self$.addOption(private$..n2i)
            self$.addOption(private$..slab)
            self$.addOption(private$..moderatorcor)
            self$.addOption(private$..methodmetamdms)
            self$.addOption(private$..mdmsmeasure)
            self$.addOption(private$..showBackTransform)
            self$.addOption(private$..moderatorType)
            self$.addOption(private$..level)
            self$.addOption(private$..showModelFit)
            self$.addOption(private$..addcred)
            self$.addOption(private$..addfit)
            self$.addOption(private$..showweights)
            self$.addOption(private$..steps)
            self$.addOption(private$..xAxisTitle)
            self$.addOption(private$..forestPlotSize)
            self$.addOption(private$..funnelPlotSize)
            self$.addOption(private$..pchForest)
            self$.addOption(private$..forestOrder)
            self$.addOption(private$..fsntype)
            self$.addOption(private$..yaxis)
            self$.addOption(private$..yaxisInv)
            self$.addOption(private$..enhanceFunnel)
        }),
    active = list(
        ai = function() private$..ai$value,
        n1i = function() private$..n1i$value,
        ci = function() private$..ci$value,
        n2i = function() private$..n2i$value,
        slab = function() private$..slab$value,
        moderatorcor = function() private$..moderatorcor$value,
        methodmetamdms = function() private$..methodmetamdms$value,
        mdmsmeasure = function() private$..mdmsmeasure$value,
        showBackTransform = function() private$..showBackTransform$value,
        moderatorType = function() private$..moderatorType$value,
        level = function() private$..level$value,
        showModelFit = function() private$..showModelFit$value,
        addcred = function() private$..addcred$value,
        addfit = function() private$..addfit$value,
        showweights = function() private$..showweights$value,
        steps = function() private$..steps$value,
        xAxisTitle = function() private$..xAxisTitle$value,
        forestPlotSize = function() private$..forestPlotSize$value,
        funnelPlotSize = function() private$..funnelPlotSize$value,
        pchForest = function() private$..pchForest$value,
        forestOrder = function() private$..forestOrder$value,
        fsntype = function() private$..fsntype$value,
        yaxis = function() private$..yaxis$value,
        yaxisInv = function() private$..yaxisInv$value,
        enhanceFunnel = function() private$..enhanceFunnel$value),
    private = list(
        ..ai = NA,
        ..n1i = NA,
        ..ci = NA,
        ..n2i = NA,
        ..slab = NA,
        ..moderatorcor = NA,
        ..methodmetamdms = NA,
        ..mdmsmeasure = NA,
        ..showBackTransform = NA,
        ..moderatorType = NA,
        ..level = NA,
        ..showModelFit = NA,
        ..addcred = NA,
        ..addfit = NA,
        ..showweights = NA,
        ..steps = NA,
        ..xAxisTitle = NA,
        ..forestPlotSize = NA,
        ..funnelPlotSize = NA,
        ..pchForest = NA,
        ..forestOrder = NA,
        ..fsntype = NA,
        ..yaxis = NA,
        ..yaxisInv = NA,
        ..enhanceFunnel = NA)
)

metaDichotomousModelResults <- if (requireNamespace('jmvcore')) R6::R6Class(
    inherit = jmvcore::Group,
    active = list(
        textRICH = function() private$.items[["textRICH"]],
        tableTauSqaured = function() private$.items[["tableTauSqaured"]],
        modelBackTransform = function() private$.items[["modelBackTransform"]],
        modelFitRICH = function() private$.items[["modelFitRICH"]],
        summaryOutputText = function() private$.items[["summaryOutputText"]],
        summaryOutputText2 = function() private$.items[["summaryOutputText2"]],
        plot = function() private$.items[["plot"]],
        plotMed = function() private$.items[["plotMed"]],
        plotLarge = function() private$.items[["plotLarge"]],
        plotSmallWide = function() private$.items[["plotSmallWide"]],
        plotMedWide = function() private$.items[["plotMedWide"]],
        plotLargeWide = function() private$.items[["plotLargeWide"]],
        pubBias = function() private$.items[["pubBias"]],
        funplot = function() private$.items[["funplot"]],
        funplotMed = function() private$.items[["funplotMed"]],
        funplotLarge = function() private$.items[["funplotLarge"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Dichotomous Models")
            self$add(jmvcore::Table$new(
                options=options,
                name="textRICH",
                refs=list(
                    "metafor"),
                title="Random-Effects Model",
                rows=2,
                columns=list(
                    list(
                        `name`="Intercept", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `type`="number"),
                    list(
                        `name`="Z", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="CILow", 
                        `title`="CI Lower Bound", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="CIHigh", 
                        `title`="CI Upper Bound", 
                        `type`="number", 
                        `format`="zto"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="tableTauSqaured",
                title="Heterogeneity Statistics",
                rows=1,
                columns=list(
                    list(
                        `name`="tauSQRT", 
                        `title`="Tau", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="tauSqComb", 
                        `title`="Tau\u00B2", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="ISqu", 
                        `title`="I\u00B2", 
                        `type`="text"),
                    list(
                        `name`="HSqu", 
                        `title`="H\u00B2", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="RSqu", 
                        `title`="R\u00B2", 
                        `type`="text"),
                    list(
                        `name`="QallDF", 
                        `title`="df", 
                        `type`="integer", 
                        `format`="zto"),
                    list(
                        `name`="Qall", 
                        `title`="Q", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="QallPval", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="modelBackTransform",
                title="Back-Transform Log Odds Ratio to Odds Ratio",
                rows=1,
                columns=list(
                    list(
                        `name`="backTransformOddsRatio", 
                        `title`="Odds Ratio", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="backTransformCILow", 
                        `title`="CI Lower Bound", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="backTransformCIHigh", 
                        `title`="CI Upper Bound", 
                        `type`="number", 
                        `format`="zto"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="modelFitRICH",
                title="Model Fit Statistics and Information Criteria",
                rows=2,
                columns=list(
                    list(
                        `name`="label", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="loglikelihood", 
                        `title`="log-likelihood", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="deviance", 
                        `title`="Deviance", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="AIC", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="BIC", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="AICc", 
                        `type`="number", 
                        `format`="zto"))))
            self$add(jmvcore::Html$new(
                options=options,
                name="summaryOutputText",
                title="Model Summary"))
            self$add(jmvcore::Html$new(
                options=options,
                name="summaryOutputText2",
                title="Model Summary"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Forest Plot",
                width=600,
                height=450,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotMed",
                title="Forest Plot",
                width=600,
                height=625,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotLarge",
                title="Forest Plot",
                width=600,
                height=900,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotSmallWide",
                title="Forest Plot",
                width=900,
                height=450,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotMedWide",
                title="Forest Plot",
                width=900,
                height=625,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotLargeWide",
                title="Forest Plot",
                width=900,
                height=900,
                renderFun=".plot",
                refs=list(
                    "metafor")))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    fsnRICH = function() private$.items[["fsnRICH"]],
                    rankRICH = function() private$.items[["rankRICH"]],
                    regRICH = function() private$.items[["regRICH"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="pubBias",
                            title="Publication Bias Assessment")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="fsnRICH",
                            title="Fail-Safe N Analysis",
                            rows=1,
                            columns=list(
                                list(
                                    `name`="failSafeNumber", 
                                    `title`="Fail-safe N", 
                                    `type`="integer", 
                                    `format`="zto"),
                                list(
                                    `name`="p", 
                                    `type`="number", 
                                    `format`="zto,pvalue"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="rankRICH",
                            title="Rank Correlation Test for Funnel Plot Asymmetry",
                            rows=1,
                            columns=list(
                                list(
                                    `name`="rankTau", 
                                    `title`="Kendall's Tau", 
                                    `type`="number", 
                                    `format`="zto"),
                                list(
                                    `name`="p", 
                                    `type`="number", 
                                    `format`="zto,pvalue"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="regRICH",
                            title="Regression Test for Funnel Plot Asymmetry",
                            rows=1,
                            columns=list(
                                list(
                                    `name`="Z", 
                                    `type`="number", 
                                    `format`="zto"),
                                list(
                                    `name`="p", 
                                    `type`="number", 
                                    `format`="zto,pvalue"))))}))$new(options=options))
            self$add(jmvcore::Image$new(
                options=options,
                name="funplot",
                title="Funnel Plot",
                width=600,
                height=450,
                renderFun=".funplot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="funplotMed",
                title="Funnel Plot",
                width=750,
                height=563,
                renderFun=".funplot",
                refs=list(
                    "metafor")))
            self$add(jmvcore::Image$new(
                options=options,
                name="funplotLarge",
                title="Funnel Plot",
                width=900,
                height=675,
                renderFun=".funplot",
                refs=list(
                    "metafor")))}))

metaDichotomousModelBase <- if (requireNamespace('jmvcore')) R6::R6Class(
    "metaDichotomousModelBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = 'MAJOR',
                name = 'metaDichotomousModel',
                version = c(1,0,0),
                options = options,
                results = metaDichotomousModelResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Dichotomous Models
#'
#' 
#' @param data .
#' @param ai .
#' @param n1i .
#' @param ci .
#' @param n2i .
#' @param slab .
#' @param moderatorcor .
#' @param methodmetamdms .
#' @param mdmsmeasure .
#' @param showBackTransform .
#' @param moderatorType .
#' @param level .
#' @param showModelFit .
#' @param addcred .
#' @param addfit .
#' @param showweights .
#' @param steps .
#' @param xAxisTitle .
#' @param forestPlotSize .
#' @param funnelPlotSize .
#' @param pchForest .
#' @param forestOrder .
#' @param fsntype .
#' @param yaxis .
#' @param yaxisInv .
#' @param enhanceFunnel .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$textRICH} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$tableTauSqaured} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$modelBackTransform} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$modelFitRICH} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$summaryOutputText} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$summaryOutputText2} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotMed} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotLarge} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotSmallWide} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotMedWide} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotLargeWide} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$pubBias$fsnRICH} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pubBias$rankRICH} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pubBias$regRICH} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$funplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$funplotMed} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$funplotLarge} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$textRICH$asDF}
#'
#' \code{as.data.frame(results$textRICH)}
#'
#' @export
metaDichotomousModel <- function(
    data,
    ai,
    n1i,
    ci,
    n2i,
    slab,
    moderatorcor,
    methodmetamdms = "REML",
    mdmsmeasure = "OR",
    showBackTransform = FALSE,
    moderatorType = "NON",
    level = 95,
    showModelFit = FALSE,
    addcred = FALSE,
    addfit = TRUE,
    showweights = FALSE,
    steps = 5,
    xAxisTitle,
    forestPlotSize = "SMALL",
    funnelPlotSize = "SMALL",
    pchForest = "15",
    forestOrder = "fit",
    fsntype = "Rosenthal",
    yaxis = "sei",
    yaxisInv = FALSE,
    enhanceFunnel = FALSE) {

    if ( ! requireNamespace('jmvcore'))
        stop('metaDichotomousModel requires jmvcore to be installed (restart may be required)')

    if ( ! missing(ai)) ai <- jmvcore::resolveQuo(jmvcore::enquo(ai))
    if ( ! missing(n1i)) n1i <- jmvcore::resolveQuo(jmvcore::enquo(n1i))
    if ( ! missing(ci)) ci <- jmvcore::resolveQuo(jmvcore::enquo(ci))
    if ( ! missing(n2i)) n2i <- jmvcore::resolveQuo(jmvcore::enquo(n2i))
    if ( ! missing(slab)) slab <- jmvcore::resolveQuo(jmvcore::enquo(slab))
    if ( ! missing(moderatorcor)) moderatorcor <- jmvcore::resolveQuo(jmvcore::enquo(moderatorcor))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(ai), ai, NULL),
            `if`( ! missing(n1i), n1i, NULL),
            `if`( ! missing(ci), ci, NULL),
            `if`( ! missing(n2i), n2i, NULL),
            `if`( ! missing(slab), slab, NULL),
            `if`( ! missing(moderatorcor), moderatorcor, NULL))


    options <- metaDichotomousModelOptions$new(
        ai = ai,
        n1i = n1i,
        ci = ci,
        n2i = n2i,
        slab = slab,
        moderatorcor = moderatorcor,
        methodmetamdms = methodmetamdms,
        mdmsmeasure = mdmsmeasure,
        showBackTransform = showBackTransform,
        moderatorType = moderatorType,
        level = level,
        showModelFit = showModelFit,
        addcred = addcred,
        addfit = addfit,
        showweights = showweights,
        steps = steps,
        xAxisTitle = xAxisTitle,
        forestPlotSize = forestPlotSize,
        funnelPlotSize = funnelPlotSize,
        pchForest = pchForest,
        forestOrder = forestOrder,
        fsntype = fsntype,
        yaxis = yaxis,
        yaxisInv = yaxisInv,
        enhanceFunnel = enhanceFunnel)

    analysis <- metaDichotomousModelClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
