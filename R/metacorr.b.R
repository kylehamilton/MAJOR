

# This file is a generated template, your changes will not be overwritten

MetaCorrClass <- R6::R6Class(
  "MetaCorrClass",
  inherit = MetaCorrBase,
  private = list(
    .run = function() {
      ri <- self$options$rcor
      ni <- self$options$samplesize
      moderator <- self$options$moderatorcor
      fsntype <- self$options$fsntype
      method2 <- self$options$methodmetacor
      cormeasure <- self$options$cormeasure
      moderatorType <- self$options$moderatorType
      slab <- self$options$slab
      #includemods <- self$options$includemods
      addcred <- self$options$addcred
      addfit <- self$options$addfit
      showweights <- self$options$showweights
      #New 03032018
      steps <- self$options$steps
      pchForest <- self$options$pchForest
      #End new 03032018
      level <- self$options$level
      #yaxis <- self$options$yaxis
      #data <- self$data
      table <- self$results$textRICH
      lowerTOST <- self$options$lowerTOST
      upperTOST <- self$options$upperTOST
      alphaTOST <- self$options$alphaTOST

      
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
          if (self$options$moderatorType == "NON") {
            if (is.null(self$options$moderatorcor) == FALSE) {
              ready <- FALSE
              # I really need to think of a better error message this is a place holder until I figure something out
              jmvcore::reject("Must Remove Moderator Variable", code =
                                '')
            }
            data <-
              data.frame(ri = self$data[[self$options$rcor]],
                         ni = self$data[[self$options$samplesize]],
                         slab = self$data[[self$options$slab]])
            data[[ri]] <- jmvcore::toNumeric(data[[ri]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            
           res <-
            metafor::rma(
              ri = ri,
              ni = ni,
              method = method2,
              measure = cormeasure,
              data = data,
              slab = slab,
              level = level
              )
           
           res_back <- if(res[["measure"]] == "COR"){predict(res)}
           res_back <- if(res[["measure"]] == "UCOR"){predict(res)}
           res_back <- if(res[["measure"]] == "ZCOR"){predict(res, tranf = transf.ztor)}

           datTOST <- 
             esc::esc_rpb(r = data$ri, totaln = data$ni, es.type = "d")
           
           res2 <- metafor::rma(yi=es, vi=var, data=datTOST)
           
           resTOST <-
             TOSTER::TOSTmeta(
               ES = res2$beta,
               se = res2$se,
               low_eqbound_d = lowerTOST,
               high_eqbound_d = upperTOST,
               alpha = alphaTOST,
               plot = FALSE,
               verbose = FALSE
             )
           
           resTOST$se <- res2$se
           
           resTOSTText <- capture.output(TOSTER::TOSTmeta(
             ES = res2$beta,
             se = res2$se,
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
          #data <- data[!is.na(data$moderator),]
          #rownames(data) <- NULL
          #data <- data %>% drop_na(moderator)
          data <-
            data.frame(
              ri = self$data[[self$options$rcor]],
              ni = self$data[[self$options$samplesize]],
              moderator = self$data[[self$options$moderatorcor]],
              slab = self$data[[self$options$slab]]
            )
          data[[ri]] <- jmvcore::toNumeric(data[[ri]])
          data[[ni]] <- jmvcore::toNumeric(data[[ni]])
          data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
          
          res <-
            metafor::rma(
              ri = ri,
              ni = ni,
              method = method2,
              measure = cormeasure,
              mods = moderator,
              data = data,
              slab = slab,
              level = level
            )
          
          datTOST <- 
            esc::esc_rpb(r = data$ri, totaln = data$ni, es.type = "d")
          
          res2 <- metafor::rma(yi=es, vi=var, data=datTOST)
          
          resTOST <-
            TOSTER::TOSTmeta(
              ES = res2$beta,
              se = res2$se,
              low_eqbound_d = lowerTOST,
              high_eqbound_d = upperTOST,
              alpha = alphaTOST,
              plot = FALSE,
              verbose = FALSE
            )
          
          resTOST$se <- res2$se
          
          resTOSTText <- capture.output(TOSTER::TOSTmeta(
            ES = res2$beta,
            se = res2$se,
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
            #data <- data[!is.na(data$moderator),]
            #rownames(data) <- NULL
            #data <- data %>% drop_na(moderator)
            data <-
              data.frame(
                ri = self$data[[self$options$rcor]],
                ni = self$data[[self$options$samplesize]],
                moderator = self$data[[self$options$moderatorcor]],
                slab = self$data[[self$options$slab]]
              )
            data[[ri]] <- jmvcore::toNumeric(data[[ri]])
            data[[ni]] <- jmvcore::toNumeric(data[[ni]])
            data[[moderator]] <- jmvcore::toNumeric(data[[moderator]])
            
            res <-
              metafor::rma(
                ri = ri,
                ni = ni,
                method = method2,
                measure = cormeasure,
                mods = ~ factor(moderator),
                data = data,
                slab = slab,
                level = level
              )
            
            datTOST <- 
              esc::esc_rpb(r = data$ri, totaln = data$ni, es.type = "d")
            
            res2 <- metafor::rma(yi=es, vi=var, measure = cormeasure, data=datTOST)
            
            resTOST <-
              TOSTER::TOSTmeta(
                ES = res2$beta,
                se = res2$se,
                low_eqbound_d = lowerTOST,
                high_eqbound_d = upperTOST,
                alpha = alphaTOST,
                plot = FALSE,
                verbose = FALSE
              )
            
            resTOST$se <- res2$se
            
            resTOSTText <- capture.output(TOSTER::TOSTmeta(
              ES = res2$beta,
              se = res2$se,
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
      
      if(self$options$cormeasure == "ZCOR"){
        res_back <- predict(res, tranf = transf.ztor)
      } else {
        res_back <- predict(res)
        }
      

      if(self$options$cormeasure =="ZCOR"){
        averageCorrelation <- round(res_back$pred, 3)
      } else {
        averageCorrelation <- round(res$b, 3)
        }

      backTransText <- if(res[["measure"]] == "COR"){"Correlation coefficents were used in analysis and reporting."}
      backTransText <- if(res[["measure"]] == "UCOR"){"Correlation coefficents were used in analysis and reporting."}
      backTransText <- if(res[["measure"]] == "ZCOR"){"Correlation coefficents were transformed to Fisher Z correlation coefficents for analysis and backtransformed for reporting."}

      if (is.null(self$options$moderatorcor) == TRUE){
      
      summaryOutputText <- self$results$summaryOutputText
      
      outputTextSummary <-
        paste(
          "A meta-analysis was conducted (k=",
          res$k,
          "). ",
          backTransText,
          "The average correlation between these variables is r = ",
          averageCorrelation,
          ", (p = ",
          round(res$pval, 3),
          ", 95% CI [",
          round(res$ci.lb, 2),
          ", ",
          round(res$ci.ub, 2),
          "]).",
          ifelse(
            res$pval > 0.05,
            " It is important to note that a p>.05 indicates lack of evidence of an effect (i.e. uncertainty) rather than evidence of no effect unless confidence intervals are sufficently narrow to rule out a clinically meaningful effect.",
            ""
          ),
          sep = ""
        )
      
      summaryOutputText$setContent(outputTextSummary)
      
      ### Second part
      summaryOutputText2 <- self$results$summaryOutputText2
      
      outputTextSummary2 <-
        paste(
          "A Cochran's Q test was conducted to examine whether variations in the observed correlation are likely to be attributable soley to sampling error (Q~(df=",
          res$k-1,
          ")~=",
          round(res$QE,2),
          ", p=",
          ifelse(res$QEp < 0.001,
                 "<.001",
                 round(res$QEp,3)),
          ".", 
          ifelse(res$QEp < 0.05, 
                 " The variation in the correlation is greater than would be expected from sampling error alone. It appears that the true correlation varies betweeen studies.",
                 "There is no evidence that the true effect size varies between studies."),
          "The I\u00B2 statistics indicates the proportion of variance in the observed effect attributable to sampling error. In this instance, the I\u00B2 = ",
          round(res$I2,2),
          "%.",
          "Note, this statistic is not an absolute measure of heterogeneity (although it is often interpreted as such). It is strongly advise against using rules of thumb such as small, medium, or large when interpreting I\u00B2 values. Instead, researchers increasingly argue that the information provided credibility or prediction intervals is more useful in understanding the heterogeneity of true effect sizes in meta-analysis.",
          "In this instance the 95% credibility intervals are",
          round(res_back$cr.lb, 2), ", ", round(res_back$cr.ub, 2),
          ". That is, it is estimated that 95% of true correlations fall between r=",
          round(res_back$cr.lb, 2)," and r=", round(res_back$cr.ub, 2), ".",
          sep = ""
        )
      
      summaryOutputText2$setContent(outputTextSummary2)
      }
      if ((self$options$moderatorType) == "CAT"){
        
        summaryOutputText <- self$results$summaryOutputText
        
        outputTextSummary <-
          paste(" ")
        
        summaryOutputText$setContent(outputTextSummary)
        
        ### Second part
        summaryOutputText2 <- self$results$summaryOutputText2
        
        outputTextSummary2 <-
          paste(" ")
        
        summaryOutputText2$setContent(outputTextSummary2)
      }      
 
      if ((self$options$moderatorType) == "CON"){
        
        summaryOutputText <- self$results$summaryOutputText
        
        outputTextSummary <-
          paste(" ")
        
        summaryOutputText$setContent(outputTextSummary)
        
        ### Second part
        summaryOutputText2 <- self$results$summaryOutputText2
        
        outputTextSummary2 <-
          paste(" ")
        
        summaryOutputText2$setContent(outputTextSummary2)
      }  

      #TOST Output 
      # datTOST <- 
      #   esc::esc_rpb(r = data$ri, totaln = data$ni, es.type = "d")
      # 
      # res2 <- metafor::rma(yi=es, vi=var, data=datTOST)
      # 
      # resTOST <-
      #   TOSTER::TOSTmeta(
      #     ES = res2$beta,
      #     se = res2$se,
      #     low_eqbound_d = lowerTOST,
      #     high_eqbound_d = upperTOST,
      #     alpha = alphaTOST,
      #     plot = FALSE,
      #     verbose = FALSE
      #   )
      #TOST Table
      TOSToutput <- self$results$TOSToutput
      TOSToutput$setRow(rowNo = 1,
                        values = list(TOST_Z1 = resTOST$TOST_Z1[1],
                                      TOST_p1 = resTOST$TOST_p1[1],
                                      TOST_Z2 = resTOST$TOST_Z2[1],
                                      TOST_p2 = resTOST$TOST_p2[1]))
      # TOSToutput$setRow(rowNo = 1,
      #                   values = list(TOST_Z1 = resTOST$TOST_Z1[1],
      #                                 TOST_p1 = resTOST$TOST_p1[1],
      #                                 TOST_Z2 = resTOST$TOST_Z2[1],
      #                                 TOST_p2 = resTOST$TOST_p2[1],
      #                                 LL_CI_TOST = resTOST$LL_CI_TOST[1],
      #                                 UL_CI_TOST = resTOST$UL_CI_TOST[1],
      #                                 LL_CI_ZTEST = resTOST$LL_CI_ZTEST[1],
      #                                 UL_CI_ZTEST = resTOST$UL_CI_ZTEST[1]))
      #TOST Text Output
      TOSToutputtext <- self$results$TOSToutputtext
      
      outputTextTOST <-
        paste(resTOSTText[18], 
              resTOSTText[20], 
              resTOSTText[21], sep = "\n")
      
      TOSToutputtext$setContent(outputTextTOST)
      
      #Visability Option TOST
      if (self$options$showTestTOST == TRUE) {
        TOSToutputtext$setVisible(visible = TRUE)
      } else {
        TOSToutputtext$setVisible(visible = FALSE)
      }
      
      #Pub Bias
      failsafePB <-
        metafor::fsn(yi = res$yi,
                     vi = res$vi,
                     type = fsntype)
      ranktestPB <- metafor::ranktest(res)
      regtestPB <- metafor::regtest(res)
      
      fsnRICH <- self$results$fsnRICH
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
          label = "Kendalls Tau",
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
      
      fsnTitle <-
        paste("Publication Bias Assessment")
      fsnNote <-
        paste("Fail-safe N Calculation Using the ",
              fsntype,
              " Approach",
              sep = "")
      fsnRICH$setTitle(title = fsnTitle)
      fsnRICH$setNote("fsnNoteTable", fsnNote)
      fsnRICH <- self$results$fsnRICH
      
      resTrimFill <- trimfill(res)
      
      summaryTrimAndFillOutput <- self$results$funnelALL$summaryTrimAndFillOutput
      
      outputTrimAndFillSummary <-
        paste(
          "Estimated number of missing studies on the ",
          resTrimFill$side,
          " side: ",
          resTrimFill$k0,
          "(SE = ",
          round(resTrimFill$se.k0, 4),
          ")",
          sep = ""
        )
      
      summaryTrimAndFillOutput$setContent(outputTrimAndFillSummary)
      
        #pcurve code
      pcurve_cor <- function(ni, 
                             ri, 
                             showText = TRUE, 
                             showPlot = TRUE) {
        
        
        # tests = paste("r(",dat$ni,")=",dat$ri, sep = "")
        
        tests = paste("r(",ni,")=",ri, sep = "")
        
        ############################################################################################################
        
        #Load libraries necessary to run programs
        library(stringr)  #Library to process string variables (text of the entered tests)
        library(poibin)   #This library has the Poisson binomial, the distribution of the sum of binomial with different 
        #underlying probabilities used to compute the binomial test given that each test has a (slightly) 
        #different probability of p<.025 depending on its own non-central parameter
        #See Hong (2013) - "On computing the distribution function for the Poisson binomial distribution" Computational 
        #Statistics and Data Analysis, V59, p.41-51 - http://dx.doi.org/10.1016/j.csda.2012.10.006 
        
        
        
        #1. create empty vectors for
        #1.1 pp-values
        t.ppr=f.ppr=c.ppr=z.ppr=c();      	#right skew
        t.ppl=f.ppl=c.ppl=z.ppl=c();       	#left
        t.pp33=f.pp33=c.pp33=z.pp33=c();   	#33%
        
        #1.2 proportions expected to be low (p<.025) for each  test
        t.plow=f.plow=c.plow=z.plow=c()
        
        #1.3 proportions for all tests are 0 so that if a type of test is missing we know it is not there when aggregating
        t.prop1=t.prop2=t.prop3=t.prop4=t.prop5=0;
        f.prop1=f.prop2=f.prop3=f.prop4=f.prop5=0;
        z.prop1=z.prop2=z.prop3=z.prop4=z.prop5=0;
        c.prop1=c.prop2=c.prop3=c.prop4=c.prop5=0;
        
        
        #1.4 Function 1 - functions that find noncentrality parameter for t,f,chi distributions that gives 33% power for those d.f.
        
        #t-test
        ncp33t =function(df) 
        {      
          xc=qt(p=1-.05/2, df=df)
          #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
          f = function(delta, pr, x, df) pt(x, df = df, ncp = delta) - 2/3
          out = uniroot(f, c(0, 37.62), x = xc, df = df)  
          return(out$root) 
        }
        
        #F-test
        ncp33f =function(df1,df2) 
        {      
          xc=qf(p=1-.05,df1=df1,df2=df2)
          f = function(delta, pr, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - 2/3
          out = uniroot(f, c(0, 37.62), x = xc, df1=df1, df2=df2)  
          return(out$root)       
        }
        
        #chi-square
        ncp33chi =function(df) 
        {      
          xc=qchisq(p=1-.05, df=df)
          #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
          f = function(delta, pr, x, df) pchisq(x, df = df, ncp = delta) - 2/3
          out = uniroot(f, c(0, 37.62), x = xc, df = df)  
          return(out$root)  
        }
        
        
        ###############################################################################
        
        #Function 2 - percent() : makes a number look like a percentage
        percent <- function(x, digits = 0, format = "f", ...)   {
          paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")
        }
        ###############################################################################
        
        #Create a "backup" so that final results table includes the n.s. results which will be excluded from most calculations
        #(not really important here, key for the online app)
        k=seq(from=1,to=length(tests))
        backup=cbind(k,tests)
        
        #(2) Split tests into t,F,X2 and Z
        
        #2.1 Turn everything to lower case
        tests=tolower(tests)
        
        #2.2 Extract the type of test (stat={t,F,c,Z)
        stat=substring(tests,1,1)   
        
        #2.3 Split vector of tests into these
        #get the t-tests
        t.text=subset(tests,stat=="t")
        #get the f-tests
        f.text=subset(tests,stat=="f")
        #get the chi2
        c.text=subset(tests,stat=="c" | stat=="x")
        #get the Z (normally distributed)
        z.text=subset(tests,stat=="z")
        #get the r (correlation) 
        r.text=subset(tests,stat=="r")
        
        #3 Get d.f. for the tests 
        #3.1 t-test
        #find the 2nd parenthesis
        t.par=str_locate(t.text,")")[,1]
        #Get the d.f. between both parenthesis
        t.df=as.numeric(substring(t.text,3,t.par -1))
        
        #3.2 f-test
        #find the comma
        f.comma=str_locate(f.text,",")[,1]
        #find the 2nd parenthesis
        f.par=str_locate(f.text,")")[,1]
        #Get the df1  (between "(" and ","
        f.df1=as.numeric(substring(f.text,3,f.comma -1))
        #Get the df2  (between "," and ")"
        f.df2=as.numeric(substring(f.text,f.comma +1,f.par -1))
        
        #3.3 Chi-square  
        #find the 1st parenthesis
        c.par1=str_locate(c.text,"\\(")[,1]
        #find the 2nd parenthesis
        c.par2=str_locate(c.text,")")[,1]
        #Get the d.f. between both parenthesis
        c.df=as.numeric(substring(c.text,c.par1+1,c.par2 -1))
        
        #3.4 Correlations
        #fine the 2nd parenthesis
        r.par=str_locate(r.text,"\\)")[,1]
        #Get the d.f. between both parenthesis
        r.df=as.numeric(substring(r.text,3,r.par -1))
        
        
        #4 Get the test values
        #4.1 Find the "=" sign
        t.eq=str_locate(t.text,"=")[,1]
        f.eq=str_locate(f.text,"=")[,1]
        z.eq=str_locate(z.text,"=")[,1]
        c.eq=str_locate(c.text,"=")[,1]
        r.eq=str_locate(r.text,"=")[,1]  
        
        #4.2 Get the number after the =
        t.value=c();r.value=c()    
        
        t.value=as.numeric(substring(t.text,t.eq+1,))
        f.value=as.numeric(substring(f.text,f.eq+1,))
        z.value=as.numeric(substring(z.text,z.eq+1,))
        c.value=as.numeric(substring(c.text,c.eq+1,))
        r.value=as.numeric(substring(r.text,r.eq+1,)) 
        
        #4.3 merge r() with t-tests
        rt.value=r.value/(sqrt((1-r.value**2)/r.df))  #r() expressed as a t-value 
        t.value=c(t.value,rt.value)                     
        t.df=c(t.df,r.df)
        t.text=c(t.text,r.text)
        
        
        #5 Keep significant p-values
        #Compute p-values
        t.p=2*(1-pt(abs(t.value),df=t.df))
        f.p=1-pf(abs(f.value),df1=f.df1,df2=f.df2)
        z.p=2*(1-pnorm(abs(z.value)))
        c.p=1-pchisq(abs(c.value),df=c.df)
        
        #Subset statistics and d.f.
        #ts
        t.value.sig=subset(t.value,t.p<.05)
        t.df.sig   =subset(t.df,   t.p<.05)
        t.text.sig =subset(t.text, t.p<.05)
        t.p.sig    =subset(t.p,    t.p<.05)
        #fs
        f.value.sig=subset(f.value,f.p<.05)
        f.df1.sig  =subset(f.df1,  f.p<.05)
        f.df2.sig  =subset(f.df2,  f.p<.05)
        f.text.sig =subset(f.text, f.p<.05)
        f.p.sig    =subset(f.p,    f.p<.05)
        #chis
        c.value.sig=subset(c.value,c.p<.05)
        c.df.sig   =subset(c.df,   c.p<.05)
        c.text.sig =subset(c.text, c.p<.05)
        c.p.sig    =subset(c.p,    c.p<.05)
        #zs
        z.value.sig=subset(z.value,z.p<.05)
        z.text.sig =subset(z.text, z.p<.05)
        z.p.sig    =subset(z.p,    z.p<.05)
        
        #All significant p-values (used for binomial)
        all.p.sig=c(t.p.sig, f.p.sig, c.p.sig, z.p.sig)
        #Number of significant results
        ktot=length(all.p.sig)    
        #Number of non-signifcant results in p-curve
        kns=length(tests)-ktot    
        
        #6 Compute pp-values 
        #6.1 For t-values
        if (length(t.value.sig)>0)  #if nonempty compute pp-values
        {
          #skew
          t.ppr=t.p.sig*(1/.05)               #pp-value for right-skew 
          t.ppl=1-t.ppr                         #pp-value for left-skew
          #33%power
          #Find the ncp (uses function from top)
          t.ncp33=mapply(ncp33t,t.df.sig)
          #Using the ncp33 compute pp33.
          t.pp33=3*(pt(t.value.sig,  df=t.df.sig,  ncp=t.ncp33)-2/3)
        }
        
        #6.2 For F-values
        if (length(f.value.sig)>0)  #if nonempty compute pp-values
        {
          f.ppr=f.p.sig*(1/.05)             #pp-value for right-skew 
          f.ppl=1-f.ppr                     #pp-value for left-skew
          f.ncp33=mapply(ncp33f, f.df1.sig,  f.df2.sig)
          f.pp33 =3*(pf(f.value.sig,  df1=f.df1.sig, df2=f.df2.sig,  ncp=f.ncp33)-2/3)
        }
        
        #6.3 z-values
        if (length(z.value.sig)>0)  #if nonempty compute pp-values
        {
          z.ppr=z.p.sig*(1/.05)
          z.ppl=1-z.ppr
          z.pp33=3*(pnorm(z.value.sig,mean=1.5285687,sd=1)-2/3)   #Compute pp33-values using the 'ncp' 1.5285687 which gives the normal 33% power
          
        }
        
        #6.4 chi-values
        if (length(c.value.sig)>0)  #if nonempty compute pp-values
        {
          c.ppr=c.p.sig*(1/.05)
          c.ppl=1-c.ppr
          c.ncp33=mapply(ncp33chi, c.df.sig)
          c.pp33=3*(pchisq(c.value.sig,  df=c.df.sig, ncp=c.ncp33)-2/3)
        }
        
        
        #7 STOUFFER: Overall tests aggregating pp-values (using Fisher's method to aggregate uniform distributions of (p)p-values)
        #7.1 Convert pp-values to Z scores, aggregate them and divide by sqrt(ktot)
        Zppr =sum(qnorm(c(t.ppr,  f.ppr  ,c.ppr,  z.ppr )))/sqrt(ktot)          #right skew
        Zppl =sum(qnorm(c(t.ppl,  f.ppl  ,c.ppl,  z.ppl )))/sqrt(ktot)          #left skew
        Zpp33=sum(qnorm(c(t.pp33, f.pp33 ,c.pp33, z.pp33)))/sqrt(ktot)          #33%
        
        #7.2 Compute overall p-values
        p.Zppr =pnorm(Zppr)	
        p.Zppl =pnorm(Zppl)	
        p.Zpp33=pnorm(Zpp33)	
        
        
        #8 Green line (Expected p-curve for 33% power)
        #8.1 t-tests 
        if (length(t.value.sig)>0)  #if nonempty compute pp-values
        {
          #Critical values,xc, for p=.05, .04, .03, .02 and ,01
          t.x5=qt(.975,df=t.df.sig); t.x4=qt(.98, df=t.df.sig); t.x3=qt(.985,df=t.df.sig); 
          t.x2=qt(.99, df=t.df.sig); t.x1=qt(.995,df=t.df.sig)
          #For Binomial test 
          t.x25=qt(.9875,df=t.df.sig)                            #critical value for t-tests to get p=.025
          t.plow=1- 3*(pt(t.x25,df=t.df.sig, ncp=t.ncp33)-2/3)   #prob(p<.025 | ncp33% & p<.05)
          
          #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
          t.pp4=3*(pt(t.x4,df=t.df.sig, ncp=t.ncp33)-2/3)
          t.pp3=3*(pt(t.x3,df=t.df.sig, ncp=t.ncp33)-2/3)
          t.pp2=3*(pt(t.x2,df=t.df.sig, ncp=t.ncp33)-2/3)
          t.pp1=3*(pt(t.x1,df=t.df.sig, ncp=t.ncp33)-2/3)
          #within bins proportions
          t.prop5=mean(t.pp4); 
          t.prop4=mean(t.pp3-t.pp4); 
          t.prop3=mean(t.pp2-t.pp3); 
          t.prop2=mean(t.pp1-t.pp2); 
          t.prop1=mean(1-t.pp1)                       
        }
        #8.2 f-tests 
        if (length(f.value.sig)>0)  #if nonempty compute pp-values
        {
          #Critical values,xc, for p=.05, .04, .03, .02 and ,01
          f.x5=qf(.95,df1=f.df1.sig, df2=f.df2.sig);  f.x4=qf(.96,df1=f.df1.sig, df2=f.df2.sig); f.x3=qf(.97,,df1=f.df1.sig, df2=f.df2.sig); 
          f.x2=qf(.98, df1=f.df1.sig, df2=f.df2.sig); f.x1=qf(.99,df1=f.df1.sig, df2=f.df2.sig) 
          #For binomial test
          f.x25 =qf(.975,df1=f.df1.sig, df2=f.df2.sig)                          #Critical F value for p=.025  
          f.plow=1-3*(pf(f.x25,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)  #Prob(p<.025|ncp33% & p<.05)
          
          
          #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
          f.pp4=3*(pf(f.x4,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
          f.pp3=3*(pf(f.x3,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
          f.pp2=3*(pf(f.x2,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
          f.pp1=3*(pf(f.x1,df1=f.df1.sig, df2=f.df2.sig, ncp=f.ncp33)-2/3)
          
          #within bins proportions
          f.prop5=mean(f.pp4); 
          f.prop4=mean(f.pp3-f.pp4); 
          f.prop3=mean(f.pp2-f.pp3); 
          f.prop2=mean(f.pp1-f.pp2); 
          f.prop1=mean(1-f.pp1)
        }
        #8.3 chi-tests 
        if (length(c.value.sig)>0)  #if nonempty compute pp-values
        {
          
          #Critical values,xc, for p=.05, .04, .03, .02 and ,01
          c.x5=qchisq(.95,df=c.df.sig); c.x4=qchisq(.96, df=c.df.sig); c.x3=qchisq(.97,df=c.df.sig); 
          c.x2=qchisq(.98, df=c.df.sig); c.x1=qchisq(.99,df=c.df.sig) 
          
          #For binomial test
          c.x25 =qchisq(.975,df=c.df.sig)                                      #Critical x2 value for p=.025
          c.plow=1-3*(pchisq(c.x25,df=c.df.sig, ncp=c.ncp33)-2/3)              #Prob(p<.025|ncp33% & p<.05)
          
          #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01 given p<.05 and ncp=ncp33
          c.pp4=3*(pchisq(c.x4,df=c.df.sig, ncp=c.ncp33)-2/3)
          c.pp3=3*(pchisq(c.x3,df=c.df.sig, ncp=c.ncp33)-2/3)
          c.pp2=3*(pchisq(c.x2,df=c.df.sig, ncp=c.ncp33)-2/3)
          c.pp1=3*(pchisq(c.x1,df=c.df.sig, ncp=c.ncp33)-2/3)
          
          #within bins proportions
          c.prop5=mean(c.pp4); c.prop4=mean(c.pp3-c.pp4); c.prop3=mean(c.pp2-c.pp3); 
          c.prop2=mean(c.pp1-c.pp2); c.prop1=mean(1-c.pp1)
        }
        #8.4 z-tests 
        if (length(z.value.sig)>0)  #if nonempty compute pp-values
        {
          #Critical values,xc, for p=.05, .04, .03, .02 and ,01
          z.x5=qnorm(.975); z.x4=qnorm(.98); z.x3=qnorm(.985); z.x2=qnorm(.99); z.x1=qnorm(.995) 
          # For Binomial test	
          z.x25 =qnorm(.9825)                                         #Critical x2 value for p=.025
          z.plow=1-3*(pnorm(z.x25,mean=1.5285687,sd=1)-2/3)           #Prob(p<.025|ncp33% & p<.05)
          #Probabilty of a p-value bigger  p=.05, .04, .03, .02 and .01, given p<.05 and ncp=ncp33
          z.pp4=3*(pnorm(z.x4,mean=1.5285687,sd=1)-2/3)
          z.pp3=3*(pnorm(z.x3,mean=1.5285687,sd=1)-2/3)
          z.pp2=3*(pnorm(z.x2,mean=1.5285687,sd=1)-2/3)
          z.pp1=3*(pnorm(z.x1,mean=1.5285687,sd=1)-2/3)
          #within bins proportions
          z.prop5=z.pp4; z.prop4=z.pp3-z.pp4; z.prop3=z.pp2-z.pp3; z.prop2=z.pp1-z.pp2; z.prop1=1-z.pp1
        }
        
        
        #9 combine t,F,chi,Z
        #proportion of all tests that are of each type
        t.share=length(t.value.sig)/ktot
        f.share=length(f.value.sig)/ktot
        c.share=length(c.value.sig)/ktot
        z.share=length(z.value.sig)/ktot
        
        #Average proportions within the 4 types of tests
        t.props=c(t.prop1, t.prop2, t.prop3, t.prop4, t.prop5)
        f.props=c(f.prop1, f.prop2, f.prop3, f.prop4, f.prop5)
        c.props=c(c.prop1, c.prop2, c.prop3, c.prop4, c.prop5)
        z.props=c(z.prop1, z.prop2, z.prop3, z.prop4, z.prop5)
        
        #overall proportions (i.e.., THE GREEN LINE)
        green=100*(t.props*t.share + f.props*f.share + c.props*c.share + z.props*z.share)
        
        
        #10 The blue line (observed p-curve)   
        
        #Put each p-value in a bin between 0 and .05
        ps=ceiling(c(all.p.sig)*100)/100
        #Count them
        blue01=sum(ps<=.01)/ktot; blue02=sum(ps==.02)/ktot; blue03=sum(ps==.03)/ktot; 
        blue04=sum(ps==.04)/ktot; blue05=sum(ps==.05)/ktot; 
        #combine
        blue=c(blue01,blue02,blue03,blue04,blue05)*100
        #Note: i could have used the Table command, but it is a pain if there are no p-value in a given range
        
        #11 Red line
        red=c(20,20,20,20,20)
        
        #12 Carry out binomial test
        #Note: for t and Z test, the critical value is for p=.0125 one sided, for Z and Chi2 it is for .025 two-sided
        
        
        #12.1 Combine the prob(p<.025) for each set of tests
        plows=c(t.plow, f.plow, c.plow, z.plow)  
        #12.2 Compute observed shared of p<.025 results
        low.obs=sum(all.p.sig<=.025)
        #12.3 Right skew: Compare observed share p<.025 with null of 50:50 and altenrative of more p<.025 than expected
        binom.r=1-pbinom(q=low.obs-1, p=.5, size=ktot)     #The binomial in R computes the probability of x<=xo. We want prob(x>=x0) so we subtract one from x, and 1-prob()
        #12.4 Left skew: Compare observed share p<.025 with null of 50:50 and altenrative of fewer p<.025 than expected
        binom.l=pbinom(q=low.obs, p=.5, size=ktot)         #Here the default x<=x0 is what we want
        #12.5 33% power: Compare observed share p<.025 with expected share ~72% based on the combination of expected shares for ncp33%
        #              The probability of p<.025|ncp33 is slightly different for each test, hence I use the poisson binomial distribtuion (see reference top of this document)
        binom.33=ppoibin(kk=low.obs,pp=plows)              
        
        
        #13.POWER ESTIMATION
        
        #13.1 SET OF FUNCTIONS 1. COMPUTE GAP BETWEEN POWER AND DESIRED POWER FOR A GIVEN NCP 
        # (minimize these in the next step to solve for the ncp that gives the desired power)
        ncp_error.t = function(delta, power, x, df)      pt(x, df = df, ncp = delta) - (1-power)   #if this equals 0, we found the ncp.
        ncp_error.f = function(delta, power, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)   
        ncp_error.c = function(delta, power, x, df)      pchisq(x, df = df, ncp = delta) - (1-power)   
        ncp_error.z = function(delta, power, x)          pnorm(x, mean = delta,sd=1) - (1-power)   
        
        #13.2 SET OF FUNCTIONS 2: MINIMIZE FUNCTIONS ABOVE
        #t-test
        getncp.t =function(df, power)   {      
          xc=qt(p=.975, df=df) # critical t-value
          return(uniroot(ncp_error.t, c(0, 37.62), x = xc, df =df, power=power)$root)   }  
        
        #F-test
        getncp.f =function(df1,df2, power)   {      
          xc=qf(p=.95, df1=df1,df2=df2) # critical F-value
          return(uniroot(ncp_error.f, c(0, 37.62), x = xc, df1 = df1,df2=df2, power=power)$root)  }
        
        
        #chisq-test
        getncp.c =function(df, power)   {      
          xc=qchisq(p=.95, df=df) # critical c-value
          return(uniroot(ncp_error.c, c(0, 37.62), x = xc, df = df, power=power)$root)   }
        
        #Normal
        getncp.z =function(power)   {      
          xc=qnorm(p=.975) # critical Z-value with df=1
          return(uniroot(ncp_error.z, c(0, 37.62), x = xc, power=power)$root)   }             
        
        # 13.3 CREATE PP-VALUES FOR EACH OF THE FOUR DISTRIBUTIONS FOR HOW WELL A GIVEN POWER_EST FITS 
        powerfit.t=function(t_obs, df_obs, power_est)    {
          ncp_est=mapply(getncp.t,df=df_obs,power=power_est)  #find ncp for each  that gives each test power.k
          p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)            #prob t>tobs given ncp_est
          ppr=(p_larger-(1-power_est))/power_est              #condition on p<.05
          return(ppr)   }
        
        powerfit.f=function(f_obs, df1_obs, df2_obs, power_est)    {
          ncp_est=mapply(getncp.f,df1=df1_obs, df2=df2_obs,power=power_est)  #find ncp for each  that gives each test power.k
          p_larger=pf(f_obs,df1=df1_obs,df2=df2_obs, ncp=ncp_est)        #prob t>tobs given ncp_est
          ppr=(p_larger-(1-power_est))/power_est          #condition on p<.05
          return(ppr)   }
        
        powerfit.z=function(z_obs, power_est)    {
          ncp_est=mapply(getncp.z,power=power_est)  
          p_larger=pnorm(z_obs,mean=ncp_est)        
          ppr=(p_larger-(1-power_est))/power_est          
          return(ppr)     }
        
        
        powerfit.c=function(c_obs, df_obs, power_est)    {
          ncp_est=mapply(getncp.c,df=df_obs,power=power_est)  
          p_larger=pchisq(c_obs,df=df_obs,ncp=ncp_est)        
          ppr=(p_larger-(1-power_est))/power_est          
          return(ppr)   }
        
        #13.4  STACK-UP ALL THE PP-VALUES INTO A VECTOR AND COMPARE THEM TO UNIFORM DISTRIBUTION USING KOLMOGOROV-SMIRNOV TEST
        
        powerfit.all=function(power_est)
        {
          ppr.all=c()
          #for each kind of test, check if there are any significant values, if there are, add ppr to overall ppr
          if (length(t.value.sig)>0) ppr.all=c(ppr.all, powerfit.t(t_obs=t.value.sig, df_obs=t.df.sig, power_est=power_est))
          if (length(f.value.sig)>0) ppr.all=c(ppr.all, powerfit.f(f_obs=f.value.sig, df1_obs=f.df1.sig, df2_obs=f.df2.sig, power_est=power_est))
          if (length(z.value.sig)>0) ppr.all=c(ppr.all, powerfit.z(z_obs=z.value.sig, power_est=power_est))
          if (length(c.value.sig)>0) ppr.all=c(ppr.all, powerfit.c(c_obs=c.value.sig, df_obs=c.df.sig, power_est=power_est))
          KSD=ks.test(ppr.all,punif)$statistic                #KS test on the resulting pprs
          return(KSD)
        }
        
        #13.5 FUNCTION THAT COMPUTES FIT FOR EACH LEVEL OF POWER, AND PLOT IT
        
        
        plotfit=function()
        {
          # Fit will be evaluated at every possible value of power between 5.1% and 99% in steps of 1%, stored in fit()
          fit=c()                                          #Create empty vector
          fit=powerfit.all(.051)                           #First evaluate fit for power of 5.1%, the lowest one can get for non-directional tests like x2 and F
          for (i in 6:99)   fit=c(fit,powerfit.all(i/100)) #Now do 6% to 99%
          # Find the minimum
          mini=match(min(fit),fit)       #which ith power level considered leads to best estimate
          hat=(mini+4)/100               #convert that into the power level, the ith value considered is (5+ith)/100
          #Plot results
          #create the x-axis
          x.power=seq(from=5,to=99)/100 
          #Draw the line
          par(mar=c(5.1,8.1,4.1,2.1))
          plot(x.power,fit,xlab="Underlying Power", ylab="",ylim=c(0,1), main="")  
          #Make red dot at the estimate
          points(hat,min(fit),pch=19,col="red",cex=2)    
          #Put a label with the estimate value
          sign="="
          if (hat<.06) sign="<"
          text(min(.7,max(.28,hat)),min(fit)-.1,paste0("Estimated Power ",sign," ",hat*100,"%"))
          mtext(c("Perfect","Terrible"),side=2,line=3,at=c(0,1),las=1,cex=1.25,col=c("blue","red"))
          mtext("How Good Is the Fit?",side=2,line=6.5,cex=1.5)
          mtext("(Kolmogorov-Smirnov D Stat)",side=2,line=5.5,col="gray")
          mtext("Do we have a good estimate of power?",side=3,line=1.75,cex=1.5,at=0.4)
          mtext("If you see a V-Shape with a low minimum-->yes",side=3,line=0.5,cex=1.25,at=0.4)
          
        }
        if(isTRUE(showPlot)){
          #Create two graphs in a single chart
          par(mfrow=c(2,1)) 
          #par(mfrow=c(1,1)) 
          
          #14 Firest the p-curve itself
          #Define x-axis as p-values (.01, .02..)
          x = c(.01,.02,.03,.04,.05)
          
          #Plot the observed p-curve
          
          plot(x,blue,   type='l', col='dodgerblue2',  main="",lwd=2, xlab="", ylab="", 
               xaxt="n",yaxt="n", xlim=c(0.01,0.055), ylim=c(0,105), bty='L', las=1);  	
          
          #x-axis value labels
          x_=c(".01",".02",".03",".04",".05")
          axis(1,at=x,labels=x_)
          #y-axis value labels
          y_=c("0%","25%","50%","75%","100%")
          y=c(0,25,50,75,100)
          axis(2,at=y,labels=y_,las=1,cex.axis=.75)
          
          #Add y-axis label
          mtext("Percentage of test results",font=2,side=2,line=2.75,cex=1.25)
          #Add y-axis label
          mtext("p            ",font=4,side=1,line=2.5,cex=1.25)
          mtext(" -value",font=2,side=1,line=2.5,cex=1.25)
          
          
          #Add little point in actual frequencies
          points(x,blue,type="p",pch=20,bg="dodgerblue2",col="dodgerblue2")
          #Add value-labels
          text(x+.00075,blue+5,percent(round(blue)/100),col='black', cex=.75)
          #Add red and green lines
          lines(x,red,   type='l', col='firebrick2',    lwd=1.5, lty=3)
          lines(x,green, type='l', col='springgreen4',  lwd=1.5, lty=5)
          
          #Legend
          #By default its x-position, legendx, is in the middle
          legendx=.035 ; 
          #Move left for p-curves that have more 80% of p-values =.02 or =.03 so that the legend does not touch blue line
          if (blue04>.80 | blue05>.80) legendx=.02
          #Print legend
          legend(legendx, 100, c('Observed p-curve','Null of 33% power', 'Null of zero effect'), 
                 box.col="white",lty=c(1,5,3), cex=.75,lwd=c(1,1),col=c('dodgerblue2','springgreen4', 'firebrick2'));
          
          #ADD THE POWER FIT CHART
          #plotfit()
        }
        #PRINT OUT RESULTS
        ### I added some extra output just to make my life easier while also trying to not modify the code base as much as possible
        ### I know this is silly but I try to be repectful of others work even if it clutters up my own -Kyle Hamilton 12/14/2019
        printout=function()
        {cat("\nTest for right-skew....Binomial: ",binom.r,"   Continuous: Z=",Zppr,"  p=",p.Zppr)
          cat("\nTest for 33%....Binomial: ",binom.33,"   Continuous: Z=",Zpp33,"  p=",p.Zpp33)
          cat("\nTest for left-skew....Binomial: ",binom.l,"   Continuous: Z=",Zppl,"  p=",p.Zppl)
          cat("\nJAMOVI Meta Output")
          cat("\nRight-skew")
          cat("\nBinomial")
          cat("\n",binom.r)
          cat("\nContinuous Z=")
          cat("\n",Zppr)
          cat("\nP=")
          cat("\n",p.Zppr)
          cat("\n33%")
          cat("\nBinomial")
          cat("\n",binom.33)
          cat("\nContinuous Z=")
          cat("\n",Zpp33)
          cat("\nP=")
          cat("\n",p.Zpp33)
          cat("\nLeft-skew")
          cat("\nBinomial")
          cat("\n",binom.l)
          cat("\nContinuous Z=")
          cat("\n",Zppl)
          cat("\nP=")
          cat("\n",p.Zppl)
        }
        
        if(isTRUE(showText)){
          printout()
        }
        
      }
        #pcurve text output
    
      pCurveTextRawOutput <-
        capture.output(pcurve_cor(
          ni = data$ni,
          ri = data$ri,
          showText = TRUE,
          showPlot = FALSE
        ))
      
        pCurveTextProcessedOutput <-
          jmvcore::toNumeric(
            c(
              pCurveTextRawOutput[8],
              pCurveTextRawOutput[10],
              pCurveTextRawOutput[12],
              pCurveTextRawOutput[15],
              pCurveTextRawOutput[17],
              pCurveTextRawOutput[19],
              pCurveTextRawOutput[22],
              pCurveTextRawOutput[24],
              pCurveTextRawOutput[26]
            )
          )
        
        pCurveText <- self$results$pcurveAll$pCurveText
        
        pCurveText$setRow(
          rowNo = 1,
          values = list(
            pcurveLabel = "Test for right-skew",
            pcurveBi = pCurveTextProcessedOutput[1],
            pcurveCon = pCurveTextProcessedOutput[2],
            pcurveP = pCurveTextProcessedOutput[3])
        )
        
        pCurveText$setRow(
          rowNo = 2,
          values = list(
            pcurveLabel = "Test for 33%",
            pcurveBi = pCurveTextProcessedOutput[4],
            pcurveCon = pCurveTextProcessedOutput[5],
            pcurveP = pCurveTextProcessedOutput[6])
        )
        
        pCurveText$setRow(
          rowNo = 3,
          values = list(
            pcurveLabel = "Test for left-skew",
            pcurveBi = pCurveTextProcessedOutput[7],
            pcurveCon = pCurveTextProcessedOutput[8],
            pcurveP = pCurveTextProcessedOutput[9])
        )
        
        pcurveExplanation <- self$results$pcurveAll$pcurveExplanation
        
        outputPCurveExplanation <- 
          paste("P-Curve analysis combines the half and full p-curve to make inferences about evidential value. ", 
                "In particular, if the half p-curve test is right-skewed with p<.05 or both the half and full test are right-skewed with p<.1,",
                "then p-curve analysis indicates the presence of evidential value.", "\nBinomial tests compare the observed proportion of significant results that are p<.025",
                "to the expected proportions when there is no effect, and when studies have 1/3 power.",
                "This latter number varies (by a few %s) as a function of the degrees of freedom of the tests submitted to p-curve.",
                "Continuous tests are obtained by computing pp-values for each test (probability of at least as extreme a p-value conditional on p<.05), and converting them to Z scores(N(0,1)). The sum of these Z scores (", res$k, "in this case), divided by the square-root of the number of tests included (again: ", res$k, 
                "in this case) is the reported Z score in that column (and corresponding p-value).",
                "\nNote that the binomial and continuous tests are by definition one-sided (e.g., more right skewed than flat). We use negative Z values to indicate deviation in the direction of the alternative hypothesis of interest; for example a negative Z value for the Right-Skew test is evidence against the flat null, and thus in favor of Right-Skew.",
                sep = ""
          )
        
        pcurveExplanation$setContent(outputPCurveExplanation)
        
        #puniform code
      
      
      # puni_test <- puniform::puniform(yi=res$yi, vi=res$vi, side="right", plot=FALSE)
      # 
      # 
      # puniformTable <- self$results$puniformTable
      # 
      # puniformTable$setRow(
      #   rowNo = 1,
      #   values = list(
      #     label = "Effect size estimation p-uniform",
      #     est1 = puni_test$est,
      #     cilb1 = puni_test$ci.lb,
      #     ciub1 = puni_test$ci.ub,
      #     L01 = puni_test$L.0,
      #     pval1 = puni_test$pval.0,
      #     ksig1 = puni_test$ksig)
      # )
      
      
      # puniformTable$setRow(
      #   rowNo = 2,
      #   values = list(
      #     label = "Kendalls Tau",
      #     failSafeNumber = ranktestPB$tau[1],
      #     p = ranktestPB$pval[1])
      # )
      # 
      # 
      # puniformTable$setRow(
      #   rowNo = 3,
      #   values = list(
      #     label = "Egger's Regression",
      #     failSafeNumber = regtestPB[["zval"]],
      #     p = regtestPB[["pval"]])
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
      fsnRICH <- self$results$fsnRICH
      
        
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

        
        #Data Prep: Results Table
        CILB <- round(res$ci.lb[1], 3)
        CIUB <- round(res$ci.ub[1], 3)
        ciLBUB <- paste(CILB, "-", CIUB)
        
        
        # Results Table
        #table <- self$results$textRICH
        # esDataNULL <- is.null(self$options$rcor)
        # if (esDataNULL == TRUE){
        #   table$setError("error")
        # }
        
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
        
        # if (self$options$includemods == TRUE){
        # titleMix <- paste("Mixed-Effects Model (k = ",res$k,")",sep="")
        # titleMixNote <- paste("Tau\u00B2 Estimator: ",tau2EstimatorName, sep="")
        # table$setTitle(title=titleMix)
        # table$setNote("mixnote",titleMixNote)
        
        if (is.null(self$options$moderatorcor) == FALSE) {
          titleMix <- paste("Mixed-Effects Model (k = ", res$k, ")", sep = "")
          titleMixNote <-
            paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
          table$setTitle(title = titleMix)
          table$setNote("mixnote", titleMixNote)
          
        } else if (self$options$methodmetacor == "FE") {
          titleFix <- paste("Fixed-Effects Model (k = ", res$k, ")", sep = "")
          table$setTitle(title = titleFix)
          
        } else {
          titleRan <- paste("Random-Effects Model (k = ", res$k, ")", sep = "")
          titleRanNote <-
            paste("Tau\u00B2 Estimator: ", tau2EstimatorName, sep = "")
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
        tauSquared <- round(res$tau2, 4)
        tauSquaredSE <- round(res$se.tau2, 4)
        tauSqCombind <- paste(tauSquared, "(SE=", tauSquaredSE, ")")
        tauOnly <- round(sqrt(res$tau2), 4)
        ISquStat <- paste(round(res$I2, 2), "%", sep = "")
        HSquStat <- round(res$H2, 4)
        
        if (is.null(self$options$moderatorcor) == FALSE) {
          RSquStat <- paste(round(res$R2, 2), "%", sep = "")
        } else {
          RSquStat <- NULL
        }
        
        #Data Prep: Heterogeneity Test
        QTestStatDF <- round(res$k - 1, 4)
        
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
            Qall = res$QE,
            QallPval = res$QEp
          )
        )
        
        
        # Influence Diagnostics
        
        inf <- influence(res)
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        image <- self$results$plot
        imageFUN <- self$results$funnelALL$funplot
        imageTRIMFILL <- self$results$funnelALL$trimfillplot
        imageTOST <- self$results$tostplot
        pcurvePlot <- self$results$pcurveAll$pcurvePlot
        imageDiagPlot1 <- self$results$diagPlotAll$diagplot1
        imageDiagPlot2 <- self$results$diagPlotAll$diagplot2
        imageDiagPlot3 <- self$results$diagPlotAll$diagplot3
        imageDiagPlot4 <- self$results$diagPlotAll$diagplot4
        imageDiagPlot5 <- self$results$diagPlotAll$diagplot5
        imageDiagPlot6 <- self$results$diagPlotAll$diagplot6
        imageDiagPlot7 <- self$results$diagPlotAll$diagplot7
        imageDiagPlot8 <- self$results$diagPlotAll$diagplot8
        imageDiagPlot9 <- self$results$diagPlotAll$diagplot9
        
        
        image$setState(res)
        imageFUN$setState(res)
        imageTRIMFILL$setState(res)
        pcurvePlot$setState(data)
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
        imageDiagPlot9$setState(res)
        pcurvePlot$setState(data)
        
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
        
        # }}))
      #}
    },
    #Influance plot functions
    .influDiagPlot1 = function(imageDiagPlot1, ...) {
      # <-- the plot function
      plotDataInfluence <- imageDiagPlot1$state
      
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        ready <- FALSE
        jmvcore::reject(
          "Correlations, Sample Sizes, and Study Label fields must be populated to run analysis",
          code = ''
        )
      } else {
        influDiagPlot9 <- qqnorm(plotDataInfluence)
        print(influDiagPlot9)
      }
      TRUE
    },
    .tostplot = function(imageTOST, ...) {
      # <-- the plot function
      plotDataTOST <- imageTOST$state
      
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        
        
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
    
    #pcurve Plot Function
    .pcurvePlot = function(pcurvePlot, ...) {
      #source("../R/pcurve_corr.R")
      # <-- the plot function
      pcurveData <- pcurvePlot$state
      
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        ready <- FALSE
        jmvcore::reject(
          "Correlations, Sample Sizes, and Study Label fields must be populated to run analysis",
          code = ''
        )
      } else {
        pcurvePlot <- pcurve_cor(ni=pcurveData$ni, ri=pcurveData$ri, showText = FALSE, showPlot = TRUE)
        print(pcurvePlot)
      }
      TRUE
    },
    #TOST plot function
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
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
    #Funnel Plot Function
    .funplot = function(imageFUN, ...) {
      # <-- the plot function
      plotDataFUN <- imageFUN$state
      yaxis <- self$options$yaxis
      yaxisInv <- self$options$yaxisInv
      enhancePlot <- self$options$enhanceFunnel
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
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
            plotFUN <- metafor::funnel(plotDataFUN, yaxis = yaxisTrans)
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
    },
    #Trim and Fill Funnel Plot Function
    .trimfillplot = function(imageTRIMFILL, ...) {
      # <-- the plot function
      plotDataTRIMFILL <- imageTRIMFILL$state
      # yaxis <- self$options$yaxis
      # yaxisInv <- self$options$yaxisInv
      # enhancePlot <- self$options$enhanceFunnel
      ready <- TRUE
      if (is.null(self$options$rcor) ||
          is.null(self$options$samplesize) ||
          is.null(self$options$slab) == TRUE) {
        #if (is.null(self$options$rcor) == TRUE){
        
        ready <- FALSE
      }
      if (is.null(imageTRIMFILL$state$yi) ||
          is.null(imageTRIMFILL$state$vi) == TRUE) {
        ready <- FALSE
      }
      if (ready == TRUE) {
            plotTRIMFILL <- metafor::funnel(trimfill(plotDataTRIMFILL), legend=TRUE)
          }
        
        print(plotTRIMFILL)
        TRUE
    }
  )
)