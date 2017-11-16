library("shiny")
library("foreign")
library("gridExtra")
library("ggplot2")
library("plotly")
source("weightfunction.R")

  shinyServer(function(input, output, session) {

  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    return(read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote,fill=TRUE))
  })

  output$contents <- renderTable({
    validate(
      need(input$file1 !="NULL", "Please upload a data file."))
filedata()
  })

output$selectdigits <- renderUI({
  selectInput(inputId = "digits", label="Select the number of significant digits to report.", choices=c(1,2,3,4,5,6,7,8,9,10),selected=c(4),multiple=FALSE)
})

output$selecteffects <- renderUI({
  #if(!is.null(filedata())){
    colNames <- colnames(filedata())
    selectInput(inputId = "effects", label="Select the variable containing your effect sizes.", choices=colNames,multiple=FALSE)#}
  #else{
  #  return()
  #}
})

output$selectvariances <- renderUI({
  #if(!is.null(filedata())){
    colNames <- colnames(filedata())
    colNames2 <- colNames[colNames != input$effects] #WORKS
    selectInput(inputId = "variances", label="Select the variable containing your sampling variances.", choices=colNames2,multiple=FALSE)#}
 # else{
#    return()
#  }
})

output$thesearemyp <- renderUI({
  colNames <- colnames(filedata())
  colNames2 <- colNames[colNames != input$effects]
  selectInput(inputId = "thesearemyp", label="Select that column.", choices=colNames2,selected=0,multiple=FALSE)
})

output$selectmods <- renderUI({
  #if(!is.null(filedata())){
    colNames <- colnames(filedata())
    colNames2 <- colNames[colNames != input$effects]
    colNames3 <- colNames2[colNames2 != input$variances]
    selectInput(inputId = "moderators", label="Select any moderator variables to include.", choices=colNames3,multiple=TRUE)#}
  #else{
  #  return()
  #}
})

output$selectsteps <- renderUI({
  #if(!is.null(filedata())){
  selectizeInput(inputId = "steps", label="Select at least one p-value cutpoint to include in your model. To include a cutpoint not provided, type it in and press enter.", choices=c(0.001,
                            0.005,
                            0.010,
                            0.020,
                            0.025,
                            0.050,
                            0.100,
                            0.200,
                            0.250,
                            0.300,
                            0.350,
                            0.500,
                            0.600,
                            0.650,
                            0.700,
                            0.750,
                            0.800,
                            0.900,
                            0.950,
                            0.975,
                            0.980,
                            0.990,
                            0.995,
                            0.999),
    multiple=TRUE,
    selected=c(0.025), options=list(create=TRUE,openOnFocus=TRUE))#}
 # else{
  #  return()
  #}
})

output$presetweights <- renderUI({

  steps <- c(sort(input$steps),1.00)
  lapply(1:length(steps), function(i) {
    if(i == 1){
      numericInput(paste("weight", i), paste('<', steps[1]), value = 1, width = '25%')
    }
    numericInput(paste("weight", i), paste(steps[i - 1], "<", steps[i]), value = 1)
  })

})


  unadjustweightnomods <- reactive({
    validate(need(input$file1 !="NULL", "Please upload a data file."),
      need(input$effects != 0, "Please enter the column numbers of the data file containing your effect sizes and variances."))
     if(length(input$moderators) == 0){
      effect <- filedata()[,input$effects]
      v <- filedata()[,input$variances]
      if(input$selectp==TRUE){
        p <- filedata()[,input$thesearemyp]
      }
      if(input$selectp==FALSE){
        p <- 1-pnorm(effect/sqrt(v))
      }
      unadnomods <- weightfunction(effect=effect, v=v,npred=0, 600, 600, p=p)
    unadnomods

  }
  })

  unadjustweightmods <- reactive({
    if(length(input$moderators) > 0){
      npred <- length(input$moderators)
    effect <- filedata()[,input$effects]
    v <- filedata()[,input$variances]
          number <- length(effect)
#           XX <- matrix(nrow=number,ncol=(npred+1))
#           XX[,1] <- rep(1,number)
#           for(i in 2:(npred+1)){ XX[,i] <- filedata()[,input$moderators[i - 1]] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Xx <- matrix(nrow=number,ncol=npred)
#           modnames <- rep(0, npred)
#           for(i in 1:npred)
#             {
#               Xx[,i] <- filedata()[,input$moderators[i]]
#           modnames[i] <- noquote(paste(c("Xx[,",i,"]","+","Xx[,",i + 1,"]"),collapse=" "))
#           }
#           XX <- model.matrix(~modnames)

  Xx <- matrix(nrow=number,ncol=npred)
  Xx <- as.data.frame(Xx)
  for(i in 1:npred)
    {
      Xx[,i] <- filedata()[,input$moderators[i]]
      colnames(Xx)[i] <- input$moderators[i]
    }
  XX <- model.matrix(~., Xx)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
npred <- dim(XX)[2] - 1
prednames <- colnames(XX)
if(input$selectp==TRUE){
  p <- filedata()[,input$thesearemyp]
}
if(input$selectp==FALSE){
  p <- 1-pnorm(effect/sqrt(v))
}



          unadmods <- weightfunction(effect=effect,
                      v=v,
                      npred, steps=600, XX=XX, prednames=prednames, p=p)
          unadmods
}
          })

  adjustweightnomods <- reactive({
    if(length(input$moderators) == 0){
    effect <- filedata()[,input$effects]
    v <- filedata()[,input$variances]
    steps <- c(as.numeric(sort(input$steps)),1.00)
    if(input$selectp==TRUE){
      p <- filedata()[,input$thesearemyp]
    }
    if(input$selectp==FALSE){
      p <- 1-pnorm(effect/sqrt(v))
    }
    if(input$woods){
      weights <- rep(0, length(steps))
      for(i in 1:length(steps)){
        weights[i] <- eval(parse(text=paste("input$'weight ", i, "'", sep="")))
      }
      adnomods <- weightfunction(effect=effect, v=v, npred=0, steps=steps, 600, weights=weights, p=p)
    }
    else{
         adnomods <- weightfunction(effect=effect, v=v, npred=0, steps=steps, 600, p=p)
    }
        adnomods
#         format(adnomods, digits=input$digits)
    
    }
  })

  adjustweightmods <- reactive({
    if(length(input$moderators) > 0){
    effect <- filedata()[,input$effects]
    v <- filedata()[,input$variances]
    npred <- length(input$moderators)
    steps <- c(as.numeric(sort(input$steps)),1.00)
    number <- length(effect)
    if(input$selectp==TRUE){
      p <- filedata()[,input$thesearemyp]
    }
    if(input$selectp==FALSE){
      p <- 1-pnorm(effect/sqrt(v))
    }


#     XX <- matrix(nrow=number,ncol=(npred+1))
#     XX[,1] <- rep(1,number)
#     for(i in 2:(npred+1)){ XX[,i] <- filedata()[,input$moderators[i - 1]] }


    Xx <- matrix(nrow=number,ncol=npred)
    Xx <- as.data.frame(Xx)
    for(i in 1:npred)
    {
      Xx[,i] <- filedata()[,input$moderators[i]]
      colnames(Xx)[i] <- input$moderators[i]
    }
    XX <- model.matrix(~.,Xx)
    npred <- dim(XX)[2] - 1
    prednames <- colnames(XX)

    if(input$woods){
      weights <- rep(0, length(steps))
      for(i in 1:length(steps)){
        weights[i] <- eval(parse(text=paste("input$'weight ", i, "'", sep="")))
      }
      admods <- weightfunction(effect=effect, v=v, npred, steps=steps, XX=XX, prednames, weights=weights, p=p)
    }
    else{
      admods <- weightfunction(effect=effect, v=v, npred, steps=steps, XX=XX, prednames, p=p)
    }
#     format(admods, digits=input$digits)
    
    admods
    }
  })

  output$effects <- renderTable({
    validate(need(input$file1 !="NULL", "Please upload a data file."))
  })

makedefaultPlot <- function(effect, v){
  range_effect <- max(effect) - min(effect)
  range_v <- max(sqrt(v)) - min(sqrt(v))
  lowbound_effect <- min(effect) - 0.05*range_effect
  upbound_effect <- max(effect) + 0.05*range_effect
  lowbound_v <- min(sqrt(v)) - 0.05*range_v
  upbound_v <- max(sqrt(v)) + 0.05*range_v
  plot(sqrt(v),effect, xlim=c(lowbound_v,upbound_v), ylim=c(lowbound_effect, upbound_effect), xlab="Standard Error", ylab="Effect Size")
}

makeotherPlot <- function(effect, v){
  range_effect <- max(effect) - min(effect)
  range_v <- max(sqrt(v)) - min(sqrt(v))
  lowbound_effect <- min(effect) - 0.025*range_effect
  upbound_effect <- max(effect) + 0.025*range_effect
  lowbound_v <- min(sqrt(v)) - 0.025*range_v
  upbound_v <- max(sqrt(v)) + 0.025*range_v
  plot(effect,sqrt(v), xlim=c(lowbound_effect,upbound_effect), ylim=c(lowbound_v, upbound_v), xlab="Effect Size", ylab="Standard Error")
}

  output$funnelplot <- renderPlot({
    validate(need(input$file1 !="NULL", "Please upload a data file."),
      need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."))
    effect <- filedata()[,input$effects]
    v <- filedata()[,input$variances]
    steps <- 1 - as.numeric(sort(input$steps))
    range_v <- max(sqrt(v)) - min(sqrt(v))
    lowbound_v <- min(sqrt(v)) - 0.05*range_v
    upbound_v <- max(sqrt(v)) + 0.05*range_v


    if(input$flip == FALSE){
      if(input$interact == FALSE){
        print(makedefaultPlot(effect, v))

        if(input$contour == FALSE){

          if(input$estimates == TRUE){
              abline(h=unadjustweightnomods()[2,2], col="red")
            }
         if(input$estimates == TRUE && length(input$moderators) > 0){
            abline(h=unadjustweightmods()[2,2], col="red")
         }
         if(input$estimates2 == TRUE){
            abline(h=adjustweightnomods()[2,2], col="blue")
          }
         if(input$estimates2 == TRUE && length(input$moderators) > 0){
           abline(h=adjustweightmods()[2,2], col="blue")
          }

       }
      else{

        testv <- seq(lowbound_v, upbound_v, 0.01)
        for(i in 1:length(steps)){
          lines(testv, 0 + -qnorm(steps[i])*testv)
          lines(testv, 0 - -qnorm(steps[i])*testv)
          # lines(testv, qnorm(steps[i], 0, testv))
        }

        if(input$estimates == TRUE){
          abline(h=unadjustweightnomods()[2,2], col="red")
        }
        if(input$estimates == TRUE && length(input$moderators) > 0){
          abline(h=unadjustweightmods()[2,2], col="red")
        }
        if(input$estimates2 == TRUE){
          abline(h=adjustweightnomods()[2,2], col="blue")
        }
        if(input$estimates2 == TRUE && length(input$moderators) > 0){
          abline(h=adjustweightmods()[2,2], col="blue")
        }

      }
      }
    }
    else {
      print(makeotherPlot(effect, v))

      if(input$contour == FALSE){

        if(input$estimates == TRUE){
          abline(v=unadjustweightnomods()[2,2], col="red")
        }
        if(input$estimates == TRUE && length(input$moderators) > 0){
          abline(v=unadjustweightmods()[2,2], col="red")
        }
        if(input$estimates2 == TRUE){
          abline(v=adjustweightnomods()[2,2], col="blue")
        }
        if(input$estimates2 == TRUE && length(input$moderators) > 0){
          abline(v=adjustweightmods()[2,2], col="blue")
        }

      }
      else{

        testv <- seq(lowbound_v, upbound_v, 0.01)
        for(i in 1:length(steps)){
          lines((0 + -qnorm(steps[i])*testv), testv)
          lines((0 - -qnorm(steps[i])*testv), testv)
          # lines(qnorm(steps[i], 0, testv), testv)
        }

        
##### NOTE to self -- I never added lines at moderators.
        ### Possibly should do?
        
        if(input$estimates == TRUE){
          abline(v=unadjustweightnomods()[2,2], col="red")
        }
        if(input$estimates == TRUE && length(input$moderators) > 0){
          abline(v=unadjustweightmods()[2,2], col="red")
        }
        if(input$estimates2 == TRUE){
          abline(v=adjustweightnomods()[2,2], col="blue")
        }
        if(input$estimates2 == TRUE && length(input$moderators) > 0){
          abline(v=adjustweightmods()[2,2], col="blue")
        }

      }

      }
    }
)
  
output$plotly <- renderPlot({
  validate(need(input$file1 !="NULL", "Please upload a data file."),
           need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."))
  effect <- filedata()[,input$effects]
  v <- filedata()[,input$variances]
  steps <- 1 - as.numeric(sort(input$steps))
  

  if(input$flip==FALSE){
    range_effect <- max(effect) - min(effect)
    range_v <- max(sqrt(v)) - min(sqrt(v))
    lowbound_effect <- min(effect) - 0.05*range_effect
    upbound_effect <- max(effect) + 0.05*range_effect
    lowbound_v <- min(sqrt(v)) - 0.05*range_v
    upbound_v <- max(sqrt(v)) + 0.05*range_v
    plot(sqrt(v),effect, xlim=c(lowbound_v,upbound_v), ylim=c(lowbound_effect, upbound_effect), xlab="Standard Error", ylab="Effect Size")
    if(input$contour==TRUE){
      testv <- seq(lowbound_v, upbound_v, 0.01)
      for(i in 1:length(steps)){
        lines(testv, 0 + -qnorm(steps[i])*testv)
        lines(testv, 0 - -qnorm(steps[i])*testv)
        }
    }
      if(input$estimates==TRUE){
          abline(h=unadjustweightnomods()[2,2], col="red")
        }
        if(input$estimates2==TRUE){
          abline(h=adjustweightnomods()[2,2], col="blue")
          }
    }
  else{
    range_effect <- max(effect) - min(effect)
    range_v <- max(sqrt(v)) - min(sqrt(v))
    lowbound_effect <- min(effect) - 0.025*range_effect
    upbound_effect <- max(effect) + 0.025*range_effect
    lowbound_v <- min(sqrt(v)) - 0.025*range_v
    upbound_v <- max(sqrt(v)) + 0.025*range_v
    plot(effect,sqrt(v), xlim=c(lowbound_effect,upbound_effect), ylim=c(lowbound_v, upbound_v), xlab="Effect Size", ylab="Standard Error")
    
    if(input$contour==TRUE){
      testv <- seq(lowbound_v, upbound_v, 0.01)
      for(i in 1:length(steps)){
        lines((0 + -qnorm(steps[i])*testv), testv)
        lines((0 - -qnorm(steps[i])*testv), testv)      
        }
    }
    if(input$estimates==TRUE){
      abline(v=unadjustweightnomods()[2,2], col="red")
    }
    if(input$estimates2==TRUE){
      abline(v=adjustweightnomods()[2,2], col="blue")
    }
  }
  ############################ ADD OPTIONS HERE FOR OTHER CHECKBOXES
  ######### LINES AT ESTIMATES, CONTOUR LINES
})

output$info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
           " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  }
  pval_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("pval=", round((1 - abs(pnorm(e$x/e$y))), digits=2),"\n")
  }
  
  paste0(
    "click: ", xy_str(input$plot_click),
    "dblclick: ", xy_str(input$plot_dblclick),
    "hover: ", pval_str(input$plot_hover),
    "brush: ", xy_range_str(input$plot_brush)
  )
})

output$density <- renderPlot({
  validate(need(input$file1 !="NULL", "Please upload a data file."),need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."),need(length(input$moderators) == 0, "Please remove the moderators from your model to view this plot. The plot does not incorporate moderators."))
  effect <- filedata()[,input$effects]
  v <- filedata()[,input$variances]
  steps <- c(as.numeric(sort(input$steps)),1.00)
  
  print(makeDensityPlot(effect, v, steps))
})

######### DENSITY PLOTS HERE TO NEXT LINE #############
makeDensityPlot <- function(effect, v, steps){
  
  ######## I THINK this works .... triple and quadruple check the damn thing!
  
  #Identifying appropriate values
  vc1 <- unadjustweightnomods()[1,2]
  mu1 <- unadjustweightnomods()[2,2]
  vc2 <- adjustweightnomods()[1,2]
  mu2 <- adjustweightnomods()[2,2]
  weights <- adjustweightnomods()[3:(length(steps)+1),2]
  cuts <- steps
  x_low_lim <- min(effect) - 2
  x_up_lim <- max(effect) + 2
  
  # print(c(vc1, mu1, vc2, mu2, weights, cuts, x_low_lim, x_up_lim))
  
  xfull <- seq(x_low_lim,x_up_lim,.01) 
  
  ########### Trying the harmonic mean from Hoaglin as the average
  ## conditional variance, rather than the median. The harmonic mean
  ## actually appears to do a worse job. Weird.
  
  vi <- median(v)
#   w <- 1/v
#   s_squared <- (length(effect)-1)/( sum( w ) - sum( w^2 / sum( w ) ) )
#   vi <- s_squared
  
  ###########
  
  fx <- ( 1/(sqrt(2*pi*(vi + vc1))) ) * exp( -1/2*( (xfull - mu1)^2 / (vi + vc1) ) )
  yfull <- fx
  A0 <- sum(rep(.01,length(xfull))*yfull)
  
  # fx2 <- ( 1/(sqrt(2*pi*(vi + vc2))) ) * exp( -1/2*( (xfull - mu2)^2 / (vi + vc2) ) )
  
  fx2 <- ( 1/(sqrt(2*pi*(vi + vc1))) ) * exp( -1/2*( (xfull - mu1)^2 / (vi + vc1) ) )

  testlist <- -1 * qnorm(steps, 0, sqrt(vi + vc2))
  
  testxfull <- findInterval(xfull,sort(testlist))
  xlist <- split(xfull, testxfull)
  
  ylist <- split(fx2, testxfull)
  weights2 <- rev(c(1, weights))
  testyfull <- mapply("*", ylist, weights2)
  
  A1 <- sum(rep(.01,length(unlist(xlist)))*unlist(testyfull))
  
  #Creating the plot
  plot(c(x_low_lim,x_up_lim), c(0,(max(as.numeric(unlist(testyfull))/A1)+0.10)), type='n', xlab='Sample Effect Size',
       ylab='Density',axes=FALSE,lwd=2,font.lab=2,main='Expected and Adjusted Densities')
  box(lwd=2)
  axis(side=2,font=2)
  axis(side=1,font=2)
  abline(c(0,0),lwd=2)
  
  #Drawing unadjusted density
  # lines(xfull,yfull,lty=2,lwd=2)
  
  lines(xfull,yfull/A0,lty=2,lwd=2)
  
  # lines(as.numeric(unlist(xlist)), as.numeric(unlist(testyfull)))
  
  lines(as.numeric(unlist(xlist)), as.numeric(unlist(testyfull))/A1)
  print("TEST")
}



  
  
  
######################################################  
  
output$funnelplot2 <- renderUI({
  plotOutput("funnelplot", width=paste0(input$width, "%"), height=input$height)
})

output$downloadfunnelplot <- downloadHandler(
   filename = function(){
    paste('funnelplot', Sys.Date(), '.pdf', sep='')
   },
  content = function(FILE=NULL) {
    effect <- filedata()[,input$effects]
    v <- filedata()[,input$variances]
    pdf(file=FILE)
    if(input$flip == FALSE){
      print(makedefaultPlot(effect,v))}
    else{
      print(makeotherPlot(effect,v))
    }
    dev.off()
  }
  )

  output$unadjustedweightfunction <- renderTable({
    if(length(input$moderators) == 0){
      format(unadjustweightnomods(), digits=input$digits)
      }
    else{
      format(unadjustweightmods(), digits=input$digits)
    }
  })

output$questionmark <- renderImage({
  list(src = './www/questionmark.png',width=17,height=17, alt = "Question_Mark")
}, deleteFile=FALSE)
output$questionmark2 <- renderImage({
  list(src = './www/questionmark.png',width=17,height=17, alt = "Question_Mark")
}, deleteFile=FALSE)

    output$adjustedweightfunction <- renderTable({
      if(length(input$moderators) == 0){
        validate(need(input$file1 !="NULL", "Please upload a data file."),
                 need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."),
                 need(input$steps !=0, "Please select at least one p-value cutpoint to include in your model."))

        intervaltally <- function(p, steps) {
          p1 <- cut(p, breaks=c(-Inf,steps), labels=steps)
          return(p1) }

        effect <- filedata()[,input$effects]
        v <- filedata()[,input$variances]
        steps <- c(as.numeric(sort(input$steps)),1.00)
        if(input$effects != "NULL") {
          if(input$selectp==TRUE){
            p <- filedata()[,input$thesearemyp]
          }
          if(input$selectp==FALSE){
            p <- 1-pnorm(effect/sqrt(v))
          }
          pvalues <- as.numeric(table(intervaltally(p, steps)))
        }
      format(adjustweightnomods(), digits=input$digits)
        # adjustweightnomods()
        
      }
else{
        intervaltally <- function(p, steps) {
          p1 <- cut(p, breaks=c(-Inf,steps), labels=steps)
          return(p1) }
        effect <- filedata()[,input$effects]
        v <- filedata()[,input$variances]
        steps <- c(as.numeric(sort(input$steps)),1.00)
        if(input$effects != "NULL") {
          if(input$selectp==TRUE){
            p <- filedata()[,input$thesearemyp]
          }
          if(input$selectp==FALSE){
            p <- 1-pnorm(effect/sqrt(v))
          }
          pvalues <- as.numeric(table(intervaltally(p, steps)))
        }
        format(adjustweightmods(), digits=input$digits)
      }
    })

output$likelihoodratio <- renderTable({
  validate(need(input$file1 !="NULL", "Please upload a data file."),
           need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."),
           need(input$steps !=0, "Please select at least one p-value cutpoint to include in your model."),
           need(input$woods==FALSE, "This is not valid under the Vevea and Woods (2005) model."))
    if(length(input$moderators) == 0){
      effect <- filedata()[,input$effects]
      v <- filedata()[,input$variances]
      steps <- c(as.numeric(sort(input$steps)),1.00)
      if(input$selectp==TRUE){
        p <- filedata()[,input$thesearemyp]
      }
      if(input$selectp==FALSE){
        p <- 1-pnorm(effect/sqrt(v))
      }
    format(likelihoodfunct(effect=effect, v=v, npred=0, steps=steps, 600,p=p),digits=input$digits)
    }
    else{
      effect <- filedata()[,input$effects]
      v <- filedata()[,input$variances]
      steps <- c(as.numeric(sort(input$steps)),1.00)
      npred <- length(input$moderators)
      if(input$selectp==TRUE){
        p <- filedata()[,input$thesearemyp]
      }
      if(input$selectp==FALSE){
        p <- 1-pnorm(effect/sqrt(v))
      }
      number <- length(effect)
      XX <- matrix(nrow=number,ncol=(npred+1))
      XX[,1] <- rep(1,number)
      for(i in 2:(npred+1)){ XX[,i] <- filedata()[,input$moderators[i - 1]] }
      format(likelihoodfunct(effect=effect, v=v, npred=npred, steps=steps, XX,p=p),digits=input$digits) }
  })

 output$samplesizes <- renderTable({
   validate(need(input$file1 !="NULL", "Please upload a data file."),
            need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."),
            need(input$steps !=0,"Please select at least one p-value cutpoint to include in your model."))
effect <- filedata()[,input$effects]
v <- filedata()[,input$variances]
  intervaltally <- function(p, steps) {
    p1 <- cut(p, breaks=c(-Inf,steps), labels=steps)
    return(p1) }
steps <- c(as.numeric(sort(input$steps)),1.00)
if(input$selectp){
  p <- filedata()[,input$thesearemyp]
}
else{
  p <- 1-pnorm(effect/sqrt(v))
}
pvalues <- as.numeric(table(intervaltally(p, steps)))
format(sampletable(p=p, pvalues=pvalues, steps=steps), digits=input$digits)
})
#toggleModal(session, "samplesizes", toggle="toggle")

output$numberofeffects <- renderTable({
  validate(need(input$file1 !="NULL", "Please upload a data file."),
           need(input$effects !=0, "Please enter the column numbers of the data file containing your effect sizes and variances."),
           need(input$steps !=0,"Please select at least one p-value cutpoint to include in your model."))
  effect <- filedata()[,input$effects]
  results <- matrix(nrow=1,ncol=1)
  results[1,1] <- length(effect)
  resultsb <- data.frame(results, row.names=c("k"))
  colnames(resultsb) <- c("Total Number of Effects")
  format(resultsb, digits=input$digits)
})

  })
