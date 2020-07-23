library(shiny)
library(ggplot2)
library(mosaic)
library(truncnorm)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  #Explore Button
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "Explore")
  })
  
  #population mean plot with true mean
  output$popMean  = renderPlot({
    
    test <- 
    read.table(textConnection("score precentage
                  200-290 0.733
                  300-390 12.892
                  400-490 26.693
                  500-590 32.984
                  600-690 17.083
                  700-800 9.616"),
               header=TRUE,
               stringsAsFactors=FALSE)
    midval <- sapply(strsplit(test$score,"-"), function(x) mean(as.numeric(x)))
    df <- data.frame(X = c(rep(midval,test$precentage)))
    ggplot(df, aes(x = X)) + 
      geom_histogram(
        breaks = seq(200, 800, by = 100), 
        aes(y = ..density..),
        color="#0000b2",
        fill="#7f7fff") + 
      stat_function(
        fun=dnorm,
        xlim = c(200,800), 
        args = list(mean = 528, sd = 117), 
        size = 1)+
      geom_vline(xintercept = 528, color = "forestgreen", size = 1.5)+
      labs(
        title = paste0("Population"),
        x ="SAT Math Scores")+
      scale_x_continuous(breaks = c(200, 300, 400, 500, 600, 700, 800))+
      scale_y_continuous(labels = percent_format()) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
  })
  
  
  
  #Calculating alpha by the confidence level input
  alpha <- reactive({
    (1 - input$level) / 2
  })
  
  #Updating Sample Size
  N <- reactive({
    as.integer(input$nsamp)
  })
  
  
  #generate 50 new sample
  Data <- reactive({
    input$new
    data.frame(
      x = do.call(
          paste0("rtruncnorm"),
          c(list(n = as.integer(input$nsamp) * 50), 
            list(a = 200, b = 800, mean = 528, sd = 117)))
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
    
  })
  
  #calculate the interval
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        sampleMean = mean(x),
        lowerbound = sampleMean + qnorm(alpha()) * 117 / sqrt(N()),
        upperbound = sampleMean - qnorm(alpha()) * 117 / sqrt(N()),
        cover = (lowerbound < 528) & (528 < upperbound) ) %>%
      ungroup()
  })
  
  
  #default as all the samples are selected
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) selected_sample <<- 1
      if (selected_sample > 50) selected_sample <<- 50
    }
    selected_sample
  })
  
  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "navy", "FALSE" = "red")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })
  
  # text messages
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value. And coverage 
          rate is ", round(100 *  sum(Intervals()$cover)/ nrow(Intervals()), 2),
          "% (",  rate$total, " samples)")
  })
  
  #print the CIplot
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    
    validate(
      need(input$nsamp >= 30,
           message = "Please input samle size larger than 30")
    )
    
    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(
          x=idx,
          ymin = lowerbound,
          ymax = upperbound,
          y = sampleMean, 
          colour = cover, 
          alpha = idx == selectedSample(),
          size = idx == selectedSample())) +
      geom_hline(
        yintercept = 528, 
        size = 2,
        colour = "forestgreen", 
        alpha = 0.5) +
      coord_flip() +
      scale_size_manual(
        values = c("TRUE" = 1.5, "FALSE" = .7), 
        guide = FALSE) +
      scale_color_manual(
        values = c("TRUE" = "navy", "FALSE" = "red"), 
        guide = FALSE) +
      scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = .5), 
        guide = FALSE) +
      lims(y = c(300, 700)) +
      labs(title = paste0(100 * input$level, "% Confidence Intervals for the mean"),
           x = "50 samples are generated every time",y="True Mean in Green Color") +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
  })
  
  output$sampMean<- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input samle size")
    )
    validate(
      need(input$nsamp >= 30,
           message = "Please input samle size larger than 30")
    )
    ggplot(data = OneSample()) +
      geom_histogram(aes(x = x), bins = 15, fill = OneSampleColor(), alpha=0.5) +
      geom_vline(
        xintercept = mean(OneSample()$x, color = "navy"),
        size = 1, 
        alpha = 0.7) +
      geom_vline(xintercept = 528, color = "forestgreen", size = 1) +
      labs(title = paste("Sample (mean = ",
                         round(mean(OneSample()$x), 2), ", s = ",
                         round(sd(OneSample()$x), 2), ")"),
           x = "SAT Math Scores") +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
  })
  
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })
  
  
  observeEvent(c( input$A, input$B, input$n, input$level),
    { rate$cover <- sum(Intervals()$cover); rate$total <- nrow(Intervals()) }
  )
  
  
  #Calculating alpha
  zalpha <- reactive({
    (1 - input$zlevel) / 2
  })
  
  zlowerbound <- reactive({
    qnorm(zalpha())
  })
  
  zupperbound <- reactive({
    -qnorm(zalpha())
  })
  
  
  output$zplot = renderPlot({
    
    # draw the normal curve
    curve(
      dnorm(x, mean = 0, sd = 1),
      xlim=c(-3,3),
      xaxt = "n", 
      main="Normal Distribution Plot (Mean = 0, StDev = 1)")
    cord.x <- c(zlowerbound(),seq(zlowerbound(),zupperbound(),0.01),zupperbound())
    cord.y <- c(0,dnorm(seq(zlowerbound(),zupperbound(),0.01)),0)
    
    polygon(cord.x, cord.y, col='skyblue')
    axis(side=1,at=round(c(zlowerbound(),zupperbound()),3))
    
  })
  
  output$feedback <- renderPrint({
    validate(
      need(
        !is.null(input$question1) & !is.null(input$question2) &
          !is.null(input$question3) & !is.null(input$question4),
        'Please answer all questions.'))
    if(
      (input$question1 == 1.645 | input$question1 == 1.65 | 
       input$question1 == 1.6 | input$question1 == 2)
       &(input$question2 == 1.960 | input$question2 == 1.96 | 
         input$question2 == 2.0)
       &(input$question3 == 2.576 | input$question3 == 2.58 | 
         input$question3 == 2.6 | input$question3 == 3)
       &(input$question4 == 'y')){
      cat('All correct. Great Job!')
    }
    
    #Render pic1
    if (input$question1!=''){
      output$pic1 = renderUI({
        
        if(input$question1 == 1.645 || input$question1 == 1.65 ||
           input$question1 == 1.6 || input$question1 == 2){
          img(src = "check.png", width = 25)
        }
        
      })}
    
    #Render pic2
    if (input$question2!=''){
      output$pic2 = renderUI({
        if(input$question2 == 1.960 || input$question2 == 1.96 ||
           input$question2 == 2.0){
          img(src = "check.png", width = 25)
        }
        
      })}
    
    #Render pic3
    if (input$question3!=''){
      output$pic3 = renderUI({
        if(input$question3 == 2.576 || input$question3 == 2.58 || 
           input$question3 == 2.6 || input$question3 == 3){
          img(src = "check.png", width = 25)
        }
        
      })}
    
    
    #Render pic4
    if (input$question4!='null'){
      output$pic4 = renderUI({
        
        if(input$question4 == 'y'){
          img(src = "check.png", width = 25)
        }
        
      })}
  })
  

  
  #Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - input$dlevel) / 2
  })
  
  #Updating Sample Size
  maleN <- reactive({
    as.integer(input$nSamp)
  })
  
  femaleN <- reactive({
    as.integer(input$nSamp)
  })
  
  standardError <- reactive({
    sqrt(118^2/(maleN())+113^2/(femaleN()))
    
  })
  
  #population mean plot with true diffmean
  output$dpopMean  = renderPlot({
    maledata <- read.table(textConnection("score precentage
                                      200-290 0
                                      300-390 11
                                      400-490 29
                                      500-590 31
                                      600-690 22
                                      700-800 7"),
                           header=TRUE,
                           stringsAsFactors=FALSE)
    malemid <- sapply(strsplit(maledata$score,"-"),
                      function(x) mean(as.numeric(x)))
    maledf <- data.frame(S = c(rep(malemid,maledata$precentage)))
    
    femaledata <- read.table(textConnection("score precentage
                                          200-290 0
                                          300-390 8
                                          400-490 30
                                          500-590 33
                                          600-690 22
                                          700-800 6"),
                             header=TRUE,
                             stringsAsFactors=FALSE)
    femalemid <- sapply(strsplit(femaledata$score,"-"),
                        function(x) mean(as.numeric(x)))
    femaledf <- data.frame(S = c(rep(femalemid,femaledata$precentage)))
   
    
    # Now, combine your two dataframes into one. First make a new column in each.
    maledf$Gender <- 'Male'
    femaledf$Gender <- 'Female'
    
    # and combine into your new data frame
    genderdf <- rbind(femaledf, maledf)
    
    # make plot
    ggplot(genderdf, aes(x=S, fill = Gender))+
      geom_histogram(
        alpha = 0.3,
        breaks=seq(200, 800, by = 100), 
        aes(y = ..density..), 
        position = 'identity')+
      stat_function(
        fun=dnorm, 
        color="blue",
        xlim = c(200,800), 
        args=list(mean=475, sd=118), 
        size = 0.8)+
      stat_function(
        fun=dnorm, 
        color="#FF69B4",
        xlim = c(200,800), 
        args=list(mean=487, sd=113),
        size = 0.8)+
      geom_vline(xintercept =475, color = "#4682B4", size = 0.9)+
      geom_vline(xintercept =487, color = "#FF1493", size = 0.9)+
      labs(
        title = paste0("population(male-female) = -12, σ(x(male)-x(female)) = ",
                       round(sqrt(113^2+118^2),3)),
        x ="true mean in blue and pink color")+
      scale_x_continuous(breaks=c(200,300,400,480,600,700,800)) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
    
  })
  
  MaleS <- reactive({
    input$newSample
    rtruncnorm(n = maleN(), a = 200, b = 800, mean = 529, sd= 118)
  })
  
  FemaleS <- reactive({
    input$newSample
    rtruncnorm(n = femaleN(),a = 200, b = 800, mean = 534, sd = 113)
  })
  
  Diff <- reactive({
    mean(MaleS()) - mean(FemaleS())
  })
  
  output$sampleDiff  = renderPlot({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    input$newSample
    # generate new sample
    malesample <- data.frame(sampleGen = MaleS())
    femalesample <- data.frame(sampleGen = FemaleS())
    
    # Now, combine your two dataframes into one.  
    # First make a new column in each that will be a variable to identify where 
    # they came from later.
    malesample$Gender <- 'Male'
    femalesample$Gender <- 'Female'
    
    # and combine into your new data frame 
    sampleTWO <- rbind(femalesample, malesample)
    ggplot(sampleTWO, aes(sampleGen, fill = Gender)) + 
      geom_density(alpha = 0.2)+
      geom_vline(xintercept = mean(MaleS()), color = "#4682B4", size = 0.9)+
      geom_vline(xintercept = mean(FemaleS()), color = "#FF1493", size = 0.9)+
      labs(
        title = paste0("xbar(male-female) = ", round(Diff(),2),
        ",σ(xbar(male)-xbar(female)) = ", round(standardError(),3)),
        x ="sample mean in blue and pink color") +
      scale_x_continuous(breaks=c(200,300,400,480,600,700,800)) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
    
  })
  
  
  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })
  
  output$CItable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$CIcheckbox)
    {
      ctable = matrix(c(dlowerbound(),dupperbound()),nrow=1)
      colnames(ctable) = c("Lower bound","Upper bound")
      #rownames(ctable) = paste((input$dlevel),"% CI",sep="")
      ctable
    }
  })
  
  pvalue <- reactive({
    2*(1-pnorm(abs(zstatistic())))
  })
  
  zstatistic <- reactive({
    Diff()/standardError()
    
  })
  
  output$samplingtable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    
    ctable = matrix(c(mean(MaleS()), mean(FemaleS())),nrow=1)  
    #ctable = matrix(c(mean(MaleS()),sd(MaleS()),
    # mean(FemaleS()),sd(FemaleS())),nrow=2)
    colnames(ctable) = c("Male","Female")
    #rownames(ctable) = c("sample mean","sample sd")
    ctable
  })
  
  output$Diffinfo = renderUI({
    validate(
      need(is.numeric(input$nSamp),
           message = "")
    )
    paste("The difference between male and female sample (male minus female) is ", 
          round(Diff(),2))
  })
  
  output$testtable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$testcheckbox)
    {
      ctable = matrix(c(zstatistic(),pvalue()),nrow=1)
      colnames(ctable) = c("z-statistic","p-value")
      ctable
    }
  })
  
  zstandard <- reactive({
    -qnorm(dalpha())
  })
  
  output$decisionZ = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input samle size")
    )
    if(input$decisioncheckbox)
    {
      if(abs(zstatistic()) <= zstandard()){
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," 
              is less than z* score = ", round(zstandard(),3),", and its p-value = ",
              round(pvalue(),3)," is larger than ",round(2*dalpha(),3),", 
              the null hypothesis provides a reasonable explanation of the data
              so we can NOT conclude that males and females have a different average 
              SAT ERW score when student's are chosen by the researcher's sampling
              procedure.")
        
      }else{
        paste("Since it is observed that |z| = ",abs(round(zstatistic(),3))," 
              is larger than z* score = ", round(zstandard(),3),
              ", and its p-value = ",round(pvalue(),3)," is less than ",
              round(2*dalpha(),3),", the null hypothesis is not a reasonable 
              explanation of the data so we have evidence that there is a difference
              between the male and female average SAT ERW score when students 
              are chosen by the researcher's sampling procedure.")
      }
    }
    
  })
  
})



