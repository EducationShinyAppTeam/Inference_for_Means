# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(mosaic)
library(truncnorm) # This is needed to simulate SAT values
library(scales)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Confidence Interval for One or Two Means"
APP_DESCP  <<- paste(
  "This app explores the behavior of confidence intervals
  for a single mean and two sample tests for the difference
  in means as the level and sample size changes."
)
# End App Meta Data------------------------------------------------------------

# Global constants, functions, and data ----

## Move all acts of reading in, cleaning data frame to here. Name them appropriately
## so that you can call them in the server

 # This chunk of codes are for population mean plot with true mean
 test <- read.table(textConnection("score precentage
                    200-290 0.733
                    300-390 12.892
                    400-490 26.693
                    500-590 32.984
                    600-690 17.083
                    700-800 9.616"),
                     header = TRUE,
                     stringsAsFactors = FALSE)
 midval <- sapply(strsplit(test$score,"-"), function(x) mean(as.numeric(x)))
 df <- data.frame(X = c(rep(midval,test$precentage)))
  
 # This chunk of codes are for population mean plot with true diff mean
 maledata <- read.table(textConnection("score precentage
                                       200-290 0
                                       300-390 11
                                       400-490 29
                                       500-590 31
                                       600-690 22
                                       700-800 7"),
                        header = TRUE,
                        stringsAsFactors = FALSE)
 malemid <- sapply(strsplit(maledata$score, "-"),
                   function(x) mean(as.numeric(x)))
 maledf <- data.frame(S = c(rep(malemid,maledata$precentage)))
 
 femaledata <- read.table(textConnection("score precentage
                                          200-290 0
                                          300-390 8
                                          400-490 30
                                          500-590 33
                                          600-690 22
                                          700-800 6"),
                          header = TRUE,
                          stringsAsFactors = FALSE)
 femalemid <- sapply(strsplit(femaledata$score, "-"),
                     function(x) mean(as.numeric(x)))
 femaledf <- data.frame(S = c(rep(femalemid, femaledata$precentage)))
 
  ## Now, combine your two data frames into one. First make a new column in each.
  maledf$Gender <- 'Male'
  femaledf$Gender <- 'Female'
 
  ## and combine into your new data frame
  genderdf <- rbind(femaledf, maledf)
 
  
# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    # Title ----
    dashboardHeader(
      title="Interval for Means",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        tags$a(href='https://shinyapps.science.psu.edu/', icon("home"))
      ),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=[Inference for Means]")
      )
    ),
    # Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("SAT Math 2019", tabName = "2019", icon = icon("wpexplorer")),
        menuItem("Difference of Means", tabName = "popdiff", icon = icon("cogs")),
        menuItem("Finding the \\(Z^*\\) Multiplier", tabName = "findz", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    # Body ----
    dashboardBody(
      # Overview" ----
      tabItems(
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Confidence Interval for One or Two Means"),
          p("This app represents an interactive supplementary module embedded in
            statistics lessons with two features. The first feature visualizes
            how the variations in confidence levels and sample size affect the
            outcome confidence interval in a single mean. The second feature
            tests the difference between two means by adjusting confidence levels
            and sample size and generating calculation results with explanations.
            The app requires students to engage in the interaction with the
            scenarios provided in context."
          ),
          h2("Instructions"),
          tags$ul(
            tags$li("Move the sample size and level sliders to see how they affect
                confidence intervals for the mean SAT math scores or two-sample
                tests for differences in SAT ERW (Evidence-Based Reading and
                Writing) scores."),
            tags$li("Click on the generate buttons to draw new samples and for the
                confidence interval app, click on the center of an interval to
                show data for that sample."),
            tags$li("On the Test for Differences page the user can change the sample
                size and/or the confidence level to explore the behavior of a
                z-test for differences between males and females on the SAT ERW
                scores based on sample data."),
            tags$li("The \\(Z^*\\) multiplier page allows the user to find critical
                values (multiplier numbers) needed in making a confidence interval.
                Complete a short quiz to show you have mastered the concept.")),
          div(style = "text-align: center",
              bsButton(
                inputId = "go",
                label = "GO!",
                icon = icon("bolt"),
                size = "large"
              )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Yingjie (Chelsea) Wang. The
            updated version was improved by Xuefei Wang. Information about
            confidence interval graph was drawn from Randall Pruim's shiny app."),
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 11/1/2020 by XW.")
        ),
        # "SAT Math 2019" ----
        tabItem(
          tabName = "2019",
          h2("Confidence Intervals for 2019 SAT Math Scores"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p("A researcher plans to take a random sample of size n students to
              do a survey about their experience on the SAT math test. However,
              she worries that sample results could be biased because the students
              who agree to participate might be different from those who don't
              (this would be an example of non-response bias). The researcher
              makes a confidence interval for the SAT math scores of the students
              in her study and compares it to the mean of 528 for the population
              of all seniors in the U.S. This app shows how confidence intervals
              of that type would come out when there is no bias. These data are
              about College Bound high school graduates in the year of 2019 who
              participated in the SAT Program. Students are counted only once,
              no matter how often they tested, and only their latest scores and
              most recent SAT Questionnaire responses are summarized."
            )
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Hypothesis"),
                p("\\(H_0\\!:\\mu = 528\\)", class = "largerFont"),
                sliderInput(
                  inputId = "level",
                  label = "Confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                ),
                sliderInput(
                  inputId = "nsamp",
                  label = "Sample size (n > 30)",
                  min = 30,
                  max = 500,
                  value = 30
                ),
                p("Please click on this button to check the sample Histogram and 
                  its confidence interval."),
                bsButton(
                  inputId = "new",
                  label = "Generate 50 Samples",
                  icon = icon("retweet"),
                  size = "large"
                ),
                bsPopover(
                  id = "new",
                  title = "Note",
                  ## Possible Error ----
                  content = paste(
                    "By clicking on this button, 50 samples each consisting of",
                    "the number of students you selected will be generated."),
                  trigger = "hover",
                  placement = "bottom"
                )
              ),
              plotOutput("sampMean", height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('sampMean').setAttribute('aria-label',
              `This is a bar chart for sample mean.`)
              })"
              )),
              bsPopover(
                id = "sampMean",
                title = "Sample Histogram",
                content = paste("This is the histogram plot of the sample you",
                                "selected on Confidence Interval Plot."),
                trigger = "hover",
                placement = "top"
              )
            ),
            column(
              width = 8,
              plotOutput("popMean", height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('popMean').setAttribute('aria-label',
              `This is a bar chart for population mean.`)
              })"
              )),
              bsPopover(
                id = "popMean",
                title = "Population Bar Graph",
                content = paste("This is the bar plot based on percentage you input.",
                                "The green line is the true mean of the population
                                in 2019. (\\(\\mu  = 528\\), \\(\\sigma = 117\\), ",
                                "N = 2,220,087)"),
                trigger = "hover",
                placement = "top"
              ),
              plotOutput(
                outputId = "CIplot",
                height = "400px",
                click = "plot_click"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('CIplot').setAttribute('aria-label',
              `This is the confidence interval plot.`)
              })"
              )),
              textOutput("CoverageRate"),
              bsPopover(
                id = "CIplot",
                title = "Confidence Interval Plot",
                content = paste("The blue lines indicate a confidence interval ",
                                "covers the population mean and the red lines ",
                                "indicate that the population mean is outside ",
                                "of the confidence interval. Click on an interval ",
                                "to show a histogram for the underlying sample."),
                trigger = "hover",
                placement = "top"
              )
            )
          )
        ),
        # Difference for Means ----
        tabItem(
          tabName = "popdiff",
          h2("Tests for Differences Between 2019 SAT ERW Score by Genders"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p("A researcher wants to sample a group of n male and n female students
              about their experiences with the SAT ERW (Evidence-Based Reading and
              Writing) test. Although the average SAT ERW score for females is 12
              higher than for males, a critic believes her sampling technique would
              provide a sample of students with a mean (\\(\\mu\\)) that did not
              depend on gender (the null hypothesis). The researcher uses her samples
              to conduct a test of that null hypothesis and this test shows how that
              test would behave when the sampling is really unbiased and the females
              have a mean that is 12 higher."),
            p("Hypothesis: There is no statistically significant difference between
              male and female performance on SAT ERW.")
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Population Information"),
                p("Population mean(diff) = -12 , Population standard deviation for
                  the difference in means = 163.38"),
                h3("Sample Information"),
                uiOutput("sampInfor"),
                br(),
                sliderInput(
                  inputId = "dlevel",
                  label = "Confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                ),
                sliderInput(
                  inputId = "nSamp",
                  label = "Sample size for both groups (n > 30)",
                  min = 30,
                  max = 1000,
                  value = 700),
                br(),
                bsButton(
                  inputId = "newSample",
                  label = "Generate New Samples",
                  icon = icon("retweet"),
                  size = "large"
                ),
                bsPopover(
                  id = "newSample",
                  title = "Note",
                  content = paste("By clicking on this button, new sample with ",
                                  "the size you input will be generated on the ",
                                  "Sample Density Graph."),
                  trigger = "hover",
                  placement = "right"
                )
              )
            ),
            column(
              width = 8,
              plotOutput(outputId = "dpopMean", height = "300px"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('dpopMean').setAttribute('aria-label',
                  `The two histograms show SAT ERW score distribution by genders.
                  The two curves roughly show the density of these two histograms.`)
                  })"
              )),
              bsPopover(
                id = "dpopMean",
                title = "Population Histogram",
                content = paste("The two histograms show SAT ERW score distribution",
                                "by genders."),
                trigger = "hover",
                placement = "bottom"
              ),
              br(),
              plotOutput(outputId = "sampleDiff", height = "300px"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('sampleDiff').setAttribute('aria-label',
                  `The two histograms show genereated sample of SAT ERW score
                  by genders.`)
                 })"
              )),
              bsPopover(
                id = "sampleDiff",
                title = "Sample Density Plot",
                content = paste("The two density curves shows genereated sample of SAT",
                                "ERW score by genders."),
                trigger = "hover",
                placement = "top"
              ),
              checkboxInput(
                inputId = "CTcheckbox",
                label = "Show Confidence Interval and Test Output",
                value = TRUE
              ),
              tableOutput(outputId = "CTtable"), # From shiny rathe than DT
              checkboxInput(
                inputId = "decisioncheckbox",
                label = "Decision about the null hypothesis:",
                value = FALSE
              ),
              textOutput(outputId = "decisionZ")
            )
          )
        ),
        # Find the Z* Mutltiplier ----
        tabItem(
          tabName = "findz",
          h2("Finding the \\(Z^*\\) Multiplier"),
          p("The value of the \\(Z^*\\) multiplier is dependent on the level of
            confidence."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "zlevel",
                  label = "Confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                )
              )
            ),
            column(
              width = 8,
              plotOutput("zplot"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('zplot').setAttribute('aria-label',
                  `This is a z score plot`)
                 })"
              )),
              bsPopover(
                id = "zplot",
                title = "Z Score Plot",
                content = paste("This is the confidence interval plot for standard",
                                "normal distribution. Multiplier Number \\(Z^*\\) is the",
                                "absolute value of the boundary value. Use the value",
                                "showed on this graph for following questions."),
                trigger = "hover",
                placement = "bottom"
              )
            )
          ),
          h3("Check Your Skills"),
          textOutput("feedback"),
          p("What is \\(Z^*\\)  Multiplier for 90% confidence level?"),
          fluidRow(
            column(
              width = 2,
              ### why the div? ----
              div(
                numericInput(
                  inputId = "question1",
                  label = "Your answer",
                  step = 0.001,
                  value = ""
                )
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = 'pic1')
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 95% confidence level?"),
          fluidRow(
            column(
              width = 2,
              div(
                numericInput(
                  inputId = "question2",
                  label = "Your answer",
                  step = 0.001,
                  value = ""
                )
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput('pic2')
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 99% confidence level?"),
          fluidRow(
            column(
              width = 2,
              div(
                numericInput(
                  inputId = "question3",
                  label = "Your answer",
                  step = 0.001,
                  value = ""
                )
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput('pic3')
            )
          ),
          p("Increasing the confidence level makes the confidence interval wider."),
          fluidRow(
            column(
              width = 2,
              div(
                selectInput(
                  inputId = "question4",
                  label = "Your answer",
                  choices = list(
                    "Select an answer" = "select",
                    "True" = "y",
                    "False" = "n"
                  ),
                  selected = "select"
                )
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput('pic4')
            )
          )
        ),
        # References ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "CollegeBoard (2019), SAT Suite of Assessments Anuual Report,
            [data table]. Available at
            https://reports.collegeboard.org/pdf/2019-total-group-sat-suite-assessments-annual-report.pdf "
          ),
          p(
            class = "hangingindent",
            "Mersmann, O., Trautmann, H., Steuer, D., Bornkam, B. (2018), truncnorm:
            Truncated Normal Distribution, R Package. Available from
            https://CRAN.R-project.org/package=truncnorm"
          ),
          p(
            class = "hangingindent",
            'Pruim, R., Kaplan, D., Horton, N. (2017), "The mosaic Package: Helping
            Students to <Think with Data> Using R". The R Journal. Available at
            https://journal.r-project.org/archive/2017/RJ-2017-024/index.html'
          ),
          p(
            class = "hangingindent",
            'Wickham, H., Seidel, D. (2020), scales: Scale Functions for Visualization,
            R Package. Available from https://CRAN.R-project.org/package=scales'
          ),
          p(
            class = "hangingindent",
            'Wickham, H. (2016), "ggplot2: Elegant graphics for data analysis",
            R Package. Springer-Verlag New York. Available at
            https: // ggplot2.tidyverse.org'
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output,session) {
  # Overview's Go button ----
  observeEvent(input$go, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "Explore"
    )
  })

  # This is "SAT Math 2019" part
  ## population mean plot with true mean
  output$popMean  = renderPlot({
    ggplot(df, aes(x = X)) +
      geom_histogram(
        breaks = seq(200, 800, by = 100),
        aes(y = ..density..),
        fill = "skyblue",
        col = "black") +
      stat_function(
        fun = dnorm,
        xlim = c(200,800),
        args = list(mean = 528, sd = 117),
        size = 1)+
      geom_vline(xintercept = 528, color = "forestgreen", size = 1.5)+
      labs(
        title = paste0("Population Histogram"),
        x = "SAT math scores",
        y = "Density") +
      scale_x_continuous(breaks = c(200, 300, 400, 500, 600, 700, 800))+
      scale_y_continuous(labels = percent_format()) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
  })

  ## Calculating alpha by the confidence level input ----
  alpha <- reactive({
    (1 - (input$level/100))/ 2
  })

  ## Get sample size ----
  N <- reactive({
    as.integer(input$nsamp)
  })
  
  ## Generate 50 new samples ----
  Data <- eventReactive(input$new, {
    data.frame(
      x = do.call(
        paste0("rtruncnorm"),
        c(list(n = as.integer(input$nsamp) * 50),
          list(a = 200, b = 800, mean = 528, sd = 117)))
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })
  
  ## Calculate the interval ----
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        sampleMean = mean(x),
        lowerbound = sampleMean + qnorm(alpha()) * 117 / sqrt(N()),
        upperbound = sampleMean - qnorm(alpha()) * 117 / sqrt(N()),
        cover = (lowerbound < 528) & (528 < upperbound),
        .groups = 'drop') %>%
      ungroup()
  })

  ## Default as all the samples are selected ----
  ## What is this code's purpose?
  selected_sample <- 50
  selectedSample <- reactive({
    if (! is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) {selected_sample <<- 1}
      if (selected_sample > 50) {selected_sample <<- 50}
    }
    selected_sample
  })
  OneSample <- reactive({
    Data() %>%
      filter( idx == selectedSample() )
  })
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "skyblue1", "FALSE" = "lightcoral")
    covers <- (Intervals() %>% filter(idx == selectedSample()) )$cover
    colors[ as.character(covers) ]
  })

  ## Text messages ----
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )

    paste(sum(Intervals()$cover), "of these",
          nrow(Intervals()), "intervals cover the parameter value. And coverage
          rate is ", round(100 *  sum(Intervals()$cover)/ nrow(Intervals()), 2),
          "%.")
  })

  ## print the CIplot ----
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )

    validate(
      need(input$nsamp >= 30,
           message = "Please input sample size larger than 30")
    )

    ggplot(data = Intervals()) +
      geom_pointrange(
        aes(
          x = idx,
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
        values = c("TRUE" = "dodgerblue3", "FALSE" = "red"),
        guide = FALSE) +
      scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = .5),
        guide = FALSE) +
      lims(y = c(300, 700)) +
      labs(title = paste0(input$level, "% Confidence Intervals for the Mean"),
           x = "50 samples are generated every time", y = "True mean in green color") +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title=element_text(size = 16))
  })

  ## This is the sample mean plot ----
  output$sampMean <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size")
    )
    validate(
      need(input$nsamp >= 30,
           message = "Please input sample size larger than 30")
    )
    ggplot(data = OneSample()) +
      geom_histogram(aes(x = x),
                     bins = 15,
                     fill = OneSampleColor(),
                     col = "black") +
      geom_vline(
        xintercept = mean(OneSample()$x, color = "black"),
        size = 1) +
      geom_vline(xintercept = 528, color = "forestgreen", size = 1) +
      labs(title = paste("Sample Histogram"),
           x = "Sample mean in black color, true mean in green color",
           y = "Count") +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16))
  })

  # This is "Finding the Z* Multiplier" Part ----
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })

  observeEvent(c( input$A, input$B, input$n, input$level),
               { rate$cover <- sum(Intervals()$cover); rate$total <- nrow(Intervals()) }
  )

  ## Calculating alpha ----
  zalpha <- reactive({
    (1 - (input$zlevel/100)) / 2
  })

  zlowerbound <- reactive({
    round(qnorm(zalpha()), digits = 3)
  })

  zupperbound <- reactive({
    round(qnorm(zalpha(), lower.tail = FALSE), digit = 3)
  })

  output$zplot = renderPlot({

    ## draw the normal curve ----
    curve(
      dnorm(x, mean = 0, sd = 1),
      xlim = c(-3,3),
      xaxt = "n",
      main = "Normal Distribution Plot (Mean = 0, StDev = 1)",
      ylab = "Density",
      cex.lab = 1.5,
      cex.main = 1.5,
      cex.axis = 1.2)
    cord.x <- c(zlowerbound(), seq(zlowerbound(), zupperbound(), 0.01), zupperbound())
    cord.y <- c(0, dnorm(seq(zlowerbound(), zupperbound(), 0.01)), 0)

    polygon(cord.x, cord.y, col = boastUtils::psuPalette[6])
    xtick <- seq(-3, 3, by = 1)
    axis(side = 1, at = c(xtick, zlowerbound(), zupperbound()), labels = TRUE, cex.axis = 1.2)
  })

  output$feedback <- renderPrint({
    validate(
      need(
        !is.na(input$question1) & !is.na(input$question2) &
          !is.na(input$question3) & input$question4 != "select",
        message = 'Please answer all questions'),
      errorClass = "temp")
    if(
      (input$question1 == 1.645 | input$question1 == 1.65 |
       input$question1 == 1.64)
      &(input$question2 == 1.960)
      &(input$question3 == 2.576 | input$question3 == 2.58 |
        input$question3 == 2.6)
      &(input$question4 == 'y')){
      cat('All correct. Great Job!')
    }

    ## Render pic1
    if (input$question1 != ''){
      output$pic1 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question1 - 1.645) <= 0.005,
          ifelse(
            input$question1 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )
      }

    ## Render pic2
    if (input$question2 != ''){
      output$pic2 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question2 - 1.960) <= 0.005,
          ifelse(
            input$question2 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )}

    ## Render pic3
    if (input$question3 != ''){
      output$pic3 <- boastUtils::renderIcon(
        icon = ifelse(
          abs(input$question3 - 2.576) <= 0.005,
          ifelse(
            input$question3 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36 #Note this is larger than what you currently have
      )}

    ## Render pic4
    if (input$question4 != "select"){
      output$pic4 <- boastUtils::renderIcon(
        icon = ifelse(
            input$question4 == 'y',
            ifelse(
              input$question4 > 0,
              "correct",
              "partial"
            ),
            "incorrect"
        ),
        width = 36
      )}
  })

  # This is "Difference of Means" part
  ## Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - (input$dlevel/100)) / 2
  })

  ## Updating Sample Size
  maleN <- reactive({
    as.integer(input$nSamp)
  })

  femaleN <- reactive({
    as.integer(input$nSamp)
  })

  standardError <- reactive({
    sqrt(118^2/(maleN())+113^2/(femaleN()))
  })

  ## population mean plot with true diff mean
  output$dpopMean  = renderPlot({
    ## make plot
    ggplot(genderdf, aes(x = S, fill = Gender))+
      geom_histogram(
        alpha = 0.5,
        breaks = seq(200, 800, by = 100),
        aes(y = ..density.., group=rev(Gender)),
        position = 'identity',
        col = "black") +
      stat_function(
        inherit.aes = FALSE,
        fun = dnorm,
        color = "dodgerblue",
        xlim = c(200,800),
        args = list(mean = 529, sd = 118),
        size = 0.8) +
      stat_function(
        inherit.aes = FALSE,
        fun = dnorm,
        color = "hotpink",
        xlim = c(200, 800),
        args = list(mean = 534, sd = 113),
        size = 0.8) +
      geom_vline(xintercept = 529, color = "dodgerblue", size = 0.9) +
      geom_vline(xintercept = 534, color = "hotpink", size = 0.9) +
      labs(
        title = paste0("Population Histogram for SAT ERW Scores"),
        x ="True mean in blue and pink color",
        y = "Density")+
      scale_x_continuous(breaks = c(200,300,400,480,600,700,800)) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)) 
      
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

  output$sampleDiff = renderPlot({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    input$newSample

    ## generate new sample
    malesample <- data.frame(sampleGen = MaleS())
    femalesample <- data.frame(sampleGen = FemaleS())
    
    ## Now, combine your two data frames into one.
    ## First make a new column in each that will be a variable to identify where
    ## they came from later.
    malesample$Gender <- 'Male'
    femalesample$Gender <- 'Female'
    
    ## and combine into your new data frame
    sampleTWO <- rbind(femalesample, malesample)
    
    ## Density plot for sample means
    ggplot(sampleTWO, aes(sampleGen, fill = Gender)) +
      geom_density(alpha = 0.5)+
      geom_vline(xintercept = mean(MaleS()), color = "dodgerblue", size = 0.9)+
      geom_vline(xintercept = mean(FemaleS()), color = "hotpink", size = 0.9)+
      labs(
        title = paste0("Sample Density Graph"),
        x = "Sample mean in blue and pink color",
        y = "Density") +
      scale_x_continuous(breaks = c(200,300,400,480,600,700,800)) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)) +
      aes(group=rev(Gender))
  })

  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })
  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })

  pvalue <- reactive({
    2*(1-pnorm(abs(zstatistic())))
  })

  zstatistic <- reactive({
    Diff()/standardError()

  })

  output$CTtable = renderTable({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$CTcheckbox)
    {
      ctable = matrix(c(dlowerbound(),dupperbound(),zstatistic(), pvalue()),nrow=1)
      colnames(ctable) = c("Lower bound","Upper bound","z-statistic", "p-value")
      ctable
    }
  })


  output$sampInfor = renderUI({
    paste("Sample means(diff) = ", round(Diff(),2), ", Sample standard deviation
          for the difference in means =", round(standardError(),3))
  })
  zstandard <- reactive({
    -qnorm(dalpha())
  })

  output$decisionZ = renderText({
    validate(
      need(is.numeric(input$nSamp),
           message = "Please input sample size")
    )
    if(input$decisioncheckbox)
    {
      if(abs(zstatistic()) <= zstandard()){
        paste("Since it is observed that |z| = ", abs(round(zstatistic(),3)),"
              is less than Z* score = ", round(zstandard(),3),", and its p-value = ",
              round(pvalue(),3)," is larger than ",round(2*dalpha(),3),",
              the null hypothesis provides a reasonable explanation of the data
              so we can NOT conclude that males and females have a different average
              SAT ERW score when student's are chosen by the researcher's sampling
              procedure.")

      }else{
        paste("Since it is observed that |z| = ", abs(round(zstatistic(),3)),"
              is larger than Z* score = ", round(zstandard(),3),
              ", and its p-value = ",round(pvalue(),3)," is less than ",
              round(2*dalpha(),3),", the null hypothesis is not a reasonable
              explanation of the data so we have evidence that there is a difference
              between the male and female average SAT ERW score when students
              are chosen by the researcher's sampling procedure.")
      }
    }

  })

}

# Boast App Call----
boastUtils::boastApp(ui = ui, server = server)