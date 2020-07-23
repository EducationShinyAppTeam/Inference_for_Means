library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Sample App]"
APP_DESCP  <<- paste(
  "This app explores the behavior of confidence intervals 
  for a single mean and two sample tests for the difference 
  in means as the level and sample size changes."
)
# End App Meta Data------------------------------------------------------------


dashboardPage(skin="purple",
  # Title
  dashboardHeader(
    title="Inference for Means",
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      tags$a(
        href='https://shinyapps.science.psu.edu/',
        icon("home")))
              ),
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
      menuItem("SAT Math 2019", tabName = "2019", icon = icon("wpexplorer")),
      menuItem("Difference of Means", tabName = "popdiff", icon = icon("cogs")),
      menuItem("Finding the Z* Multiplier", tabName = "findz", icon = icon("gamepad")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
   ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::psu_eberly_logo("reversed")
    )
  ),
              
  # Content within the tabs
  dashboardBody(
    tags$head(
       tags$link(
         rel = "stylesheet", 
         type = "text/css",
         href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
     ),
    tabItems(
      tabItem(
        tabName = "Overview",
        withMathJax(),
        h1("Confident Interval for Two Means"),
        p("This app represents an interactive supplementary module embedded in 
          statistics lessons with two features. The first feature visualizes how 
          the variations in confidence levels and sample size affect the outcome 
          confidence interval in a single mean. The second feature tests the 
          difference between two means by adjusting confidence levels and sample
          size and generating calculation results with explanations. The app 
          requires students to engage in the interaction with the scenarios 
          provided in context."),
        h2("Instructions"),
        tags$ul(
        tags$li("Move the sample size and level sliders to see how they affect 
                confidence intervals for the mean SAT math scores or two-sample 
                tests for differences in SAT ERW (Evidence-Based Reading and
                Writing) scores."),
        tags$li("Click on the generate buttons to draw new samples and - for the
                confidence interval app - click on the center of an interval to 
                show data for that sample."),
        tags$li("On the Test for Differences page the user can change the sample 
                size and/or the confidence level to explore the behavior of a 
                z-test for differences between males and females on the SAT ERW
                scores based on sample data."),
        tags$li("The Z* multiplier page allows the user to find critical values 
                (multiplier numbers) needed in making a confidence interval. 
                Complete a short quiz to show you have mastered the concept."),      
        div(style = "text-align: center",
          bsButton(
            inputId = "go", 
            label = "GO!",
            icon = icon("bolt"),
            size = "large"))),
        br(),
        br(),
        h2("Acknowledgements"),
        p("This app was developed and coded by Yingjie (Chelsea) Wang. The updated
          version was improved by Xuefei Wang. Information about confidence interval
          graph was drawn from Randall Pruim's shiny app.",
          br(),
          br(),
          "These data are about college-bound high school graduates in the year 
          of 2019 who participated in the SAT Program. Students are counted only 
          once, no matter how often they tested, and only their latest scores and
          most recent SAT Questionnaire responses are summarized."),
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 7/20/2020 by XW.")               
        ),
                  
                  
      tabItem(tabName = "2019",
        h2("Confidence Intervals for 2019 SAT Math Score"),
        fluidRow(
          column(4,
            wellPanel(
              h3("Design"),
              p("A researcher plans to take a random sample of size n students to
                do a survey about their experience on the SAT math test. However,
                she worries that sample results could be biased because the students
                who agree to participate might be different from those who don't 
                (this would be an example of non-response bias). The researcher 
                makes a confidence interval for the SAT math scores of the students
                in her study and compares it to the mean of 528 for the population
                of all seniors in the U.S. This app shows how confidence intervals
                of that type would come out when there is no bias."),
              br(),
              h3("Hypothesis"),
              h4("Ho: μ = 528"),
              br(),
              sliderInput(
                inputId = "level",
                label = "Confidence Level",
                min=.50, 
                max = 0.99, 
                value = 0.90, 
                step = 0.01),
              sliderInput(
                inputId = "nsamp", 
                label = "Sample Size (n > 30)",
                min=30,
                max = 500,
                value = 30,
                step = 5),
              actionButton(
                inputId = "new", 
                label = "Generate 50 New Samples",
                icon = icon("retweet")),
              bsPopover(
                id = "new",
                title = "Note",
                content = paste("By clicking on this button, new 50 sample with",
                    "the size you have input in each sample will be generated."),
                trigger="hover",
                placement="bottom")
              )
          
          ),
          column(4, 
            plotOutput("popMean", height = "300px"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('popMean').setAttribute('aria-label',
              `This is a bar chart for population mean.`)
              })"
            ))
          ),
          column(4,
            plotOutput("sampMean", height = "300px"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('sampMean').setAttribute('aria-label',
              `This is a bar chart for sample mean.`)
              })"
            ))
          ),
          column(8,
            plotOutput(outputId = "CIplot", height = "450px",click = "plot_click"),
            tags$script(HTML(
              "$(document).ready(function() {
              document.getElementById('CIplot').setAttribute('aria-label',
              `This is the confidence interval plot.`)
              })"
            )),
            textOutput("CoverageRate"),
            tags$head(tags$style("#CoverageRate{color: green;
                      font-size: 18px;
                      font-style: italic;
                      }"))
          )
        ),
            bsPopover(
              id = "sampMean",
              title = "Sample Histogram",
              content = paste("This is the histogram plot of the sample you",
              "selected on Confidence Interval Plot. The green line is the true",
              "mean of the population and the black line is the mean of the sample."),
              trigger="hover",
              placement="bottom"),
            bsPopover(
              id = "popMean",
              title = "Population Bar Graph",
              content = paste("This is the bar plot based on percentage you input.",
              "The green line is the true percentage in 2019.",
              "(μ = 528, σ = 117, N = 2220087)"),
              trigger="hover",
              placement="bottom"),
            bsPopover(
              id = "CIplot",
              title = "Confidence Interval Plot",
              content = paste("The blue lines indicate a confidence interval covers",
              "the population mean and the red lines indicate that the population",
              "mean is outside of the confidence interval. Click on an interval to",
              "show a histogram for the underlying sample."),
              trigger="hover",
              placement="top")
              
      ),

      tabItem(
        tabName = "popdiff",
        box(
          title = h3("Design"), 
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          width = '100%',
          p("A researcher wants to sample a group of n male and n female students
            about their experiences with the SAT ERW test. Although the average SAT
            ERW score for females is 12 higher than for males, a critic believes her 
            sampling technique would provide a sample of students with a mean (µ)
            that did not depend on gender (the null hypothesis). The researcher 
            uses her samples to conduct a test of that null hypothesis and this 
            test shows how that test would behave when  the sampling is really 
            unbiased and the females have a mean that is 12 higher."
          )
        ),
        h2("Tests for Differences Between 2019 SAT ERW Score by Genders"),
          fluidRow(
            column(4,
              wellPanel(
              h3("Hypothesis: "),
              h4("Assume there is no difference between male and female performance
                 on SAT ERW"),
              h4("Ho: μ(male) = μ(female)"),
              h4("Ha: μ(male) ≠ μ(female)"),
              wellPanel(
                h3("Sample mean information:"),
                tableOutput("samplingtable"),
                uiOutput("Diffinfo")
              ),
              sliderInput(
                inputId = "dlevel",
                label = "Confidence Level",
                min = 0.50, 
                max = 0.99, 
                value = 0.90, 
                step = 0.01),
              sliderInput(
                inputId = "nSamp",
                label = "Sample Size for each of the two groups (n > 30)",
                min=30,
                max = 1000,
                value = 700),
              br()
              ),
              br(),
              wellPanel(
                checkboxInput(
                  inputId = "CIcheckbox",
                  label = "Show Confidence Interval:", 
                  value = FALSE), 
                tableOutput(outputId = "CItable")
              ),
              wellPanel(
                checkboxInput(
                  inputId = "testcheckbox",
                  label = "Show Test Output:", 
                  value = FALSE), 
                tableOutput(outputId = "testtable")
              )
            ),              
            column(8,
              wellPanel(
                plotOutput(outputId = "dpopMean", height = "300px"),
                tags$script(HTML(
                  "$(document).ready(function() {
                  document.getElementById('dpopMean').setAttribute('aria-label',
                  `The two histograms show SAT ERW score distribution by genders.`)
                  })"
                )),
              bsPopover(
                id = "dpopMean",
                title = "Population Histogram",
                content = paste("The two histograms show SAT ERW score distribution",
                          "by genders. The pink and blue vertical lines indicate", 
                          "the true means for two groups."),
                trigger="hover",
                placement="bottom"),
              br(),
              plotOutput(outputId = "sampleDiff", height = "300px"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('samppleDiff').setAttribute('aria-label',
                  `The two histograms show genereated sample of SAT ERW score
                  by genders..`)
                 })"
              ))
              ),
              bsPopover(
                id = "sampleDiff",
                title = "Sample Histogram",
                content = paste("The two histograms show genereated sample of SAT",
                          "ERW score by genders. The pink and blue vertical lines", 
                          "indicate the sample means for two groups."),
                trigger="hover",
                placement="top"),
              actionButton(
                inputId = "newSample", 
                label = "Generate New Samples",
                icon("retweet")),
              bsPopover(
                id = "newSample",
                title = "Note",
                content = paste("By clicking on this button, new sample with the",
                          "size you input will be generated on the Sample Histogram."),
                trigger="hover",
                placement="right"),
              wellPanel(
                checkboxInput(
                  inputId = "decisioncheckbox",
                  label = "Decision about the null hypothesis:", 
                  value = FALSE), 
                textOutput(outputId = "decisionZ")
              )
            )
          )
        ),
      
      tabItem(
        tabName = "findz",
          h2("Confidence Intervals for a population mean"),
          fluidRow(
            column(4,
              wellPanel(
              h3("Finding the z∗ Multiplier"),
              p("The value of the z∗ multiplier is dependent on the level of
                confidence."),
              sliderInput(
                inputId = "zlevel",     
                label = "Confidence Level",
                min=.85,
                max = 0.99,
                value = 0.90,
                step = 0.01)),
              br(),
              wellPanel(
                style = "background-color: #A9A9A9;",
                h3("Quiz"),
                h4("What is z∗  Multiplier for 90% confidence level?",
                   style="font-size:90%"),
                div(style="display:inline-block",
                    textInput(inputId = "question1", 
                              label = " ", 
                              width = '2cm')),
                div(style="display:inline-block", 
                    htmlOutput(outputId = 'pic1')),
                h4("What is z∗  Multiplier for 95% confidence level?",
                   style="font-size:90%"),
                div(style="display:inline-block",
                    textInput(
                      inputId = "question2", 
                      label = " ", 
                      width='2cm',
                      value = "")),
                div(style="display:inline-block",
                    htmlOutput('pic2')),
                h4("What is z∗  Multiplier for 99% confidence level?",
                   style="font-size:90%"),
                div(style="display:inline-block",
                    textInput(
                      inputId = "question3",
                      label = " ",
                      width='2cm',
                      value = "")),
                div(style="display:inline-block",
                    htmlOutput('pic3')),
                h4("Increasing the confidence level makes the confidence 
                   interval wider.",style="font-size:90%"),
                div(style="display:inline-block",
                    selectInput(
                      inputId = "question4", 
                      label = " ",
                      choices = c("True" = "y","False" = "n", " " = "null"),
                      width='2cm',
                      selected = "null")),
                div(style="display:inline-block",
                    htmlOutput('pic4'))
              )
            ),             
            column(8,
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
                          "normal distribution. Multiplier Number (z*) is the",
                          "absolute value of the boundary value. Use the value", 
                          "showed on this graph for following questions."),
                trigger="hover",
                placement="bottom"),
              br(),
              br(),
              h3("Feedback: "),
              textOutput("feedback"),
              tags$head(
                tags$style("#feedback{color:green; font-size: 35px;}"))
            )
          )
      ),
                  
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
          https://reports.collegeboard.org/pdf/2019-total-group-sat-suite-
          assessments-annual-report.pdf "
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
                  
    )#end of tabItem
  )#end of dashboardBody
              
)
