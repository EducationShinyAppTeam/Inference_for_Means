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
APP_TITLE <<- "Confidence Intervals for One or Two Means"
APP_DESCP <<- paste(
  "This app explores the behavior of confidence intervals
  for a single mean and two sample tests for the difference
  in means as the level and sample size changes."
)
# End App Meta Data------------------------------------------------------------

# Global constants, functions, and data ----

## Population plot for the SAT Math scores
satMath <- data.frame(
  scoreGroup = c("200-290", "300-390", "400-490", "500-590", "600-690", "700-800"),
  midScore = c(245, 345, 445, 545, 645, 750),
  percentage = c(0.733, 12.892, 26.693, 32.984, 17.083, 9.616)
)

mathPopPlot <- ggplot(
  data = satMath,
  mapping = aes(x = midScore, weight = percentage)
) +
  geom_histogram(
    mapping = aes(y = ..density..),
    breaks = seq(200, 800, by = 100),
    fill = "skyblue",
    col = "black"
  ) +
  stat_function(
    fun = dnorm,
    xlim = c(200, 800),
    args = list(mean = 528, sd = 117),
    size = 1
  ) +
  geom_vline(xintercept = 528, color = boastPalette[3], size = 1.5) +
  labs(
    title = "Population Histogram",
    x = "SAT math scores",
    y = "Density"
  ) +
  scale_x_continuous(breaks = c(200, 300, 400, 500, 600, 700, 800)) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 16)
  )

## This chunk of codes are for population mean plot with true diff mean
satERW <- data.frame(
  scoreGroup = c("200-290", "300-390", "400-490", "500-590", "600-690", "700-800"),
  midScore = c(245, 345, 445, 545, 645, 750),
  malePercentage = c(0, 11, 29, 31, 22, 7),
  femalePercentage = c(0, 8, 30, 33, 22, 6)
)

erwPopPlot <- ggplot(
  data = satERW,
  mapping = aes(x = midScore)
) +
  geom_histogram(
    mapping = aes(y = ..density.., weight = malePercentage, fill = "male"),
    breaks = seq(200, 800, by = 100),
    col = "black",
    alpha = 0.5
  ) +
  geom_histogram(
    mapping = aes(y = ..density.., weight = femalePercentage, fill = "female"),
    breaks = seq(200, 800, by = 100),
    col = "black",
    alpha = 0.5
  ) +
  stat_function(
    inherit.aes = FALSE,
    fun = dnorm,
    color = "dodgerblue",
    xlim = c(200, 800),
    args = list(mean = 529, sd = 118),
    size = 0.8
  ) +
  stat_function(
    inherit.aes = FALSE,
    fun = dnorm,
    color = "hotpink",
    xlim = c(200, 800),
    args = list(mean = 534, sd = 113),
    size = 0.8
  ) +
  geom_vline(xintercept = 529, color = "dodgerblue", size = 0.9) +
  geom_vline(xintercept = 534, color = "hotpink", size = 0.9) +
  labs(
    title = "Pop. Histogram of SAT ERW Scores by Sex",
    x = "ERW score",
    y = "Density"
  ) +
  scale_x_continuous(breaks = c(200, 300, 400, 480, 600, 700, 800)) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 16),
    legend.position = "right"
  ) +
  scale_fill_manual(
    name = "sex",
    labels = c(
      "male" = "Male",
      "female" = "Female"
    ),
    values = c(
      "male" = "dodgerblue",
      "female" = "hotpink"
    )
  )

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    # Title ----
    dashboardHeader(
      title = "Conf. Intervals for Means",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        tags$a(
          target = "_blank", icon("comments"),
          href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Inference_for_Means"
        )
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
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
      # Overview ----
      tabItems(
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Confidence Intervals for One or Two Means"),
          p("This app represents an interactive supplementary module embedded in
            statistics lessons with two features. The first feature visualizes
            how the variations in confidence levels and sample size affect the
            outcome confidence interval in a single mean. The second feature
            tests the difference between two means by adjusting confidence levels
            and sample size and generating calculation results with explanations.
            The app requires students to engage in the interaction with the
            scenarios provided in context."),
          h2("Instructions"),
          tags$ul(
            tags$li(strong("SAT Math 2019 and Difference of Means pages")),
            tags$ul(
              tags$li("Move the sample size and level sliders to see how they
                      affect confidence intervals for the mean SAT math scores or
                      two-sample tests for differences in SAT ERW (Evidence-Based
                      Reading and Writing) scores."),
              tags$li("Click on the generate buttons to draw new samples and for
                      the confidence interval app, click on the center of an
                      interval to show data for that sample."),
              tags$li(
                strong("Difference of Means page: "),
                "You can change the sample size and/or the confidence level
                    and explore the behavior of a Z-test for differences between
                    males and females on the SAT ERW scores based on sample data."
              )
            ),
            br(),
            tags$li(strong("Finding the \\(Z^*\\) Multipler page")),
            tags$ul(
              tags$li("The \\(Z^*\\) multiplier page allows the user to find
                      critical values (multiplier numbers) needed in making a
                      confidence interval. Complete a short quiz to show you have
                      mastered the concept.")
            )
          ),
          div(
            style = "text-align: center;",
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
          div(class = "updated", "Last Update: 11/3/2020 by NJH.")
        ),
        # SAT Math 2019 ----
        tabItem(
          tabName = "2019",
          h2("Confidence Intervals for 2019 SAT Math Scores"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            p("A researcher plans to take a random sample of size n students to
              do a survey about their experience on the SAT math test. The researcher
              makes a confidence interval for the SAT math scores of the students
              in her study and compares it to the mean of 528 for the population
              of all seniors in the U.S. These data are about College Bound high
              school graduates in the year of 2019 who participated in the SAT
              Program. Students are counted only once, no matter how often they
              tested, and only their latest scores and most recent SAT
              Questionnaire responses are summarized.")
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
                  value = 30,
                  step = 1
                ),
                p("Click the following button to create 50 samples of your
                  selected sample size to see their confidence intervals and
                  histograms."),
                bsButton(
                  inputId = "new",
                  label = "Generate 50 Samples",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput("popMean", height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('popMean').setAttribute('aria-labelledby',
                'mathPopPara')
                })"
              )),
              p(
                id = "mathPopPara",
                "The population chart shows the density of all SAT Math scores,
                N = 2,220,087 test takers. The population mean appears as a green
                vertical line, \\(\\mu=528\\); the population standard deviation
                is 117. We've overlaid a smoothed density curve in black."
              )
            )
          ),
          uiOutput("sampleMessage"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampMean"),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('sampMean').setAttribute('aria-label',
              `This is a bar chart for a single sample.`)
              })"
              )),
              uiOutput("sampleColors")
            ),
            column(
              width = 6,
              plotOutput(
                outputId = "CIplot",
                click = "plot_click"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('CIplot').setAttribute('aria-label',
              `This is the confidence interval plot.`)
              })"
              )),
              textOutput("CoverageRate")
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
            width = "100%",
            p("A researcher wants to sample a group of n male and n female students
              about their experiences with the SAT ERW (Evidence-Based Reading and
              Writing) test. Although the average SAT ERW score for females is 12
              higher than for males, a critic believes her sampling technique would
              provide a sample of students with a mean (\\(\\mu\\)) that did not
              depend on sex (the null hypothesis). The researcher uses her samples
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
                p("Subtraction order: male − female"),
                p(
                  "Population difference in means is -12,",
                  br(),
                  "Standard deviation for the difference in population means
                  is 163.38"
                ),
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
                  value = 700
                ),
                br(),
                bsButton(
                  inputId = "newSample",
                  label = "Generate New Samples",
                  icon = icon("retweet"),
                  size = "large"
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
              br(),
              plotOutput(outputId = "sampleDiff", height = "300px"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('sampleDiff').setAttribute('aria-label',
                  `The two density curves show genereated sample of SAT ERW score
                  by sexes.`)
                 })"
              )),
              checkboxInput(
                inputId = "CTcheckbox",
                label = "Show Confidence Interval and Test Output",
                value = FALSE,
                width = "100%"
              ),
              tableOutput(outputId = "CTtable"), # From shiny rather than DT
              checkboxInput(
                inputId = "decisioncheckbox",
                label = "Show decision about the null hypothesis",
                value = FALSE,
                width = "100%"
              ),
              textOutput(outputId = "decisionZ")
            )
          )
        ),
        # Find the Z* Mutltiplier ----
        tabItem(
          tabName = "findz",
          h2("Finding the \\(Z^*\\) Multiplier"),
          p("When constructing a confidence interval for a mean or a difference
            of two means, we often need to find a critical value to help us
            calculate the appropriate width. One of the most common tools to do
            this is the Standard Normal distribution which has a mean of zero and
            a standard devation of one. The values we get from this distribution
            are often denoted \\(Z^*\\) and function as a multipler. The value of
            the \\(Z^*\\) multiplier will depend on our selected level of
            confidence."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "zlevel",
                  label = "Selected confidence level",
                  min = 85,
                  max = 99,
                  value = 90,
                  post = "%"
                )
              ),
              br(),
              p("Observe what happens in the plot when you adjust the confidence
                  level slider. When you are ready, check your skill by answering
                  the questions below."),
              p("You will have to answer all of the questions before your answers
                will be checked.")
            ),
            column(
              width = 8,
              plotOutput("zplot"),
              tags$script(HTML(
                "$(document).ready(function() {
                  document.getElementById('zplot').setAttribute('aria-label',
                  `This is a Z-score plot, which shows the multipler value as
                  the boundaries of the shaded region. Use these values to
                  answer the following questions.`)
                 })"
              ))
            )
          ),
          h3("Check Your Skills"),
          uiOutput("feedback"),
          p("What is \\(Z^*\\)  Multiplier for 90% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question1",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput(outputId = "pic1")
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 95% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question2",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput("pic2")
            )
          ),
          p("What is \\(Z^*\\)  Multiplier for 99% confidence level?"),
          fluidRow(
            column(
              width = 2,
              numericInput(
                inputId = "question3",
                label = "Your answer",
                step = 0.001,
                value = ""
              )
            ),
            column(
              width = 10,
              br(),
              uiOutput("pic3")
            )
          ),
          p("True or False: Increasing the confidence level makes the confidence interval wider."),
          fluidRow(
            column(
              width = 3,
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
            ),
            column(
              width = 9,
              br(),
              uiOutput("pic4")
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
            "Carey, R. and Hatfield, N. J. (20202), boastUtils: BOAST Utilities,
            R Package.
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
            "Wickham, H., Seidel, D. (2020), scales: Scale Functions for Visualization,
            R Package. Available from https://CRAN.R-project.org/package=scales"
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
server <- function(input, output, session) {
  # Overview's Go button ----
  observeEvent(input$go, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "2019"
    )
  })

  # sample message
  observeEvent(input$new, {
    output$sampleMessage <- renderUI({
      "Click on the confidence intervals (on the right) to view the histogram of
      the sample on the left."
    })

    output$sampleColors <- renderUI({
      "The sample histogram will change colors (red or blue) depending on whether
      the confidence interval constructed from the sample contains the population
      mean."
    })
  })

  # This is "SAT Math 2019" part
  ## population mean plot with true mean
  output$popMean <- renderPlot({
    mathPopPlot
  })

  ## Calculating alpha by the confidence level input ----
  alpha <- eventReactive(input$new, {
    (1 - (input$level / 100)) / 2
  })

  ## Get sample size ----
  N <- eventReactive(input$new, {
    as.integer(input$nsamp)
  })

  ## Generate 50 new samples ----
  Data <- eventReactive(input$new, {
    data.frame(
      x = do.call(
        paste0("rtruncnorm"),
        c(
          list(n = as.integer(input$nsamp) * 50),
          list(a = 200, b = 800, mean = 528, sd = 117)
        )
      )
    ) %>%
      mutate(idx = rep(1:50, each = input$nsamp))
  })

  ## Calculate the interval ----
  Intervals <- reactive({
    Data() %>%
      group_by(idx) %>%
      summarise(
        .groups = "keep",
        sampleMean = mean(x),
        lowerbound = sampleMean + qnorm(alpha()) * 117 / sqrt(N()),
        upperbound = sampleMean - qnorm(alpha()) * 117 / sqrt(N()),
        cover = (lowerbound < 528) & (528 < upperbound),
        .groups = "drop"
      ) %>%
      ungroup()
  })

  ## Default as all the samples are selected ----
  ## TODO: What is this code's purpose?
  selected_sample <- 50
  selectedSample <- reactive({
    if (!is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) {
        selected_sample <<- 1
      }
      if (selected_sample > 50) {
        selected_sample <<- 50
      }
    }
    selected_sample
  })
  OneSample <- reactive({
    Data() %>%
      filter(idx == selectedSample())
  })

  OneSampleColor <- reactive({
    colors <- c("TRUE" = "skyblue1", "FALSE" = "lightcoral")
    covers <- (Intervals() %>% filter(idx == selectedSample()))$cover
    colors[as.character(covers)]
  })

  ### Store xAPI interacted statement ----
  observeEvent(input$plot_click, {

    stmt <- boastUtils::generateStatement(
      session,
      verb = "interacted",
      object = "CIplot",
      description = "90% Confidence Intervals for the Mean",
      interactionType = "numeric",
      response = jsonlite::toJSON(input$plot_click)
    )

    boastUtils::storeStatement(session, stmt)
  })

  ## Text messages ----
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )

    paste0(
      sum(Intervals()$cover),
      " of these ",
      nrow(Intervals()),
      " intervals cover the parameter value. The coverage rate is ",
      round(100 * sum(Intervals()$cover) / nrow(Intervals()), 2),
      "%."
    )
  })

  ## print the CIplot ----
  output$CIplot <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )

    validate(
      need(input$nsamp >= 30,
        message = "Please input sample size larger than 30"
      )
    )

    ggplot(data = Intervals()) +
      geom_pointrange(
        mapping = aes(
          x = idx,
          ymin = lowerbound,
          ymax = upperbound,
          y = sampleMean,
          colour = cover,
          alpha = idx == selectedSample(),
          size = idx == selectedSample()
        )
      ) +
      geom_hline(
        mapping = aes(yintercept = 528, color = "zpop"),
        size = 1.25,
        alpha = 1
      ) +
      coord_flip() +
      scale_size_manual(
        values = c("TRUE" = 1.5, "FALSE" = .7),
        guide = FALSE
      ) +
      scale_color_manual(
        name = NULL,
        labels = c(
          "TRUE" = "Captures",
          "FALSE" = "Fails",
          "zpop" = "Population Mean"
        ),
        values = c(
          "TRUE" = "dodgerblue3",
          "FALSE" = "red",
          "zpop" = boastPalette[3]
        )
      ) +
      scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = .5),
        guide = FALSE
      ) +
      labs(
        title = paste0(input$level, "% Confidence Intervals for the Mean"),
        x = NULL,
        y = "SAT math score"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
  })

  ## This is the sample mean plot ----
  output$sampMean <- renderPlot({
    validate(
      need(is.numeric(input$nsamp),
        message = "Please input sample size"
      )
    )
    validate(
      need(input$nsamp >= 30,
        message = "Please input sample size larger than 30"
      )
    )
    ggplot(data = OneSample()) +
      geom_histogram(
        mapping = aes(x = x),
        bins = 15,
        fill = OneSampleColor(),
        col = "black"
      ) +
      geom_vline(
        mapping = aes(xintercept = mean(OneSample()$x), color = "sample"),
        size = 1
      ) +
      geom_vline(
        mapping = aes(xintercept = 528, color = "pop"),
        size = 1
      ) +
      labs(
        title = "Histogram of Selected Sample",
        x = "SAT math score",
        y = "Count"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      ) +
      scale_color_manual(
        name = NULL,
        labels = c(
          "sample" = "Sample mean",
          "pop" = "Population mean"
        ),
        values = c(
          "sample" = "black",
          "pop" = boastPalette[3]
        )
      )
  })

  # This is "Finding the Z* Multiplier" Part ----
  rate <- reactiveValues(cover = 0, total = 0)
  observeEvent(input$more, {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })

  observeEvent(c(input$A, input$B, input$n, input$level), {
    rate$cover <- sum(Intervals()$cover)
    rate$total <- nrow(Intervals())
  })

  ## Calculating alpha ----
  zalpha <- reactive({
    (1 - (input$zlevel / 100)) / 2
  })

  zlowerbound <- reactive({
    round(qnorm(zalpha()), digits = 3)
  })

  zupperbound <- reactive({
    round(qnorm(zalpha(), lower.tail = FALSE), digits = 3)
  })

  output$zplot <- renderPlot({
    shadedPortion <- function(fun, alpha) {
      function(x) {
        y <- fun(x)
        y[x <= qnorm(alpha, lower.tail = TRUE) |
          x >= qnorm(alpha, lower.tail = FALSE)] <- NA
        return(y)
      }
    }

    ggplot(
      data = data.frame(x = c(-3, 3)),
      mapping = aes(x = x)
    ) +
      stat_function(fun = dnorm) +
      stat_function(
        fun = shadedPortion(dnorm, zalpha()),
        geom = "area",
        fill = boastUtils::psuPalette[6],
        alpha = 1
      ) +
      geom_segment(
        mapping = aes(
          x = zlowerbound(), y = 0,
          xend = zlowerbound(), yend = dnorm(zlowerbound())
        ),
        color = "black",
        size = 2,
        lineend = "round"
      ) +
      geom_segment(
        mapping = aes(
          x = zupperbound(), y = 0,
          xend = zupperbound(), yend = dnorm(zupperbound())
        ),
        color = "black",
        size = 2,
        lineend = "round"
      ) +
      geom_text(
        mapping = aes(x = zlowerbound(), y = dnorm(zlowerbound())),
        nudge_x = -0.1,
        nudge_y = 0.025,
        label = zlowerbound(),
        size = 8
      ) +
      geom_text(
        mapping = aes(x = zupperbound(), y = dnorm(zupperbound())),
        nudge_x = 0.1,
        nudge_y = 0.025,
        label = zupperbound(),
        size = 8
      ) +
      theme_bw() +
      xlab("Z Score") +
      ylab("Density") +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      ) +
      scale_x_continuous(breaks = seq.int(from = -3, to = 3, by = 1)) +
      scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.01)))
  })

  output$feedback <- renderPrint({
    validate(
      need(
        !is.na(input$question1) & !is.na(input$question2) &
          !is.na(input$question3) & input$question4 != "select",
        message = "Please answer all questions"
      ),
      errorClass = "leftParagraphError"
    )
    if (
      (input$question1 == 1.645 | input$question1 == 1.65 |
        input$question1 == 1.64)
      & (input$question2 == 1.960)
      & (input$question3 == 2.576 | input$question3 == 2.58 |
          input$question3 == 2.6)
      & (input$question4 == "y")) {
      cat("All correct. Great Job!")
    }

    ## Render pic1
    if (input$question1 != "") {
      success <- abs(input$question1) - 1.645 <= 0.005
      output$pic1 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question1 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 90% confidence level?",
        interactionType = "numeric",
        response = input$question1,
        success = success
      )

      boastUtils::storeStatement(session, stmt)
    }

    ## Render pic2
    if (input$question2 != "") {
      success <- abs(input$question2) - 1.960 <= 0.005
      output$pic2 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question2 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 95% confidence level?",
        interactionType = "numeric",
        response = input$question2,
        success = success
      )

      boastUtils::storeStatement(session, stmt)
    }

    ## Render pic3
    if (input$question3 != "") {
      success <- abs(input$question3) - 2.576 <= 0.005
      output$pic3 <- boastUtils::renderIcon(
        icon = ifelse(
          success,
          ifelse(
            input$question3 > 0,
            "correct",
            "partial"
          ),
          "incorrect"
        ),
        width = 36
      )

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "What is Z* Multiplier for 99% confidence level?",
        interactionType = "numeric",
        response = input$question3,
        success = success
      )

      boastUtils::storeStatement(session, stmt)
    }

    ## Render pic4
    if (input$question4 != "select") {
      output$pic4 <- boastUtils::renderIcon(
        icon = ifelse(
          input$question4 == "y",
          "correct",
          "incorrect"
        ),
        width = 36
      )

      ### Store xAPI statement ----
      stmt <- boastUtils::generateStatement(
        session,
        verb = "answered",
        object = "shiny-tab-findz",
        description = "True or False: Increasing the confidence level makes
        the confidence interval wider.",
        interactionType = "choice",
        response = input$question4,
        success = success
      )

      boastUtils::storeStatement(session, stmt)
    }
  })

  # This is "Difference of Means" part
  ## Calculating alpha by the confidence level input
  dalpha <- reactive({
    (1 - (input$dlevel / 100)) / 2
  })

  ## Updating Sample Size
  maleN <- reactive({
    as.integer(input$nSamp)
  })

  femaleN <- reactive({
    as.integer(input$nSamp)
  })

  standardError <- reactive({
    sqrt(118^2 / (maleN()) + 113^2 / (femaleN()))
  })

  ## population mean plot with true diff mean
  output$dpopMean <- renderPlot({
    erwPopPlot
  })

  MaleS <- reactive({
    input$newSample
    rtruncnorm(n = maleN(), a = 200, b = 800, mean = 529, sd = 118)
  })

  FemaleS <- reactive({
    input$newSample
    rtruncnorm(n = femaleN(), a = 200, b = 800, mean = 534, sd = 113)
  })

  Diff <- reactive({
    mean(MaleS()) - mean(FemaleS())
  })

  output$sampleDiff <- renderPlot({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )

    input$newSample
    ## generate new sample
    malesample <- data.frame(sampleGen = MaleS())
    femalesample <- data.frame(sampleGen = FemaleS())

    ## Now, combine your two data frames into one.
    ## First make a new column in each that will be a variable to identify where
    ## they came from later.
    malesample$Gender <- "Male"
    femalesample$Gender <- "Female"

    ## and combine into your new data frame
    sampleTWO <- rbind(femalesample, malesample)

    ## Density plot for sample means
    ggplot(
      data = sampleTWO,
      mapping = aes(sampleGen, fill = Gender)
    ) +
      geom_density(
        mapping = aes(group = rev(Gender)),
        alpha = 0.5
      ) +
      geom_vline(xintercept = mean(MaleS()), color = "dodgerblue", size = 1) +
      geom_vline(xintercept = mean(FemaleS()), color = "hotpink", size = 1) +
      labs(
        title = paste0("Sample Density Graph"),
        x = "Sample mean in blue and pink color",
        y = "Density"
      ) +
      scale_x_continuous(breaks = c(200, 300, 400, 480, 600, 700, 800)) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      ) +
      scale_fill_manual(
        name = "Sex",
        values = c(
          "Male" = "dodgerblue",
          "Female" = "hotpink"
        )
      )
  })

  dlowerbound <- reactive({
    Diff() + qnorm(dalpha()) * standardError()
  })

  dupperbound <- reactive({
    Diff() - qnorm(dalpha()) * standardError()
  })

  pvalue <- reactive({
    2 * (1 - pnorm(abs(zstatistic())))
  })

  zstatistic <- reactive({
    Diff() / standardError()
  })

  output$CTtable <- renderTable({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$CTcheckbox) {
      ctable <- matrix(c(dlowerbound(), dupperbound(), zstatistic(), pvalue()), nrow = 1)
      colnames(ctable) <- c("Lower bound", "Upper bound", "z-statistic", "p-value")
      ctable
    }
  })

  output$sampInfor <- renderUI({
    paste0(
      "The difference in sample means is ",
      round(Diff(), digits = 2),
      ". The value of the sample standard deviation for the difference in means is ",
      round(standardError(), digits = 3)
    )
  })
  zstandard <- reactive({
    -qnorm(dalpha())
  })

  output$decisionZ <- renderText({
    validate(
      need(is.numeric(input$nSamp),
        message = "Please input sample size"
      )
    )
    if (input$decisioncheckbox) {
      if (abs(zstatistic()) <= zstandard()) {
        paste(
          "Since it is observed that |z| = ", abs(round(zstatistic(), 3)), "
              is less than Z* score = ", round(zstandard(), 3), ", and its p-value = ",
          round(pvalue(), 3), " is larger than ", round(2 * dalpha(), 3), ",
              the null hypothesis provides a reasonable explanation of the data
              so we can NOT conclude that males and females have a different average
              SAT ERW score when student's are chosen by the researcher's sampling
              procedure."
        )
      } else {
        paste(
          "Since it is observed that |z| = ", abs(round(zstatistic(), 3)), "
              is larger than Z* score = ", round(zstandard(), 3),
          ", and its p-value = ", round(pvalue(), 3), " is less than ",
          round(2 * dalpha(), 3), ", the null hypothesis is not a reasonable
              explanation of the data so we have evidence that there is a difference
              between the male and female average SAT ERW score when students
              are chosen by the researcher's sampling procedure."
        )
      }
    }
  })
}

# Boast App Call----
boastUtils::boastApp(ui = ui, server = server)
