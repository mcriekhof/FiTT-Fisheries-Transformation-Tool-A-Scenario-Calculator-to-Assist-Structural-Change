############################## Scenario Calculator #############################
# Introduction
# The Scenario Calculator allows users to model and analyze different fishery 
# scenarios using predefined and adjustable parameters.

# Default Values
# Default parameters for quotas, costs, prices, and stock developments are set 
# in an external file named `default_values`. These can be customized to fit 
# specific requirements.

# Libraries and Functions
# Core libraries and helper functions required for calculations are stored 
# in the file `libraries_functions`.

# Main File
# The main file is structured into:
# 1. Input Definitions: Collects user inputs and initializes parameters.
# 2. Reactive Logic: Performs calculations and updates dynamically based 
#    on user inputs.
# 3. Outputs: Displays results in text, tables, and interactive plots.

############################ Import functions ##################################

# Source helper functions
source("libraries_functions.R")

############################# Page Setup #######################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("/* Main slider bar color */
    .irs-bar, .irs-bar-edge {
      background: #12acbc !important;  /* teal */
      border-top: 1px solid #12acbc;
      border-bottom: 1px solid #12acbc;}

    /* Handle (knob) color */
    .irs-slider {
      background: #ed7d31 !important;  /* orange */
      border: 1px solid #ed7d31;}

    /* Base line of slider */
    .irs-line {
      background: #cccccc;
      border: 1px solid #aaaaaa;}
    .irs-single, .irs-from, .irs-to {
      background: #12acbc !important;     /* teal background */
      color: white !important;            /* white text */
      border: none !important;
      box-shadow: none !important;
      font-weight: bold;}

    .irs-single:after, .irs-from:after, .irs-to:after {
      border-top-color: #12acbc !important;}

    /* Tick mark color */
    .irs-grid-pol {
      background-color: #666666;}

    /* Tick labels */
    .irs-grid-text {
      color: #444444;
      font-size: 12px;}"))),
  
  theme = shinytheme("flatly"),
  shiny::tags$h2("FiTT Western Baltic Sea Fishery", 
                 style = "text-align: center; color: #61a9ae; font-weight: bold;"),
  hr(),
  navbarPage("",
             
############################ Title Page ########################################
tabPanel("Welcome",
         fluidRow(
           column(
             7,
             tags$div(style = "padding: 20px;",
                      tags$img(src = "map.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 2px 6px rgba(0,0,0,0.2);", deleteFile = FALSE),
                      tags$figcaption(
                        style = "font-style: italic; text-align: center; margin-top: 10px;",
                        "Map of the Western Baltic Sea (Subdivisions 22-24 according to ICES management regions)"))),
           column(5,tags$div(style = "text-align: center; padding: 30px 10px;",
                      HTML("<h2 style='color:#61a9ae; font-weight:bold;'>Welcome</h2>"),
                      HTML("<h3 style='margin-top: -10px;'>to the</h3>"),
                      HTML("<h2 style='font-weight:bold;'>Fisheries Transformation Tool</h2>"),
                      HTML("<h4 style='color: gray; font-style: italic;'>– A Scenario Calculator to assist Structural Change –</h4>")))),
         fluidRow(
           column(
             12,
             hr(),
             wellPanel(
               style = "background-color: #f9f9f9; border-left: 5px solid #12acbc; padding: 20px;",
               HTML("
          <p style='font-size:16px; line-height:1.6;'>
            The scenario tool has been developed for the German Western Baltic Sea fisheries.It enables a quantitative assessment of changes in the ecological, economic and governmental boundary conditions and their effects, especially on fishery income and structure of the fisheries sector.
          </p>
          <p style='font-size:16px; line-height:1.6;'>
            In total, <strong>4 fisheries business types</strong> have been identified based multiple categories (e.g. organisation type). The different scenarios, building on potential fish stock development (positiv or stagnant), focus on the year <strong>2035</strong>.
          </p>
          <p style='font-size:16px; line-height:1.6;'>
            The scenario tool allows to adapt parameters to demonstrate the impact from changing parameters on the economic viability of the fishery. The settings can be made for just one fishery business type or more.
          </p>"))))),


############################### Calculator #####################################
  tabPanel("Calculator",
           wellPanel(
             fluidRow(column(12, strong("Stock development scenarios:"))),
             fluidRow(
               column(6,
                      selectInput("bestand_hering",
                                  "Herring",c("Positive overall development" = params$Stock_development$h_positive,
                                              "Stagnant overall development" = params$Stock_development$h_stagnant)),
                      textOutput("hering"),
                      hr()),
               column(6,
                      selectInput("bestand_cod",
                                  "Cod", c("Positive overall development" = params$Stock_development$c_positive,
                                           "Stagnant overall development" = params$Stock_development$c_stagnant)),
                      textOutput("cod"),
                      hr())),
             fluidRow(
               column(6, span(strong("Possible (German) landing quantity (tons/year):"),
                              textOutput("landing"))),
               column(6, sliderInput("Hother",
                                     "Possible landing quantity other species (total biomass) e.g., plaice (tons/year):", 
                                     min = 0, max = 20000, value = params$Stock_development$other, step = 1)))),
           renderCheckbox,
           fluidRow(
             column(3, createConditionalSlider("nA", "1", "Number of fishery businesses (A):", 1, 500, params$Number_fishers$A, 1)),
             column(3, createConditionalSlider("nB", "2", "Number of fishery businesses (B):", 1, 500, params$Number_fishers$B, 1)),
             column(3, createConditionalSlider("nC", "3", "Number of fishery businesses (C):", 1, 500, params$Number_fishers$C, 1)),
             column(3, createConditionalSlider("nD", "4", "Number of fishery businesses (D):", 1, 500, params$Number_fishers$D, 1))
           ),
           fluidRow(
             column(12, renderHerring)),
           fluidRow(
             column(12, renderHerringOut)),
           fluidRow(
             column(12, renderCod)),
           fluidRow(
             column(12, renderCodOut)),
           fluidRow(
             column(12, renderOther)),
           fluidRow(
             column(12, renderOtherOut)),
           fluidRow(
             column(3, createConditionalSlider("CfA", "1", "Cost per business (EUR/year):", 500, 150000, params$Costs$CfA, 1)),
             column(3, createConditionalSlider("CfB", "2", "Cost per business (EUR/year):", 500, 150000, params$Costs$CfB, 1)),
             column(3, createConditionalSlider("CfC", "3", "Cost per business (EUR/year):", 500, 150000, params$Costs$CfC, 1)),
             column(3, createConditionalSlider("CfD", "4", "Cost per business (EUR/year):", 500, 150000, params$Costs$CfD, 1))
           ),
           fluidRow(
             column(3, createConditionalSlider("CA", "1", "Cost per kg fish (EUR):", 0.1, 15.0, params$Costs$CA, 0.01)),
             column(3, createConditionalSlider("CB", "2", "Cost per kg fish (EUR):", 0.1, 15.0, params$Costs$CB, 0.01)),
             column(3, createConditionalSlider("CC", "3", "Cost per kg fish (EUR):", 0.1, 15.0, params$Costs$CC, 0.01)),
             column(3, createConditionalSlider("CD", "4", "Cost per kg fish (EUR):", 0.1, 15.0, params$Costs$CD, 0.01))
           ),
           fluidRow(
             column(3, renderPreiseA),
             column(3, renderPreiseB),
             column(3, renderPreiseC),
             column(3, renderPreiseD)
           ),
           fluidRow(
             column(3, 
                    conditionalPanel("input.choice.indexOf('1') !== -1",
                                     wellPanel(style = "background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
                                               fluidRow(
                                                 column(12, style = "text-align:center", 
                                                        strong("Monthly revenue per business owner (EUR)"), 
                                                        textOutput("UA"), br(), 
                                                        strong("Monthly income per business owner (EUR after taxes)"), 
                                                        textOutput("MEA")))))),
             column(3, 
                    conditionalPanel("input.choice.indexOf('2') !== -1",
                                     wellPanel(style = "background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
                                               fluidRow(
                                                 column(12, style = "text-align:center", 
                                                        strong("Monthly revenue per business owner (EUR)"), 
                                                        textOutput("UB"), br(), 
                                                        strong("Monthly income per business owner (EUR after taxes)"), 
                                                        textOutput("MEB")))))),
             column(3, 
                    conditionalPanel("input.choice.indexOf('3') !== -1",
                                     wellPanel(style = "background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
                                               fluidRow(
                                                 column(12, style = "text-align:center", 
                                                        strong("Monthly revenue per business owner (EUR)"), 
                                                        textOutput("UC"), br(), 
                                                        strong("Monthly income per business owner (EUR after taxes)"), 
                                                        textOutput("MEC")))))),
             column(3, 
                    conditionalPanel("input.choice.indexOf('4') !== -1",
                                     wellPanel(style = "background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
                                               fluidRow(
                                                 column(12, style = "text-align:center", 
                                                        strong("Monthly revenue per business owner (EUR)"), 
                                                        textOutput("UD"), br(), 
                                                        strong("Monthly income per business owner (EUR after taxes)"), 
                                                        textOutput("MED"))))))),
           fluidRow(
             column(12, 
                    wellPanel(sliderInput("ZL", "Monthly Target Income per Business Owner (EUR after taxes):", 250,  10000, params$Monthly_Target_Income, 1)))),
           wellPanel(style = "background:#2c3e50;",
                     fluidRow(
                       column(12, style="text-align:center; color: #ffffff;", strong("Annual Income per Business Owner")))),
           
############################### Plots ##########################################           
           fluidRow(
             column(6, conditionalPanel("input.choice.indexOf('1') !== -1", plotlyOutput('plotlyA'), br(), br())),
             column(6, conditionalPanel("input.choice.indexOf('2') !== -1", plotlyOutput("plotlyB"), br(), br()))),
           fluidRow(
             column(6, conditionalPanel("input.choice.indexOf('3') !== -1", plotlyOutput('plotlyC'), br(), br())),
             column(6, conditionalPanel("input.choice.indexOf('4') !== -1", plotlyOutput("plotlyD"), br(), br()))),

############################### Note ###########################################
fluidRow(
  column(12,
         wellPanel(
           style = "background:#fbe4d5; border-left: 6px solid #ed7d31; padding: 20px;", 
           fluidRow(
             column(6, ), 
             column(6, h4(HTML("<b>- Note -</b>"), style="color:#ed7d31;text-align:center"))),
           fluidRow(
             column(6, icon("circle", class = "fa-solid fa-circle fa-lg", lib = "font-awesome", style="color:#ed7d31;"), "Annual income per fishery business given the settings"),
             column(6, "If below target income (orange line), additional income sources outside of fishing are necessary"))))),


######################## Download Option #######################################
           tags$head(tags$style(HTML("#download_excel {background-color: #d65a16;     /* darker orange */ color: white; border: none; padding: 10px 20px;
              font-size: 16px; border-radius: 6px;} #download_excel:hover {background-color: #bf4f12;     /* even darker on hover */}"))),
           fluidRow(
              column(12, align = "center", downloadButton("download_excel", "Download Data as Excel Table")))),

############################# Background Information ###########################
tabPanel("Background Information",
         fluidRow(column(12, tableOutput('tableall'))),
         # Add the select box and description output
         fluidRow(column(12,
             selectInput(
               inputId = "select", 
               label = "Description:", 
               choices = c("Type A", "Type B", "Type C", "Type D"),
               selected = "Type A"), # Default selection
             wellPanel(style = "background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
               textOutput("description")))),  # Display the dynamic description
         fluidRow(column(12, renderHintergrund)))
  ),
                           
################################ Copyright note ################################
tags$footer(HTML("<footer>
                  <div class='text-right py-3'>
                  © 2023 Copyright:
                  <a href='https://oceanandsociety.org/de/startseite/'> Center for Ocean and Society</a>
                  </div>
                  </footer>")))


############################## Server ##########################################
server <- function(input, output, session) {

#################### Collect Input values in vector ############################  
  fishery <- reactive({
    # Input parameters
    quotas <- list(
      Qh = c(input$QhA, input$QhB, input$QhC, input$QhD) / 100,
      Qc = c(input$QcA, input$QcB, input$QcC, input$QcD) / 100,
      Qo = c(input$QoA, input$QoB, input$QoC, input$QoD) / 100
    )
    prices <- list(
      Ph = c(input$PhA, input$PhB, input$PhC, input$PhD),
      Pc = c(input$PcA, input$PcB, input$PcC, input$PcD),
      Po = c(input$PoA, input$PoB, input$PoC, input$PoD)
    )
    costs <- list(
      per_kg = c(input$CA, input$CB, input$CC, input$CD),
      fixed = c(input$CfA, input$CfB, input$CfC, input$CfD)
    )
    n_fishers <- c(input$nA, input$nB, input$nC, input$nD)
    stock <- list(
      Herring = as.numeric(input$bestand_hering),
      Cod = as.numeric(input$bestand_cod),
      Other = input$Hother
    )
    annual_target_income <- input$ZL * 12
    HH <- params$Landing_share$Herring * stock$Herring
    HC <- params$Landing_share$Cod * stock$Cod
    
    landings <- list(
      Hh = sapply(1:4, function(i) calculate_landings(quotas$Qh[i], HH, n_fishers[i])),
      Hc = sapply(1:4, function(i) calculate_landings(quotas$Qc[i], HC, n_fishers[i])),
      Ho = sapply(1:4, function(i) calculate_landings(quotas$Qo[i], stock$Other, n_fishers[i]))
    )
    
    # Depreciation per type
    depreciations <- c(params$Depreciation$A, params$Depreciation$B, params$Depreciation$C, params$Depreciation$D)
    
    # Calculate results for each type
    results <- lapply(1:4, function(i) {
      income_results <- calculate_fishery_income(
        list(Ph = prices$Ph[i], Pc = prices$Pc[i], Po = prices$Po[i]),
        list(per_kg = costs$per_kg[i]),
        list(Hh = landings$Hh[i], Hc = landings$Hc[i], Ho = landings$Ho[i]),
        costs$fixed[i],
        n_fishers[i],
        depreciations[i],
        annual_target_income
      )
      
      # Calculate data for plotting income vs number of fishers
      NNs <- seq(min(n_fishers, na.rm = TRUE) * 0.8, max(n_fishers, na.rm = TRUE) * 1.2, by = 1)
      
      # Recalculate landings for varying number of fishers
      Hh_adjusted <- quotas$Qh[i] * HH / NNs
      Hc_adjusted <- quotas$Qc[i] * HC / NNs
      Ho_adjusted <- quotas$Qo[i] * stock$Other / NNs
      
      # Calculate income based on adjusted landings
      eink <- (prices$Ph[i] - costs$per_kg[i]) * Hh_adjusted * 1000 +
        (prices$Pc[i] - costs$per_kg[i]) * Hc_adjusted * 1000 +
        (prices$Po[i] - costs$per_kg[i]) * Ho_adjusted * 1000 - costs$fixed[i]
      
      # Net income
      Neink <- sapply(eink, NEINK, Abs = depreciations[i])
      
      # Create plot data
      plot_data <- data.frame("Einkommen" = Neink, "Anzahl" = NNs)

      
      # Combine results
      c(income_results, list(
        n_fishers = n_fishers[i],
        HH = HH,
        HC = HC,
        IL = annual_target_income,
        plot_data = plot_data
      ))
    })
    
    # Combine landings with results for detailed output
    list(
      type_A = c(results[[1]], landings = list(Hh = landings$Hh[1], Hc = landings$Hc[1], Ho = landings$Ho[1])),
      type_B = c(results[[2]], landings = list(Hh = landings$Hh[2], Hc = landings$Hc[2], Ho = landings$Ho[2])),
      type_C = c(results[[3]], landings = list(Hh = landings$Hh[3], Hc = landings$Hc[3], Ho = landings$Ho[3])),
      type_D = c(results[[4]], landings = list(Hh = landings$Hh[4], Hc = landings$Hc[4], Ho = landings$Ho[4]))
    )
  })
  
  
############################### Total catch #################################### 
output$hering <- renderText({
    paste("Herring: Total catch ", input$bestand_hering, "metric tonnes")})

output$cod <- renderText({
    paste("Cod: Total catch ", input$bestand_cod, "metric tonnes")})
  
########################## Allowable Landings ##################################  
output$landing <- renderText({
    paste0("Herring: ", as.numeric(input$bestand_hering)*params$Landing_share$Herring ,",   ", 
           "Cod: ", as.numeric(input$bestand_cod)*params$Landing_share$Cod ,",   ", 
           "Other: ", input$Hother)
  })
  
######################## Landings per Fishery  #################################  
  output$SumH <- renderText({paste0("The distributed total quota currently stands at ", input$QhA + input$QhB + input$QhC + input$QhD)})
  output$SumH2 <- renderText({paste0("The distributed total quota currently stands at ", input$QhA + input$QhB + input$QhC + input$QhD)})

  output$HpFA <- renderText({paste0(round(fishery()$type_A$landings.Hh, 2), " t/year")})
  output$HpFB <- renderText({paste0(round(fishery()$type_B$landings.Hh, 2), " t/year")})
  output$HpFC <- renderText({paste0(round(fishery()$type_C$landings.Hh, 2), " t/year")})
  output$HpFD <- renderText({paste0(round(fishery()$type_D$landings.Hh, 2), " t/year")})
  
  output$SumC <- renderText({paste0("The distributed total quota currently stands at ", input$QcA + input$QcB + input$QcC + input$QcD)})
  output$SumC2 <- renderText({paste0("The distributed total quota currently stands at ", input$QcA + input$QcB + input$QcC + input$QcD)})
  
  output$CpFA <- renderText({paste0(round(fishery()$type_A$landings.Hc, 2), " t/year")})
  output$CpFB <- renderText({paste0(round(fishery()$type_B$landings.Hc, 2), " t/year")})
  output$CpFC <- renderText({paste0(round(fishery()$type_C$landings.Hc, 2), " t/year")})
  output$CpFD <- renderText({paste0(round(fishery()$type_D$landings.Hc, 2), " t/year")})
  
  output$SumO <- renderText({paste0("The distributed total quota currently stands at ", input$QoA + input$QoB + input$QoC + input$QoD)})
  output$SumO2 <- renderText({paste0("The distributed total quota currently stands at ", input$QoA + input$QoB + input$QoC + input$QoD)})
  
  output$OpFA <- renderText({paste0(round(fishery()$type_A$landings.Ho, 2), " t/year")})
  output$OpFB <- renderText({paste0(round(fishery()$type_B$landings.Ho, 2), " t/year")})
  output$OpFC <- renderText({paste0(round(fishery()$type_C$landings.Ho, 2), " t/year")})
  output$OpFD <- renderText({paste0(round(fishery()$type_D$landings.Ho, 2), " t/year")})
  
  ################################ Revenue #####################################
  
  output$UA <- renderText({paste0(round(fishery()$type_A$revenue / 12, 0), " €")}) 
  output$UB <- renderText({paste0(round(fishery()$type_B$revenue / 12, 0), " €")}) 
  output$UC <- renderText({paste0(round(fishery()$type_C$revenue / 12, 0), " €")}) 
  output$UD <- renderText({paste0(round(fishery()$type_D$revenue / 12, 0), " €")}) 
  
  ######################### Monthly Net Income #################################
  
  output$MEA <- renderText({paste0(round(fishery()$type_A$net_income / 12, 0), " €")}) 
  output$MEB <- renderText({paste0(round(fishery()$type_B$net_income / 12, 0), " €")}) 
  output$MEC <- renderText({paste0(round(fishery()$type_C$net_income / 12, 0), " €")}) 
  output$MED <- renderText({paste0(round(fishery()$type_D$net_income / 12, 0), " €")}) 
  
############################# Plots ###########################################
  observe({
    net_income_values <- c(
      fishery()$type_A$net_income,
      fishery()$type_B$net_income,
      fishery()$type_C$net_income,
      fishery()$type_D$net_income
    )
    
    number_fishers <- c(
      fishery()$type_A$n_fishers,
      fishery()$type_B$n_fishers,
      fishery()$type_C$n_fishers,
      fishery()$type_D$n_fishers
    )
    
    # Compute global min and max with a buffer
    y_min <- ifelse(min(net_income_values, na.rm = TRUE) < 0, 
                    min(net_income_values, na.rm = TRUE) * 1.3,  # Increase by 10% if negative
                    min(net_income_values, na.rm = TRUE) * 0.7)  # Reduce by 10% if positive
    y_max <- ifelse(max(net_income_values, na.rm = TRUE) < 0,
                    max(net_income_values, na.rm = TRUE) * 0.7,
                    max(net_income_values, na.rm = TRUE) * 1.3)       # Increase by 10%
    
    x_min <- min(number_fishers, na.rm = TRUE)*0.8
    x_max <- max(number_fishers, na.rm = TRUE)*1.2
    
    # Define the shared y-axis settings
    shared_yaxis <- list(
      title = "Annual income / Business owner [€]",
      showline = TRUE,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = TRUE,
      linecolor = 'black',
      linewidth = 2,
      ticks = 'outside',
      tickcolor = 'black',
      tickwidth = 2,
      ticklen = 5,
      tickfont = list(family = 'Arial', size = 12, color = 'black'),
      range = c(y_min, y_max)  # Set the fixed range for all plots
    )
    output$plotlyA <- renderPlotly({
      figA <- plot_ly(fishery()$type_A$plot_data, x=~Anzahl, y=~Einkommen,
                      customdata=ifelse(fishery()$type_A$necessary_other_income>0,fishery()$type_A$necessary_other_income,0), # Notwendiges Einkommen aus anderen Quellen
                      type = 'scatter', mode="lines", 
                      line = list(color = 'black', width = 2, dash = 'dash'),
                      hovertemplate = paste('<i>Number of fishery businesses</i>: %{x}', '<br><b>Income</b>: %{y:.0f}€<br><extra></extra>')) %>% 
        layout(title = "<b>Type A</b>",
               xaxis = list(title = "Number of fishery businesses \n(each with 2 permanent employees)",
                            #rangemode="nonnegative",
                            showline = TRUE,
                            showgrid = FALSE,
                            showticklabels = TRUE,
                            linecolor = 'black',
                            linewidth = 2,
                            #autotick = FALSE,
                            ticks = 'outside',
                            tickcolor = 'black',
                            tickwidth = 2,
                            ticklen = 5,
                            tickfont = list(family = 'Arial',
                                            size = 12,
                                            color = 'black'),
                            range = c(x_min, x_max)),
               yaxis = shared_yaxis, 
               showlegend = FALSE, annotations = list(
                 xref = 'paper',
                 yref = "y",
                 x = 1,
                 y = fishery()$type_A$IL,
                 xanchor = 'right',
                 yanchor = 'bottom',
                 text = ~paste('Target income '),
                 font = list(family = 'Arial',
                             size = 16,
                             color = "#ed7d31"),
                 showarrow = FALSE)) %>% 
        add_trace(y = ~fishery()$type_A$IL, name = 'Target income', mode = 'lines', line = list(color = "#ed7d31", width = 3, dash="solid"), hovertemplate = "") %>% 
        add_trace(x = ~fishery()$type_A$n_fishers, y = ~fishery()$type_A$net_income, type = 'scatter', mode = 'lines+markers', marker = list(color = "#ed7d31", size = 20),
                  hovertemplate = paste('<b>Annual income per \nfishery business given \nthe settings: %{y:.0f}€</b>','<br><i>Necessary income \nfrom other sources</i>: %{customdata:.0f}€<extra></extra>'))
    })
    
    output$plotlyB <- renderPlotly({
      figB <- plot_ly(fishery()$type_B$plot_data, x=~Anzahl, y=~Einkommen,
                      customdata=ifelse(fishery()$type_B$necessary_other_income>0,fishery()$type_B$necessary_other_income,0),
                      type = 'scatter', mode="lines", line = list(color = 'black', width = 2, dash = 'dash'),
                      hovertemplate = paste('<i>Number of fishery businesses</i>: %{x}', '<br><b>Income</b>: %{y:.0f}€<br><extra></extra>')) %>% 
        layout(title = "<b>Type B</b>", 
               xaxis = list(title = "Number of fishery businesses \n(each with 1 permanent employee)",
                            #rangemode="nonnegative",
                            showline = TRUE,
                            showgrid = FALSE,
                            showticklabels = TRUE,
                            linecolor = 'black',
                            linewidth = 2,
                            #autotick = FALSE,
                            ticks = 'outside',
                            tickcolor = 'black',
                            tickwidth = 2,
                            ticklen = 5,
                            showarrow=TRUE,
                            tickfont = list(family = 'Arial',
                                            size = 12,
                                            color = 'black'),
                            range = c(x_min, x_max)),
               yaxis = shared_yaxis,  
               showlegend = FALSE, 
               annotations = list(
                 xref = 'paper',
                 yref = "y",
                 x = 1,
                 y = fishery()$type_B$IL,
                 xanchor = 'right',
                 yanchor = 'bottom',
                 text = ~paste('Target income '),
                 font = list(family = 'Arial',
                             size = 16,
                             color = "#ed7d31"),
                 showarrow = FALSE)) %>% 
        add_trace(y = ~fishery()$type_B$IL, name = 'Target income', mode = 'lines', line = list(color = "#ed7d31", width = 3, dash="solid"), hovertemplate = "") %>% 
        add_trace(x = ~fishery()$type_B$n_fishers, y = ~fishery()$type_B$net_income, type = 'scatter', mode = 'lines+markers', marker = list(color = "#ed7d31", size = 20),
                  hovertemplate = paste('<b>Annual income per \nfishery business given \nthe settings: %{y:.0f}€</b>','<br><i>Necessary income \nfrom other sources</i>: %{customdata:.0f}€<extra></extra>'))
    })
    
    
    output$plotlyC <- renderPlotly({
      figC <- plot_ly(fishery()$type_C$plot_data, x=~Anzahl, y=~Einkommen,
                      customdata=ifelse(fishery()$type_C$necessary_other_income>0,fishery()$type_C$necessary_other_income,0),
                      type = 'scatter', mode="lines", line = list(color = 'black', width = 2, dash = 'dash'),
                      hovertemplate = paste('<i>Number of fishery businesses</i>: %{x}', '<br><b>Income</b>: %{y:.0f}€<br><extra></extra>')) %>% 
        layout(title = "<b>Type C</b>", 
               xaxis = list(title = "Number of fishery businesses \n(each with 1 seasonal employee)",
                            #rangemode="nonnegative",
                            showline = TRUE,
                            showgrid = FALSE,
                            showticklabels = TRUE,
                            linecolor = 'black',
                            linewidth = 2,
                            #autotick = FALSE,
                            ticks = 'outside',
                            tickcolor = 'black',
                            tickwidth = 2,
                            ticklen = 5,
                            showarrow=TRUE,
                            tickfont = list(family = 'Arial',
                                            size = 12,
                                            color = 'black'),
                            range = c(x_min, x_max)),
               yaxis = shared_yaxis,  
               showlegend = FALSE, 
               annotations = list(
                 xref = 'paper',
                 yref = "y",
                 x = 1,
                 y = fishery()$type_C$IL,
                 xanchor = 'right',
                 yanchor = 'bottom',
                 text = ~paste('Target income '),
                 font = list(family = 'Arial',
                             size = 16,
                             color = "#ed7d31"),
                 showarrow = FALSE)) %>% 
        add_trace(y = ~fishery()$type_C$IL, name = 'Target income', mode = 'lines', line = list(color = "#ed7d31", width = 3, dash="solid"), hovertemplate = "") %>% 
        add_trace(x = ~fishery()$type_C$n_fishers, y = ~fishery()$type_C$net_income, type = 'scatter', mode = 'lines+markers', marker = list(color = "#ed7d31", size = 20),
                  hovertemplate = paste('<b>Annual income per \nfishery business given \nthe settings: %{y:.0f}€</b>','<br><i>Necessary income \nfrom other sources</i>: %{customdata:.0f}€<extra></extra>'))
    })
    
    output$plotlyD <- renderPlotly({
      figD <- plot_ly(fishery()$type_D$plot_data, x=~Anzahl, y=~Einkommen,
                      customdata=ifelse(fishery()$type_D$necessary_other_income>0,fishery()$type_D$necessary_other_income,0),
                      type = 'scatter', mode="lines", line = list(color = 'black', width = 2, dash = 'dash'),
                      hovertemplate = paste('<i>Number of fishery businesses</i>: %{x}', '<br><b>Income</b>: %{y:.0f}€<br><extra></extra>')) %>% 
        layout(title = "<b>Type D</b>", 
               xaxis = list(title = "Number of fishery businesses \n(no employees)",
                            #rangemode="nonnegative",
                            showline = TRUE,
                            showgrid = FALSE,
                            showticklabels = TRUE,
                            linecolor = 'black',
                            linewidth = 2,
                            #autotick = FALSE,
                            ticks = 'outside',
                            tickcolor = 'black',
                            tickwidth = 2,
                            ticklen = 5,
                            showarrow=TRUE,
                            tickfont = list(family = 'Arial',
                                            size = 12,
                                            color = 'black'),
                            range = c(x_min, x_max)),
               yaxis = shared_yaxis,  
               showlegend = FALSE, 
               annotations = list(
                 xref = 'paper',
                 yref = "y",
                 x = 1,
                 y = fishery()$type_D$IL,
                 xanchor = 'right',
                 yanchor = 'bottom',
                 text = ~paste('Target income '),
                 font = list(family = 'Arial',
                             size = 16,
                             color = "#ed7d31"),
                 showarrow = FALSE)) %>% 
        add_trace(y = ~fishery()$type_D$IL, name = 'Target income', mode = 'lines', line = list(color = "#ed7d31", width = 3, dash="solid"), hovertemplate = "") %>% 
        add_trace(x = ~fishery()$type_D$n_fishers, y = ~fishery()$type_D$net_income, type = 'scatter', mode = 'lines+markers', marker = list(color = "#ed7d31", size = 20),
                  hovertemplate = paste('<b>Annual income per \nfishery business given \nthe settings: %{y:.0f}€</b>','<br><i>Necessary income \nfrom other sources</i>: %{customdata:.0f}€<extra></extra>'))
    })
  })
  
  
####################### Overview Table: Fishery Description ####################
  output$tableall <- renderTable(
    data.frame("Category" = c("Occupational Form", "Operational Structure", "Fishing Gear", "Vessel","Target Fish Species", "Mooring Place", "Marketing to Producer Organization","Direct Marketing", "Income from Fisheries"),
               "Type A"= c("Main Occupation / Organized", "2 Permanent employees", "100% Trawl Net", "1 Vessel, > 12 m","Cod, Herring, Sprat, various flatfish species, Pollock, Whiting, Mackerel", "1 Mooring place", "100%", "0%", "100%"),
               "Type B"= c("Main Occupation / Organized", "1 Permanent employee", "50% Gillnet & 50% Trawl Net", "2 Vessels, 8m < 12 m, <8 m","Cod, Herring, Sprat, various flatfish species, Pollock, Whiting, Mackerel", "2 Mooring places", "70%", "30%", "100%"),
               "Type C"= c("Main Occupation / Unorganized", "1 Seasonal employee on a €450 basis (May-September)", "85% Gillnet & 15% Basket Trap", "1 Vessel, 8m < 12 m","Cod, Herring, Eel, various flatfish species, Garfish, Salmon, Sea Trout, Mackerel", "1 Mooring place", "0%", "100%", "65%"),
               "Type D"= c("Secondary Occupation / Unorganized", "No employees", "75% Gillnet & 25% Basket Trap", "1 Vessel, <8 m","Cod, Herring, Eel, various flatfish species, Garfish, Salmon, Sea Trout", "1 Mooring place", "0%", "100%", "40%"),
               check.names = FALSE),
    colnames = TRUE,
    align = 'r',
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE
  )
  
  # Render the description dynamically based on the selected type
  output$description <- renderText({
    descriptions <- list(
      "Type A" = "This fishery is the main source of income for this operation, generating 100% of its income from fishing. The fishery operates within both a fishery cooperative and a producer organization, incurring annual membership fees. Fishery Type A markets 100% of its catch to the producer organization based on its quota shares, resulting in lower income per kilogram of fish compared to Types B, C, and D. The operation utilizes a fishing vessel categorized as >12 meters, requiring two additional employees due to its size. Personnel costs are a significant expense in this operation. Additionally, compared to Types B, C, and D, extra equipment such as a vessel monitoring system (VMS) must be carried on board, incurring maintenance costs. Only trawling nets are used for fishing due to vessel size restrictions, including a prohibition on operating in coastal waters (up to 3 nautical miles). Trawling requires a significantly higher fuel consumption. The fishery has a berth, but its length results in higher monthly costs compared to other fishery types.",
      "Type B" = "This fishery is the main source of income for this operation, generating 100% of its income from fishing. The fishery operates within both a fishery cooperative and a producer organization, incurring annual membership fees. According to the statute, this requires the fishery to deliver 70% of the catch to the producer organization. Only 30% can be marketed directly to, for example, restaurants, allowing the operation to achieve a higher income per kilogram of fish. By directly marketing fish, Fishery Type B contributes to the revitalization of an active harbor, thereby enhancing the cultural value of the fishery. Overall, the fishery has two fishing vessels (2 berths), one in the 8m < 12m segment (trawling net, set net) and another < 8m (exclusively set net). Both vessels operate with 50% trawling net and 50% set net; the latter allows for more selective fishing. The operation requires 2 berths for the fishing vessels, incurring higher costs compared to Types A, C, and D. There is an additional person employed year-round in the operation; this represents a major cost factor in the fishery.",
      "Type C" = "This fishery operation is registered as the main source of income, not organized within a fishery cooperative or producer organization, hence no annual membership fees and 100% direct marketing is possible. Not only does this fishery operation earn the most per kilogram of fish caught, it also fosters an active harbor life through direct sales of fish from the boat, enhancing the cultural value of the fishery. Fishery Type C owns a fishing vessel ranging from 8m < 12m in length; it exclusively uses passive fishing gear for catching fish (85% set net, 15% trap net), enabling selective fishing practices. Overall, 65% of the total income comes from fishing; the remaining 35% comes from activities such as room rentals and sightseeing fishing tours, employing a part-time worker on a €450 per month basis from May to September for the latter.",
      "Type D" = "This fishery operation is registered as a supplementary income business, not organized within a fishery cooperative or producer organization, hence no annual membership fees and 100% direct marketing is possible. This allows the operation to sell its caught fish at higher prices. The fishing is exclusively done using passive fishing gear, with 75% using set nets and the remaining 25% using trap nets. Overall, this fishery operation generates 40% of its income from fishing; the remaining 60% comes from another primary occupation. Unlike Types A, B, and C, there are no additional employees in this operation, which reduces a significant cost factor.")
    descriptions[[input$select]] # Fetch the description for the selected type
  })
  
###################  Preparation Export ######################################
  
  SzenarioH <- reactive({
    i <- ifelse(as.numeric(input$bestand_hering) == 50000, "Positive", "Stagnant") 
    return(i)
  })
  
  SzenarioC <- reactive({
    y <- ifelse(as.numeric(input$bestand_cod) == 6000, "Positive", "Stagnant") 
    return(y)
  })
  
############################## Export ##########################################  
  
  output$download_excel <- downloadHandler(
    filename = function() {
      "Scenario_Calculator.xlsx"
    },
    content = function(file) {
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Data"
      )
      
      setColWidths(
        my_workbook,
        1,
        cols = 1:5,
        widths = c(55, 10, 10, 10,10)
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("Calculator for the Structure of German Coastal Fisheries 2035"),
        startRow = 1,
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 16,
          textDecoration = "bold"
        ),
        rows = 1,
        cols = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("Center for Ocean and Society"),
        startRow = 2,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("Date:"),
        startRow = 2,
        startCol = 4
      )
      
      formatted_date <- format(Sys.Date(), "%d.%m.%Y")
      writeData(
        my_workbook,
        sheet = 1,
        c(formatted_date),
        startRow = 2,
        startCol = 5
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("herring"),
        startRow = 5,
        startCol = 2
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("cod"),
        startRow = 5,
        startCol = 3
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("other"),
        startRow = 5,
        startCol = 4
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          textDecoration = "bold",
          fgFill = "#ecf0f1",
          halign = "center"
        ),
        rows = 5,
        cols = 1:4,
        gridExpand = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        colNames = FALSE,
        data.frame("Name"=c("stock development scenario", "total catch (t)", "possible landing amount (t/y)"),
                   "herring"=c(SzenarioH(), as.numeric(input$bestand_hering), fishery()$type_A$HH), # fishery()[19, 16] = BH, HH
                   "cod"=c(SzenarioC(), as.numeric(input$bestand_cod), fishery()$type_A$HC), # fishery()[20, 17] = BC, HC
                   "other"=c(NA, NA, input$Hother)),
        startRow = 6,
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#ecf0f1"
        ),
        rows = 6:8,
        cols = 1,
        gridExpand = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery A"),
        startRow = 11,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery B"),
        startRow = 11,
        startCol = 3
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery C"),
        startRow = 11,
        startCol = 4
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery D"),
        startRow = 11,
        startCol = 5
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          textDecoration = "bold",
          fgFill = "#ecf0f1",
          halign = "center"
        ),
        rows = 11,
        cols = 1:5,
        gridExpand = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        colNames = FALSE,
        data.frame(
          Name = c(
            "number of fishery business",
            "quota herring (%)",
            "quota cod (%)",
            "landed quantity of other species, e.g., plaice (%)",
            "quota herring per business (kg/y)",
            "quota cod per business (kg/y)",
            "landed quantity of other species, e.g., plaice per business (kg/y)",
            "costs per business (EUR/y)",
            "cost per fish (EUR/kg)",
            "average price of herring (EUR/kg)",
            "average price of cod (EUR/kg)",
            "average price of other species (EUR/kg)",
            "monthly revenue (EUR)",
            "monthly tax (EUR)",
            "monthly income per business (EUR after taxes)",
            "monthly target income per business (EUR after taxes)"
          ),
          "fishery A" = c(input$nA,
                          input$QhA,
                          input$QcA,
                          input$QoA,
                          round(fishery()$type_A$landings.Hh*1000,2),
                          round(fishery()$type_A$landings.Hc*1000,2),
                          round(fishery()$type_A$landings.Ho*1000,2),
                          input$CfA,
                          input$CA,
                          input$PhA,
                          input$PcA,
                          input$PoA,
                          round(fishery()$type_A$revenue/12,0),
                          round((fishery()$type_A$income_before_tax-fishery()$type_A$net_income)/12,2),
                          round(fishery()$type_A$net_income/12,0),
                          input$ZL), # fishery()[21] = ZL
          "fishery B" = c(input$nB,
                          input$QhB,
                          input$QcB,
                          input$QoB,
                          round(fishery()$type_B$landings.Hh*1000,2),
                          round(fishery()$type_B$landings.Hc*1000,2),
                          round(fishery()$type_B$landings.Ho*1000,2),
                          input$CfB,
                          input$CB,
                          input$PhB,
                          input$PcB,
                          input$PoB,
                          round(fishery()$type_B$revenue/12,0),
                          round((fishery()$type_B$income_before_tax-fishery()$type_B$net_income)/12,2),
                          round(fishery()$type_B$net_income/12,0),
                          input$ZL), # fishery()[21] = ZL
          "fishery C" = c(input$nC,
                          input$QhC,
                          input$QcC,
                          input$QoC,
                          round(fishery()$type_C$landings.Hh*1000,2),
                          round(fishery()$type_C$landings.Hc*1000,2),
                          round(fishery()$type_C$landings.Ho*1000,2),
                          input$CfC,
                          input$CC,
                          input$PhC,
                          input$PcC,
                          input$PoC,
                          round(fishery()$type_C$revenue/12,0),
                          round((fishery()$type_C$income_before_tax-fishery()$type_C$net_income)/12,2),
                          round(fishery()$type_C$net_income/12,0),
                          input$ZL), # fishery()[21] = ZL
          "fishery D" = c(input$nD,
                          input$QhD,
                          input$QcD,
                          input$QoD,
                          round(fishery()$type_D$landings.Hh*1000,2),
                          round(fishery()$type_D$landings.Hc*1000,2),
                          round(fishery()$type_D$landings.Ho*1000,2),
                          input$CfD,
                          input$CD,
                          input$PhD,
                          input$PcD,
                          input$PoD,
                          round(fishery()$type_D$revenue/12,0),
                          round((fishery()$type_D$income_before_tax-fishery()$type_D$net_income)/12,2),
                          round(fishery()$type_D$net_income/12,0),
                          input$ZL)), # fishery()[21] = ZL
        startRow = 12,
        startCol = 1
      )
      
      
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#ecf0f1"
        ),
        rows = 12:15,
        cols = 1,
        gridExpand = TRUE
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontColour = "white",
          fgFill = "#2c3e50"
        ),
        rows = 16:18,
        cols = 1:5,
        gridExpand = TRUE
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#ecf0f1"
        ),
        rows = 19:23,
        cols = 1,
        gridExpand = TRUE
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontColour = "white",
          fgFill = "#2c3e50"
        ),
        rows = 24:26,
        cols = 1:5,
        gridExpand = TRUE
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#ecf0f1"
        ),
        rows = 27,
        cols = 1,
        gridExpand = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery A"),
        startRow = 29,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery B"),
        startRow = 29,
        startCol = 3
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery C"),
        startRow = 29,
        startCol = 4
      )
      writeData(
        my_workbook,
        sheet = 1,
        c("fishery D"),
        startRow = 29,
        startCol = 5
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          #fontColour = "white",
          textDecoration = "bold",
          fgFill = "#ecf0f1",
          halign = "center"
        ),
        rows = 29,
        cols = 1:5,
        gridExpand = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        colNames = FALSE,
        data.frame(
          Name = c("net annual income per business",
                   "necessary income from other sources",
                   "share of income from fishing (%)"),
          "fishery A" = c(round(fishery()$type_A$net_income,2),                          
                          ifelse(fishery()$type_A$necessary_other_income>0,round(fishery()$type_A$necessary_other_income,2),0),  
                          round(fishery()$type_A$percentage_fishery*100,2)),           
          "fishery B" = c(round(fishery()$type_B$net_income,2),                          
                          ifelse(fishery()$type_B$necessary_other_income>0,round(fishery()$type_B$necessary_other_income,2),0),  
                          round(fishery()$type_B$percentage_fishery*100,2)),
          "fishery C" = c(round(fishery()$type_C$net_income,2),                          
                          ifelse(fishery()$type_C$necessary_other_income>0,round(fishery()$type_C$necessary_other_income,2),0),  
                          round(fishery()$type_C$percentage_fishery*100,2)),
          "fishery D" = c(round(fishery()$type_D$net_income,2),                          
                          ifelse(fishery()$type_D$necessary_other_income>0,round(fishery()$type_D$necessary_other_income,2),0),  
                          round(fishery()$type_D$percentage_fishery*100,2))),
        startRow = 30,
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontColour = "white",
          fgFill = "#2c3e50"
        ),
        rows = 30:32,
        cols = 1:5,
        gridExpand = TRUE
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
}




# Run the application 
shinyApp(ui = ui, server = server)
