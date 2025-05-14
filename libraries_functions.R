library(shiny)
library(ggplot2)
library(shinythemes)
library(tibble)
library(tidyr)
library(ggpubr)
library(readxl)
library(scales)
library(shinyWidgets)
library(dipsaus)
library(openxlsx)
library(plotly)


# Source Default values
source("default_values.R")


# --- Helper Functions ---
# Function to create a slider
createSlider <- function(id, label, min, max, value, step, width = NULL) {
  sliderInput(id, label, min = min, max = max, value = value, step = step, width = width)
}

# Function to create conditional slider
createConditionalSlider <- function(id, choice, label, min, max, value, step) {
  conditionalPanel(
    condition = sprintf("input.choice.indexOf('%s') !== -1", choice),
    wellPanel(fluidRow(column(12, createSlider(id, label, min, max, value, step))))
  )
}

# --- Checkbox ---
renderCheckbox <- fluidRow(
  column(
    12,
    checkboxGroupButtons(
      inputId = "choice",
      label = "Select one or more fishery types:",
      choiceNames = list(
        tags$span("Fishery A", style = "font-size: 16px;"),
        tags$span("Fishery B", style = "font-size: 16px;"),
        tags$span("Fishery C", style = "font-size: 16px;"),
        tags$span("Fishery D", style = "font-size: 16px;")
      ),
      choiceValues = list(1, 2, 3, 4),
      justified = TRUE,
      status = "primary",
      selected = 1,
      checkIcon = list(
        yes = tags$i(
          class = "fa fa-check-square",
          style = "color: #12acbc"
        ),
        no = tags$i(
          class = "fa fa-square-o",
          style = "color: #12acbc"
        )
      )
    )
  )
)

###################### Quota herring, cod and other ############################

renderHerring <- wellPanel(
  style = "margin-bottom: 0px;",
  fluidRow(
    column(3, sliderInput("QhA", "Herring quota (%):", min = 0, max = 100, value = params$Quotas_herring$QhA, step = 0.1)),
    column(3, sliderInput("QhB", "Herring quota (%):", min = 0, max = 100, value = params$Quotas_herring$QhB, step = 0.1)),
    column(3, sliderInput("QhC", "Herring quota (%):", min = 0, max = 100, value = params$Quotas_herring$QhC, step = 0.1)),
    column(3, sliderInput("QhD", "Herring quota (%):", min = 0, max = 100, value = params$Quotas_herring$QhD, step = 0.1))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QhA + input.QhB + input.QhC + input.QhD <= 100",
                                h4(strong(textOutput("SumH")), style="color:#12acbc;text-align:center")))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QhA + input.QhB + input.QhC + input.QhD > 100",
                                h4(strong(textOutput("SumH2")), style="color:#bf4f12;text-align:center"),
                                h4(strong("The sum of quotas should be less than 100"), style="text-align:center")))
  )
)


renderHerringOut <- wellPanel(
  style = "margin-top: 0px; background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
  fluidRow(
    column(12,
           fluidRow(
             column(1, tags$img(src = "hering.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Herring quota per business")),
             column(1, tags$img(src = "hering.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Herring quota per business")),
             column(1, tags$img(src = "hering.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Herring quota per business")),
             column(1, tags$img(src = "hering.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Herring quota per business"))
           )
    )
  ),
  fluidRow(
    column(12,
           fluidRow(
             column(1,),
             column(2, style = "text-align:center;", textOutput("HpFA")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("HpFB")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("HpFC")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("HpFD"))
           )
    )
  )
)



renderCod <- wellPanel(
  style = "margin-bottom: 0px;",
  fluidRow(
    column(3, sliderInput("QcA", "Cod quota (%):", min = 0, max = 100, value = params$Quotas_cod$QcA, step = 0.1)),
    column(3, sliderInput("QcB", "Cod quota (%):", min = 0, max = 100, value = params$Quotas_cod$QcB, step = 0.1)),
    column(3, sliderInput("QcC", "Cod quota (%):", min = 0, max = 100, value = params$Quotas_cod$QcC, step = 0.1)),
    column(3, sliderInput("QcD", "Cod quota (%):", min = 0, max = 100, value = params$Quotas_cod$QcD, step = 0.1))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QcA + input.QcB + input.QcC + input.QcD <= 100",
                                h4(strong(textOutput("SumC")), style = "color:#12acbc;text-align:center")))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QcA + input.QcB + input.QcC + input.QcD > 100",
                                h4(strong(textOutput("SumC2")), style = "color:#bf4f12;text-align:center"),
                                h4(strong("The sum of quotas should be less than 100"), style = "text-align:center")))
  )
)


renderCodOut <- wellPanel(
  style = "margin-top: 0px; background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
  fluidRow(
    column(12,
           fluidRow(
             column(1, tags$img(src = "dorsch.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Cod quota per business")),
             column(1, tags$img(src = "dorsch.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Cod quota per business")),
             column(1, tags$img(src = "dorsch.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Cod quota per business")),
             column(1, tags$img(src = "dorsch.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Cod quota per business"))
           )
    )
  ),
  fluidRow(
    column(12,
           fluidRow(
             column(1,),
             column(2, style = "text-align:center;", textOutput("CpFA")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("CpFB")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("CpFC")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("CpFD"))
           )
    )
  )
)



renderOther <- wellPanel(
  style = "margin-bottom: 0px;",
  fluidRow(
    column(3, sliderInput("QoA", "Landings of other species e.g. plaice (%):", min = 0, max = 100, value = params$Quotas_other$QoA, step = 0.1)),
    column(3, sliderInput("QoB", "Landings of other species e.g. plaice (%):", min = 0, max = 100, value = params$Quotas_other$QoB, step = 0.1)),
    column(3, sliderInput("QoC", "Landings of other species e.g. plaice (%):", min = 0, max = 100, value = params$Quotas_other$QoC, step = 0.1)),
    column(3, sliderInput("QoD", "Landings of other species e.g. plaice (%):", min = 0, max = 100, value = params$Quotas_other$QoD, step = 0.1))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QoA + input.QoB + input.QoC + input.QoD <= 100",
                                h4(strong(textOutput("SumO")), style = "color:#12acbc;text-align:center")))
  ),
  fluidRow(
    column(12, conditionalPanel("input.QoA + input.QoB + input.QoC + input.QoD > 100",
                                h4(strong(textOutput("SumO2")), style = "color:#bf4f12;text-align:center"),
                                h4(strong("The sum of landing shares should be less than 100"), style = "text-align:center")))
  )
)


renderOtherOut <- wellPanel(
  style = "margin-top: 0px; background:#e6f0f3; border-left: 5px solid #12acbc; padding: 15px;",
  fluidRow(
    column(12,
           fluidRow(
             column(1, tags$img(src = "andere_arten.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Other species landings per business")),
             column(1, tags$img(src = "andere_arten.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Other species landings per business")),
             column(1, tags$img(src = "andere_arten.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Other species landings per business")),
             column(1, tags$img(src = "andere_arten.png", width = "100%", deleteFile = FALSE, style = "display: block; margin-left: auto; margin-right: auto;")),
             column(2, style = "text-align:center;", strong("Other species landings per business"))
           )
    )
  ),
  fluidRow(
    column(12,
           fluidRow(
             column(1,),
             column(2, style = "text-align:center;", textOutput("OpFA")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("OpFB")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("OpFC")),
             column(1,),
             column(2, style = "text-align:center;", textOutput("OpFD"))
           )
    )
  )
)



################################### Prices #####################################

renderPreiseA <- conditionalPanel("input.choice.indexOf('1') !== -1",
                                  wellPanel(
                                    fluidRow(
                                      column(12,
                                             sliderInput("PhA", "Average price of herring (EUR/kg):", min = 0.1, max = 10.0, value = params$Prices$A$h, step = 0.01),
                                             sliderInput("PcA", "Average price of cod (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$A$c, step = 0.01),
                                             sliderInput("PoA", "Average price of other species e.g. plaice (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$A$o, step = 0.01)
                                      )
                                    )
                                  )
)

renderPreiseB <- conditionalPanel("input.choice.indexOf('2') !== -1",
                                  wellPanel(
                                    fluidRow(
                                      column(12,
                                             sliderInput("PhB", "Average price of herring (EUR/kg):", min = 0.1, max = 10.0, value = params$Prices$B$h, step = 0.01),
                                             sliderInput("PcB", "Average price of cod (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$B$c, step = 0.01),
                                             sliderInput("PoB", "Average price of other species e.g. plaice (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$B$o, step = 0.01)
                                      )
                                    )
                                  )
)

renderPreiseC <- conditionalPanel("input.choice.indexOf('3') !== -1",
                                  wellPanel(
                                    fluidRow(
                                      column(12,
                                             sliderInput("PhC", "Average price of herring (EUR/kg):", min = 0.1, max = 10.0, value = params$Prices$C$h, step = 0.01),
                                             sliderInput("PcC", "Average price of cod (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$C$c, step = 0.01),
                                             sliderInput("PoC", "Average price of other species e.g. plaice (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$C$o, step = 0.01)
                                      )
                                    )
                                  )
)

renderPreiseD <- conditionalPanel("input.choice.indexOf('4') !== -1",
                                  wellPanel(
                                    fluidRow(
                                      column(12,
                                             sliderInput("PhD", "Average price of herring (EUR/kg):", min = 0.1, max = 10.0, value = params$Prices$D$h, step = 0.01),
                                             sliderInput("PcD", "Average price of cod (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$D$c, step = 0.01),
                                             sliderInput("PoD", "Average price of other species e.g. plaice (EUR/kg):", min = 0.1, max = 30.0, value = params$Prices$D$o, step = 0.01)
                                      )
                                    )
                                  )
)


######################## Background Information ################################

renderHintergrund <- wellPanel(
  fluidRow(
    column(4, strong("Parameter")),
    column(8, strong("Description"))
  ),
  hr(),
  fluidRow(
    column(4, strong("Monthly target income")),
    column(8, "The net income per fishery type A, B, C, and D, i.e., the sum of all income sources after taxes and other expenses (e.g., social security contributions) that is intended to be achieved.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Number of fishery operations")),
    column(8, "Total number of fishery operations per described fishery type.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Stock development cod (positive)")),
    column(8, "Fishery management is successfully implemented using the maximum sustainable yield (MSY) approach. Climate change has a minimal impact on stock productivity. Stocks are recovering but do not reach the levels of the 1980s due to changed environmental conditions.",
           br(), em("Total catch: 6,000 tons"))
  ),
  hr(),
  fluidRow(
    column(4, strong("Stock development cod (stagnant)")),
    column(8, "Climate change and/or suboptimal management prevent substantial recovery of stocks. Fishing opportunities stagnate at the 2022 level.",
           br(), em("Total catch: 700 tons"))
  ),
  hr(),
  fluidRow(
    column(4, strong("Stock development herring (positive)")),
    column(8, "Fishery management is successfully implemented using the maximum sustainable yield (MSY) approach. Climate change has a minimal impact on stock productivity. Stocks are recovering but do not reach the levels of the 1980s due to changed environmental conditions.",
           br(), em("Total catch: 50,000 tons"))
  ),
  hr(),
  fluidRow(
    column(4, strong("Stock development herring (stagnant)")),
    column(8, "Climate change and/or suboptimal management prevent substantial recovery of stocks. Fishing opportunities stagnate at the 2022 level.",
           br(), em("Total catch: 6,000 tons"))
  ),
  hr(),
  fluidRow(
    column(4, strong("Price cod")),
    column(8, "Price in € per kg of cod (based on the marketing strategy of a fishery type).")
  ),
  hr(),
  fluidRow(
    column(4, strong("Price herring")),
    column(8, "Price in € per kg of herring (based on the marketing strategy of a fishery type).")
  ),
  hr(),
  fluidRow(
    column(4, strong("Price 'other'")),
    column(8, "Price in € per kg of 'other' fish (based on the marketing strategy of a fishery type). The value is averaged across all species caught and marketed by the respective fishery type, which varies depending on the fishery type and the area in which it operates.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Costs per kg/cod")),
    column(8, "Costs in € per kg of cod caught; including fuel costs, depreciation of fishing vessel, fishing gear and its maintenance, and additional costs specific to fishery types A, B, C, D.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Costs per kg/herring")),
    column(8, "Costs in € per kg of herring caught; including fuel costs, depreciation of fishing vessel, fishing gear and its maintenance, and additional costs specific to fishery types A, B, C, D.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Costs per kg/'other'")),
    column(8, "Costs in € per kg of 'Other' fish caught; including fuel costs, depreciation of fishing vessel, fishing gear and its maintenance, and additional costs specific to fishery types A, B, C, D.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Operational costs")),
    column(8, "All costs incurred in the fishery operation regardless of the catch volume, converted into monthly costs. These include insurances (See-Berufsgenossenschaft (See BG), social security, health insurance, pension insurance), conduct of fishing (mooring, lease of fishing area, lease of hut, net and ship supplies, work clothing, possibly fuel costs, ice, clean boxes), personnel costs, technical equipment (logbook, maintenance of e.g., vessel monitoring system (VMS), radio equipment, life raft), maintenance of fishing vessel (e.g., underwater paint), membership fees (e.g., cooperative, German Fisheries Association, fishing association) and others (e.g., office expenses, car).")
  ),
  hr(),
  fluidRow(
    column(4, strong("Distribution quota cod")),
    column(8, "Relative share of the German quotas for cod (International Council for the Exploration of the Seas (ICES) Subdivisions 22-24) for the respective fishery type A, B, C, and D.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Distribution quota herring")),
    column(8, "Relative share of the German quotas for herring (ICES Subdivisions 20-24) for the respective fishery type A, B, C, and D.")
  ),
  hr(),
  fluidRow(
    column(4, strong("Distribution quota 'other'")),
    column(8, "Relative share of the German quotas for species like plaice and sprat as well as other fish species for the respective fishery type A, B, C, and D.")
  )
)

########################### Define Income Tax ##################################

EST <- function(Ainc) {
  if (Ainc <= 11604) {
    est <- 0
  } else if (Ainc <= 17005) {
    est <- ((922.98 * (Ainc - 11604) / 10000 + 1400) * (Ainc - 11604) / 10000)
  } else if (Ainc <= 66760) {
    est <- ((181.19 * (Ainc - 17005) / 10000 + 2397) * (Ainc - 17005) / 10000 + 1025.38)
  } else if (Ainc <= 227825) {
    est <- (0.42 * Ainc - 10602.13)
  } else {
    est <- (0.45 * Ainc - 18963.88)
  }
  return(est)
}

NEINK <- function(eink, Abs) {
  Ainc <- eink-Abs
  est <- EST(Ainc)
  if(est > 18130) {
    Neink <- eink - est - 0.09 * est - 0.055 * est # Net income = income – Est- KiSt- SolZ
  } else {
    Neink <- eink - est - 0.09 * est  # Net income = income – Est- KiSt
  }
  return(Neink)
}

# Helper function to calculate allowed landings
calculate_landings <- function(Q, HH_or_HC_or_Hother, n) {
  return(Q * HH_or_HC_or_Hother / n)
}

calculate_fishery_income <- function(prices, costs, landings, fixed_cost, n_fishers, depreciation, annual_target_income) {
  # Revenue (Umsatz)
  revenue <- sum(
    prices$Ph * landings$Hh * 1000,
    prices$Pc * landings$Hc * 1000,
    prices$Po * landings$Ho * 1000
  )
  
  # Income before tax (Jahreseinkommen)
  income_before_tax <- sum(
    (prices$Ph - costs$per_kg) * landings$Hh * 1000,
    (prices$Pc - costs$per_kg) * landings$Hc * 1000,
    (prices$Po - costs$per_kg) * landings$Ho * 1000
  ) - fixed_cost
  
  # Net income (Nettoeinkommen)
  net_income <- NEINK(income_before_tax, depreciation)
  
  # Necessary income from other sources
  necessary_other_income <- annual_target_income - net_income
  
  # Percentage from fishery
  percentage_fishery <- net_income / annual_target_income
  
  return(list(
    revenue = revenue,
    income_before_tax = income_before_tax,
    net_income = net_income,
    necessary_other_income = necessary_other_income,
    percentage_fishery = percentage_fishery
  ))
}

