#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Justified First Cost"),
   fluidRow(
     
     column(3,
            
            wellPanel(

              
              sliderInput("total_increment_cost",
                          "Total Increment Cost ($ per Dwelling Unit):",
                          min = 0,
                          max = 10000,
                          value = 400),
              sliderInput("discount_rate",
                          "Discount Rate (%):",
                          min = 0,
                          max = 30,
                          value = 4),
              sliderInput("EUL",
                          "Economic Useful Life (Years):",
                          min = 0,
                          max = 100,
                          value = 30)
            ),
            wellPanel(
              sliderInput("energy_price_elec",
                          "Energy Price - Electricity ($/kBtu):",
                          min = 0,
                          max = 800,
                          value = 30),
              sliderInput("energy_escalation_rate_elec",
                          "Energy Escalation Rate - Electricity (%):",
                          min = 0,
                          max = 30,
                          value = 0.6)
            ),
            wellPanel(
              sliderInput("energy_price_ff",
                          "Energy Price - Fossil Fuels ($/kBtu):",
                          min = 0,
                          max = 800,
                          value = 0),
              sliderInput("energy_escalation_rate_ff",
                          "Energy Escalation Rate - Fossil Fuels (%):",
                          min = 0,
                          max = 30,
                          value = 0.6),
              checkboxInput("avoided_cost_carbon",
                            "Factor in Avoided Cost of Carbon")
            )
     ),
     
     # beginning of right side
     mainPanel(plotOutput("distPlot"),
               tags$br(),
               p("This plot shows the cumulative yearly Life Cycle Cost (LCC) savings of a given energy-saving mechanism to help determine the justified first cost. With most measures it is ideal for the LCC savings to equal zero either at the end of the mechanism’s Expected Useful Life (EUL) or at the end of the building’s mortgage (assumed here to be 30 years, marked by the dotted vertical line) -- so you may want the solid black line to fall at zero at the end of the plot."),
               tags$br(),
               p("LCC (the solid black line) is calculated as the cumulative sum of yearly energy savings (in green), yearly tax deduction (in blue), and yearly property tax (in orange), all respectively divided by the sum of one and the annual discount rate and raised to the power of the number of years passed, and initial cost (in yellow) and yearly mortgage loan payment (in red). Once the mortgage term of 30 years has passed, the loan payment and tax deduction are no longer included in the equation."),
               tags$br(),
               p("Resources:"),
               p(a(href = "[https://www.energypolicy.columbia.edu/research/report/levelized-cost-carbon-abatement-improved-cost-assessment-methodology-net-zero-emissions-world]", "Levelized Cost of Carbon")),
               p(a(href = "[https://news.climate.columbia.edu/2021/04/01/social-cost-of-carbon/]", "Social Cost of Carbon")),
               p(a(href = "[https://news.stanford.edu/2021/06/07/professors-explain-social-cost-carbon/ ]", "Social Cost of Carbon")),
               p(a(href = "[https://database.aceee.org/state/carbon-pricing]", "ACEEE Carbon Pricing")),
               p(a(href = "[https://www.wbdg.org/resources/life-cycle-cost-analysis-lcca]", "Life Cycle Cost Analysis"))
               #column end
     
   )
   
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     calculate_LCC_savings <- function(total_increment_cost, energy_price_elec, energy_price_ff, EUL, energy_escalation_rate_elec, energy_escalation_rate_ff, discount_rate){ #4%){#0.06%){
       
       #variables
       mortgage_interest_rate <- 0.04 #4%
       loan_term <- 30 #years
       down_payment_rate <- 0.2 #20%
       points_and_loan_fees <- 0.005 #.5%
       property_tax_rate <- .0165  #1.7%
       income_tax_rate <- .2133 #21.3%
       home_price_escalation_rate <- 0.019 #1.9%
       inflation_rate <- 0.019 #1.9%
       
       mortgage_payment_multiplier <- ((( 1 - down_payment_rate) * 12)/
                                         ((((1 + (mortgage_interest_rate/12))^12 * loan_term) - 1)/
                                            (mortgage_interest_rate/12 * (1 + (mortgage_interest_rate/12))^12 * loan_term)))
       
       
       
       mortgage_payment <- total_increment_cost * mortgage_payment_multiplier
       
       
       initial_cost <- (down_payment_rate * total_increment_cost) +
         (points_and_loan_fees * total_increment_cost * (1 - down_payment_rate))
       
       LCC_savings <- initial_cost
       
       mortgage_sum <- 0
       property_tax_sum <- 0
       energy_savings_sum <- 0
       tax_deduction_sum <- 0
       
       LCC_savings_vec <- c(LCC_savings)
       LCC_savings_data <- data.frame(year = 0, component = "Initial Cost", value = LCC_savings)
       
       
       length <- EUL#(if(EUL < loan_term) (EUL) else loan_term) 
       
       for(i in 1:length){
         year <- i
         
         mortgage_sum <- (if(year <= loan_term)(mortgage_payment*year)else(mortgage_payment*loan_term))
         
         property_tax <- property_tax_rate * total_increment_cost * 
           (1 + home_price_escalation_rate)^year
         property_tax_sum <- property_tax_sum + (property_tax)/(1 + discount_rate)^year
         
         energy_savings <- -1 * ((energy_price_elec * (1 + energy_escalation_rate_elec)^year) + (energy_price_ff * (1 + energy_escalation_rate_ff)^year))
         energy_savings_sum <- energy_savings_sum + (energy_savings)/(1 + discount_rate)^year
         
         tax_deduction <- -1 * (income_tax_rate * 
                                  (property_tax_rate + mortgage_payment * mortgage_interest_rate) * 
                                  ((1 + mortgage_interest_rate)^(loan_term - year + 1)-1)/
                                  (mortgage_interest_rate * (1 + mortgage_interest_rate)^(loan_term - year + 1)))
         tax_deduction_sum <- tax_deduction_sum + if(year <= loan_term)(tax_deduction/(1 + discount_rate)^year)else(0)
         
         LCC_savings <- LCC_savings + 
           (if(year <= loan_term)(mortgage_payment)else(0)) + 
           (property_tax + energy_savings + (if(year <= loan_term)(tax_deduction)else(0)))/
           (1 + discount_rate)^year
         
         LCC_savings_data <- rbind(LCC_savings_data, 
                                   c(year, "Initial Cost", initial_cost),
                                   c(year, "Mortgage Payment", mortgage_sum),
                                   c(year, "Property Tax", property_tax_sum),
                                   c(year, "Energy Savings", energy_savings_sum),
                                   c(year, "Tax Deduction", tax_deduction_sum)
         )
         
         LCC_savings_vec <- c(LCC_savings_vec,  LCC_savings)
         
       }
       
       savings_data <- data.frame(year = 0:length, LCC_savings = LCC_savings_vec)
       
       print(LCC_savings)
       
       LCC_savings_data$year <- as.numeric(LCC_savings_data$year)
       LCC_savings_data$value <- as.numeric(LCC_savings_data$value)
       LCC_savings_data$component <- factor(LCC_savings_data$component, levels = c("Mortgage Payment", "Property Tax", "Initial Cost", "Tax Deduction", "Energy Savings"))
       
       ggplot() + 
         geom_col(data = LCC_savings_data, aes(x=year, y=value, fill=component))+
         geom_line(data = savings_data, aes(x = year, y = LCC_savings), size = 2) +
         geom_vline(xintercept = 30, linetype = 2) +
         theme_gray(base_size = 14)+
         labs(y = "Total Savings (-) Versus Cost (+) ($)", x = "Year", fill = NULL, title = "Cumulative Life Cycle Cost (LCC) Savings")+
         scale_fill_manual(values = c("#FFA3A3", "#FFBF75", "#FFE299", "#89CEEB", "#97E2C0"))
       
     }
     
     calculate_LCC_savings(input$total_increment_cost, input$energy_price_elec, input$energy_price_ff, input$EUL, input$energy_escalation_rate_elec/100, input$energy_escalation_rate_ff/100, input$discount_rate/100)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

