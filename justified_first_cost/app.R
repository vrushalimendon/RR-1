#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Justified First Cost"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("total_increment_cost",
                     "Total Increment Cost:",
                     min = 1,
                     max = 5000,
                     value = 30),
           sliderInput("avoided_cost_carbon",
                       "Avoided Cost of Carbon:",
                       min = 1,
                       max = 500,
                       value = 30),
         sliderInput("EUL",
                     "Economic Useful Life:",
                     min = 1,
                     max = 100,
                     value = 30)
      ),

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     calculate_LCC_savings <- function(total_increment_cost, avoided_cost_carbon, EUL){
       
       #variables
       mortgage_interest_rate <- 0.04 #4%
       loan_term <- 30 #years
       down_payment_rate <- 0.2 #20%
       points_and_loan_fees <- 0.005 #.5%
       discount_rate <- 0.04 #4%
       property_tax_rate <- .0165  #1.7%
       income_tax_rate <- .2133 #21.3%
       home_price_escalation_rate <- 0.019 #1.9%
       inflation_rate <- 0.019 #1.9%
       energy_escalation_rate <- 0.006 #0.06%
       
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
         
         energy_savings <- -1 * avoided_cost_carbon * (1 + energy_escalation_rate)^year 
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
         labs(y = "Total Savings (-) Versus Cost (+) ($)", x = "Year", fill = NULL, title = "Cumulative Life Cycle Cost (LCC) Savings")+
         scale_fill_manual(values = c("#FFA3A3", "#FFBF75", "#FFE299", "#89CEEB", "#97E2C0"))
       
     }
     
     calculate_LCC_savings(input$total_increment_cost, input$avoided_cost_carbon, input$EUL)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

