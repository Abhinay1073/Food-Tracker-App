library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)



# Food Data
food_data <- tibble(
  Food = c("🍎 Apple", "🍚 Rice", "🍗 Chicken", "🥛 Milk", "🥚 Egg", "🍞 Roti",
           "🍉 Watermelon", "🍇 Grapes", "🍊 Orange", "🍓 Strawberry", "🥦 Broccoli", "🥑 Avocado",
           "🥩 Mutton", "🐟 Fish", "🍤 Prawns", "🍗 Fried Chicken", "🍖 Lamb", "🦀 Crab",
           "🫓 Chapathi", "🧈 Ghee", "🍛 Dhalan"),
  Calories = c(95, 130, 240, 120, 70, 100, 30, 62, 50, 33, 55, 160, 294, 206, 144, 320, 250, 97,
               110, 90, 180),
  Protein = c(0.5, 2.7, 27, 8, 6, 3, 0.6, 0.6, 1, 0.7, 3.7, 2, 25, 22, 17, 31, 26, 20,
              3.5, 0.1, 7),
  MealTime = c("Snack / Breakfast", "Lunch / Dinner", "Lunch / Dinner", "Breakfast / Snack", "Breakfast / Lunch", "Lunch / Dinner",
               "Snack", "Snack", "Breakfast / Snack", "Snack", "Lunch / Dinner", "Breakfast / Snack",
               "Lunch / Dinner", "Lunch / Dinner", "Lunch / Dinner", "Dinner", "Lunch / Dinner", "Lunch / Dinner",
               "Lunch / Dinner", "Lunch / Dinner", "Lunch / Dinner")
)

# Weekly Menu
menu_data <- tibble(
  Day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
  Breakfast = c("Milk, Egg", "Apple, Roti", "Milk, Orange", "Avocado, Roti", "Milk, Strawberry", "Milk, Grapes", "Avocado, Orange"),
  Lunch = c("Rice, Chicken", "Roti, Fish", "Rice, Prawns", "Roti, Mutton", "Rice, Lamb", "Roti, Crab", "Rice, Chicken"),
  Dinner = c("Broccoli, Egg", "Chicken, Rice", "Fried Chicken, Roti", "Fish, Broccoli", "Crab, Rice", "Mutton, Avocado", "Lamb, Rice")
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      style = "width: 100%; text-align: center;",
      tags$span("🥗 Food", style = "font-size: 32px; font-weight: bold; color: #FF5722;")
    ),
    tags$li(class = "dropdown", textOutput("current_time"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("🗓️ Weekly Menu", tabName = "weeklymenu"),
      menuItem("➕ Add Meal", tabName = "add"),
      menuItem("📊 Summary", tabName = "summary"),
      menuItem("📈 Compare Foods", tabName = "compare")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .quote-text {text-align:center; background-color: #e0ffe0; padding: 10px; border-radius: 8px;}
      .quote-heading{text-align:center; background-color: #d0f0c0; padding: 10px; border-radius: 8px;}
      .remaining-info {font-weight:bold; font-size: 16px; padding: 10px; color: #145A32; background-color: #e6f9f0; border-radius: 8px; margin-top: 10px;}
      body, .content-wrapper { background-color: #f0f9ff !important; }
    "))),
    tabItems(
      tabItem(tabName = "weeklymenu",
              box(title = "📅 Weekly Food Menu", width = 12, solidHeader = TRUE, status = "primary",
                  DTOutput("weekly_menu_table"))
      ),
      tabItem(tabName = "add",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "info",
                    div(class = "quote-box",
                        div(class = "quote-heading", "💡 Daily Wellness Tip"),
                        div(class = "quote-text", "Eat good. Feel good. Fuel your body with color, variety, and joy!")
                    )
                )
              ),
              fluidRow(
                box(title = "🧍 Your Info", width = 4, solidHeader = TRUE, status = "primary",
                    numericInput("height", "Height (cm)", value = 170, min = 100),
                    numericInput("weight", "Weight (kg)", value = 65, min = 30),
                    verbatimTextOutput("calorie_need"),
                    verbatimTextOutput("water_need"),
                    verbatimTextOutput("walk_need")
                ),
                box(title = "🍽️ Select Food", width = 4, solidHeader = TRUE, status = "info",
                    selectInput("food_select", "Food Item", choices = food_data$Food),
                    numericInput("quantity", "Quantity (grams)", 100, min = 1, step = 10),
                    verbatimTextOutput("suggested_qty"),
                    actionButton("add_btn", "➕ Add to Meal", class = "btn btn-success", style = "font-size:16px")
                ),
                box(title = "📉 Remaining Daily Quota", width = 4, solidHeader = TRUE, status = "warning",
                    htmlOutput("remaining_calories_protein"))
              ),
              fluidRow(
                box(title = "📋 Meal Log (Edit & Save)", width = 12, solidHeader = TRUE, status = "success",
                    div(style = "height:250px; overflow-y:auto; font-size:13px;", DTOutput("meal_log")),
                    br(),
                    actionButton("save_changes", "💾 Save Changes", class = "btn btn-primary", style = "margin-top:5px"))
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "🥣 Nutrition Summary Pie Chart", width = 12, solidHeader = TRUE, status = "warning",
                    plotlyOutput("nutrition_pie", height = "400px"))
              )
      ),
      tabItem(tabName = "compare",
              box(title = "📊 Compare Foods", width = 12, solidHeader = TRUE, status = "info",
                  checkboxGroupInput("compare_items", "Select Foods", choices = food_data$Food),
                  DTOutput("compare_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  calorie_goal <- reactive({
    req(input$height, input$weight)
    bmr <- 10 * input$weight + 6.25 * input$height - 5 * 22 + 5
    round(bmr * 1.375)
  })
  
  output$calorie_need <- renderText({
    paste("🔥 Estimated Calories:", calorie_goal(), "kcal/day")
  })
  
  output$water_need <- renderText({
    water <- round(input$weight * 35)
    paste("💧 Water Needed:", water, "ml (", round(water / 1000, 2), "liters)")
  })
  
  output$walk_need <- renderText({
    walk_kcal <- round(0.5 * 3.5 * input$weight)
    steps <- round(walk_kcal * 20)
    km <- round(steps / 1300, 2)
    paste("🚶 Walk: ~30 min (~", km, "km), burns ~", walk_kcal, "kcal")
  })
  
  output$current_time <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%d-%b-%Y %H:%M:%S")
  })
  
  output$suggested_qty <- renderText({
    selected <- food_data %>% filter(Food == input$food_select)
    if (nrow(selected) == 1 && calorie_goal() > 0) {
      portion_kcal <- 0.15 * calorie_goal()
      grams <- (portion_kcal / selected$Calories) * 100
      paste("📏 Suggested Quantity:", round(grams), "g")
    }
  })
  
  rv <- reactiveValues(meals = tibble(
    Time = character(),
    Food = character(),
    Quantity = numeric(),
    Calories = numeric(),
    Protein = numeric(),
    RecommendedFor = character()
  ))
  
  observeEvent(input$add_btn, {
    selected <- food_data %>% filter(Food == input$food_select)
    qty <- input$quantity / 100
    new_entry <- tibble(
      Time = format(Sys.time(), "%H:%M:%S"),
      Food = selected$Food,
      Quantity = input$quantity,
      Calories = round(selected$Calories * qty, 1),
      Protein = round(selected$Protein * qty, 1),
      RecommendedFor = selected$MealTime
    )
    rv$meals <- bind_rows(rv$meals, new_entry)
  })
  
  output$meal_log <- renderDT({
    datatable(rv$meals, editable = TRUE, options = list(pageLength = 5, dom = 't', autoWidth = TRUE))
  })
  
  observeEvent(input$meal_log_cell_edit, {
    info <- input$meal_log_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- info$value
    rv$meals[i, j] <- DT::coerceValue(v, rv$meals[[j]])
  })
  
  observeEvent(input$save_changes, {
    showNotification("✅ Changes Saved", type = "message")
  })
  
  output$nutrition_pie <- renderPlotly({
    totals <- rv$meals %>% summarise(Calories = sum(Calories), Protein = sum(Protein))
    plot_ly(
      labels = c("Calories (kcal)", "Protein (g)"),
      values = c(totals$Calories, totals$Protein),
      type = "pie",
      textinfo = "label+percent",
      marker = list(colors = c("#F1948A", "#85C1E9"))
    ) %>% layout(title = "Nutrient Breakdown")
  })
  
  output$compare_table <- renderDT({
    req(input$compare_items)
    datatable(
      food_data %>%
        filter(Food %in% input$compare_items) %>%
        mutate(
          Calories = paste0(Calories, " kcal"),
          Protein = paste0(Protein, " g")
        ) %>%
        select(Food, Calories, Protein) %>%
        arrange(desc(Calories)),
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
  
  output$weekly_menu_table <- renderDT({
    datatable(menu_data, options = list(pageLength = 7, dom = 't'), rownames = FALSE)
  })
  
  output$remaining_calories_protein <- renderUI({
    req(input$height, input$weight)
    totals <- rv$meals %>% summarise(Calories = sum(Calories), Protein = sum(Protein))
    remaining_cal <- calorie_goal() - totals$Calories
    remaining_pro <- round((input$weight * 0.8) - totals$Protein, 1)
    
    bmi <- input$weight / ((input$height / 100) ^ 2)
    sleep <- if (bmi < 18.5) {
      "8.5 hours"
    } else if (bmi < 25) {
      "8 hours"
    } else if (bmi < 30) {
      "7.5 hours"
    } else {
      "7 hours"
    }
    
    HTML(paste0(
      "<div class='remaining-info'>🧮 Remaining: ", remaining_cal, " kcal & ", remaining_pro, " g protein</div>",
      "<div class='remaining-info'>😴 Recommended Sleep: ", sleep, "</div>"
    ))
  })
}

# Run the App
shinyApp(ui, server)

