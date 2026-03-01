# Author: Timothy Josef A. Reyes
# Final Project
# GUI code - prepares the necessary data for the computation and provides the format and design of the User Interface
# Update date: December 5, 2024

library(shiny) # necessary libraries to run the app
library(DT)
library(readxl)
source('Simplex.R') # import simplex source code

nutrition <- read_excel("nutritional_values.xlsx") # import nutrition table
nutritional_reqs <- read_excel("nutritional_requirements.xlsx") # imports the nutritional requirements for each nutrient

ui <- fluidPage(
  
  tags$head( # portion of the code that focuses on the design and behavior of the elements in the app
    tags$style(HTML("
    /* customization for the body and other built in classes */
    body {
      background-image: url('bg.jpg'); /* set up of background image */
      background-size: cover;
      background-repeat: no-repeat;
      background-attachment: fixed;
      font-family: 'Lato', sans-serif; 
    }
      
    h2 {
      font-weight: bold; /* sets teh format of text within a header of size 2 */
      color: #333333;
    }
    
    
    /* customization for the buttons and its behavoir */
    .custom-btn {
      background: #45A049;
      color: white;
      font-size: 16px;
      border: none;
      border-radius: 25px;
      padding: 10px 30px;
      cursor: pointer;
      transition: all 0.3s ease; /* makes transition from forms look more seamless */
    }
      
    .custom-btn:hover { /* button changes color and enlarges when user hovers over it */
      color: #45A049;
      background: black;
      transform: scale(1.1);
    }
    
    
    /* customization for the checkbox and its behaviour */
    .custom-checkbox input[type='checkbox'] { /* hides the default appreance of the checkbox */
      display: none;
    }

    .custom-checkbox label { /* custom apperance of the text of the checkbox */
      font-size: 20px;
      display: inline-flex;
      align-items: center;
      cursor: pointer;
      transition: all 0.3s ease; /* makes transition from forms look more seamless */
    }

    .custom-checkbox label::before { /* custom apperance of the checkbox itself */
      content: '';
      width: 20px;
      height: 20px;
      border: 3px solid #45A049;
      border-radius: 4px;
      margin-right: 10px;
      background-color: white;
      transition: all 0.3s ease; /* makes transition from forms look more seamless */
    }
    
    .custom-checkbox label:hover::before { /* enlarges and changes the color of the checkbox if cursor hovers over it*/
      border-color: black; 
      transform: scale(1.5);
    }
    
    .custom-checkbox label:hover { /* enlarges and changes the color of the text label of a checkbox */
      font-size: 22px;
      color:  #45A049;
    }

    .custom-checkbox input[type='checkbox']:checked + label::before { /* appeaarnce of the checkbox after being pressed */
      background-color: #45A049;
      border-color: #45A049;
      box-shadow: 0 0 4px rgba(0, 0, 0, 0.2);
    }
    
    .custom-checkbox input[type='checkbox']:checked + label { /* appearance of the checkbox text after being pressed */
      font-size: 22px;
      color:  #45A049;
    }

    /* customization for the divs */
    .main-container {
      background-color: rgba(255, 255, 255, 0.5); /* makes the divs have a low opacity*/
      border-radius: 15px;
      position: relative;
      padding: 20px;
      margin-top: 10px;
    }

    .header-container {
      background: white;
      border-radius: 15px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      padding: 20px;
      display: flex; 
      justify-content: space-between; 
      align-items: center;
    }
    
    .box-container {
      background-color: rgba(255, 255, 255, 0.7);
      border-radius: 15px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      padding: 20px;
      margin-top: 30px;
      font-size: 20px;
    }
    
    .scroll-box {
      max-height: 400px;
      overflow-y: auto; /* a croll bar within this box will appear if the number of elements within it cannot fit within the box*/
      padding: 10px;
    }
     
    .button-row {
      background: white;
      box-shadow: 0px -2px 6px rgba(0, 0, 0, 0.1);
      padding: 15px;
      display: flex;
      width: 100%;
      gap: 20px; /* increases the distance between the buttons */
      justify-content: center;
      position: fixed;
      bottom: 0;
      left: 0;
      gap: 20px;
    }
    
    .notification .modal-body {
      font-size: 24px;
      font-weight: bold; 
      text-align: center;
    }
    
    /* customization for the tool tip*/
    .tooltip {
      width: 250px;
      white-space: normal;
      pointer-events: none;
    }

    .tooltip > .tooltip-inner {
      pointer-events: none;
      background-color: #73AD21;
      color: #FFF;
      border: 1px solid green;
      padding: 10px;
      font-size: 14px;
      text-align: left;
      margin-left: 0;
      word-wrap: break-word;
    }

    .tooltip > .arrow::before {
      border-right-color: #73AD21;
    }
  ")),
    tags$script(HTML(" /* portion of the code that has important code implementation for a better user experience*/
    $(function () {
      $('[data-toggle=tooltip]').tooltip({
        html: true /* allows the code to interpret tooltip as an HTML, allowing html formatting to be applied*/
      })
    })
    $(document).on('click', 'button', function() {
      $(this).blur(); /* removes the focus from a button after clicking it*/
    });
    "))
  ),
  
  div(class = "main-container", # main container for most of the divs used
      div(class = "header-container", # a header div that displays the app's name ad button to know the nutritional requirements
          h2("FoodFunder"),
          actionButton("nutritional_requirements", "Nutritional Requirements", class = "custom-btn")
      ),
      fluidRow(
        column(6,
               div(class = "box-container",
                     h2("Food Choices:"),
                     
                     div(class = "scroll-box", # a div that has a scroller that contains all of the check boxes
                         lapply(seq_along(nutrition$Foods), function(i) { # applies the function to all elements of nutrition$Foods
                           div( # each check box is a div that is designated as a check box in terms of input
                             class = "custom-checkbox",
                             tags$input(
                               type = "checkbox",
                               id = paste0("checkbox_", gsub(" ", "_", tolower(nutrition$Foods[i]))) # assigns the id of each check box
                             ),
                             tags$label( # label of the check box 
                               `for` = paste0("checkbox_", gsub(" ", "_", tolower(nutrition$Foods[i]))), # for = id, actual_label
                               nutrition$Foods[i],
                               `data-toggle` = "tooltip", `data-placement` = "right", # activates data-toggle for each check box 
                               `title` = paste("<b>Price/Serving:</b> $", nutrition$`Price/Serving`[i], "<br>", # format for all nutritional data within the tool tip
                                               "<b>Serving Size:</b> ", nutrition$`Serving Size`[i], " ", nutrition$`Serving Type`[i], "<br>", #<br> was properly implemented due to the tooltip being set as HTML
                                               "<b>Calories:</b> ", nutrition$`Calories`[i], "<br>",
                                               "<b>Cholesterol:</b> ", nutrition$`Cholesterol mg`[i], " mg<br>",
                                               "<b>Total Fat:</b> ", nutrition$`Total_Fat g`[i], " g<br>",
                                               "<b>Sodium:</b> ", nutrition$`Sodium mg`[i], " mg<br>",
                                               "<b>Carbohydrates:</b> ", nutrition$`Carbohydrates g`[i], " g<br>",
                                               "<b>Dietary Fiber:</b> ", nutrition$`Dietary_Fiber g`[i], " g<br>",
                                               "<b>Protein:</b> ", nutrition$`Protein g`[i], " g<br>",
                                               "<b>Vitamin A:</b> ", nutrition$`Vit_A IU`[i], " IU<br>",
                                               "<b>Vitamin C:</b> ", nutrition$`Vit_C IU`[i], " IU<br>",
                                               "<b>Calcium:</b> ", nutrition$`Calcium mg`[i], " mg<br>",
                                               "<b>Iron:</b> ", nutrition$`Iron mg`[i], " mg",
                                               sep="")
                             )
                           )
                         })
                     )
                 )
        ),
        column(6,
              div(class = "box-container", # box that will contain all of the selected foods
                  h2("Selected Foods:"),
                  div(class = "scroll-box", # gives the box a scroller
                      uiOutput("selected_foods")
                  ) 
              )
        )
      ),
      div(class = "button-row", # fixed row at the bottom that contains all of the functional buttons
          actionButton("select_all", "Select All", class = "custom-btn"),
          actionButton("clear_all", "Clear All", class = "custom-btn"),
          actionButton("compute", "Start Computation", class = "custom-btn")
      )
  )
)

server <- function(input, output, session) { # server - portion of the code that handles the input and output portion
  selected_foods <- reactive({ # a reactive function that returns all of the selected food
    food_ids <- paste0("checkbox_", gsub(" ", "_", tolower(nutrition$Foods))) # lists all of the ids of all of the check boxes
    foods <- which(sapply(food_ids, function(id) input[[id]])) # input[[id]] checks if a check box is checked and which() returns the index of that if it is indeed checked
    
    return(foods) # returns the indices of the chosen foods
  })
  
  update_all_checkboxes <- function(value) { # a function that changes all of the input elements that have an id that starts with checkbox
    for (id in names(input)) {
      if (startsWith(id, "checkbox")) {
        updateCheckboxInput(session, id, value = value)
      }
    }
  }
  
  observeEvent(input$select_all, { # if select_all is press all of the value of the check boxes are converted to TRUE
    update_all_checkboxes(TRUE)
  })
  
  observeEvent(input$clear_all, { # if clear_all is press all of the value of the check boxes are converted to FALSE
    update_all_checkboxes(FALSE)
  })
  
  output$nutritional_requirements <- renderDT({ # table for the nutritional requirements
    datatable(nutritional_reqs, 
              options = list(
                dom = 't',
                pageLength = 11,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all") # Center align all columns
              )), rownames = FALSE)
  })
  
  observeEvent(input$nutritional_requirements, { # if Nutritional Requirements is pressed, a table of the min and max for each nutrient is shown
    showModal(modalDialog(
      title = "Nutritional Requirements",
      DTOutput("nutritional_requirements"),
      div("NOTE: ANY FOOD WILL ONLY HAVE UP TO 10 SERVINGS", style = "text-align: center; font-weight: bold;"),
      easyClose = TRUE,       
      footer = modalButton("Close") 
    ))
  })
  
  computed_diet <- reactiveVal(NULL) # a reactive value for the final output
  
  observeEvent(input$compute, { # event for computation
    foods <- selected_foods() # gets the list of selected food items
    
    if (length(foods) > 0) { # if there is food selected, it try to compute the answer
      result <- tryCatch({ # try catch to capture any possible computational errors
        ComputeSimplex(foods, nutrition) # performs simplex computation
      }, error = function(e) {
        showNotification("An error occurred during computation.", type = "error")
        return(NULL)
      })
      
      computed_diet(result) # inserts the results in the computed_diet reactive Value
      
      if (!is.null(result)) { # if there is a viable answer
        tabs <- lapply(names(result), function(name) { # creates tabs for all of the data tables taken from result
          tabPanel(
            title = name, 
            DTOutput(paste0("table_", gsub(" ", "_", name))) # generates the id for each table
          )
        })

        showModal(modalDialog( # main popup table that allows user to see each iteration of the simplex computation and the final table
          title = "RESULTS",
          size = "l",
          tabsetPanel(id = "diet_tabs", !!!tabs),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        for (name in names(result)) { # for each table within results
          local({ # local makes sure that each iteration or tables are separated from one another, so their elements don't conflict with one another
            tab_name <- name
            output_id <- paste0("table_", gsub(" ", "_", tab_name))
            table_data <- result[[tab_name]] # gets the actual table
            
            output[[output_id]] <- renderDT({ # renders the table to its designated tab
              datatable(table_data, options = list(pageLength = 5, scrollX = TRUE)) # output for every tab
            })
          })
        }
      } else { # a pop up that informs the user that their food choices are not nutritionally viable
        showModal(modalDialog(
          title = "WARNING",
          div("NO SOLUTION: CHOSEN FOOD/S ARE NOT ENOUGH TO MEET NUTRITIONAL REQUIREMENTS"),
          easyClose = TRUE,
          size = "m", 
          footer = NULL,  
          style = "background-color: #f8d7da; color: #721c24;"
        ) %>%
          tagAppendAttributes(class = "notification")
        )
      }
    } else { # a pop up that informs the user that they have not chosen any food
      showModal(modalDialog(
        title = "WARNING",
        div("NO SOLUTION: NO FOOD IS SELECTED"),
        easyClose = TRUE,
        size = "m", 
        footer = NULL,
        style = "background-color: #f8d7da; color: #721c24;"
      ) %>%
        tagAppendAttributes(class = "notification")
      )
    }
  })

  output$selected_foods <- renderUI({ # handles the output of the selected foods
    foods <- selected_foods() # gets the indices of selected foods
    
    if (length(foods) > 0) { # lists all of the selected foods
      output_content <- paste("<ul>", paste("<li>", nutrition$Foods[foods], "</li>", collapse = ""), "</ul>")
      HTML(output_content)
    } else { # informs the user that they have not selected anything yet
      HTML("<p>No food selected.</p>")
    }
  })
  
}

shinyApp(ui, server)