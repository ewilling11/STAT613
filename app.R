#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT) #potentially remove later
library(rjson)

recipe.data <- read_csv("epi_r.csv")
recipe.json <- fromJSON(file = "full_format_recipes.json")

recipes <- recipe.data %>% 
    mutate(id = row.names(recipe.data))

#The sum_ingredients function is used to pull new data according to our app selection criteria

sum_ingredients <- function(x){
    
    #If at least one ingreadient name is passed
    if (length(x) > 0){
        
        #create an object that will hold our recipe data - each recipe is assigned a numeric value
        #this value is reset to 0 every time reactive is called to ensure there is no mixing with previous iterations
        protein_index <- 0
        
        #For each ingredient, cycle through recipes and add 1 if the chosen ingredient is found
        for(i in 1:length(x)){
            protein_index <- protein_index + protein_list[[x[i]]]
        }
    } else {
        #if no ingredient names are passed, return all data
        protein_index <- rep(1, nrow(recipes))
    }
    
    #Any recipes with value > 0 will have one of the specified ingredients. Return all indices that matched one of our input ingredients
    return(protein_index)
}

#create list of protein types
protein_list <- list(Beef = recipes$beef |
                         recipes$`beef rib` |
                         recipes$`beef shank` |
                         recipes$`beef tenderloin` |
                         recipes$brisket |
                         recipes$`ground beef` |
                         recipes$hamburger |
                         recipes$meatball |
                         recipes$meatloaf |
                         recipes$roast |
                         recipes$steak, 
                     Poultry = recipes$chicken |
                         recipes$duck |
                         recipes$poultry |
                         recipes$`poultry sausage` |
                         recipes$quail |
                         recipes$turkey,
                     Pork = recipes$pork |
                         recipes$`pork chop` |
                         recipes$`pork rib` |
                         recipes$`pork tenderloin` |
                         recipes$prosciutto |
                         recipes$sausage |
                         recipes$ham,
                     Fish = recipes$cod |
                         recipes$fish |
                         recipes$salmon |
                         recipes$sardine |
                         recipes$snapper |
                         recipes$swordfish |
                         recipes$tilapia |
                         recipes$trout |
                         recipes$tuna, 
                     Seafood = recipes$clam |
                         recipes$crab |
                         recipes$lobster |
                         recipes$octopus |
                         recipes$oyster |
                         recipes$scallop |
                         recipes$shrimp |
                         recipes$shellfish |
                         recipes$squid, 
                     `Other Meat` = recipes$`ground lamb` |
                         recipes$lamb |
                         recipes$`lamb chop` |
                         recipes$`lamb shank` |
                         recipes$`rack of lamb` |
                         recipes$rabbit |
                         recipes$veal |
                         recipes$venison, 
                     Egg = recipes$egg |
                         recipes$omelet |
                         recipes$quiche
)

ui <- fluidPage(

    # Application title
    titlePanel("Epicurious Recipe Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            #Protein Selection
            
            #When selecting a protein; all recipes with tag should be present
            
                #If pork box is checked but egg is not; we expect a recipe with both pork and egg would still be present
            
            checkboxGroupInput("protein",
                               "Choose Your Protein",
                               names(protein_list),
                               selected = c("Beef"
                                            #"poultry",
                                            #"pork",
                                            #"fish",
                                            #"seafood",
                                            #"egg",
                                            #"other_meat"  -> should be names(protein_list)
                                            ))
        ),

        
        #Dietary Restriction -- Waseem
            #Checkboxgroup - default is all unchecked
                #Vegetarian
                #Vegan
                #Pescatarian
                #will have to calculate gluten free
            #Should be text noting that this selection overrides protein but works in combo with dietary restriction
            #We assume that all recipes with dietary restriction tags are removed when the box is checked
    
        
        #Allergies -- Waseem
            #checkboxgroup - default is all unchecked
                #peanut
                #tree nuts - look at accuracy
                #dairy - look at accuracy (tags for dairy and dairy-free; are these inverse of each other)
                #eggs
                #shellfish
            #Should be text noting that this selection overrides protein but works in combo with dietary restriction
            #We assume that all recipes with dietary restriction tags are removed when the box is checked
        
        #Twitter/Ratings -- Ethan
            #Should search for tweets with recipe title in tweet
            #Pull ratings and responses?
        
        #Help Button -- Steve
            #Should give info on how searches are working
        
        
        #Google URL for recipe or Pulling directions from json -- Steve
            #Want to pull some steps allowing someone to make the recipe
            #Needs to be a ui component - how does a user select the specific recipe
        
        
        # Show a plot of the generated distribution
        mainPanel(
          
          #Button displays the output from our input checkbox - used for debugging
          verbatimTextOutput("button"),
          
          #ButtonOut displays our filtered data table
           dataTableOutput("buttonOut")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$button <- reactive({ input$protein #c(input$containsBeef, input$containsPoultry, input$containsPork, input$containsFish, input$containsSeafood, input$containsOtherMeat, input$containsEgg)
                })
  
  in_data <- reactive({recipes[sum_ingredients(input$protein) > 0, c(681, 1:6)]})
  
    output$buttonOut <- DT::renderDataTable({
      
        DT::datatable(
          in_data(), 
          selection = "single",
          rownames = FALSE
        )
    })
    
    # code for row clicking
    
    observeEvent(input$buttonOut_rows_selected,
                 {
                   row_number <- as.numeric(in_data()$id[input$buttonOut_rows_selected])
                   this_recipe <- recipe.json[[row_number]]
                   showModal(modalDialog(
                     title = this_recipe[["title"]],
                     HTML(paste0("Ingredients:","<br>",
                                 this_recipe[["ingredients"]]),
                          paste0("<br><br>",
                                 "Directions:","<br>"),
                                 paste0(this_recipe[["directions"]], collapse = ",")
                     )
                   ))
                 })

}

# Run the application 
shinyApp(ui = ui, server = server)
