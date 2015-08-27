
library(shiny)

source("db.R")

shinyUI(fluidPage(

tags$head(
      tags$script("
$(document).ready(function() {
  $('#app_hash').val(window.location.hash);
});", type = 'text/javascript')
    ),
    tags$input(id = 'app_hash', type = 'text', style = 'display:none;'),

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("CPSC 240 -- Level tracker"),

    mainPanel(
        tabsetPanel(
            tabPanel("Levels", tableOutput("xpPlot")),
            tabPanel("New adventurer",
                h2("Join the CPSC 240 quest!"),
                textInput("realname",
                    label="Your real life name (e.g., Emilia Clarke)",
                    value=""),
                textInput("charname",
                    label="Your character name (e.g., daenerys4prez)",
                    value=""),
                actionButton("addchar",label="Begin adventure!"),
                textOutput("msg")
            ),
            selected="New adventurer"
        )
    )
))
