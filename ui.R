
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

    tags$head(tags$link(rel="stylesheet", type="text/css", href="326style.css")),

    titlePanel(HTML("CPSC 326 &mdash; Scoreboard")),

    mainPanel(
        tabsetPanel(
            tabPanel("Levels", tableOutput("xpPlot")),
            tabPanel("New computer scientist",
                h2("Join the CPSC 326 investigation!"),
                textInput("realname",
                    label=HTML("Your real life name (<i>e.g.</i>, Alonzo Church)"),
                    value=""),
                textInput("charname",
                    label=HTML("Your screen name (<i>e.g.</i>, lambdaCalc4me)"),
                    value=""),
                actionButton("addchar",label="Begin journey!"),
                textOutput("msg")
            ),
            tabPanel("My XP",
                h2("My XP"),
                uiOutput("myxpstuff"),
                tableOutput("myxpPlot"),
                textOutput("myxpmsg")
            ),
            selected="Levels"
        )
    )
))
