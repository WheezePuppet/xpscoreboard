
library(shiny)
library(dplyr)
library(xtable)

shinyServer(function(input,output,session) {

compute.level <- function(xp) {
    if (xp < 5) { return("Suck") } else { return("nice") }
}

    output$xpPlot <- renderTable({

        db.src <- src_mysql("stephen",user="stephen",password="davies4ever")
        chars <- collect(tbl(db.src, "chars"))
        xp <- collect(tbl(db.src, "xp"))
        display <- inner_join(chars,xp,by=c("charname"="username")) %>% 
            group_by(charname) %>% 
            summarize(XP=sum(xp), compute.level(XP), latest=max(thetime)) %>%
            arrange(desc(XP))
        xtable(as.data.frame(display))
    })

    output$msg <- renderText({
        if (input$addchar == 0) {
            ""
        } else {
            isolate({
                tryCatch({
                    dbGetQuery(get.connection(),
                        paste0("insert into chars values (",
                        "'",input$realname,"',",
                        "'",input$charname,"')")
                    )
                    dbGetQuery(get.connection(),
                        paste0("insert into xp values (",
                        "'",input$charname,"',1,'sign-up',now())")
                    )
                    "Account created!"
                }, error=function(e) paste0("Could not create account! ",
                    conditionMessage(e)))
            })
        }
    })
})
