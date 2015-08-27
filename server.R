
library(shiny)
library(dplyr)
library(xtable)

shinyServer(function(input,output,session) {

    output$xpPlot <- renderTable({

        db.src <- src_mysql("stephen",user="stephen",password="davies4ever")
        chars <- tbl(db.src, "chars")
        xp <- tbl(db.src, "xp")
        display <- left_join(chars,xp,by=c("charname"="username")) %>% 
            group_by(username) %>% summarize(XP=sum(xp))
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
                    "Account created!"
                }, error=function(e) paste0("Could not create account! ",
                    conditionMessage(e)))
            })
        }
    })
})
