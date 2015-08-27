
library(shiny)
library(dplyr)
library(xtable)

shinyServer(function(input,output,session) {

compute.level <- function(xp) {
    levels <- c("Dungeon Master"=1200,
                "Master Adventurer"=1100,
                "Wizard"=1050,
                "Master"=950,
                "Adventurer"=900,
                "Junior Adventurer"=850,
                "Novice Adventurer"=800,
                "Amateur Adventurer"=750,
                "Inferior Adventurer"=700,
                "Deficient Adventurer"=600,
                "Inadequate Adventurer"=500,
                "n00b"=0)
                
    names(levels[xp > levels][1])
}

    output$xpPlot <- renderTable({

        input$addchar
        db.src <- src_mysql("stephen",user="stephen",password="davies4ever")
        chars <- collect(tbl(db.src, "chars"))
        xp <- collect(tbl(db.src, "xp"))
        secret <- collect(tbl(db.src, "secret"))
        display <- inner_join(chars,xp,by=c("charname"="username"))
        if (!is.null(input$app_hash) && input$app_hash == paste0("#",
            secret[1,1])) {
            display <- display %>% group_by(realname)
        } else {
            display <- display %>% group_by(charname)
        }
        display <- display %>% 
            summarize(Level=compute.level(sum(xp)), XP=sum(xp), 
            "Most recent experience"=tag[thetime==max(thetime)],
            "Entered"=max(thetime)) %>%
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
