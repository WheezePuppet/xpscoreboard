
library(shiny)
library(dplyr)
library(xtable)

source("mysql_config.R")

shinyServer(function(input,output,session) {

    INFLATION.ADJUSTMENT <- 0
    MAX.OUT.PTS <- 1100

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
                    
        names(levels[xp >= levels][1])
    }

    compute.grade <- function(xp) {
        levels <- c("A+"=600,
                    "A"=580,
                    "A-"=550,
                    "B+"=500,
                    "B"=470,
                    "B-"=430,
                    "C+"=400,
                    "C"=350,
                    "C-"=320,
                    "D+"=300,
                    "D"=250,
                    "F"=0)
                    
        names(levels[xp >= levels][1])
    }

    compute.score <- function(pts) {
        if (pts+INFLATION.ADJUSTMENT >= MAX.OUT.PTS) {
            return("enough")
        } else {
            return(as.character(as.integer(pts)+INFLATION.ADJUSTMENT))
        }
    }

    output$xpPlot <- renderTable({

        input$addchar
        db.src <- src_mysql(mysql.db.name,user=mysql.user,
            password=mysql.password)
        chars <- collect(tbl(db.src, paste0("chars_", mysql.db.table.suffix)))
        xp <- collect(tbl(db.src, paste0("xp_", mysql.db.table.suffix)))
        secret <- collect(tbl(db.src, paste0("secret_", mysql.db.table.suffix)))
        display <- inner_join(chars,xp,by=c("charname"="username"))
        if (!is.null(input$app_hash) && input$app_hash == paste0("#",
            secret[1,1])) {
            display <- display %>% group_by(realname)
            display <- display %>% 
                summarize(Name=realname,
                Character=charname,Grade=compute.grade(sum(xps)), 
                Level=compute.level(sum(xps)), 
                XP=compute.score(sum(xps)), 
                "Most recent experience"=tag[thetime==max(thetime)],
                "Entered"=max(thetime)) %>% select(-realname)
        } else {
            display <- display %>% group_by(charname)
            display <- display %>% 
                summarize(Name=charname,Level=compute.level(sum(xps)), 
                XP=compute.score(sum(xps)), 
                "Most recent experience"=tag[thetime==max(thetime)],
                "Entered"=max(thetime)) %>% select(-charname)
        }
        display <- rbind(dplyr::filter(display, XP=="enough"),
            dplyr::filter(display, XP!="enough") %>% 
            arrange(desc(as.integer(XP))))
            
        xtable(as.data.frame(display))
    })

    output$msg <- renderText({
        if (input$addchar == 0) {
            ""
        } else {
            isolate({
                tryCatch({
                    if (length(grep("^([[:alpha:]]|[[:digit:]]| )*$",
                        c(input$realname, input$charname))) != 2) {
                        stop("(Only numbers and digits, please!)")
                    }
                    conn <- get.connection(TRUE)
                    dbGetQuery(conn,
                        paste0("insert into chars_", mysql.db.table.suffix,
                        " values (",
                        "'",input$realname,"',",
                        "'",input$charname,"')")
                    )
                    dbGetQuery(conn,
                        paste0("insert into xp_", mysql.db.table.suffix,
                        " values (",
                        "'",input$charname,"',1,'sign-up',now())")
                    )
                    dbDisconnect(conn)
                    "Account created!"
                }, error=function(e) paste0("Could not create account! ",
                    conditionMessage(e)))
            })
        }
    })

    output$myxpstuff <- renderUI({
        div(
            textInput("myxprealname",
                label="Your real life name (e.g., Emilia Clarke)",
                value=""),
            textInput("myxpcharname",
                label="Your character name (e.g., daenerys4prez)",
                value=""),
            actionButton("viewxp",label="View XP")
        )
    })

    observeEvent(input$viewxp, {
        
        db.src <- src_mysql(mysql.db.name,user=mysql.user,
            password=mysql.password)
        secret <- collect(tbl(db.src, paste0("secret_", 
            mysql.db.table.suffix)))
        chars <- collect(tbl(db.src, paste0("chars_", 
            mysql.db.table.suffix)))
        xp <- collect(tbl(db.src, paste0("xp_", mysql.db.table.suffix)))

        entered.real.name <- input$myxprealname
        entered.char.name <- input$myxpcharname

        display <- inner_join(chars,xp,by=c("charname"="username"))
        if (!is.null(input$app_hash) && input$app_hash == paste0("#",
            secret[1,1])) {

            entered.char.name <- (chars %>%
                dplyr::filter(realname==entered.real.name) %>% 
                select(charname))[[1]]
        }

        if (nchar(entered.real.name) == 0 ||
            nchar(entered.char.name) == 0) {
            output$myxpmsg <- renderText({ 
                "Enter your real name and character name."
            })
            return(NULL)
        }

        display <- inner_join(chars,xp,by=c("charname"="username")) %>%
            dplyr::filter(charname==entered.char.name,
                   realname==entered.real.name) %>%
            arrange(thetime) %>%
            transmute(Experience=tag,XP=xps,Entered=thetime)

        if (nrow(display) == 0) {
            if (nrow(chars %>% dplyr::filter(charname==entered.char.name)) 
                                                                        == 0){
                output$myxpmsg <- renderText({ 
                    paste0("No such character '",entered.char.name,"'.")
                })
            } else if (nrow(chars %>% 
                    dplyr::filter(realname==entered.real.name)) 
                                                                        == 0){
                output$myxpmsg <- renderText({ 
                    paste0("No such student '",entered.real.name,"'.")
                })
            } else {
                output$myxpmsg <- renderText({ 
                    "Wrong character name!"
                })
            }
            return(NULL)
        }

        output$myxpPlot <- renderTable({
            xtable(as.data.frame(display))
        })

        output$myxpstuff <- renderUI({
            div(
                h3(paste("Experience for",input$myxprealname))
            )
        })
    })
})
