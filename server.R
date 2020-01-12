
library(shiny)
library(dplyr)
library(xtable)

source("mysql_config.R")

shinyServer(function(input,output,session) {

    INFLATION.ADJUSTMENT <- 0
    MAX.OUT.PTS <- 1100

    compute.level <- function(xp) {
        levels <- c("Agent Smith"=650,
                    "Ava"=600,
                    "Ash"=560,
                    "BB-8"=530,
                    "Bishop"=500,
                    "Baymax"=470,
                    "C-3PO"=440,
                    "Cortana"=420,
                    "Cylon"=400,
                    "R2-D2"=380,
                    "Data"=360,
                    "Dolores"=340,
                    "Terminator"=320,
                    "HAL-9000"=300,
                    "Ultron"=280,
                    "Maeve"=260,
                    "K-2SO"=240,
                    "#6"=220,
                    "TARS"=200,
                    "J.A.R.V.I.S."=175,
                    "Rachael"=150,
                    "L3-37"=125,
                    "Eve"=100,
                    "Wall-E"=75,
                    "Rosie the Maid"=50,
                    "Power droid"=25,
                    "Dead battery"=0)

        names(levels[xp >= levels][1])
    }

    compute.image <- function(xp) {
        levels <- c("agentsmith.png"=650,
                    "ava.jpg"=600,
                    "ash.jpg"=560,
                    "bb8.png"=530,
                    "bishop.jpg"=500,
                    "baymax.png"=470,
                    "c3po.jpg"=440,
                    "cortana.png"=420,
                    "cylon.png"=400,
                    "r2d2.png"=380,
                    "data.jpg"=360,
                    "dolores.jpg"=340,
                    "terminator.jpg"=320,
                    "hal9000.png"=300,
                    "ultron.jpg"=280,
                    "maeve.jpg"=260,
                    "k2so.png"=240,
                    "number6.png"=220,
                    "tars.jpg"=200,
                    "jarvis.jpg"=175,
                    "rachael.jpg"=150,
                    "l337.jpg"=125,
                    "eve.jpg"=100,
                    "walle.png"=75,
                    "rosie.png"=50,
                    "powerDroid.png"=25,
                    "deadBattery.png"=0)

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
            display <- display %>% group_by(realname,charname)
            display <- display %>%
                summarize(Grade=compute.grade(sum(xps)),
                Level=paste0("<img width=44 src=\"http://cs.umw.edu/~stephen/cpsc415/", compute.image(sum(xps)), "\" /> &nbsp; ",compute.level(sum(xps))),
                XP=compute.score(sum(xps)),
                "Most recent experience"=tag[thetime==max(thetime)],
                "Entered"=max(thetime)) %>% rename(Name=realname)
        } else {
            display <- display %>% group_by(charname)
            display <- display %>%
                summarize(
                Level=paste0("<img width=44 src=\"http://cs.umw.edu/~stephen/cpsc415/", compute.image(sum(xps)), "\" /> &nbsp; ",compute.level(sum(xps))),
                XP=compute.score(sum(xps)),
                "Most recent experience"=tag[thetime==max(thetime)],
                "Entered"=max(thetime)) %>% rename(Name=charname)
        }
        display <- rbind(dplyr::filter(display, XP=="enough"),
            dplyr::filter(display, XP!="enough") %>%
            arrange(desc(as.integer(XP))))

        xtable(as.data.frame(display))
    }, sanitize.text.function = function(x) x)

    output$msg <- renderText({
        if (input$addchar == 0) {
            ""
        } else {
            isolate({
                tryCatch({
                    if (length(grep("^([[:alpha:]]|[[:digit:]]| |-)*$",
                        c(input$realname, input$charname))) != 2) {
                        stop("(Only numbers, digits, and hyphens please!)")
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
