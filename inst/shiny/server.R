library(devtools);
library(xtable);
## load_all();

shinyServer(function(input, output, session) {

    ##load functions
    source("visit_ui.R",      local = TRUE)
    source("visit_server.R",  local = TRUE)

    userLog      <- reactiveValues()
    ##  0: Projects Gallery
    ##  1: VISIT
    ##  2: BASIM
    userLog$entrance <- 0


    ##---------------------------------------------------------------
    ##---------HEADER BUTTONS----------------------------------------
    ##---------------------------------------------------------------
    observeEvent(input$btnClose, {
        stopApp()
    });

    observeEvent(input$btnProjects, {
        userLog$entrance <- 0;
    });

    observeEvent(input$btnVisit, {
        userLog$entrance <- 1;
    });

    observeEvent(input$btnBasim, {
        userLog$entrance <- 2;
    });


    ##----------------------------------------------------------------
    ##------------------------MAIN PAGE-------------------------------
    ##----------------------------------------------------------------
    output$mainpage <- renderUI({
        if (0 == userLog$entrance) {
            includeHTML("www/file_entrance.html")
        } else if (1 == userLog$entrance) {
            visit_main()
        } else if (2 == userLog$entrance) {
            basim_main()
        }
    })

})

