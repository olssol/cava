library(xtable);
## load_all();
##library(devtools);

shinyServer(function(input, output, session) {

    ##---------------------------------------------------------------
    ##---------LOAD FUNCTIONS----------------------------------------
    ##---------------------------------------------------------------
    source("cava_tools.R",    local = TRUE)
    source("visit_ui.R",      local = TRUE)
    source("visit_server.R",  local = TRUE)
    source("basim_ui.R",      local = TRUE)
    source("basim_server.R",  local = TRUE)

    ##---------------------------------------------------------------
    ##---------GLOBAL ENVIRONMENT------------------------------------
    ##---------------------------------------------------------------
    userLog <- reactiveValues()

    ##  0: Projects Gallery
    ##  1: VISIT
    ##  2: BASIC
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
    output$title <- renderUI({
        if (0 == userLog$entrance) {
            tit <- "Cancer Vaccine Clinical Trial Design and Analysis"
        } else if (1 == userLog$entrance) {
            tit <- "visit: Phase I Dose Escalation Design"
        } else if (2 == userLog$entrance) {
            tit <- "basic: Phase II Batch-Effect Adjusted Simon's Two-Stage Design"
        }

        get_head(tit)
    })


    output$mainpage <- renderUI({
        if (0 == userLog$entrance) {
            rst <- includeHTML("www/file_entrance.html")
        } else if (1 == userLog$entrance) {
            rst <- visit_main()
        } else if (2 == userLog$entrance) {
            rst <- basim_main()
        }

        ## add footer
        div(rst, get_foot())
    })
})
