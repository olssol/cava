

##----------------------------------------------------------------------
##                  MAINPAGE UI
##----------------------------------------------------------------------

basim_main <- function() {
    tabsetPanel(type = "pills",
        id = "mainpanel",
        ba_about(),
        ba_setting(),
        ba_design(),
        ba_report(),
        selected = "Setting"
    )
}

ba_about <- function() {
    tabPanel(
        title = "About",
        fluidRow(
            column(8,
                wellPanel(
                    fluidRow(
                        withMathJax(includeHTML('www/file_basim.html')),
                        style = 'padding-left: 30px; padding-right: 30px;'
                    ),
                    style = 'padding-left: 30px;'
                ),
                offset = 2
            )
        )
    )
}

ba_setting <- function() {
    tabPanel(
        title = "Setting",
        fluidRow(
            column(8,
                   panel_par1(),
                   panel_par2(),
                   panel_par3(),
                   offset = 2
                   )
        )
    )
}

ba_design <- function() {
    tabPanel(
        title = "Design",
        fluidRow(
            column(8,
                   wellPanel(
                       actionButton(inputId = "baBtnDesign",
                                    label = "Get Design", width = '135px'),
                       uiOutput("baDesignRst"),
                       align = 'center'
                   ),
                   offset = 2
                   )
        )
    )
}


ba_report <- function() {
    tabPanel(
        title = "Report",
        fluidRow(
            column(8,
                   panel_report(),
                   offset = 2
                   )
        )
    )
}


##----------------------------------------------------------------------
##                  Setting
##----------------------------------------------------------------------
panel_par1 <- function() {
    wellPanel(get_row_bottom("Design Parameters"),
              fluidRow(
                  column(4,
                         numericInput(inputId = "ba_InP0",
                                      label = "Null Hypothesis P0",
                                      value = 0.4, min = 0,
                                      max = 1, step = 0.1, width = "200px"),
                         numericInput(inputId = "ba_InP1",
                                      label = "Alternative Hypothesis P1",
                                      value = 0.6, min = 0,
                                      max = 1, step = 0.1, width = "200px")
                         ),
                  column(4,
                         numericInput(inputId = "ba_InAlpha",
                                      label = "Type I Error",
                                      value = 0.05, min = 0,
                                      max = 1, step = 0.05, width = "200px"),
                         numericInput(inputId = "ba_InPower",
                                      label = "Power",
                                      value = 0.8, min = 0,
                                      max = 1, step = 0.05, width = "200px")
                         ),
                  column(4,
                         numericInput(inputId = "ba_InRho",
                                      label = "$$\\rho$$",
                                      value = 0.2, min = 0,
                                      max = 1, step = 0.05, width = "200px")
                         ),
                  style = get_styles("mar20")
              ))
}

panel_par2 <- function() {
    wellPanel(get_row_bottom("Design Parameters"),
              fluidRow(
                  column(8,
                         sliderInput(
                             inputId = "ba_InMinMax",
                             label = "Number of Patients",
                             min = 10,
                             max = 200,
                             value = c(20, 100),
                             step = 1),
                         sliderInput(
                             inputId = "ba_InBatch",
                             label = "Batch Sizes",
                             min = 1,
                             max = 20,
                             value = c(3, 6),
                             step = 1)),
                  style = get_styles("mar20")
              ))
}

panel_par3 <- function() {
    wellPanel(get_row_bottom("Other Parameters"),
              fluidRow(
                  column(4,
                         numericInput(
                             inputId = "ba_InNrep",
                             label = "Number of Simulations",
                             min = 1000,
                             max = 10000,
                             value = 5000,
                             step = 500),
                  style = get_styles("mar20")
                  ))
              )
}

##----------------------------------------------------------------------
##                  Report
##----------------------------------------------------------------------
panel_report <- function() {
    wellPanel(
        get_row_bottom("Download Report Analysis"),
        fluidRow(
            radioButtons(
                inputId = "format",
                label = "",
                choices = c('PDF', 'HTML', 'Word')
            ),
            downloadButton(
                outputId = "baBtnDownload"
            ),
            style = 'margin-left: 30px;'
        )
    )
}
