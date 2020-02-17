shinyUI(
    fluidPage(
        ## CSS
        tags$head(
                 tags$title("CAVA"),
                 tags$link(rel = "stylesheet",
                           type = "text/css",
                           href = "styles.css"),
                 tags$style(".shiny-file-input-progress {display: none}"),
                 tags$link(rel = "stylesheet",
                           type = "text/css",
                           href = "//fonts.googleapis.com/css?family=Oswald"),
                 tags$link(rel = "stylesheet",
                           type = "text/css",
                           href = "//fonts.googleapis.com/css?family=Lora"),
                 tags$script(src = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML",
                             type = "text/javascript")
        ),

        ## Title
        uiOutput("title"),

        ##wait for starting
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",
                                  id = "loadmessage")),

        ## Main Page
        uiOutput("mainpage")
    )
)
