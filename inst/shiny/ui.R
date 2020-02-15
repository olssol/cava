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

        withTags({
            div(class = "cheader",
                "Cancer Vaccine Clinical Trial Design and Analysis",
                lapply(list(c("btnClose", "Exit", "setTimeout(function(){window.close();},500);"),
                            c("btnProjects", "Designs", "")),
                       function (x) {
                           tags$button(
                                    id = x[1],
                                    type = "button",
                                    class = "btn action-button",
                                    onclick = x[3],
                                    x[2],
                                    style = "float: right; background-color: #2B3E50;"
                                )
                           }))
        }),

        ##wait for starting
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...", id = "loadmessage")),


        ## Main Page
        uiOutput("mainpage"),

        ## foot
        div(class = "cfooter",
            HTML("By the <a href='http://pathology.jhu.edu/ccspore/index.html'
                  target = '_blank'>Cervical Cancer SPORE</a> Team."))
    )
)
