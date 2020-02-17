##----------------------------------------------------------------------
##                  STYLES
##----------------------------------------------------------------------

get_styles <- function(type) {
    switch(type,
           bottom = "padding: 20px; margin-left: 24px; padding-bottom: 0px; padding-top: 10px;",
           mar20  = 'margin-left: 20px; margin-right: 20px;'
           )
}


##----------------------------------------------------------------------
##                  HEADER AND FOOTER
##----------------------------------------------------------------------
get_head <- function(title) {
    tags$div(class = "cheader",
             title,
             lapply(list(c("btnClose", "Exit", "setTimeout(function(){window.close();},500);"),
                         c("btnProjects", "Designs", "")),
                    function(x) {
                        tags$button(
                                 id = x[1],
                                 type = "button",
                                 class = "btn action-button",
                                 onclick = x[3],
                                 x[2],
                                 style = "float: right; background-color: #2B3E50;"
                             )
                    }))

}

get_foot <- function() {
    div(class = "cfooter",
        HTML("By the <a href='http://pathology.jhu.edu/ccspore/index.html'
                      target = '_blank'>Cervical Cancer SPORE</a> Team."))
}

##----------------------------------------------------------------------
##                  DISPLAY ERROR
##----------------------------------------------------------------------
writeError <- function(errors) {
    rst <- NULL;
    for (i in 1:length(errors)) {
        rst <- paste(rst, '<h6>', errors[[i]], '</h6>');
    }

    showModal(
        modalDialog(
            title = "Error",
            footer = NULL,
            size = 'm',
            easyClose = TRUE,
            (HTML(rst))
        )
    )
}

##----------------------------------------------------------------------
##                  SHOW BAR
##----------------------------------------------------------------------
get_row_bottom <- function(title) {
    fluidRow(
        h4(title),
        style = 'margin-left: 20px;
                 border-bottom: 2px solid #E3E3E3;
                 margin-right: 20px;
                 margin-bottom: 10px;
                '
    )
}
