## Design Results

output$baDesignRst <- renderTable({

    if (0 == input$baBtnDesign)
        return(NULL)

    p0     <- input$ba_InP0
    p1     <- input$ba_InP1
    alpha  <- input$ba_InAlpha
    power  <- input$ba_InPower
    rho    <- input$ba_InRho
    bsizes <- input$ba_InBatch
    bsizes <- seq(bsizes[1], bsizes[2], by = 1)
    minmax <- input$ba_InMinMax
    nreps  <- input$ba_InNrep

    rst <- baDesignBetaBin(list(P0 = p0, P1 = p1,
                                ALPHA = alpha, POWER = power),
                           rho    = rho, bsizes = bsizes,
                           nmin   = minmax[1], nmax = minmax[2], nreps = nreps)
    xtable(rst)
})
