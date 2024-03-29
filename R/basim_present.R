#' S3 method for plotting the random error
#'
#' @param x ClsBaErr object
#' @param opts optional parameters
#' @param pos.legend legend position
#'
#' @noRd 
plot.ClsBaErr <- function(x, opts = NULL, pos.legend = c(0.8, 0.6), ...) {

    Error <- NULL;
    Type <- NULL;

    dta <- rbind(data.frame(Type = "gamma", Error = x$gamma),
                 data.frame(Type = "delta", Error = x$delta),
                 data.frame(Type = "epsilon", Error = x$epsilon));

    ggplot(data = dta, aes(x = Error)) +
      stat_density(aes(group=Type, linetype = Type, color = Type),
                   position="identity",
                   geom = "line",
                   adjust = 1.2, na.rm = TRUE) +
      scale_color_manual(values = c("gamma"   = "red",
                                    "delta"   = "blue",
                                    "epsilon" = "black"),
                         labels = c(expression(gamma),
                                    expression(delta),
                                    expression(epsilon))) +
      scale_linetype_manual(values = c("gamma"  = "solid",
                                      "delta"   = "twodash",
                                      "epsilon" = "dashed"),
                         labels = c(expression(gamma),
                                    expression(delta),
                                    expression(epsilon))) +
      theme_bw() +
      theme(legend.position = pos.legend,
            legend.title = element_blank()) +
      labs(x = "Random Effects", y = "Density") +
      opts;
}



#' Plot histogram of t-cells Y0 Y1 and Y1/Y0 for a given scenario
#'
#' @param par.err a list of gamma, delta, epsilon error
#' @param par.other other parameters including u0, u1, v, and beta
#' @param nreps number of repetitions
#' @param pos.legend legend position
#' @param f.simu simulating function
#' @param fname file name to save to
#' @param ... reserved parameters
#' @param ry.quants quantiles
#'
#' @return a plot of histogram of t-cells Y0 Y1 and Y1/Y0 for a given scenario
#'
#' @export
#'
baPltTcell <- function(par.err, par.other, nreps = 100000, pos.legend = c(0.8, 0.6),
                       f.simu = baSimuTcell, fname = NULL, ..., ry.quants = c(0.1,0.5,0.9)) {

    ..count.. <- NULL;
    Error <- NULL;
    Outcome <- NULL;
    Type <- NULL;
    Y <- NULL;
    x <- NULL;


    error   <- baSimuBatchError(bsize = 1, par.err, nreps = nreps);
    errplot <- plot(error);

    test.outcome <- f.simu(1, par.err= par.err, par.other = par.other, nreps = nreps);

    dta.outcome  <- rbind(data.frame(Outcome = "Y0", Y = test.outcome$y0),
                          data.frame(Outcome = "Y1", Y = test.outcome$y1));

    yplot <- ggplot(data = dta.outcome, aes(x        = Y,
                                            fill     = Outcome,
                                            linetype = Outcome,
                                            col      = Outcome)) +
        geom_histogram(aes(y = ..count.. / sum(..count..)),
                       alpha = 0.1,
                       position = "identity",
                       na.rm = TRUE,
                       ...) +
        labs(x = "T-Cell Counts", y = "Proportion") +
        theme_bw() +
        theme(legend.position = pos.legend,
              legend.title = element_blank());

    ryplot <- ggplot(data = data.frame(x = test.outcome$y), aes(x = x)) +
        geom_density(na.rm = TRUE) +
        labs(x = "Proliferation Index", y = "Density") +
        theme_bw();

    for (qu in ry.quants) {
        cur.q <- quantile(test.outcome$y, qu);
        ryplot <- ryplot + geom_vline(xintercept = cur.q,
                                      linetype = 2,
                                      col = "red");
    }


    if (!is.null(fname)) {
        rst <- plot_grid(errplot, yplot, ryplot, nrow=1);
        save_plot(fname, rst, base_height = 4, base_width = 12);
    }

    list(errplot, yplot, ryplot);
}

#' Plot histogram of t-cells Y0 Y1 and Y1/Y0 for all scenarios
#'
#' @param lst.par a list containing par.err and par.other as described in \code{baPltTcell}
#' @param xlims x limit of the plot
#' @param ylims y limit of the plot
#' @param label label for the plot
#' @param ... reserved parameters
#'
#' @export
#'
baPltAllTcell <- function(lst.par, xlims = list(c(-1,1), c(0,2000), c(0,20)),
                          ylims = c(9, 0.2, 0.3),
                          label = "Scenario", ...) {
    lst.plt <- NULL;
    labels  <- NULL;
    for (i in 1:length(lst.par)) {
        cur.plt <- baPltTcell(lst.par[[i]]$PAR.ERR, lst.par[[i]]$PAR.OTHER, ...);

        for (j in 1:length(cur.plt))
            cur.plt[[j]] <- cur.plt[[j]] +
                scale_x_continuous(limits = xlims[[j]]) +
                scale_y_continuous(limits = c(0, ylims[j]));

        lst.plt <- c(lst.plt, cur.plt);
        labels  <- c(labels, paste(label, as.roman(i), sep = " "),  "", "");
    }

    do.call(plot_grid, c(lst.plt,
                         nrow = length(lst.par),
                         scale = 0.9, vjust = 1.1, hjust = -0.2, label_size = 12,
                         list(labels = labels)));
}
