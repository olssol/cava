#' Get designs for BetaBinomial designs
#'
#' @param par.design design parameters (ALPHA, POWER, P0, P1)
#' @param rho inter-class correlation 1/(a+b+1)
#' @param bsizes batch size, can be list or an integer
#' @param nmin minimum sample size
#' @param nmax maximum sample size
#' @param seed Seed
#' @param ... reserved parameters
#' 
#' @return a list of simon's two stage designs containing the optimal and
#'     minimax designs for all possible batch sizes 
#' 
#' @export
#'
baDesignBetaBin <- function(par.design, rho = 0, bsizes = 1, nmin = 10, nmax = 100,
                            ..., seed = NULL) {

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    rho <- rep(rho, 2)
    yp0 <- baSimuBetaBin(max(bsizes), p = par.design$P0, rho = rho[1], ...)$y
    yp1 <- baSimuBetaBin(max(bsizes), p = par.design$P1, rho = rho[2], ...)$y

    rst <- NULL;
    for (i in 1:length(bsizes)) {
        cur.rst         <- bacSimonDesign(yp0, yp1, nmax, nmin, bsizes[i],
                                         par.design$ALPHA, 1 - par.design$POWER);
        cur.rst        <- data.frame(cur.rst);
        cur.rst$design <- c("Optimal", "Minimax");
        cur.rst$bsize  <- bsizes[i];
        rst            <- rbind(rst, cur.rst);
    }

    rst$rho0  <- rho[1];
    rst$rho1  <- rho[2];
    rst$p0    <- par.design$P0;
    rst$p1    <- par.design$P1;
    rst$alpha <- par.design$ALPHA;
    rst$beta  <- par.design$BETA;

    ## return
    if (!is.null(seed))
        set.seed(old_seed)

    rownames(rst) <- NULL
    rst
}

#' Get type I error and power given y0, y1, n1, r1, n, r
#'
#' @param yp0 cumulative binomial distribution of (?)
#' @param yp1 cumulative binomial distribution of (?)
#' @param n1 first stage sample size
#' @param r1 first stage response rate
#' @param nt total sample size
#' @param r  second stage response rate
#' @param bsize batch size
#'
#' @return a vector containing error, power, en0, pet0, en1, pet1
#' 
#' @export
#'
baGetRejRate <- function(yp0, yp1, n1, r1, nt, r, bsize = 1) {

    stopifnot(ncol(yp0) >= bsize &
              ncol(yp1) >= bsize);

    cumu0 <- bacCumProb(yp0, nt, n1, bsize);
    cumu1 <- bacCumProb(yp1, nt, n1, bsize);
    cp0   <- bacSimonSingle(cumu0, n1, r1, nt, r);
    cp1   <- bacSimonSingle(cumu1, n1, r1, nt, r);

    c(type1 = cp0[3], power = cp1[3],
      en0   = cp0[1], en1   = cp1[1],
      pet0  = cp0[2], pet1  = cp1[2]);
}

#' Evaluate design sensitivity
#'
#' Get type I error and power given p0, p1, rho, batch size, n1, r1, n, r for
#' one rho
#'
#' @param rho inter-class correlation
#' @param p0 null hypothesis probability
#' @param p1 alternative hypothesis probability
#' @param bsize batch size
#' @param n1 first stage sample size
#' @param r1 first stage response rate
#' @param nt total sample size
#' @param r  second stage response rate
#' @param ... reserved parameters
#' 
#' @return a vector containing error, power, en0, pet0, en1, pet1 for the given
#'     rho
#'
#' @export
#'
baDesignSensSingle <- function(rho, p0, p1, bsize, n1, r1, nt, r, ...) {
    rho   <- rep(rho, 2)
    yp0   <- baSimuBetaBin(bsize, p = p0, rho = rho[1], ...)$y
    yp1   <- baSimuBetaBin(bsize, p = p1, rho = rho[2], ...)$y

    cumu0 <- bacCumProb(yp0, nt, n1, bsize)
    cumu1 <- bacCumProb(yp1, nt, n1, bsize)
    cp0   <- bacSimonSingle(cumu0, n1, r1, nt, r)
    cp1   <- bacSimonSingle(cumu1, n1, r1, nt, r)

    c(type1 = cp0[3], power = cp1[3],
      en0   = cp0[1], en1   = cp1[1],
      pet0  = cp0[2], pet1  = cp1[2])
}

#' Evaluate design sensitivity
#'
#' Get type I error and power given p0, p1, rho, batch size, n1, r1, n, r
#'
#' @param rhos a vector inter-class correlation 1/(a+b+1)
#' @param design A list with p0, p1, batch size, n1, r1, n, r
#' @param ... reserved parameters
#' @param seed Seed
#' 
#' @export
#'
baDesignSens <- function(rhos, design, ..., seed = NULL) {
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    rst <- NULL
    for (i in 1:length(rhos)) {
        rho     <- rep(rhos[i], 2)
        cur_rst <- with(design, {
            yp0   <- baSimuBetaBin(bsize, p = p0, rho = rho[1], ...)$y
            yp1   <- baSimuBetaBin(bsize, p = p1, rho = rho[2], ...)$y

            cumu0 <- bacCumProb(yp0, n, n1, bsize)
            cumu1 <- bacCumProb(yp1, n, n1, bsize)
            cp0   <- bacSimonSingle(cumu0, n1, r1, n, r)
            cp1   <- bacSimonSingle(cumu1, n1, r1, n, r)

            c(rho   = rhos[i],
              type1 = cp0[3], power = cp1[3],
              en0   = cp0[1], en1   = cp1[1],
              pet0  = cp0[2], pet1  = cp1[2])
        })

        rst <- rbind(rst, cur_rst)
    }

    if (!is.null(seed))
        set.seed(old_seed)

    rownames(rst) <- NULL
    data.frame(rst)
}


## #' Get actuarial type I and II error in Sargent's method
## baSaAlphaBeta <- function(bsizes, r, p0, p1, rho = 0) {
##     v0    <- baSaGetVar(bsizes, p0, rho);
##     v1    <- baSaGetVar(bsizes, p1, rho);
##     n     <- sum(bsizes);
##     m0    <- n * p0;
##     m1    <- n * p1;

##     alpha <- 1 - pnorm( (r + 1.5 - m0)/sqrt(v0) );
##     beta  <- pnorm( (r + 1.5 - m1)/sqrt(v1) );

##     c(alpha, beta);
## }
