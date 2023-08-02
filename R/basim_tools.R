
#' Get batch indices
#'
#' @param batch.sizes a vector of batch sizes
#' 
#' @return a vector where all subjects in batch.sizes are denoted by its batch
#'     number, and the length of this vector is total number of subjects. 
#' 
#' @export
#'
baBatInd <- function(batch.sizes) {
    nbatch <- length(batch.sizes);
    bis    <- apply(cbind(1:nbatch, batch.sizes),
                    1,
                    function(x) rep(x[1], x[2]));
    bis    <- as.numeric(unlist(bis));
}


#' Define outcome based on longitudinal outcomes
#'
#' @param mat.y outcomes represented as a matrix
#' @param type type of longitudinal outcomes
#'
#' @return if type is "base", the first column is returned. Otherwise
#'     the ratio of last column and first column is returned.
#'
#' @export
#'
baGetOutcome <- function(mat.y, type = c("ratio", "change", "base")) {
    type <- match.arg(type);
    switch(type,
           "ratio"  = mat.y[,ncol(mat.y)] / mat.y[,1],
           "change" = mat.y[,ncol(mat.y)] / mat.y[,1],
           "base"   = mat.y[,1]);
}


#' Simulate NB using mu and phi instead of n and phi
#'
#' @param mu mean
#' @param phi target for number of successful trials
#' @param n (Optional) number of observations
#' @param is.log logical; if TRUE, mu, phi are are given as log(mu) and log(phi)
#' 
#' @return Negative Binomial Distribution with n as length of mu
#' 
#' @export
#'
baRNB <- function(mu, phi, n = NULL, is.log = TRUE) {

    if (is.null(n))
        n <- length(mu);

    if (is.log) {
        mu  <- exp(mu);
        phi <- exp(phi);
    }

    rnbinom(n, size = phi, prob = phi/(mu + phi));
}



#' Fit mixed effect model to evaluate batch effect
#'
#' @param dat existing experiment data
#' @param fml mixed model formula that has batch factor with random effect
#' @param des_mu design vector for obtaining mean. By default, this will be the intercept
#' @param nbs number of bootstraps
#'
#' @return A matrix with 3 columns and \code{nbs} rows. The columns correspond
#'     to 1) standard deviation of the batch effect, 2) standard deviation of
#'     the random error, and 3) mean
#' @export
#'
baElicitFitMix <- function(dat, fml, des_mu = 1, nbs = 1000) {
    n_pt <- nrow(dat)
    rst  <- NULL
    for (i in seq_len(nbs)) {
        if (i > 1) {
            cur_dta <- dat[sample(1:n_pt, replace = TRUE), ]
        } else {
            cur_dta <- dat
        }

        fit_rst <- lmer(fml, data = cur_dta)

        sig_B <- attr(VarCorr(fit_rst)[[1]], "stddev")
        sig   <- attr(VarCorr(fit_rst),     "sc")

        beta  <- fit_rst@beta
        if (1 == i) {
            if (length(des_mu) < length(beta))
                des_mu <- c(des_mu,
                            rep(0, length.out = length(beta) - length(des_mu)))
        }

        mu0   <- sum(des_mu * beta)
        rst   <- rbind(rst, c(sig_B, sig, mu0))
    }

    rst
}

#' Elicit ICC based on batch effect and random error standard deviation
#'
#' For vaccine studies using pre- vs. post_vaccine proliferation ratio as the
#' endpoint
#'
#' @param pars parameters with 3 columns. 1) std_batch std of the batch effect
#'     2) std_err: std of the random error and 3) mu intercept in the
#'     proliferation model
#' @param p_resp response rate
#' @param bsize batch size
#' @param beta beta coefficient in the proliferation model
#' @param ntest number of replications
#' @param threshold threshold to be considered response
#' @param take_exp whether the proliferation model is log transformed
#' @param seed Seed 
#' 
#' @return Estimated ICC with its (bootstrap) SD.
#'
#' @export
#'
baElicitICC <- function(pars, p_resp, bsize = 5,
                        beta = NULL, ntest = 10000,
                        threshold = 2, take_exp = TRUE,
                        seed = NULL) {

    f_ratio <- function(y0, y1, b_err0, b_err1) {
        if (take_exp) {
            ratio <- exp(y1 + b_err1) / exp(y0 + b_err0)
        } else {
            ratio <- (y1 + b_err1) / (y0 + b_err0)
        }
        ratio
    }

    f_beta <- function(sig_B, sig, mu0) {
        f_dif <- function(beta) {
            y1    <- rnorm(ntest, beta * y0, sig)
            ratio <- f_ratio(y0, y1, b0, b1)
            p_resp - mean(ratio > threshold)
        }

        y0    <- rnorm(ntest, mu0, sig)
        b0    <- rnorm(ntest, 0,  sig_B)
        b1    <- rnorm(ntest, 0,  sig_B)

        rst   <- uniroot(f_dif, c(0, 2))$root
        rst
    }

    f_icc <- function(beta, sig_B, sig, mu0) {
        all_ratio <- NULL
        for (b in seq_len(ntest)) {
            y0        <- rnorm(bsize, mu0, sig);
            y1        <- rnorm(bsize, beta * y0, sig);
            b01       <- rnorm(2, 0, sig_B);
            ratio     <- f_ratio(y0, y1, b01[1], b01[2])
            all_ratio <- rbind(all_ratio, ratio);
        }
        bacICC(all_ratio)
    }

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    cur_icc <- apply(pars, 1,
        function(x) {
            print(x)
            beta <- f_beta(x[1], x[2], x[3])
            icc  <- f_icc(beta, x[1], x[2], x[3])
        }
    )
    cur_sd <- sd(cur_icc, na.rm = TRUE)

    if (is.null(seed))
        set.seed(old_seed)

    return(list(
        icc_sd = c(cur_icc[1], cur_sd),
        all_icc = cur_icc
    ))
}
