#' Cancer Vaccine Clinical Trial Design
#'
#' @docType package
#' @name cava-package
#' @aliases cava
#' ## @useDynLib cava, .registration = TRUE
#' @useDynLib cava
#'
#' @importFrom rstan sampling extract stanc rstan_options traceplot stan_rhat
#' @importFrom grDevices colors
#' @importFrom graphics axis box legend lines par plot points text arrows grid rect
#' @importFrom parallel detectCores
#' @importFrom utils as.roman
#' @importFrom lme4 lmer VarCorr
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous aes geom_histogram labs theme_bw theme element_blank geom_density geom_vline scale_color_manual scale_linetype_manual element_blank stat_density
#' @importFrom cowplot plot_grid save_plot
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @import stats
#' @import Rcpp
#' @import methods
#'
#' @description
#'
#' This package contains the functions for implementing the \strong{visit}
#' design for Phase I cancer vaccine trials and the \strong{basim} design for
#' batch-effect adjusted Simon's two-stage design.
#'
#' @section Background:
#'
#' Phase I clinical trials are the first step in drug development to apply a new
#' drug or drug combination on humans. Typical designs of Phase I trials use
#' toxicity as the primary endpoint and aim to find the maximum tolerable
#' dosage. However, these designs are generally inapplicable for the development
#' of cancer vaccines because the primary objectives of a cancer vaccine Phase I
#' trial often include determining whether the vaccine shows biologic activity.
#'
#' The \strong{visit} design allows dose escalation to simultaneously account
#' for immunogenicity and toxicity. It uses lower dose levels as the reference
#' for determining if the current dose level is optimal in terms of immune
#' response. It also ensures subject safety by capping the toxicity rate with a
#' given upper bound. These two criteria are simultaneously evaluated using an
#' intuitive decision region that avoids complicated safety and immunogenicity
#' trade-off elicitation from physicians.
#'
#' There are several considerations that are clinically necessary for developing
#' the design algorithm. First, we assume that there is a non-decreasing
#' relationship that exists between toxicity and dosage, i.e., the toxicity risk
#' does not decrease as dose level increases. Second, the immune response rate
#' may reach a plateau or even start to decline as the dose level increases.
#'
#' @section Notation:
#'
#' For subject \eqn{s}, let \eqn{D_s=l} (\eqn{l=1,\ldots,L}) denote the received
#' dose level, \eqn{T_s=1} if any DLT event is observed from the subject and
#' \eqn{0} otherwise, \eqn{R_s=1} if immune response is achieved for the subject
#' and \eqn{0} otherwise.
#'
#' Let \eqn{\theta^{(l)}_{ij}=P(T=i, R=j|D=l)} for \eqn{i,j=0,1},
#' \eqn{\theta^{(l)}=\{\theta_{ij}^{(l)}:i,j=0,1\}} and \eqn{\Theta =
#' \{\theta^{(l)}: l=1,\ldots,L\}}. Furthermore, for dose level \eqn{l}, let
#' \eqn{p^{(l)}=P(T=1|D=l)=\theta_{10}^{(l)}+\theta_{11}^{(l)}} be the DLT risk,
#' \eqn{q^{(l)}=P(R=1|D=l)=\theta_{01}^{(l)}+\theta_{11}^{(l)}} be the immune
#' response probability, and
#' \eqn{r^{(l)}=\theta_{00}^{(l)}\theta_{11}^{(l)}/\theta_{01}^{(l)}\theta_{10}^{(l)}}
#' be the odds ratio. Let \eqn{n_{ij}^{(l)}} be the observed number of subjects
#' with \eqn{T=i} and \eqn{R=j} at dose level \eqn{l},
#' \eqn{n^{(l)}=\{n_{ij}^{(l)}:i,j=0,1\}} and \eqn{H} denote all the data
#' observed by the time the current analysis is conducted.
#'
#' @section Dose escalation algorithm:
#'
#' The dose escalation algorithm is based on the posterior probability
#' distribution of \eqn{\pi(p^{(l)}, q^{(l)}|H)}, where \eqn{p^{(l)}} and
#' \eqn{q^{(l)}} represent the DLT risk and immune response rate, respectively,
#' of the current dose level \eqn{l}, and \eqn{H} denotes the cumulative data at
#' the time of interim analysis.
#'
#' Let \eqn{p_l} denote the lower boundary of DLT risk below which the dose is
#' considered absolutely safe, \eqn{p_u} denote the upper boundary of DLT risk
#' above which the dose is considered toxic. \strong{visit} implements a sequential
#' identification approach based on conditional probabilities derived from
#' \eqn{\pi(p^{(l)}, q^{(l)}|H)}. Let \eqn{C_1, C_2, C_3} be fixed cut-off
#' values in \eqn{[0,1]}. The steps are as follows:
#'
#' \describe{
#'
#' \item{Step 1.}{If \eqn{Prob(p^{(l)} > p_U|H) > C_1}, then the current dose level is
#' considered to be \strong{too toxic}. The trial should be stopped and the next lower
#' dose level should be reported as the recommended dose.}
#'
#' \item{Step 2.}{\eqn{Prob(q^{(l)} \leq q_L| p^{(l)} \leq p_U, H) > C_2}, then the
#' current dose level is considered to be \strong{no more effective than its lower dose}
#' levels. The trial should be stopped and the next lower dose level should be
#' reported as the recommended dose.}
#'
#' \item{Step 3.}{If \eqn{Prob(p^{(l)} \leq p_L| p^{(l)} \leq p_U, q^{(l)} >
#' q_L, H) > C_3}, then the current dose level is considered to be \strong{safe and
#' effective}. The trial will escalate to dose level \eqn{l+1}.}

#'
#' \item{Step 4.}{The current dose level is considered to be \strong{uncertain}. The
#' trial should continue to treat more patients at dose level \eqn{l}.}
#'
#' }
#'
#' The values of should be chosen \eqn{C_1, C_2, C_3} prior to study initiation
#' and reflect the considerations of the investigators and patients. These
#' thresholds should also give reasonable overall study operating
#' characteristics.
#'
#' We can see that, based on the posterior distribution of \eqn{\pi(p^{(l)},
#' q^{(l)}|H)}, the currently dose level is in one of the four regions:
#' \strong{1: too toxic}, \strong{2: no more effective than its lower dose},
#' \strong{3: safe and effective}, and \strong{4: uncertain}. These regions are termed
#' as a \code{Decision Map}.
#'
#'
#' @section Probability models:
#'
#' \strong{visit} provides several options for the probability models that can
#' be considered for Bayesian inference. The models are non-decreasing with
#' respect to the dose-toxicity relationship and avoid monotonic assumptions for
#' the dose-immune response curve.
#'
#' \subsection{Non-parametric model}{As one of the simplest models, we posit
#' no assumptions on the dose-toxicity or dose-immune response relationships and
#' assume the outcome data \eqn{n_{00}, n_{01}, n_{10}, n_{11}} follow a
#' multinomial distribution.
#' }
#'
#' \subsection{Non-parametric+ model}{This is the simplified
#' \strong{non-parametric} model with the odds ratios \eqn{r=1}. }
#'
#' \subsection{Partially parametric model}{Compared to non-parametric models, a
#' parametric model may allow the incorporation of dose-toxicity, dose-efficacy,
#' and toxicity-efficacy relationships in dose escalation. In the context of
#' evaluating cancer vaccines, however, it is difficult to posit assumptions on
#' the dose-efficacy relationship, since the immune response rate may even
#' decrease as the dose level increases. On the other hand, it remains
#' reasonable to assume that the dose-toxicity curve is non-decreasing.
#' Therefore, we propose a partially parametric model that only makes
#' assumptions about dose-toxicities but leaves the dose-immune response
#' relationship unspecified.
#'
#' Specifically, we construct the dose-toxicity model as: \deqn{ \log p^{(l)}=
#' e^\alpha \log \tau^{(l)}. } The \eqn{\tau^{(l)}}'s are deterministic design
#' parameters reflecting the expectation of the DLT risk at dose level \eqn{l}
#' with \eqn{\tau^{(l)} > \tau^{(l')}} for \eqn{l> l'}.
#'
#' For the immune response and the odds ratio, we assume \eqn{q^{(l)}} and
#' \eqn{r^{(l)}} at different dose levels are independent a priori.
#' }
#'
#' \subsection{Partially parametric+ model}{This is the simplified
#' \strong{partially parametric} model with the odds ratios \eqn{r=1}.}
#'
#' @section Graphical user interface:
#'
#' This package provides a web-based graphical user interface developed using R
#' Shiny. See \code{\link{cvShiny}} for details.
#'
#' @references
#'
#' Wang, C., Rosner, G. L., & Roden, R. B. (2019). A Bayesian design for phase I cancer
#' therapeutic vaccine trials. Statistics in medicine, 38(7), 1170-1189.
NULL

## @importFrom clinfun ph2simon


#' Parameters
#'
#' Parameters that are shared by multiple functions
#'
#' @name parameters
#'
#' @param obs.y Observed data matrix with \eqn{l} rows and 4 columns. Row \eqn{k} in the matrix
#'     represents the observed data from dose level \eqn{k}. The columns are
#'
#' \itemize{
#'     \item{column 1: }{number of patient with no DLT, no immune response}
#'     \item{column 2: }{number of patient with no DLT, immune response}
#'     \item{column 3: }{number of patient with DLT, no immune response}
#'     \item{column 4: }{number of patient with DLT, immune response}
#' }
#'
#' @param prob.mdl Option of the probability models:
#'
#' \itemize{\item{NONPARA: }{non-parametric+ model}
#'
#'  \item{NONPARA+: }{non-parametric model}
#'
#' \item{PARA: }{partially parametric model}
#'
#' \item{PARA+: }{partially parametric+ model} }
#'
#' Default value is \code{NONPARA}. See \code{\link[cava]{parameters}} for details.
#'
#' @param priors A class \code{VTPRIOR} object created by
#'     \code{\link{vtPriorPar}} for \code{PARA} and \code{PARA+} model.
#'
#'
#' @param etas Vector of length 2 representing \eqn{(p_L, p_U)}. \eqn{p_L}: lower
#'     bound of DLT risk, below which the current dose is considered absolutely
#'     safe; \eqn{p_U}: upper bound of DLT risk above which the current dose is
#'     considered too toxic
#'
#' @param prev.res Response rate from the next lower dose level, say, \eqn{l-1}.
#'     This can be a scalar representing the mean of the response rate
#'     \eqn{E(q^{(l-1)})}, or a vector of posterior samples of the response rate
#'     \eqn{q^{(l-1)}}. For \eqn{l=1}, this value is set to \eqn{0}.
#'
#' @param dec.cut Thresholds \eqn{C_1,C_2,C_3}. If the vector length is shorter
#'     than \eqn{3}, it is repeated to have \eqn{3} elements. See
#'     \code{\link[cava]{parameters}} for details.
#'
#' @param digits Digits for print
#'
#' @param seed Random seed
#'
#' @param ... Reserved parameters
#'
NULL


#' Example dataset
#'
#' @description An example dataset from an assay experiment for eliciting ICC
#'
#' @docType data
#'
#' @name ex_data
#'
#' @usage data(ex_data)
#'
#' @format A dataframe with the following variables:
#' \describe{
#'   \item{BatchID}{Batch ID}
#'   \item{Week}{Week when the experiment was done, i.e., time effect}
#'   \item{Concentration}{Concentration level}
#'   \item{Y}{Log transformed outcome}
#' }
#'
NULL
