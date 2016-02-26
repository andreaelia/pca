\name{TQresidualshiny}
\alias{TQresidualshiny}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function for creating the T2 vs Q residual
}
\description{
This function works within the pca shiny application. It works with the pcashiny function and use the data set to create the T2 vs Q residual plot, together with the contribution plot
}
\usage{
TQresidualshiny(data, centering, scaling, pc_number, labels, point_dim, legend_name, LegendPos, legend_dim, Title, text.row, text.labels, point_type, CP_Point, CP_txt_Point, PLOT, CP_dim_var, CP_las)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data matrix
}
  \item{centering}{
TRUE or FALSE
}
  \item{scaling}{
TRUE or FALSE
}
  \item{pc_number}{
number of principal component
}
  \item{labels}{
vector of scores labels
}
  \item{point_dim}{
dimension of scores
}
  \item{legend_name}{
name of the legend
}
  \item{LegendPos}{
position of the legend
}
  \item{legend_dim}{
dimension of the legend
}
  \item{Title}{
title of the plot
}
  \item{text.row}{
TRUE or FALSE
}
  \item{text.labels}{
vector for texting the scores
}
  \item{point_type}{
type of point
}
  \item{CP_Point}{
cp point
}
  \item{CP_txt_Point}{
cp text point
}
  \item{PLOT}{
TRUE or FALSE
}
  \item{CP_dim_var}{
variable dimension cp
}
  \item{CP_las}{
cp las
}
}
\details{
There are no more details
}

\references{
package.skeleton code
}
\author{
Elia Andrea
}
\note{
There are no further notes
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, centering, scaling, pc_number, labels, point_dim, 
    legend_name, LegendPos, legend_dim, Title, text.row, text.labels, 
    point_type, CP_Point, CP_txt_Point, PLOT, CP_dim_var, CP_las) 
{
    if (missing(CP_dim_var)) {
        CP_dim_var = 0.8
    }
    if (missing(CP_las)) {
        CP_las = 2
    }
    if (CP_las == TRUE) {
        CP_las = 2
    }
    if (CP_las == FALSE) {
        CP_las = 1
    }
    if (missing(text.labels)) {
        text.labels = row.names(data)
    }
    X <- scale(data, center = centering, scale = scaling)
    row.names(X) <- c(1:nrow(X))
    nc <- ncol(X)
    X.cov <- cov(X)
    T <- eigen(X.cov)
    L <- T[[1]]
    V <- sqrt(L[1:pc_number])
    sgt <- sum(apply(X, 2, "var"))
    T <- T[[2]]
    S <- as.matrix(X) \%*\% T
    PCA <- list(dataset = X, sdev = V, sgt = sgt, loadings = T, 
        scores = S, center = centering, scale = scaling, n.obs = nc)
    m <- ncol(X)
    if (missing(pc_number) == FALSE) {
        ncp <- pc_number
    }
    if (missing(pc_number)) {
        ncp = ncol(PCA$loadings) - 1
    }
    if (missing(legend_dim)) {
        legend_dim <- 0.5
    }
    if (missing(legend_name)) {
        legend_name = "Group"
    }
    n <- nrow(PCA$dataset)
    X <- matrix(PCA$dataset, nrow = n, ncol = m)
    P <- PCA$loadings[, 1:ncp]
    L <- (PCA$sdev[1:ncp])^2
    sgl <- sum(L)
    sgr <- PCA$sgt - sgl
    MQ <- diag(rep(1, m)) - (P \%*\% t(P))
    MT <- P \%*\% (diag(1/L)) \%*\% t(P)
    Q <- diag(X \%*\% MQ \%*\% t(X))
    T <- diag(X \%*\% MT \%*\% t(X))
    Tlim <- (n - 1) * ncp/(n - ncp) * qf(0.95, ncp, n - ncp)
    if (is.na(Tlim)) 
        Tlim <- 0
    mT <- max(T, Tlim)
    t1 <- sgr
    t2 <- sgr^2
    t3 <- sgr^3
    h0 = 1 - 2 * t1 * t2/3/t3^2
    Qlim <- t1 * (1 + h0 * qnorm(0.95) * sqrt(2 * t2)/t1 + t2 * 
        h0 * (h0 - 1)/t1^2)^(1/h0)
    if (is.na(Qlim)) 
        Qlim <- 0
    mQ <- max(Q, Qlim)
    liv <- factor(labels, ordered = TRUE)
    if (missing(text.row)) {
        text.row = FALSE
    }
    if (PLOT == "Diagnostic Plot T2 vs Q Residuals") {
        if (missing(labels)) {
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                    0.2))
                  par(mar = c(5, 4, 4, 2) + 0.1)
                }
            }
            if (text.row == FALSE) {
                plot(T, Q, ylim = c(0, mQ * 1.1), xlim = c(0, 
                  mT * 1.1), ylab = "Q Residuals", xlab = "T^2 Hotelling Index", 
                  cex = point_dim, cex.lab = 1.2, col = labels, 
                  pch = point_type)
                title(main = paste("T^2 vs Q residuals, Comp. Number:", 
                  ncp), sub = "Confidence: 95\%", cex.sub = 0.6)
                grid()
                if (Tlim != 0) 
                  abline(v = Tlim, lty = 2, col = "red")
                if (Qlim != 0) 
                  abline(h = Qlim, lty = 2, col = "red")
                if ((Tlim != 0) & (Qlim != 0)) {
                  QT <- data.frame(Q = Q, T = T, tx = row.names(data))
                  QTs <- subset(QT, ((T > Tlim) & (Q > Qlim)))
                  if (nrow(QTs) != 0) 
                    text(QTs$T, QTs$Q, label = QTs$tx, cex = 0.5, 
                      pos = 3)
                }
            }
            if (text.row == TRUE) {
                plot(T, Q, type = "n", ylim = c(0, mQ * 1.1), 
                  xlim = c(0, mT * 1.1), ylab = "Q Residuals", 
                  xlab = "T^2 Hotelling Index")
                title(main = paste("T^2 vs Q residuals, Comp. Number:", 
                  ncp), sub = "Confidence: 95\%", cex.sub = 0.6)
                if (Tlim != 0) 
                  abline(v = Tlim, lty = 2, col = "red")
                if (Qlim != 0) 
                  abline(h = Qlim, lty = 2, col = "red")
                col_text <- c(1:length(levels(liv)))
                text(T, Q, labels = text.labels, cex = point_dim, 
                  col = col_text[liv])
                grid()
            }
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  par(mar = c(5, 0, 4, 2) + 0.1)
                  plot(1:3, rnorm(3), pch = 1, lty = 1, ylim = c(-2, 
                    2), type = "n", axes = FALSE, ann = FALSE)
                  legend(1, 1, col = unique(liv), unique(liv), 
                    pch = unique(point_type), bty = "n", cex = legend_dim, 
                    title = legend_name)
                }
                if (!LegendPos == FALSE) {
                  legend(LegendPos, col = unique(liv), legend = unique(liv), 
                    pch = unique(point_type), bty = "n", cex = legend_dim, 
                    title = legend_name)
                }
            }
        }
        if (missing(labels) == FALSE) {
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                    0.2))
                  par(mar = c(5, 4, 4, 2) + 0.1)
                }
            }
            if (text.row == FALSE) {
                plot(T, Q, ylim = c(0, mQ * 1.1), xlim = c(0, 
                  mT * 1.1), ylab = "Q Residuals", xlab = "T^2 Hotelling Index", 
                  cex = point_dim, cex.lab = 1.5, pch = point_type, 
                  col = liv)
                title(main = paste("T^2 vs Q residuals, Comp. Number:", 
                  ncp), sub = "Confidence: 95\%", cex.main = 1.5, 
                  cex.sub = 1)
                grid()
                if (Tlim != 0) 
                  abline(v = Tlim, lty = 2, col = "red")
                if (Qlim != 0) 
                  abline(h = Qlim, lty = 2, col = "red")
                if ((Tlim != 0) & (Qlim != 0)) {
                  QT <- data.frame(Q = Q, T = T, tx = row.names(data))
                  QTs <- subset(QT, ((T > Tlim) & (Q > Qlim)))
                  if (nrow(QTs) != 0) 
                    text(QTs$T, QTs$Q, label = QTs$tx, cex = 0.5, 
                      pos = 3)
                }
            }
            if (text.row == TRUE) {
                plot(T, Q, type = "n", ylim = c(0, mQ * 1.1), 
                  xlim = c(0, mT * 1.1), ylab = "Q Residuals", 
                  xlab = "T^2 Hotelling Index", cex = point_dim, 
                  cex.lab = 1.5, pch = point_type, col = liv)
                title(main = paste("T^2 vs Q residuals, Comp. Number:", 
                  ncp), sub = "Confidence: 95\%", cex.main = 1.5, 
                  cex.sub = 1)
                if (Tlim != 0) 
                  abline(v = Tlim, lty = 2, col = "red")
                if (Qlim != 0) 
                  abline(h = Qlim, lty = 2, col = "red")
                col_text <- c(1:length(levels(liv)))
                text(T, Q, labels = text.labels, cex = point_dim, 
                  col = col_text[liv])
                grid()
            }
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  par(mar = c(5, 0, 4, 2) + 0.1)
                  plot(1:3, rnorm(3), pch = 1, lty = 1, ylim = c(-2, 
                    2), type = "n", axes = FALSE, ann = FALSE)
                  legend(1, 1, col = unique(liv), unique(liv), 
                    pch = unique(point_type), bty = "n", cex = legend_dim, 
                    title = legend_name)
                }
                if (!LegendPos == FALSE) {
                  legend(LegendPos, col = unique(liv), legend = unique(liv), 
                    pch = unique(point_type), bty = "n", cex = legend_dim, 
                    title = legend_name)
                }
            }
        }
    }
    Res <- list(dataset = X, sdev = V, sgt = sgt, loadings = T, 
        scores = S, center = centering, scale = scaling, n.obs = nc)
    QT <- data.frame(Q = Q, T = T, tx = row.names(data))
    QTs <- subset(QT, ((T > Tlim) & (Q > Qlim)))
    if (PLOT == "Contribution Plot") {
        if (missing(CP_Point) == FALSE) {
            if (is.vector(CP_Point) == TRUE) {
                S <- Res$scores[, 1:ncp]
                MQ <- S \%*\% t(P)
                MT <- P \%*\% (diag(1/L)) \%*\% t(P)
                T <- X \%*\% MT
                Q <- sign(X - MQ) * (X - MQ)^2
                colnames(T) <- colnames(data)
                colnames(Q) <- colnames(data)
                Ti <- T[CP_Point, ]
                Qi <- Q[CP_Point, ]
                lim_Qi <- max(abs(min(Qi)), max(Qi))
                lim_Ti <- max(abs(min(Ti)), max(Ti))
                op <- par(mfrow = c(1, 2))
                barplot(Qi, main = paste("ContrPlot Q point:", 
                  CP_txt_Point), ylim = c(-lim_Qi, lim_Qi), col = 4, 
                  cex.lab = 1.2, cex.names = CP_dim_var, las = CP_las, 
                  plot.grid = TRUE, cex.axis = 0.6)
                barplot(Ti, main = paste("ContrPlot Ti^2 point:", 
                  CP_txt_Point), ylim = c(-lim_Ti, lim_Ti), col = 4, 
                  cex.lab = 1.2, cex.names = CP_dim_var, las = CP_las, 
                  plot.grid = TRUE, cex.axis = 0.6)
                par(op)
                Res <- list(dataset = X, sdev = V, sgt = sgt, 
                  loadings = T, scores = S, center = centering, 
                  scale = scaling, n.obs = nc, T = T, Q = Q)
            }
        }
    }
    Res
  }
}

\keyword{ data }
