\name{pcashiny}
\alias{pcashiny}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to make the PCA analysis
}
\description{
This function works within the shiny application for PCA. It works on a table data and computes pca according to scaling and centering preprocessing data. The output are in the form of scores and loadings plot, cumulative and explained variance, T2 vs Q residual plot.
}
\usage{
pcashiny(data, pca_type, centering, scaling, labels, var_col_group, pc_number, covmat, CVsegments, CVsegment.type, header, point_dim, var_dim, legend_dim, legend_name, text.row, text.labels, Title, point_type, CP_Point, CP_txt_Point, PLOT, pc_axis_x, pc_axis_y, LegendPos, CP_dim_var, CP_las, CE, CE_level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
two-dimensional data matrix
}
  \item{pca_type}{
type of pca
}
  \item{centering}{
TRUE or FALSE
}
  \item{scaling}{
TRUE or FALSE
}
  \item{labels}{
vector of labels for the scores (color)
}
  \item{var_col_group}{
vector of colours of variables
}
  \item{pc_number}{
number of principal component
}
  \item{covmat}{
covariance matrix
}
  \item{CVsegments}{
number of CV segments
}
  \item{CVsegment.type}{
type of CV segment
}
  \item{header}{
TRUE or FALSE
}
  \item{point_dim}{
scores point dimension
}
  \item{var_dim}{
variable dimension
}
  \item{legend_dim}{
legend dimension
}
  \item{legend_name}{
legend name
}
  \item{text.row}{
TRUE or FALSE for the scores
}
  \item{text.labels}{
vector for texting the scores
}
  \item{Title}{
title of the plot
}
  \item{point_type}{
type of point
}
  \item{CP_Point}{
cp point
}
  \item{CP_txt_Point}{
cp txt point
}
  \item{PLOT}{
TRUE or FALSE
}
  \item{pc_axis_x}{
pc for the x axis
}
  \item{pc_axis_y}{
pc for the y axis
}
  \item{LegendPos}{
position of the legend
}
  \item{CP_dim_var}{
cp dimension of variables
}
  \item{CP_las}{
cp las
}
  \item{CE}{
confidence interval
}
  \item{CE_level}{
percentage of confidence level
}
}
\details{
There are no more detailes
}

\references{
There are no references
}
\author{
Elia Andrea
}
\note{
there are no notes
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, pca_type, centering, scaling, labels, var_col_group, 
    pc_number, covmat, CVsegments, CVsegment.type, header, point_dim, 
    var_dim, legend_dim, legend_name, text.row, text.labels, 
    Title, point_type, CP_Point, CP_txt_Point, PLOT, pc_axis_x, 
    pc_axis_y, LegendPos, CP_dim_var, CP_las, CE, CE_level) 
{
    palette("default")
    if (missing(var_col_group)) {
        var_col_group <- c(rep(1, ncol(data)))
    }
    if (missing(pc_axis_x)) {
        pc_axis_x = 1
    }
    if (missing(pc_axis_y)) {
        pc_axis_y = 2
    }
    if (missing(LegendPos)) {
        LegendPos = FALSE
    }
    if (!LegendPos == FALSE && !LegendPos == "bottomright" && 
        !LegendPos == "bottom" && !LegendPos == "bottomleft" && 
        !LegendPos == "left" && !LegendPos == "topleft" && !LegendPos == 
        "top" && !LegendPos == "topright" && !LegendPos == "right" && 
        !LegendPos == "center") {
        stop("LegendPos should be one of FALSE, bottomright, bottom, bottomleft, \n                                                              left, topleft, top, topright, right, center")
    }
    if (missing(CE)) {
        CE = FALSE
    }
    if (missing(CE_level)) {
        CE_level = 95
    }
    if (missing(PLOT)) {
        PLOT = FALSE
    }
    data <- na.omit(data)
    if ((is.matrix(data) == FALSE) & (is.data.frame(data) == 
        FALSE)) {
        stop("Error: data should be in dataframe or matrix form")
    }
    if (missing(header)) {
        header = FALSE
    }
    if (missing(point_type)) {
        point_type = 20
    }
    if (header == TRUE) {
        colnames(data) <- data[1, ]
        data <- data[-c(1), ]
    }
    if (missing(pca_type)) {
        pca_type = "princomp_covmat"
    }
    if (!pca_type == "prcomp_svd" && !pca_type == "princomp_corr" && 
        !pca_type == "princomp_covmat" && !pca_type == "princomp_robust_MCD" && 
        !pca_type == "princomp_robust_MAD" && !pca_type == "nipals") {
        stop("Error: pca_type should be one from prcomp_svd, princomp_corr, princomp_covmat, princomp_robust_MCD, princomp_robust_MAD, nipals")
    }
    if (missing(centering)) {
        centering = TRUE
    }
    if (missing(scaling)) {
        scaling = TRUE
    }
    if (!centering == "TRUE" && !centering == "FALSE" && !scaling == 
        "TRUE" && !scaling == "FALSE") {
        stop("Error: centering and scaling should be TRUE or FALSE")
    }
    if (missing(labels)) {
        labels <- c(1)
    }
    if (is.data.frame(labels)) {
        if (nrow(unique(labels)) == 0) {
            labels <- c(1)
        }
    }
    if (missing(legend_name)) {
        legend_name = "Group"
    }
    if (is.data.frame(labels)) {
        labels <- as.matrix(labels)
    }
    if (is.vector(labels) == FALSE) {
        labels <- as.vector(labels)
    }
    if (is.atomic(labels) == FALSE) {
        labels <- t(labels)
    }
    if (missing(point_dim)) {
        point_dim = 1
    }
    if (missing(var_dim)) {
        var_dim = 1
    }
    if (missing(legend_dim)) {
        legend_dim = 1
    }
    if (missing(text.row)) {
        text.row = FALSE
    }
    if (missing(text.labels)) {
        text.labels = row.names(data)
    }
    pr_data <- scale(data, center = centering, scale = scaling)
    n <- nrow(data)
    m <- ncol(data)
    Frase0 <- NULL
    Frase1 <- NULL
    Frase2 <- NULL
    if (missing(Title)) {
        Title = ""
    }
    if ((pca_type == "prcomp_svd") == TRUE) {
        Frase0 <- "Principal Component SVD"
        Frase1 <- "The calculation is done by a singular value decomposition of the (centered and possibly scaled) data matrix n x m, not by using eigen on the covariance matrix.\n    Since many data sets can have more variables than observation, the covariance-approach can result in numerical problems because the cov.matrix does not have full rank.\n    So the algorithm avoids the direct computation of the eigenvectors of the m x m covariance matrix, (results are equivalent to Jacobi rotation when the sample covariance matrix is used):\n    this because SVD is based on eigenvector decompositions of cross-product matrices (for any n x m)."
        library(stats)
        pca <- prcomp(pr_data)
        pca
        SDev <- pca$sdev
        Scores <- pca$x
        Loadings <- pca$rotation
        pca <- list(sdev = SDev, loadings = Loadings, Scores = Scores, 
            n.obs = dim(pr_data[1]), center = pca$center, scale = pca$scale)
    }
    if ((pca_type == "princomp_corr") == TRUE) {
        if ((dim(data)[1] > dim(data)[2]) == FALSE) {
            stop("Error: There have to be more units than variable, \n           otherwise pca_type prcomp_svd is suggested")
        }
        library(stats)
        Frase0 <- "Principal Component (Standard)"
        Frase1 <- "The calculation is done using eigen on the correlation matrix.\n    Working with the correlation matrix instead of the covariance matrix is equivalent to working with standardized variables,\n    so the results are the same (using the sample covariance matrix) if a scaling process is applied."
        pca <- princomp(pr_data, cor = TRUE)
        pca
        SDev <- pca$sdev
        Scores <- pca$scores
        Loadings <- pca$loadings
    }
    if ((pca_type == "princomp_covmat") == TRUE) {
        if ((dim(data)[1] > dim(data)[2]) == FALSE) {
            stop("Error: There have to be more units than variable, \n           otherwise pca_type prcomp_svd is suggested")
        }
        if (missing(covmat)) {
            Frase0 <- "Principal Component (Standard)"
            Frase1 <- "The calculation is done using eigen on the covariance matrix.\n      The PCA loadings are taken as the eigenvectors of the estimated covariance matrix. \n      The eigen vector with the largest eigenvalue determines PC1, the second larger PC2 and so on.\n      Jacobi rotation is the used method for calculation of the eigenvectors and eigenvalues. \n      Scores matrix is then obtained multiplying the loadings matrix with the original data matrix."
            pca <- princomp(pr_data)
            pca
            SDev <- pca$sdev
            Scores <- pca$scores
            Loadings <- pca$loadings
        }
        else {
            if (is.matrix(covmat) == FALSE) {
                stop("Covariance Matrix should be a matrix")
            }
            Frase0 <- "Principal Component (Standard)"
            Frase1 <- "The calculation is done using eigen on the covariance matrix (provided in this case).\n      The PCA loadings are taken as the eigenvectors of the estimated covariance matrix. \n      The eigen vector with the largest eigenvalue determines PC1, the second larger PC2 and so on.\n      Jacobi rotation is the used method for calculation of the eigenvectors and eigenvalues. \n      Scores matrix is then obtained multiplying the loadings matrix with the original data matrix."
            pca <- princomp(pr_data, covmat = covmat)
            pca
            SDev <- pca$sdev
            Scores <- pca$scores
            Loadings <- pca$loadings
        }
    }
    if ((pca_type == "princomp_robust_MCD") == TRUE) {
        if ((((dim(data)[1]/dim(data)[2])) > 2) == FALSE) {
            stop("For Robust PCA using Minimum Covariance Determinant, the units should\n           be at least twice than variables")
        }
        else {
            library(robustbase)
            Frase0 <- "Robust Principal Component (Minimum Covariance Determinant)"
            Frase1 <- "Robust estimation of the PCA directions in such a way that a robust measure of variance is maximized instead of the classical variance.\n      The Scores matrix is then obtained multiplying the eigenvectors of the robust covariance matrix with the original data matrix.\n      A limitation is that at least twice as many observations than variables are required."
            C_MCD <- covMcd(data, cor = TRUE)
            pca <- princomp(data, covmat = C_MCD, cor = TRUE)
            pca
            SDev <- pca$sdev
            Scores <- pca$scores
            Loadings <- pca$loadings
        }
    }
    if ((pca_type == "princomp_robust_MAD") == TRUE) {
        if (missing(pc_number)) {
            stop("pc_number is missing")
        }
        if (is.numeric(pc_number) == FALSE) {
            stop("Error: pc_number should be a number")
        }
        if (((dim(data)[1] > 5 * dim(data)[2]) == TRUE) + ((dim(data)[1] < 
            0.2 * dim(data)[2]) == TRUE) == 0) {
            stop("For Robust PCA using Median Absolute Deviation, the number of variables\n           should be much larger than the number of observation or viceversa")
        }
        else {
            library(mvtnorm)
            library(pcaPP)
            Frase0 <- "Robust Principal Component (Median Absolute Deviation)"
            Frase1 <- "A robust PCA is obtained by the Projection Pursuit approach.\n      Similarly to NIPALS, the PCs are computed one after the other, and they are determined as directions maximizing a robust measure of variance, like the median absolute deviation (MAD).\n      This method works in situations where the number of variables is much larger than the observation and vice versa."
            pca <- PCAgrid(pr_data, k = pc_number)
            pca
            SDev <- pca$sdev
            Scores <- pca$scores
            Loadings <- pca$loadings
        }
    }
    if ((pca_type == "nipals") == TRUE) {
        if (missing(pc_number)) {
            stop("pc_number is missing")
        }
        library(chemometrics)
        Frase0 <- "Principal Component - NIPALS"
        Frase1 <- "NIPALS is a nonlinear iterative partial least-squares algorithm.\n    The PCs are calculated step-by-step, so it is very efficient when a few components are required.\n    NIPALS starts with an initial score vector u (usually the variable with the highest variance); the normalized loading vector b is then calculated from the product of the transpose matrix and the initial score vector.\n    Multiplying the original data matrix and the loading vector the first approximation of the score vector is obtained. \n    The cycle is repeated until convergence of u and b is reached: final value are the loadings p and the scores t for the first pc.\n    This is so peeled-off from the current data matrix, in a process called Deflation where a projection of the object points on to a subspace (which is orthogonal to p) is created.\n    The obtained (residual) matrix is then used as a new matrix for the second PC.\n    The process is stopped until the desired number of PCs are calculated."
        pca <- nipals(pr_data, a = pc_number)
        SDev <- apply(pca$T, 2, sd)
        Scores <- pca$T
        Loadings <- pca$P
    }
    print(pca)
    Res <- list(PCA = pca, StDev = SDev, Scores = Scores, Loadings = Loadings)
    Res
    if (missing(SDev) == FALSE) {
        ev <- SDev^2
        var_pc <- round((ev/sum(ev) * 100), 2)
        cum_var_pc <- c(1:length(var_pc))
        for (i in 1:length(var_pc)) {
            cum_var_pc[i] <- sum(var_pc[0:i])
        }
        if (PLOT == "Expl_Cum_Var") {
            barplot(var_pc, col = "blue", space = 0.05, names.arg = c(1:length(var_pc)), 
                main = "Explained & Cumulative Variance for component", 
                xlab = "Principal component", ylab = "\% Explained Variance", 
                ylim = c(0, 101))
            points(cum_var_pc, type = "o", col = "blue")
        }
    }
    labels <- factor(labels)
    liv <- factor(labels, ordered = TRUE)
    vect <- c(1:ncol(Loadings))
    if (missing(pc_number)) {
        if (length(vect) > 3) {
            pc_number <- 3
        }
        if (length(vect) <= 3) {
            pc_number <- length(vect) - 1
        }
    }
    vect <- c(1:ncol(Loadings))
    var_colvector <- var_col_group
    if (missing(text.row)) {
        text.row = FALSE
    }
    if (PLOT == "BIPLOT Scores and Loadings Plot") {
        Ellipse <- dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
            pc_axis_y], draw = FALSE, levels = CE_level/100, 
            center.pch = FALSE, plot.points = FALSE, lwd = 1, 
            col = 4, add = TRUE)
        if (CE == TRUE) {
            maxxS <- max(max(max(Scores[, pc_axis_x]), max(Ellipse[, 
                1]), abs(min(Scores[, pc_axis_x]))), max(max(Scores[, 
                pc_axis_y]), max(Ellipse[, 2]), abs(min(Scores[, 
                pc_axis_y]))))
        }
        if (!CE == TRUE) {
            maxxS <- max(max(max(Scores[, pc_axis_x]), abs(min(Scores[, 
                pc_axis_x]))), max(max(Scores[, pc_axis_y]), 
                abs(min(Scores[, pc_axis_y]))))
        }
        lim_Scores <- c(-maxxS - 1/100 * (maxxS/5), maxxS + 1/100 * 
            (maxxS/5))
        if (!legend_name == FALSE) {
            if (LegendPos == FALSE) {
                layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                  0.2))
                par(mar = c(5, 4, 4, 2) + 0.1)
            }
        }
        if ((text.row) == FALSE) {
            plot(Scores[, pc_axis_x], Scores[, pc_axis_y], xlab = paste("PC", 
                pc_axis_x, var_pc[pc_axis_x], "\%"), xlim = lim_Scores, 
                ylim = lim_Scores, ylab = paste("PC", pc_axis_y, 
                  var_pc[pc_axis_y], "\%"), cex = point_dim, asp = 1, 
                pch = point_type, col = liv)
            mtext(text = "PCA Scores and Loadings Plot", side = 3, 
                line = 2, cex = 1)
            mtext(side = 3, text = paste("Total Variance", var_pc[pc_axis_x] + 
                var_pc[pc_axis_y], "\%"), line = 3, cex = 0.7)
            if (CE == TRUE) {
                dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                  pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                  plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                mtext(side = 3, text = paste("Confidence Ellipse ", 
                  CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
            }
            abline(h = 0, v = 0, col = "gray")
        }
        if ((text.row) == TRUE) {
            plot(Scores[, pc_axis_x], Scores[, pc_axis_y], xlab = paste("PC", 
                pc_axis_x, var_pc[pc_axis_x], "\%"), type = "n", 
                xlim = lim_Scores, ylim = lim_Scores, ylab = paste("PC", 
                  pc_axis_x, var_pc[pc_axis_y], "\%"), cex = point_dim, 
                asp = 1, pch = point_type, col = liv)
            col_text <- c(1:length(levels(liv)))
            text(Scores[, pc_axis_x], Scores[, pc_axis_y], labels = text.labels, 
                cex = point_dim, col = col_text[liv])
            mtext(text = "PCA Scores and Loadings Plot", side = 3, 
                line = 2, cex = 1)
            mtext(side = 3, text = paste("Total Variance", var_pc[pc_axis_x] + 
                var_pc[pc_axis_y], "\%"), line = 3, cex = 0.7)
            if (CE == TRUE) {
                dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                  pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                  plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                mtext(side = 3, text = paste("Confidence Ellipse ", 
                  CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
            }
            abline(h = 0, v = 0, col = "gray")
        }
        maxxL <- max(max(max(Loadings[, pc_axis_x]), abs(min(Loadings[, 
            pc_axis_x]))), max(max(Loadings[, pc_axis_y]), abs(min(Loadings[, 
            pc_axis_y]))))
        lim_Loadings <- c(-maxxL - 1/10 * (maxxL/2), maxxL + 
            1/10 * (maxxL/2))
        par(new = TRUE)
        dev.hold()
        on.exit(dev.flush(), add = TRUE)
        plot(Loadings[, pc_axis_x], Loadings[, pc_axis_y], axes = FALSE, 
            type = "n", xlim = lim_Loadings, ylim = lim_Loadings, 
            asp = 1, xlab = "", ylab = "")
        axis(3)
        axis(4)
        arrows(x0 = 0, y0 = 0, x1 = Loadings[, pc_axis_x], y1 = Loadings[, 
            pc_axis_y], code = 2, length = 0.1, col = (var_colvector))
        text(x = Loadings[, pc_axis_x], y = Loadings[, pc_axis_y], 
            labels = row.names(Loadings), cex = var_dim, col = (var_colvector))
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
                if (CE == TRUE) {
                  dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                    pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                    plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                  mtext(side = 3, text = paste("Confidence Ellipse ", 
                    CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
                }
                legend(LegendPos, col = unique(liv), legend = unique(liv), 
                  pch = unique(point_type), bty = "n", cex = legend_dim, 
                  title = legend_name)
            }
        }
    }
    if (pca_type == "princomp_corr" | pca_type == "princomp_covmat" | 
        pca_type == "princomp_robust_MCD") {
        if (PLOT == "Diagnostic Plot Orthogonal vs Score Distance") {
            library(chemometrics)
            res <- pcaDiagplot(pr_data, pca, a = pc_number, plot = FALSE)
            maxSD <- max(res$SDist, res$critSD)
            maxOD <- max(res$ODist, res$critOD)
            SDlimit <- c(0, maxSD * 1.1)
            ODlimit <- c(0, maxOD * 1.1)
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                    0.2))
                  par(mar = c(5, 4, 4, 2) + 0.1)
                }
            }
            plot(y = res$ODist, x = res$SDist, main = "Diagnostic Plot", 
                ylab = "Orthogonal distance", xlab = "ScoreDistance", 
                ylim = ODlimit, xlim = SDlimit, cex = point_dim, 
                col = liv, pch = point_type)
            abline(h = res$critOD, lty = 2, col = "red")
            abline(v = res$critSD, lty = 2, col = "red")
            if ((SDlimit[2] != 0) & (ODlimit[2] != 0)) {
                OS <- data.frame(y = res$ODist, x = res$SDist, 
                  tx = row.names(Scores))
                OSsub <- subset(OS, ((x > res$critSD) & (y > 
                  res$critOD)))
                if (nrow(OSsub) != 0) 
                  text(OSsub$x, OSsub$y, label = OSsub$tx, cex = 0.7, 
                    pos = 3)
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
        if (PLOT == "Ëxplained Variance in Cross Validation") {
            if (missing(CVsegment.type) == FALSE) {
                if (!CVsegment.type == "random" && !CVsegment.type == 
                  "consecutive" && !CVsegment.type == "interleaved") {
                  stop("Error: CVsegment.type should be one from random, consecutive or interleaved")
                }
            }
            if (missing(CVsegments) == TRUE) {
                if (missing(CVsegment.type) == TRUE) {
                  Frase3 <- "Explained Varince in Cross Validation for each component is displayed.\n          The idea is to split the data into training data and test data, fit the PCA model to \n          the training data and look at the errors in terms of explained variance on the \n          validation data.\n          For this Cross-Validation Leave one out procedure is applied."
                  cat(Frase3, file = (paste("PCAout3.txt")), 
                    sep = "\n", append = FALSE)
                  CVres <- pcaCV(X = as.matrix(data), main = "Expl. Variance with Cross Validation", 
                    center = centering, scale = scaling, border = 4)
                }
                if (missing(CVsegments) == FALSE) {
                  if (missing(CVsegment.type) == TRUE) {
                    Frase3 <- paste("Explained Varince in Cross Validation for each component is displayed.\n                        The idea is to split the data into training data and test data, fit the PCA model to \n                        the training data and look at the errors in terms of explained variance on the \n                        validation data.\n                        For this Cross-Validation ", 
                      CVsegments, "segments are used to split the data.")
                    cat(Frase3, file = (paste("PCAout3.txt")), 
                      sep = "\n", append = FALSE)
                    CVres <- pcaCV(X = as.matrix(data), main = "Expl. Variance with Cross Validation", 
                      center = centering, scale = scaling, border = 4, 
                      segments = CVsegments)
                    mtext(text = paste("Segment number:", CVsegments), 
                      cex = 0.6, side = 3)
                  }
                  if (missing(CVsegment.type) == FALSE) {
                    Frase3 <- paste("Explained Varince in Cross Validation for each component is displayed.\n                        The idea is to split the data into training data and test data, fit the PCA model to \n                        the training data and look at the errors in terms of explained variance on the \n                        validation data. For this Cross-Validation ", 
                      CVsegments, CVsegment.type, "segments \n                        are used to split the data.")
                    cat(Frase3, file = (paste("PCAout3.txt")), 
                      sep = "\n", append = FALSE)
                    CVres <- pcaCV(X = as.matrix(data), main = "Expl. Variance with Cross Validation", 
                      center = centering, scale = scaling, border = 4, 
                      segments = CVsegments, segment.type = CVsegment.type)
                    mtext(text = paste("Segment number:", CVsegments, 
                      "Type:", CVsegment.type), cex = 0.6, side = 3)
                  }
                }
            }
        }
    }
    if (pc_number > 1) {
        TvsRes <- TQresidualshiny(data, centering, scaling, 
            pc_number = pc_number, labels = labels, text.row = text.row, 
            text.labels = text.labels, point_dim = point_dim, 
            legend_dim = legend_dim, legend_name = legend_name, 
            LegendPos = LegendPos, Title = Title, point_type = point_type, 
            CP_Point = CP_Point, CP_txt_Point, PLOT, CP_dim_var, 
            CP_las)
    }
    if (PLOT == "Scores Plot") {
        Ellipse <- dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
            pc_axis_y], draw = FALSE, levels = CE_level/100, 
            center.pch = FALSE, plot.points = FALSE, lwd = 1, 
            col = 4, add = TRUE)
        if (CE == TRUE) {
            maxxS <- max(max(max(Scores[, pc_axis_x]), max(Ellipse[, 
                1]), abs(min(Scores[, pc_axis_x]))), max(max(Scores[, 
                pc_axis_y]), max(Ellipse[, 2]), abs(min(Scores[, 
                pc_axis_y]))))
        }
        if (!CE == TRUE) {
            maxxS <- max(max(max(Scores[, pc_axis_x]), abs(min(Scores[, 
                pc_axis_x]))), max(max(Scores[, pc_axis_y]), 
                abs(min(Scores[, pc_axis_y]))))
        }
        lim_Scores <- c(-maxxS - 1/100 * (maxxS/5), maxxS + 1/100 * 
            (maxxS/5))
        if (!legend_name == FALSE) {
            if (LegendPos == FALSE) {
                layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                  0.2))
                par(mar = c(5, 4, 4, 2) + 0.1)
            }
        }
        if ((text.row) == FALSE) {
            plot(Scores[, pc_axis_x], Scores[, pc_axis_y], main = "PCA Scores Plot", 
                xlab = paste("PC", pc_axis_x, var_pc[pc_axis_x], 
                  "\%"), xlim = lim_Scores, ylim = lim_Scores, 
                ylab = paste("PC", pc_axis_y, var_pc[pc_axis_y], 
                  "\%"), cex = point_dim, asp = 1, pch = point_type, 
                col = liv)
            mtext(side = 3, text = paste("Total Variance", var_pc[pc_axis_x] + 
                var_pc[pc_axis_y], "\%"), line = 3, cex = 0.7)
            if (CE == TRUE) {
                dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                  pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                  plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                mtext(side = 3, text = paste("Confidence Ellipse ", 
                  CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
            }
            abline(h = 0, v = 0, col = "gray")
        }
        if ((text.row) == TRUE) {
            plot(Scores[, pc_axis_x], Scores[, pc_axis_y], main = "PCA Scores Plot", 
                type = "n", xlab = paste("PC", pc_axis_x, var_pc[pc_axis_x], 
                  "\%"), xlim = lim_Scores, ylim = lim_Scores, 
                ylab = paste("PC", pc_axis_y, var_pc[pc_axis_y], 
                  "\%"), cex = point_dim, asp = 1, pch = point_type, 
                col = liv)
            mtext(side = 3, text = paste("Total Variance", var_pc[pc_axis_x] + 
                var_pc[pc_axis_y], "\%"), line = 3, cex = 0.7)
            if (CE == TRUE) {
                dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                  pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                  plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                mtext(side = 3, text = paste("Confidence Ellipse ", 
                  CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
            }
            col_text <- c(1:length(levels(liv)))
            text(Scores[, pc_axis_x], Scores[, pc_axis_y], labels = text.labels, 
                cex = point_dim, col = col_text[liv])
            abline(h = 0, v = 0, col = "gray")
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
                if (CE == TRUE) {
                  dataEllipse(x = Scores[, pc_axis_x], y = Scores[, 
                    pc_axis_y], levels = CE_level/100, center.pch = FALSE, 
                    plot.points = FALSE, lwd = 1, col = 4, add = TRUE)
                  mtext(side = 3, text = paste("Confidence Ellipse ", 
                    CE_level, "\%"), line = 0.5, cex = 0.7, col = 4)
                }
                legend(LegendPos, col = unique(liv), legend = unique(liv), 
                  pch = unique(point_type), bty = "n", cex = legend_dim, 
                  title = legend_name)
            }
        }
    }
    if (PLOT == "SCORES PLOT PC1 vs Index") {
        if (length(vect) < 2) {
            if (!legend_name == FALSE) {
                if (LegendPos == FALSE) {
                  layout(matrix(c(1, 2), nrow = 1), widths = c(0.7, 
                    0.2))
                  par(mar = c(5, 4, 4, 2) + 0.1)
                }
            }
            if ((text.row) == FALSE) {
                plot(Scores[, 1], main = "PCA Scores Plot", ylab = paste("PC1", 
                  var_pc[1], "\%"), xlab = paste("Index"), cex = point_dim, 
                  asp = 1, pch = point_type, col = liv)
                abline(h = 0, v = 0, col = "gray")
            }
            if ((text.row) == TRUE) {
                plot(Scores[, 1], main = "PCA Scores Plot", type = "n", 
                  ylab = paste("PC1", var_pc[1], "\%"), xlab = paste("Index"), 
                  cex = point_dim, asp = 1, pch = point_type, 
                  col = liv)
                col_text <- c(1:length(levels(liv)))
                text(Scores[, i], Scores[, j], labels = text.labels, 
                  cex = point_dim, col = col_text[liv])
                abline(h = 0, v = 0, col = "gray")
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
    if (PLOT == "Loadings Plot") {
        maxxL <- max(max(max(Loadings[, pc_axis_x]), abs(min(Loadings[, 
            pc_axis_x]))), max(max(Loadings[, pc_axis_y]), abs(min(Loadings[, 
            pc_axis_y]))))
        lim_Loadings <- c(-maxxL - 1/10 * (maxxL/2), maxxL + 
            1/10 * (maxxL/2))
        plot(Loadings[, pc_axis_x], Loadings[, pc_axis_y], main = "PCA Loadings Plot", 
            xlab = paste("PC", pc_axis_x, var_pc[pc_axis_x], 
                "\%"), xlim = lim_Loadings, ylim = lim_Loadings, 
            ylab = paste("PC", pc_axis_y, var_pc[pc_axis_y], 
                "\%"), cex = point_dim, asp = 1, pch = 20, col = (var_colvector))
        mtext(side = 3, text = paste("Total Variance", var_pc[pc_axis_x] + 
            var_pc[pc_axis_y], "\%"), line = 3, cex = 0.7)
        text(Loadings[, pc_axis_x], Loadings[, pc_axis_y], row.names(Loadings), 
            col = (var_colvector), cex = var_dim, pos = 4)
        abline(h = 0, v = 0, col = "gray")
    }
    if (PLOT == "LOADINGS PLOT PC1 vs Index") {
        if (length(vect) < 2) {
            plot(Loadings[, 1], main = "PCA Loadings Plot", ylab = paste("PC1", 
                var_pc[1], "\%"), xlab = paste("Index"), cex = point_dim, 
                asp = 1, pch = 20, col = (var_colvector))
            text(Loadings[, 1], row.names(Loadings), cex = var_dim, 
                pos = 4)
            abline(h = 0, v = 0, col = "gray")
        }
    }
    if (missing(pc_number)) {
        pc_number = ncol(Loadings)
    }
    Tab1 <- c(paste("PCA Type"), Frase0)
    Tab2 <- c(paste("Centering"), centering)
    Tab3 <- c(paste("Scaling"), scaling)
    Tab4 <- c(paste("PC Number"), pc_number)
    Tab5 <- c(paste("n row data"), n)
    Tab6 <- c(paste("m column data"), m)
    Tab <- cbind(Tab1[2], Tab2[2], Tab3[2], Tab4[2], Tab5[2], 
        Tab6[2])
    colnames(Tab) <- c(Tab1[1], Tab2[1], Tab3[1], Tab4[1], Tab5[1], 
        Tab6[1])
    Tab <- t(Tab)
    colnames(Tab) <- c("Info")
    Tab <- as.data.frame(Tab)
    data <- as.data.frame(cbind(labels, data))
    colnames(data)[1] <- "Labels"
    pr_data <- as.data.frame(cbind(labels, pr_data))
    colnames(pr_data)[1] <- "Labels"
    if (PLOT == "LOADINGS PC1 vs Variables") {
        Ylim <- c(min(-0.01, min(PCARes$Loadings)), max(0.01, 
            max(PCARes$Loadings)))
        plot(x = c(1:length(var_colvector)), y = PCARes$Loadings[, 
            1], type = "b", col = 4, ylim = Ylim, xaxt = "n", 
            xlab = "", ylab = "Loadings of Variables on PC1", 
            main = "Loadings Value on PC1")
        axis(side = 1, at = c(1:length(var_colvector)), labels = row.names(PCARes$Loadings), 
            las = 2, cex.axis = 0.8)
        abline(h = 0, lty = 2, col = 2)
    }
    Variance_table <- rbind(var_pc, cum_var_pc)
    row.names(Variance_table) <- c("\% of Variance explained by each Component", 
        "\% of Cumulative Variance")
    Res <- list(Summary = Tab, PCA = Res$PCA, StDev = Res$StDev, 
        Scores = Res$Scores, Loadings = Res$Loadings, Data = data, 
        ProcessedData = pr_data, Title = Title, var_pc = var_pc, 
        cum_var_pc = cum_var_pc, Variance_table = Variance_table, 
        Info = paste(Frase0, Frase1))
    Res
  }
}

\keyword{ data }
