# nonparametric bootstrap
# provinces are fixed, re-sample 'cluster' variable for cluster bt

clusterbt <- function(ydata, zdata, cluster) {
  clusters <- unique(cluster) # these clusters should be sampled 
  btclusters <- sample(clusters, length(clusters), replace = T)
  
  ybootdat <- list()
  zbootdat <- list()
  i <- 0
  for (j in btclusters) {
    i <- i + 1
    ybootdat[[i]] <- ydata[cluster == j]
    zbootdat[[i]] <- zdata[cluster == j]
  }
  return(list(y = unlist(ybootdat), 
              z = unlist(zbootdat)))
}


clustersample <- function(y, z, provincevector, clustervector) {
  ybootdat <- list()
  zbootdat <- list()
  # first, keep provinces fixed
  for (province in unique(provincevector)) {
    index = provincevector == province
    ydata <- y[index]
    zdata <- z[index]
    cluster <- clustervector[index]
    bootdat <- clusterbt(ydata, zdata, cluster) 
    ybootdat[[province]] <- bootdat$y
    zbootdat[[province]] <- bootdat$z
  }
  return(list(y = prevalence(unlist(ybootdat)), 
              z = prevalence(unlist(zbootdat))))
}

prevalence <- function(x){mean(x >= 140)}

npbt <- function(object, data, fixedgroup, clustergroup, R = 1000) {
  
  id <- c(F, names(lme4::getME(object, "cnms")) == "int.id") # int.id identifyer
  idx <- lme4::getME(object, "Gp")[c(which(id) - 1, which(id))] + c(1, 0) #range
  b <- lme4::getME(object, "b")[idx[1]:idx[2]] # interviewer RE
  ni <- dplyr::tally(data)$n
  ranef <- rep(b, ni)
  
  y <- data$sbp
  z <- y - ranef
  
  
  prevalence <- function(x){mean(x >= 140)}
  
  
  obsy <- prevalence(y)
  obsz <- prevalence(z)
 
  n <- length(y)
  #bty <- matrix(sample(y, R * n, TRUE), R, n)
  bt <- mapply(function(..) clustersample(y, z, fixedgroup, clustergroup), 1:R, SIMPLIFY = F)
  bty <- sapply(bt, function(x) x$y)
  #btz <- matrix(sample(z, R * n, TRUE), R, n)
  btz <- sapply(bt, function(x) x$z)
  
  df <- tibble::tibble(p = c(bty, btz), 
                       Model = rep(c("Uncorrected", "Corrected"), each = R))
  
  
  require("ggplot2")
  g <- ggplot(df) + theme_classic() + 
    geom_density(aes(x = p, y = ..density.., fill = Model), alpha = 0.2) + 
    xlab("Prevalence") + ylab("") + 
    geom_vline(aes(xintercept = obsy), linetype = "dashed") + 
    geom_vline(aes(xintercept = obsz), linetype = "dotted")
  
  print(g)
  
  
  return(list(plot = g, 
              observed = tibble::tibble(Uncorrected = obsy, corrected = obsz),
              corrected = tibble::tibble(Uncorrected = bty, corrected = btz)))
  
  
}
