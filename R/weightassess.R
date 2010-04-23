weightassess <-
function(inputter, dataframe, weightvec, 
    prevec = NULL) {
    require(Hmisc)
    out <- list()
    if (is.null(prevec)) {
        prevec <- rep(1, length(weightvec))
    }
    for (i in names(inputter)) {
        target <- eval(parse(text = paste("inputter$", i, sep = "")))
        orign <- wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weight = prevec)$sum.of.weights
        if (is.factor(eval(parse(text = paste("dataframe$", i, 
            sep = ""))))) {
          target <- target[match(names(orign), names(target))]
        }
        if (is.logical(eval(parse(text = paste("dataframe$", 
            i, sep = ""))))) {
          names(orign) <- c("TRUE", "FALSE")
          target <- target[match(names(orign), names(target))]
        }
        origpct <- wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weight = prevec)$sum.of.weights/sum(wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weight = prevec)$sum.of.weights)
        newn <- wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weights = weightvec)$sum.of.weights
        newpct <- wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weights = weightvec)$sum.of.weights/sum(wtd.table(eval(parse(text = paste("dataframe$", 
            i, sep = ""))), weights = weightvec)$sum.of.weights)
        chpct <- newpct - origpct
        rdisc <- target - newpct
        odisc <- target - origpct
        nout <- cbind(target, orign, origpct, newn, newpct, chpct, 
            rdisc, odisc)
        colnames(nout) <- c("Target", "Unweighted N", "Unweighted %", 
            "Wtd N", "Wtd %", "Change in %", "Resid. Disc.", 
            "Orig. Disc.")
        eval(parse(text = paste("out$", i, "<- nout", sep = "")))
    }
    out
}

