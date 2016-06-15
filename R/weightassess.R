weightassess <-
    function(inputter, dataframe, weightvec, 
             prevec = NULL) {
        prx <- "Unweighted"
        if(!is.null(prevec))
            prx <- "Old Weights"
        if(sum(prevec==1)==length(prevec))
            prx <- "Unweighted"
        out <- list()
        if (is.null(prevec)) {
            prevec <- rep(1, length(weightvec))
        }
        for (i in names(inputter)) {
            target <- inputter[[i]]
            orign <- wtd.table(dataframe[,i], weights = prevec)$sum.of.weights
            if(is.factor(dataframe[, i])) {
                target <- target[match(names(orign), names(target))]
            }
            if(is.logical(dataframe[,i])) {
                names(target) <- c("TRUE", "FALSE")
            }
            if(sum(names(target) %in% names(orign))==2)
                target <- target[match(names(orign), names(target))]
            #if(sum(names(target) %in% names(orign))!=2 & length(target)==2){
            #    names(target) <- c("FALSE", "TRUE")
            #    target <- target[match(names(orign), names(target))]
            #}
            origpct <- wpct(dataframe[,i], prevec)
            newn <- wtd.table(dataframe[,i], weights = weightvec)$sum.of.weights
            newpct <- wpct(dataframe[,i], weightvec)
            chpct <- newpct - origpct[match(names(origpct), names(newpct))]
            rdisc <- target - newpct[match(names(newpct), names(target))]
            odisc <- target - origpct[match(names(origpct), names(target))]
            nout <- cbind(target, orign[match(names(orign), names(target))], origpct[match(names(origpct), names(target))], newn[match(names(newn), names(target))], newpct[match(names(newpct), names(target))], chpct[match(names(chpct), names(target))], rdisc[match(names(rdisc), names(target))], odisc[match(names(odisc), names(target))])
            nout2 <- rbind(nout, Total=apply(nout, 2, function(x) sum(abs(x), na.rm=TRUE)))
            colnames(nout2) <- c("Target", paste(prx, "N"), paste(prx, "%"), 
                                 "Wtd N", "Wtd %", "Change in %", "Resid. Disc.", 
                                 "Orig. Disc.")
            out[[i]] <- nout2
        }
        out
    }
