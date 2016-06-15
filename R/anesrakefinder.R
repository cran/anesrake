anesrakefinder <-
function(inputter, dataframe, weightvec = NULL, 
    choosemethod = "total") {
    if (is.null(weightvec)) {
        weightvec <- rep(1, dim(dataframe)[1])
    }
    findoff <- lapply(names(inputter), function(x) {
        discrep(dataframe[,x], 
                unlist(inputter[x]), 
                weightvec)
    })
    names(findoff) <- names(inputter)
    if (choosemethod == "total") {
        out <- sapply(findoff, function(x) {
            x <- sum(abs(x), na.rm = TRUE)
        })
    }
    if (choosemethod == "max") {
        out <- sapply(findoff, function(x) {
            x <- range(abs(x), na.rm = TRUE)[2]
        })
    }
    if (choosemethod == "average") {
        out <- sapply(findoff, function(x) {
            x <- mean(abs(x), na.rm = TRUE)
        })
    }
    if (choosemethod == "totalsquared") {
        out <- sapply(findoff, function(x) {
            x <- sum(x^2, na.rm = TRUE)
        })
    }
    if (choosemethod == "maxsquared") {
        out <- sapply(findoff, function(x) {
            x <- range(x^2, na.rm = TRUE)[2]
        })
    }
    if (choosemethod == "averagesquared") {
        out <- sapply(findoff, function(x) {
            x <- mean(x^2, na.rm = TRUE)
        })
    }
    out
}
