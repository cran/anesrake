anesrake <-
function(inputter, dataframe, caseid, 
    weightvec = NULL, cap = 5, verbose = FALSE, maxit = 1000, 
    type = "pctlim", pctlim = 5, nlim = 5, filter = 1, choosemethod = "total", 
    iterate = TRUE, convcrit = 0.01) {
    dataframe <- dataframe[filter == 1, ]
    caseid <- caseid[filter == 1]
    weightvec <- weightvec[filter == 1]
    mat <- as.data.frame(dataframe)
    origtype = type
    fullvars <- 0
    if (is.null(weightvec)) {
        weightvec <- rep(1, length(caseid))
    }
    if (length(weightvec) != length(caseid)) {
        stop("weight vector does not contain the same number of cases as data frame")
    }
    discrep1 <- anesrakefinder(inputter, dataframe, weightvec, 
        choosemethod)
    if (type == "nolim") {
        towers <- inputter
    }
    if (type == "pctlim") {
        towers <- selecthighestpcts(discrep1, inputter, pctlim)
    }
    if (type == "nlim") {
        towers <- selectnhighest(discrep1, inputter, nlim)
    }
    if (type == "nmin") {
        towers <- selecthighestpcts(discrep1, inputter, pctlim, 
            tostop = 0)
        towers2x <- selectnhighest(discrep1, inputter, nlim)
        if (length(towers) > length(towers2x)) {
            type <- "pctlim"
        }
        if (length(towers) < length(towers2x)) {
            towers <- towers2x
        }
    }
    if (type == "nmax") {
        fullvars <- 0
        discrep1 <- anesrakefinder(inputter, dataframe, weightvec, 
            choosemethod)
        towers <- selecthighestpcts(discrep1, inputter, pctlim)
        towers2x <- selectnhighest(discrep1, inputter, nlim)
        if (length(towers) > length(towers2x)) {
            towers <- towers2x
            fullvars <- 1
        }
    }
    ranweight <- rakelist(towers, mat, caseid, weightvec, cap, 
        verbose, maxit, convcrit)
    iterations <- ranweight$iterations
    weightout <- ranweight$weightvec
    if (type == "pctlim" & iterate == TRUE) {
        ww <- 0
        it <- 0
        while (ww < 1) {
            it <- it + 1
            addtotowers <- selecthighestpcts(anesrakefinder(inputter, 
                dataframe, weightout), inputter, pctlim, tostop = 0, 
                warn = 0)
            tow2 <- c(towers, addtotowers)
            towersx <- towers
            towers <- unique(tow2)
            names(towers) <- unique(names(tow2))
            if (sum(as.numeric(names(towersx) %in% names(towers))) == 
                length(towers)) {
                ww <- 1
            }
            if (sum(as.numeric(names(towersx) %in% names(towers))) != 
                length(towers)) {
                ranweight <- rakelist(towers, mat, caseid, weightvec, 
                  cap, verbose, maxit, convcrit)
                weightout <- ranweight$weightvec
            }
            if (it > 10) {
                ww <- 1
            }
        }
    }
    if (type == "nmax" & fullvars == 0 & iterate == TRUE) {
        ww <- 0
        it <- 0
        rundiscrep <- discrep1
        discrep2 <- rep(0, length(discrep1))
        while (ww < 1) {
            it <- it + 1
            rundiscrep <- rundiscrep + discrep2
            discrep2 <- anesrakefinder(inputter, dataframe, weightout)
            addtotowers <- selecthighestpcts(discrep2, inputter, 
                pctlim, tostop = 0)
            tow2 <- c(towers, addtotowers)
            towersx <- towers
            towers <- unique(tow2)
            names(towers) <- unique(names(tow2))
            if (sum(as.numeric(names(towersx) %in% names(towers))) == 
                length(towers)) {
                ww <- 1
            }
            if (sum(as.numeric(names(towersx) %in% names(towers))) != 
                length(towers)) {
                ranweight <- rakelist(towers, mat, caseid, weightvec, 
                  cap, verbose, maxit, convcrit)
                weightout <- ranweight$weightvec
            }
            if (sum(as.numeric(names(towersx) %in% names(towers))) > 
                nlim) {
                print("variable maximum reached, running on most discrepant overall variables")
                towers <- selectnhighest(discrep1, inputter, 
                  nlim)
                ranweight <- rakelist(towers, mat, caseid, weightvec, 
                  cap, verbose, maxit, convcrit)
                weightout <- ranweight$weightvec
                iterations <- 0
                ww <- 1
            }
            if (it >= 10) {
                ww <- 1
            }
            iterations <- iterations + ranweight$iterations
        }
    }
    out <- list(weightvec = weightout, type = type, caseid = caseid, 
        varsused = names(towers), choosemethod = choosemethod, 
        converge = ranweight$converge, nonconvergence = ranweight$nonconvergence, 
        targets = inputter, dataframe = dataframe, iterations = iterations, 
        iterate = iterate)
    class(out) <- c("anesrake", "anesrakelist")
    out
}

