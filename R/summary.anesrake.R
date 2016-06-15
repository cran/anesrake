summary.anesrake <-
function(object, ...) {
    namer <- c("convergence", "base.weights", "raking.variables", "weight.summary", 
        "selection.method", "general.design.effect")
    bwstat <- "Using Base Weights Provided"
    if(sum(object$prevec==1)==length(object$prevec))
      bwstat <- "No Base Weights Were Used"
    part1list <- list(paste(object$converge, "after", object$iterations, 
        "iterations"), bwstat, object$varsused, summary(object$weightvec),
        paste("variable selection conducted using _", object$type, 
            "_ - discrepancies selected using _", object$choosemethod, 
            "_.", sep = ""), generaldesigneffect(object$weightvec))
    names(part1list) <- namer
    part2list <- weightassess(object$targets, object$dataframe, 
        object$weightvec, object$prevec)
    out <- c(part1list, part2list)
    out
}

