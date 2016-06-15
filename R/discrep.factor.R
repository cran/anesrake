discrep.factor <-
function(datavec, targetvec, weightvec) {
    dat <- sapply(names(targetvec), function(x) {
        sum(weightvec[datavec == x & !is.na(datavec)])/sum(weightvec[!is.na(datavec)])
    })
    out <- targetvec - dat
    out
}
