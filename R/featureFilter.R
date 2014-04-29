featureFilter <-
function(filter=NULL, data=NULL) {
    if(is.null(filter)) {
        stop("No filter function given!")
    }
    if(is.null(data)) {
        stop("No data given!")
    }
    data$original_features = data$features
    b = breakBestTies(data)
    data$features = filter(b~., subset(data$data, T, data$features))
    return(data)
}
