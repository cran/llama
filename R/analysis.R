contributions <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine contributions!")
    }

    times = .jarray(as.matrix(subset(data$data, T, data$performance)), dispatch=T)
    #coalitionValues = .jcall("shapleyComputation/CoalitionValueCalculator", "[D", "computeCoalitionValuesBasedOnMinimumTime", times)
    coalitionValues = J("shapleyComputation/CoalitionValueCalculator")$computeCoalitionValuesBasedOnMinimumTime(times)
    J("shapleyComputation/CoalitionValueCalculator")$deductFromNonEmptyCoalitionsTheMaxSingletonValue(coalitionValues)
    contributions = J("shapleyComputation/ShapleyComputation")$computeShapleyValues(coalitionValues)
    names(contributions) = data$performance

    return(contributions)
}
