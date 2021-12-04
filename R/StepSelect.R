#'Step Selection
#'
#'Conduct forward or backward selection for linear regression models
#'
#'@param full the full model for backward selection to start and the upper limit for forward selection
#'@param direction should be either "backward" or "forward"
#'@param k numeric, the penalty per parameter to be used in AIC; the default k = 2 is the classical AIC.
#'
#'@return the final linear regression result after selection
#'
#'@examples
#'full_lm = lm(mpg ~ ., data = mtcars)
#'n = dim(mtcars)[1]
#'StepSelect(full_lm, direction = "forward", k = log(n))
#'StepSelect(full_lm, direction = "backward", k = log(n))
#'
#'@export
#'
StepSelect = function(full, direction, k = 2){
    # extract data
    data = model.frame(full)
    # extract X and Y names
    Ynam = as.character(as.list(attr(terms(full), "variables"))[[2]])
    Xnam = attr(terms(full), "term.labels")
    if (direction == "backward"){
        while (length(Xnam) > 0) {
            # start from the full model
            AIC_full = AIC(full, k = k)
            newAICs = rep(NA, length(Xnam))
            for (i in 1:length(Xnam)) {
                # try the new set by taking each variable off
                Xs = Xnam[-i]
                newform = as.formula(paste(Ynam, " ~ ", paste(Xs, collapse= "+")))
                newlm = eval(bquote(lm(.(newform), data = data)))
                newAICs[i] = AIC(newlm, k = k)
            }
            # compare new AIC's with the current AIC
            if (min(newAICs) < AIC_full){
                Xnam = Xnam[-which.min(newAICs)]
                newform = as.formula(paste(Ynam, " ~ ", paste(Xnam, collapse= "+")))
                # update the current full model
                full = eval(bquote(lm(.(newform), data = data)))
            }else{
                # if the current AIC of the full model is the smallest, no improvement can be made -> break
                break
            }
        }
        return(full)
    } else if (direction == "forward"){
        # start from the null model
        newform = as.formula(paste(Ynam, "~", 1))
        nullmodel = eval(bquote(lm(.(newform), data = data)))
        Xselect = NULL
        while (length(Xnam) > 0) {
            AIC0 = AIC(nullmodel, k = k)
            newAICs = rep(NA, length(Xnam))
            for (i in 1:length(Xnam)) {
                # try the new set of current selected list with each unselected variable
                Xs = c(Xselect, Xnam[i])
                newform = as.formula(paste(Ynam, " ~ ", paste(Xs, collapse= "+")))
                newlm = eval(bquote(lm(.(newform), data = data)))
                newAICs[i] = AIC(newlm, k = k)
            }
            # compare new AIC's with the AIC of the null model
            if (min(newAICs) < AIC0){
                # Update the selected list
                Xselect = c(Xselect, Xnam[which.min(newAICs)])
                Xnam = Xnam[-which.min(newAICs)]
                newform = as.formula(paste(Ynam, " ~ ", paste(Xselect, collapse= "+")))
                # update the null model
                nullmodel = eval(bquote(lm(.(newform), data = data)))
            }else {
                # if the current AIC of the null model is the smallest, no improvement can be made -> break
                break
            }
        }
        return(nullmodel)
    } else {
        warning("Error in direction: should be 'backward' or 'forward' ")
    }
}
