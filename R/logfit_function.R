#' A function to get some common pseudo R2 measures from a glm
#' 
#' This function takes a fitted glm object and returns some common psuedo R2 measures.
#' It currently returns the McFadden, Cox and Snell and Nagelkerke pseudo R square.
#' @param model The fitted glm object
#' @keywords glm
#' @export
#' @examples 
#' 
#' # A fictitious example predicting vote for party "CDU" (0: no, 1: yes) 
#' # with political orientation (1: left, ..., 10: right)
#' lr <-  c(1, 1, 2, 3, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 8, 8, 9, 10)
#' cdu <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
#' cdu <- factor(cdu, levels = c(0, 1), labels = c("no", "yes"))
#' df <- data.frame(lr, cdu)
#' lm1 <- glm(cdu ~ lr, data = df, family = binomial(link = "logit"))
#' logfit(lm1)

logfit <- function(model) {
  
  # Check if model is glm
  if(!("glm" %in% class(model))) {
    stop("Model is not a glm")
  } 
  
  # McFadden's R2
  llp <- -2^-1 * model$deviance
  llo <- -2^-1 * model$null.deviance
  mcfadden <- 1 - (llp / llo)
  
  # Cox and Snell R2
  G <- model$null.deviance - model$deviance
  n <- dim(model$model)[1]
  
  coxsnell <- 1 - exp(-G / n)
  
  # Nagelkerke R2
  nagelkerke <- coxsnell / (1 - exp(-(model$null.deviance / n)))
  
  # Return the pseuod R2s
  return(list("McFaddens R2" = mcfadden, 
              "Cox and Snell R2" = coxsnell, 
              "Nagelkerke R2" = nagelkerke))
}