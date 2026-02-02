#' Data Request
#'
#' This function count frequencies of win or loss x switch and stay for an input 
#' of two vectors one with outcomes, the second with responses.
#'
#' @param outcome Vector with 0s and 1s representing rewards or outomes
#' @param response Vector with responses levels (in prl is 3 response levels)
#' @return data.frame with 4 columns, one for each cell in a 2 (win, loss) x (change, repeat responses)
#' @examples
#' dataRequest("prl_long_wide")
#' 
#' #' @author Santiago Castiello De Obeso <santiago.castiellodeobeso@yale.edu>
#' 

winLossSwitchStay <- function(outcome, response) {
  # get outcomes in 0 or 1
  outcome <- ifelse(outcome == 1, 1, 0)
  # next response same or not
  next_resp_same <- response == c(response[2:length(response)],NA)
  # frequencies wins/loss vs stay/switch
  wl_ss <- table(outcome,next_resp_same)
  # debug warning if no 2x2 table
  if (sum(dim(wl_ss)==c(2,2)) != 2) {warning(i)}
  # wide format
  wl_ss <- data.frame(loss_switch=wl_ss[1,1],win_switch=wl_ss[2,1],
                      loss_stay=wl_ss[1,2],win_stay=wl_ss[2,2])
  # function output
  return(wl_ss)
}
