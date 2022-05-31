#Function to run model for bootstrapping in chapter 2

run_mod2 <- function(dat, R.Intercept, WMA, CVf, CVf2){
  y <- exp(R.Intercept +
             (WMA * dat$WMA_Releases_by_Yr) +
             (CVf * dat$CV_flow) +
             (CVf2 * (dat$CV_flow^2)))
}
