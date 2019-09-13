#'
#' Function used to examine the combined results from all imputed datasets.
#'
#' @param imputeddata Imputed dataset.
#' @param formula Model formula.
#' @param reg_type Type of model. 1 = lm. 2 = lrm
#' @param outcome_levels Outcome levels. E.g. "2" if binary.
#' @param imputations Number of imputations.
#'
#' @return
#' @export
#' @import mice
#' @import CaTools
#' @import MASS
#' @import tidymodels
#' @import dplyr
#' @import rms
#' @examples
#' \dontrun {
#' dw_combine_imputations
#' (imputeddata,formula,reg_type,outcome_levels,imputations)
#' }
dw_combine_imputations <- function(imputeddata,
                                   formula,reg_type,outcome_levels,
                                  imputations){

  model<-formula



  Stat_sumc_SE<-NULL
  Stat_sumc_coef<-NULL
  Stat_sumc_var<-NULL
  Stat_psr2<-NULL
  for (i in 1:imputations){
    datado<-complete(imputeddata,action=i)

    if(  reg_type==1){
      mod<-lm(formula(model),data=datado)
      preddys<-2
    } else if ( reg_type==2){
      datado<-droplevels(datado)
      mod<- rms::lrm(formula(model),data=datado)
      Stat_psr2<-c(Stat_psr2,mod$stats[10])
      preddys<-outcome_levels
    } else {
      print( "no such type yet supported")
    }
    Stat_sumc_SE<-rbind(Stat_sumc_SE,sqrt(diag(vcov(mod))))
    Stat_sumc_var<-rbind(Stat_sumc_var,(diag(vcov(mod))))
    Stat_sumc_coef<-rbind(Stat_sumc_coef,mod$coef)

  }
  # ender<-length(attr(mod$terms,'predvars'))
  ender<-length(Stat_sumc_SE[1,])

  Stat_sumc_SE<-Stat_sumc_SE[,preddys:ender]
  Stat_sumc_coef<- Stat_sumc_coef[,preddys:ender]
  Stat_sumc_var<- Stat_sumc_var[,preddys:ender]



  m<-length(datado[,1])
  qmeans<-colMeans(Stat_sumc_coef)
  within_var<-colMeans(Stat_sumc_var)

  between_var<-(t(Stat_sumc_coef)-qmeans)^2
  bewteen_var_full<-apply(between_var,1,sum)*(1/(m-1))
  full_var<-within_var+bewteen_var_full*(1+1/m)
  full_SE<-sqrt(full_var)
  pvalues<-format(2*pt(-abs(qmeans/full_SE),df=m-1),digits=3,scientific=FALSE)

  if (  reg_type==1){
    print('last imputation')
    print(summary(mod))
    print('IMPORTANT ! The above is last imputation only ! ')
  } else if (  reg_type==2){
    print('Last imputaion')
    print(mod)
    print('IMPORTANT ! The above is last imputation only !')
    print('Pseudo-R2 by imputation ')
    print( Stat_psr2)
    print('mean Pseudo-R2')
    print( mean(Stat_psr2)) }

  print('Combined p-values')
  print(pvalues)

}
