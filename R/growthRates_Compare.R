#'
#' @title Plot growth rates as functions of temperature for all life stages
#'
#' @description Function to plot growth rates as functions of temperature for all life stages.
#'
#' @param T - temperature (deg C)
#'
#' @return list of ggplot2 plots for growth rates vs. temperature
#'
#' @details From Hurst et al., 2010.
#' #'
#' @import ggplot2
#' @import gridExtra
#' @import viridis
#'
#' @export
#'
growthRates_CompareAllStages<-function(T){
  plots<-list();

  #--growth in mass
  gM_preDW<-rPacificCod::preflexionLarvae_GrowthRateDW(T);
  gM_pstDW<-rPacificCod::postflexionLarvae_GrowthRateDW(T);
  gM_pstWW<-rPacificCod::postflexionLarvae_GrowthRateWW(T);
  gM_juvWW<-rPacificCod::juveniles_GrowthRateWW(T);
  dfr<-data.frame(temp=T,
                  "preflexion larvae (DW)"=gM_preDW,
                  "postflexion larvae (DW)"=gM_pstDW,
                  "postflexion larvae (WW)"=gM_pstWW,
                  "benthic juvenile (WW)"  =gM_juvWW,
                  check.names=FALSE);
  dfrp<-reshape2::melt(dfr,id.vars="temp")
  p <- ggplot(dfrp,mapping=aes_string(x="temp",y="value",colour="variable"));
  p <- p + geom_line();
  p <- p + labs(x="temperature (deg C)",y="growth rate (1/d)");
  p <- p + ylim(c(0,NA));
  p <- p + viridis::scale_colour_viridis(discrete=TRUE);
  p <- p + ggtitle("growth in mass");
  plots[["biomass"]]<-p;

  #--growth in length
  gM_preSL<-rPacificCod::preflexionLarvae_GrowthRateSL(T);
  gM_pstSL<-rPacificCod::postflexionLarvae_GrowthRateSL(T);
  gM_pstTL<-rPacificCod::postflexionLarvae_GrowthRateTL(T);
  gM_juvTL<-rPacificCod::juveniles_GrowthRateTL(T);
  dfr<-data.frame(temp=T,
                  "preflexion larvae (SL)"= gM_preSL,
                  "postflexion larvae (SL)"=gM_pstSL,
                  "postflexion larvae (TL)"=gM_pstTL,
                  "benthic juvenile (TL)"  =gM_juvTL,
                  check.names=FALSE);
  dfrp<-reshape2::melt(dfr,id.vars="temp")
  p <- ggplot(dfrp,mapping=aes_string(x="temp",y="value",colour="variable"));
  p <- p + geom_line();
  p <- p + labs(x="temperature (deg C)",y="growth rate (mm/d)");
  p <- p + ylim(c(0,NA));
  p <- p + viridis::scale_colour_viridis(discrete=TRUE);
  p <- p + ggtitle("growth in length");
  plots[["length"]]<-p;

  gridExtra::grid.arrange(plots[[1]],plots[[2]],ncol=1);

  return(plots);
}

#'
#' @title Compare growth rates as functions of temperature for benthic juveniles
#'
#' @description Function to compare growth rates as functions of temperature for benthic juveniles.
#'
#' @param T - temperature (deg C)
#'
#' @return ggplot2 plot for growth rates vs. temperature
#'
#' @details From Hurst et al., 2010 and Hurst et al. 2018.
#' #'
#' @import ggplot2
#' @import gridExtra
#' @import viridis
#'
#' @export
#'
growthRates_CompareJuveniles<-function(T,W=6.8,EDc=4138){
  #--growth in mass from 2010 paper
  gM_juv2010<-rPacificCod::juveniles_GrowthRateWW(T);#relative growth (i.e., per-day)
  #--growth in mass from 2018 paper
  gM_juv2018<-rPacificCod::juv_GT(T)/EDc;             #need to convert from J/g fish/day to relative growth
  #--bioenergetically-determined growth rate from 2018 paper
  dfr<-rPacificCod::juv_BioEnGrowth(W,T);
  gM_juvBioE<-dfr$G/EDc;                              #need to convert from J/g fish/day to relative growth
  dfr<-data.frame(temp=T,
                  "2010 fit"  =gM_juv2010,
                  "2018 fit"  =gM_juv2018,
                  "BioE prd"  =gM_juvBioE,
                  check.names=FALSE);
  dfrp<-reshape2::melt(dfr,id.vars="temp")
  p <- ggplot(dfrp,mapping=aes_string(x="temp",y="value",colour="variable"));
  p <- p + geom_line();
  p <- p + labs(x="temperature (deg C)",y="growth rate (1/d)");
  p <- p + ylim(c(0,NA));
  p <- p + viridis::scale_colour_viridis(discrete=TRUE);
  p <- p + ggtitle("growth in mass");

  return(p);
}

