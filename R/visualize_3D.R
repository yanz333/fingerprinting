#' Visualize the translated and 3D data
#'
#' @details This function creates one figure that shows translated walking data and corresponding 3D images of a subject for a specified time interval.
#'
#' @param subject_id numeric, id of the subject.
#' @param df_fit tbl_df, the processed dataset obtained from process.
#' @param time_lags vector, a vector of time lags for signal translation.
#'
#' @return one figure containing 4 panels.
#'
#' @import ggplot2
#' @import tidyverse
#' @import dplyr
#' @import ggpubr
#' @import readr
#' @import viridis
#' @import MASS
#'
#' @export
#'
#' @examples
#' visualize_3D(21, df_fit, c(0.15, 0.3))
#'


visualize_3D <- function(subject_id, df_fit, time_lags=c(0.15, 0.3)){
  train_j <- 1:200
  df_train <- subset(df_fit, J %in% train_j)

  if (length(time_lags)!=2){
    stop("Two time lags are required.")
  } else{
    xdf <- xdft <- data.frame(time_group=seq(from=1, to=1*100, by=1),
                              signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[3,])))
    xdft$time_group <- xdf$time_group+time_lags[1]*100
    xdf1 <- xdf %>%  mutate(Type = "Original") %>%
      bind_rows(xdft %>%
                  mutate(Type = "Time Lag"))

    p1 <- ggplot() +
      geom_line(data=xdf, aes(x=time_group/100+2, y=signal, color="Original: v(s)", linetype = "Original: v(s)")) +
      geom_line(data=xdft, aes(x=time_group/100+2, y=signal, color="Lagged: v(s-u)", linetype = "Lagged: v(s-u)")) +
      scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      geom_vline(xintercept=2+time_lags[1]+0.01, color="brown2") +
      geom_point(data=xdf[xdf$time_group==time_lags[1]*100+1,], aes(x=time_lags[1]+0.01+2, y=signal), color="brown2", size=3) +
      geom_point(data=xdft[xdft$time_group==time_lags[1]*100+1,], aes(x=time_lags[1]+0.01+2, y=signal), color="brown2", size=3) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.20))

    xdf2 <- xdft2 <- data.frame(time_group=seq(from=1, to=1*100, by=1),
                                signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[3,])))
    xdft2$time_group <- xdf2$time_group+time_lags[2]*100

    p2 <- ggplot()+
      geom_line(data=xdf2, aes(x=time_group/100+2, y=signal, color="Original: v(s)", linetype = "Original: v(s)")) +
      geom_line(data=xdft2, aes(x=time_group/100+2, y=signal, color="Lagged: v(s-u)", linetype = "Lagged: v(s-u)")) +
      scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      geom_vline(xintercept=2+time_lags[2]+0.01, color="brown2") +
      geom_point(data=xdf[xdf2$time_group==time_lags[2]*100+1,], aes(x=time_lags[2]+0.01+2, y=signal), color="brown2", size=3) +
      geom_point(data=xdft2[xdft2$time_group==time_lags[2]*100+1,], aes(x=time_lags[2]+0.01+2, y=signal), color="brown2", size=3) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.20,3.35)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))

    # full density
    get_density <- function(x, y, ...) {
      dens <- MASS::kde2d(x, y, ...)
      ix <- findInterval(x, dens$x)
      iy <- findInterval(y, dens$y)
      ii <- cbind(ix, iy)
      return(dens$z[ii])
    }

    xdfd1 <- xdftd1 <- data.frame(time_group=seq(from=1, to=200*100, by=1),
                                  signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[1:200,])))
    xdftd1$time_group <- xdfd1$time_group+time_lags[1]*100
    xdf1d1 <- xdfd1 %>%  mutate(Type = "Original") %>%
      bind_rows(xdftd1 %>%
                  mutate(Type = "Time Lag"))

    xdf2d1 <- inner_join(xdfd1, xdftd1, by="time_group")
    names(xdf2d1) <- c("time_group", "s", "u")

    xdf2d1$density <- get_density(xdf2d1$u, xdf2d1$s, n=100)

    p3 <- ggplot(xdf2d1) +
      geom_point(aes(x=u, y=s, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"),
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() +
      geom_point(data=xdf2d1[xdf2d1$time_group==201+time_lags[1]*100,], aes(x=u, y=s), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25))


    xdfd2 <- xdftd2 <- data.frame(time_group=seq(from=1, to=200*100, by=1),
                                  signal=as.vector(t(subset(df_train, id == subject_id)$smat_sub[1:200,])))
    xdftd2$time_group <- xdfd2$time_group+time_lags[2]*100
    xdf1d2 <- xdfd2 %>%  mutate(Type = "Original") %>%
      bind_rows(xdftd2 %>%
                  mutate(Type = "Time Lag"))

    xdf2d2 <- inner_join(xdfd2, xdftd2, by="time_group")
    names(xdf2d2) <- c("time_group", "s", "u")

    xdf2d2$density <- get_density(xdf2d2$u, xdf2d2$s, n=100)

    p4 <- ggplot(xdf2d2) +
      geom_point(aes(x=u, y=s, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"),
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() +
      geom_point(data=xdf2d2[xdf2d2$time_group==201+time_lags[2]*100,], aes(x=u, y=s), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25))

    ggarrange(p1+labs(title=paste("Time lag of ", time_lags[1], "s", sep="")),
              p2+labs(title=paste("Time lag of ", time_lags[2], "s", sep="")),
              p3, p4, ncol = 2, nrow = 2)
  }
}
