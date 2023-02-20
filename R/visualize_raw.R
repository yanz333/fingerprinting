#' Visualize the raw and calculated data
#'
#' @details This function creates one figure that shows raw walking data and vector magnitude of a subject for a specified time interval.
#'
#' @param subject_data data.frame, the raw walking data for a subject.
#' @param seconds numeric, the number of seconds to show.
#'
#' @return one figure containing 4 panels.
#'
#' @import ggplot2
#' @import tidyverse
#' @import dplyr
#' @import ggpubr
#' @import tidyr
#'
#' @export
#'
#' @examples
#' visualize_raw(subject_data, 20)
#'

visualize_raw <- function(subject_data, seconds=20){
  if (seconds<15) {
    stop("The minimum time interval for plotting is 15s.")
  } else {
    sub <- subject_data[,1:5] %>% filter(activity==1) %>%
      mutate(vector_magnitude = sqrt(lw_x^2+lw_y^2+lw_z^2))

    p1 <- sub %>%
      filter(time_s>=sub$time_s[1] & time_s<sub$time_s[1]+seconds) %>% rename(c("x"="lw_x", "y"="lw_y", "z"="lw_z")) %>%
      gather(key = Axis, value = Amplitude,
             c("x", "y", "z")) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = Amplitude, group = Axis, colour = Axis)) +
      geom_line(size=0.3) +
      facet_grid(Axis~., scales = "free_y") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Acceleration" ~ "(" ~ m/s^2 ~ ")"))

    p2 <- sub %>%
      filter(time_s>=sub$time_s[1] & time_s<sub$time_s[1]+seconds) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) +
      geom_line(size=0.3) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      geom_vline(xintercept=c(2, 3), color="darkorange", linetype="solid", size=0.6) +
      geom_vline(xintercept=c(10, 15), color="purple", linetype="solid", size=0.6) +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))

    p3 <- sub %>%
      filter(time_s>=sub$time_s[1]+2 & time_s<sub$time_s[1]+3) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) +
      geom_line(size=0.5, color="darkorange") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))

    p4 <- sub %>%
      filter(time_s>=sub$time_s[1]+10 & time_s<sub$time_s[1]+15) %>%
      ggplot(aes(x=(time_s-sub$time_s[1]), y = vector_magnitude)) +
      geom_line(size=0.4, color="purple") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))

    ggarrange(p1, p3, p2, p4, ncol = 2, nrow = 2)
  }
}

