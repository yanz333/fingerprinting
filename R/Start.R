#' Start introduction of the fingerprinting project and dataset used
#'
#' Step by step tutorial for downloading the dataset
#'
#' @details This function introduces the fingerprinting project and dataset.
#'
#' @return general information of the fingerprinting project.
#'
#' @import ggplot2
#' @import tidyverse
#' @import dplyr
#' @import utils
#'
#' @export
#'
#' @examples
#' Start()
#'

Start <- function(){
  username <- readline(prompt="Enter name: ")
  cat(paste("Hi ", username, ", welcome to the introduction of the Fingerprinting project! This package provides a step-by-step tutorial for downloading, processing, and visualizing the dataset used in this project.\n\n", sep=""))
  cont1 <- menu(c("Yes", "No"), title="Do you want to learn more?")
  if (cont1==1){
    cat("The Fingerprinting project tries to identify individuals based on their walking patterns from data collected by body-worn accelerometers. The method includes transforming the raw data into 3D images and predicting the subjects using selected key predictors in one vs. all logistic regression models. \n\n")
    invisible(readline(prompt="Press [enter] to continue"))
    cat("Starting from the data, it is from a study conducted at the Department of Biostatistics Fairbanks School of Public Health at Indiana University and published in 2021 by Marta Karas, et al. The study collected raw accelerometry data for 4 different physical activities and for 32 healthy subjects. Data were simultaneously collected from different body locations including left wrist, left hip, left ankle and right ankle at a sampling frequency of 100 Hz. \n\n")
    invisible(readline(prompt="Press [enter] to continue"))
    cat("This analysis uses raw accelerometry walking data for the 32 subjects collected from accelerometers worn at their left wrists. There are at least 380 seconds of walking data for each subject. Training dataset contains the first 200 seconds of walking, and testing dataset contains the rest 180 seconds of walking.\n\n")
    invisible(readline(prompt="Press [enter] to continue"))
    cat("The data is freely available for download at: https://physionet.org/content/accelerometry-walk-climb-drive/1.0.0/\n\n")
    invisible(readline(prompt="Press [enter] to continue"))
    cat("Once you have the raw dataset, you can proceed to use process(), visualize_raw(), visualize_3D(), and Acknowledge() functions in this package.")
  } else {message("Please use Start() to learn more about this project.")}
}
