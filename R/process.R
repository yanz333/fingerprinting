#' Process the raw accelerometry data
#'
#' @details This function processes the raw accelerometry data.
#'
#' @param folder character, the folder that contains the raw data in csv files for the 32 subjects.
#' @param demo data.frame, the dataset containing subject demographics.
#'
#' @return One organized data object that contains walking data from left wrists that can be used in further visualization or analyses.
#'
#' @import tidyverse
#' @import dplyr
#' @import fields
#' @import gridExtra
#' @import devtools
#' @import data.table
#'
#' @export
#'
#' @examples
#' process("raw_data")
#'

process <- function(folder, demo){
  if(dir.exists(folder)==0){message("The specified folder does not exist in the current working directory.")}
  else{
  cat("Combining the raw data files for 32 individuals into one dataset...\n\n")
  accel_files <- list.files(file.path(folder))
  df_ls <- lapply(accel_files, function(x) data.frame(fread(file.path("raw_data",x))))
  xdf <- bind_rows(df_ls, .id="id")
  rm(list=c("df_ls","accel_files"))

  cat("Creating labels for the types of physical activty recorded...\n\n")
  map_vec <- rep(NA, 99)
  map_vec[c(1,2,3,4,77,99)] <- c("walking", "descending_stairs",
                                 "ascending_stairs", "driving", "clapping",
                                 "non_study_activity")
  xdf$activity_lab <- map_vec[xdf$activity]

  cat("Generating the vector magnitude of raw signals for left-wrist data...\n\n")
  xdf <-
    xdf %>%
    mutate("VM_lw" = sqrt(lw_x^2 + lw_y^2 + lw_z^2), # computed signal variable combining x, y and z
           "id" = as.numeric(id))

  cat("Grouping vector magnitudes into 1 second intervals...\n\n")
  tlen <- 1
  xdf_w <-
    xdf %>%
    filter(activity_lab=="walking") %>%
    dplyr::select(id, time_s,VM_lw) %>%
    dplyr::mutate(time_g = time_s - min(time_s),
                  time_g = floor(time_g/tlen)) %>%
    group_by(id) %>%
    mutate(time_g = time_g-min(time_g), # rounded time groups
           j = 1:n() #here n() gives the # of the time_g observations in each id group
    ) %>%
    # ungroup() %>%
    group_by(id, time_g) %>%
    dplyr::mutate(n_g = n(), #gives the # of observations in each time_g group of each id
                  time_s_g=1:n()) %>%
    ungroup() %>%
    filter(n_g == tlen*100)

  uid <- unique(xdf_w$id)
  nid <- length(uid) # number of individuals with qualified data (100 observations in each time_g group)

  nk <- (tlen*100)*(tlen*100-1)/2
  ji <- vapply(uid, function(x) length(unique(xdf_w$time_g[xdf_w$id==x])), numeric(1)) # the number of unique time_g groups in each individual
  N <- sum(ji)
  smat_w <- umat_w <- matrix(NA, ncol=nk, nrow=N)
  id_vec <- time_g_vec <- rep(NA, N)

  inx_samp <- lapply(1:(100*tlen-1), function(x){
    ret <- data.frame("u"=x,"s"=(x+1):(100*tlen))
    ret
  }) # created a list of 99 matrices each with a fixed u and s greater than u until 100
  inx_samp <- bind_rows(inx_samp)

  inx <- 1
  for(i in seq_along(uid)){ # loop around each individual
    df_i <- filter(xdf_w, id == i)
    j_i    <- unique(df_i$time_g)
    for(j in seq_along(j_i)){ # loop around each unique time_g of the individual
      Y_ij <- df_i$VM_lw[df_i$time_g == j_i[j]] # obtain computed signal values for each time_g group
      umat_w[inx,] <- Y_ij[inx_samp$u] # computed signal values of the time_g group at all u's
      smat_w[inx,] <- Y_ij[inx_samp$s] # computed signal values of the time_g group at all s's
      time_g_vec[inx] <- ji[j]
      id_vec[inx] <- df_i$id[1]
      inx <- inx + 1
    }
  }
  # Every row of the final matrices contains the computed signal data of each
  # time_g group for all 32 subjects


  set.seed(1)

  df_fit <- data.frame("id" = id_vec,
                       "umat" = I(umat_w),
                       "smat" = I(smat_w),
                       "lmat" = I(matrix(1/nk, ncol=nk, nrow=N)))
  rm(list=c("smat_w","umat_w","xdf_w","xdf"))
  gc()

  demo$id <- 1:nrow(demo)
  df_fit <- left_join(df_fit, demo, by="id")
  df_fit <- df_fit %>%
    group_by(id) %>%
    dplyr::mutate(J = 1:n()) %>%
    ungroup() %>%
    dplyr::mutate(male = as.numeric(gender=="male")) # gives 1 if gender is male

  # Each row of smat_sub contains all the 100 computed signal data in an unique
  # time_g group for one subject, or the variable Y_ij in previous loop
  df_fit$smat_sub <- I(cbind(df_fit$umat[,1], df_fit$smat[,1:99]))
  df_fit$lmat_sub <- I(matrix(1/100,ncol=100,nrow=nrow(df_fit)))
  rm(list=setdiff(ls(), "df_fit"))
  cat("Final dataset created: a dataframe containing several elements and a large matrix named smat_sub in which each row contains a subject's 1 second interval signals - 100 vector magnitudes. You can save this dataset to your workspace by running df_fit <- process(folder, demo)\n\n")
  return(df_fit)
  }
}

