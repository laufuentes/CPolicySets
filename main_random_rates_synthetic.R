
SL.out$rate_cal_labels_unweighted <- SL.out$rate_scores_unweighted_cal<- matrix(0,nrow=nrow(test), ncol=n_rate)

SL.out$rate_scores_unweighted_true <- matrix(0,nrow=nrow(test), ncol=n_rate)

for(i in 1:n_rate){
  rate <- random_rate[i]
  mix_factor<- stats::rbinom(nrow(test),1,prob=rate)
  # Apply random rate to all tree kind of label generation mechanisms 
  SL.out$rate_cal_labels_unweighted[,i]<- mix_factor*A_rd +  (1-mix_factor)*unweighted_cal
  # Compute associated scores 
  SL.out$rate_scores_unweighted_cal[,i] <- margin_po[cbind(1:nrow(test), SL.out$rate_cal_labels_unweighted[,i])]

  SL.out$rate_scores_unweighted_true[,i] <- potential_outcomes_test[cbind(1:nrow(test), SL.out$rate_cal_labels_unweighted[,i])]
}

SL.out$rate_cal_labels_behavioral <- margin_po[cbind(1:nrow(test), test[,treatment_name])]

data_toghether <- dplyr::bind_rows(
  lapply(1:ncol(SL.out$rate_scores_unweighted_cal), function(i) {
    data.frame(
      value = SL.out$rate_scores_unweighted_cal[,i],
      model = "Estimated score",
      mechanism = "Unweighted",
      type = random_rate[i]
    )
  }))

behavioral_data <-  dplyr::bind_rows(
  data.frame(
    value = SL.out$rate_cal_labels_behavioral,
    model = "Estimated score",
    mechanism = "Unweighted",
    type = 0))

data_toghether<- dplyr::bind_rows(data_toghether,
                                    lapply(1:ncol(SL.out$rate_scores_unweighted_true), function(i) {
                                      data.frame(
                                        value = SL.out$rate_scores_unweighted_true[,i],
                                        model = "True score",
                                        mechanism = "Unweighted",
                                        type = random_rate[i]
                                      )}))
  
  data_true_all <- dplyr::bind_rows(
    data.frame(
      value = as.vector(SL.out$true_score),
      model = "Estimated score",
      mechanism = "Unweighted"))
  
  data_true_all <- dplyr::bind_rows(data_true_all, 
                                    data.frame(
                                      value = as.vector(SL.out$true_score_true),
                                      model = "True score",
                                      mechanism = "Unweighted"))
