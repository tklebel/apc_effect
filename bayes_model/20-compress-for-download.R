hm <- read_rds("bayes_model/final_models/17-brm-large-sample.rds")
write_rds(hm, "bayes_model/final_models/17-brm-large-sample.rds.bz2",
          compress = "bz2")
