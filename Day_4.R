
# 521 records seem to be an anomaly, same drug_name for multiple generic_name ?
dim(presc_detail %>% select(everything()) %>% unique())[[1]] - dim(presc_detail)[[1]]
dim(presc_detail)[[1]] - dim(presc_detail %>%
                               select(doc_id, nppes_provider_city,
                                      nppes_provider_state, drug_name) %>%
                               unique())[[1]]
dim(presc_detail)[[1]] - dim(presc_detail %>%
                               select(doc_id, nppes_provider_city,
                                      nppes_provider_state, drug_name, generic_name) %>%
                               unique())[[1]]
drug_name_anomaly_1 <- presc_detail %>% split(f = as.factor(presc_detail$drug_name))
length(drug_name_anomaly_1) - length(unique(presc_detail$drug_name))
drug_name_anomaly_2 <- drug_name_anomaly_1 %>%
                          sapply(FUN = function(x){
                            x %>% select(drug_name, generic_name) %>%
                              unique() %>% nrow()
                          })
head(drug_name_anomaly_2)
drug_name_anomaly_3 <- drug_name_anomaly_2[which(drug_name_anomaly_2 > 1)]
drug_name_anomaly_3
drug_name_anomaly_4 <- drug_name_anomaly_1[which(names(drug_name_anomaly_1) %in% names(drug_name_anomaly_3))]
drug_name_anomaly_4