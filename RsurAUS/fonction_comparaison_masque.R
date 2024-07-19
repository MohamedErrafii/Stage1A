# Comparaison --------------------------------
comparaison_deux_masque <-function(masq1,masq2, cat_var = 1:2) {
  
  masq_jt <- masq2 %>% 
    select(-is_secret_prim, -MAX) %>% 
    rename(Status_ta = Status) %>% 
    left_join(
      masq1 %>%
        select(-N_OBS) %>% 
        mutate(Status_gauss = case_when(
          primary ~ "B",
          suppressed ~"D",
          TRUE ~ "V"
        )) %>% select(-primary, -suppressed),
      by = c(names(masq2)[cat_var])
    ) %>% 
    mutate(is_status_diff = Status_ta != Status_gauss)
  
  stats_n_diff <- masq_jt %>%
    group_by(Status_gauss, Status_ta) %>% summarise(n_diff = sum(is_status_diff), .groups = "drop")
  stats_n_diff <- stats_n_diff %>% 
    bind_rows(stats_n_diff %>% summarise(n_diff = sum(n_diff))) %>% 
    mutate(Status_gauss = ifelse(is.na(Status_gauss), "Total", Status_gauss)) %>% 
    mutate(Status_ta = ifelse(is.na(Status_ta), "Total", Status_ta))
  
  stats <- masq_jt %>%
    tidyr::pivot_longer(
      cols = c("Status_ta", "Status_gauss"),
      names_to = "type", values_to = "status"
    ) %>% 
    group_by(type, status) %>% 
    summarise(n = n(), val = sum(N_OBS), .groups="drop") %>% 
    group_by(type) %>% 
    mutate(n_pc = round(n/sum(n)*100,2), val_pc = round(val/sum(val)*100,2)) %>% 
    tidyr::unite(col = "v", n, val, n_pc, val_pc, sep = "___") %>% 
    tidyr::pivot_wider(names_from = type, values_from = v) %>% 
    tidyr::separate(Status_gauss, into = paste0(c("n", "val", "n_pc", "val_pc"), "_gauss"), sep = "___") %>% 
    tidyr::separate(Status_ta, into = paste0(c("n", "val", "n_pc", "val_pc"), "_ta"), sep = "___") %>% 
    mutate(across(-1, as.numeric))
  
  stats <- stats %>% 
    bind_rows(stats %>% summarise(across(-1, sum))) %>% 
    mutate(status = ifelse(is.na(status), "Total", status))
  stats <- stats[, c(1, 2, 6, 3, 7, 4, 8, 5, 9)]
  
  # Stat_masq_Gauss<-masq1 %>% 
  #   group_by(primary,suppressed) %>% 
  #   summarise(
  #     ncell_Gauss = n(),
  #     valcell_Gauss = sum(N_OBS)
  #   )
  # Stat_masq_Gauss<-Stat_masq_Gauss %>% 
  #   mutate(Status_Gauss = case_when(
  #     primary ~ "B",
  #     suppressed ~"D",
  #     TRUE ~ "V"
  #   )) %>% 
  #   mutate()
  # 
  # 
  # Stat_masq_Gauss <- Stat_masq_Gauss[,(ncol(Stat_masq_Gauss)-2):ncol(Stat_masq_Gauss)]
  # sums <- colSums(Stat_masq_Gauss[,1:2])
  # Stat_masq_Gauss[,1]<-(Stat_masq_Gauss[,1]*100)/sums[1]
  # Stat_masq_Gauss[,2]<-(Stat_masq_Gauss[,2]*100)/sums[2]
  # Stat_masq_Gauss <- Stat_masq_Gauss %>%
  #   mutate(across(where(is.numeric), ~ round(., 2)))
  # sums <- c(sums,"Total")
  # Stat_masq_Gauss <- rbind(Stat_masq_Gauss,sums)
  # Stat_masq_Gauss <- Stat_masq_Gauss %>%
  #   select(Status_Gauss, everything())
  # Stat_masq_Gauss
  # 
  # 
  # #Stat sur le masq de tau-argus
  # 
  # Stat_masq_tau<- masq2 %>% 
  #   group_by(is_secret_prim,is_secret = Status!="V") %>% 
  #   summarise(
  #     ncell_tau = n(),
  #     valcell_tau = sum(N_OBS)
  #   )
  # Stat_masq_tau<-Stat_masq_tau %>% 
  #   mutate(Status_tau = case_when(
  #     is_secret_prim ~ "B",
  #     is_secret ~"D",
  #     TRUE ~ "V"
  #   ))
  # 
  # Stat_masq_tau
  # Stat_masq_tau <- Stat_masq_tau[,(ncol(Stat_masq_tau)-2):ncol(Stat_masq_tau)]
  # sums <- colSums(Stat_masq_tau[,1:2])
  # Stat_masq_tau[,1]<-(Stat_masq_tau[,1]*100)/sums[1]
  # Stat_masq_tau[,2]<-(Stat_masq_tau[,2]*100)/sums[2]
  # Stat_masq_tau <- Stat_masq_tau %>%
  #   mutate(across(where(is.numeric), ~ round(., 2)))
  # sums <- c(sums,"Total")
  # Stat_masq_tau <- rbind(Stat_masq_tau,sums)
  # Stat_masq_tau <- Stat_masq_tau %>%
  #   select(Status_tau, everything())
  # Stat_masq_tau
  # 
  # Stat_tot <- cbind(Stat_masq_Gauss,Stat_masq_tau)
  # Stat_tot <- Stat_tot[,-4]
  # colnames(Stat_tot)[1] <- "Status"
  # Stat_tot <- Stat_tot[, c(1, 2, 4, 3,5)]
  # print(stats)
  latex <- stats %>% knitr::kable(format = "latex", digits = 1)
  latex_diff <- stats_n_diff %>% knitr::kable(format = "latex", digits = 1)
  return(list(stats = stats, n_diff = stats_n_diff, latex = latex, latex_diff = latex_diff))
}
