
# Meta 1 de 2026 – Julgar mais processos que os distribuídos

Determinar.Meta01 <- function(dge) {
        
    t_0 <- "2025-12-20"     
    t_f <- "2026-12-20"     
    ant_0 <- "2025-01-01"   
    
    ref <- 20270101  ### Alterado para a meta 1
    
    dge <- dge %>% mutate(
        cnm1 = if_else((dt_recebimento >= t_0 & dt_recebimento <t_f), 1, 0),
        baixm1_ate = if_else((data_total_primeira_baixa < t_0 & !is.na(data_total_primeira_baixa)) | (data_total_primeiro_julgamento_sem_pronuncia < t_0 & !is.na(data_total_primeiro_julgamento_sem_pronuncia)) | (data_total_primeiro_procedimento_resolvido < t_0 & !is.na(data_total_primeiro_procedimento_resolvido)) , "1", "0"),
        primeirasentm1 = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
        baixm1 = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f & is.na(data_total_primeiro_julgamento_sem_pronuncia), 1, 0),
        decm1 = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
        mpum1 = if_else(mpu_meta == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0))
    dge <- dge %>% mutate(
        cnm1 = if_else((id_ultima_classe == "12377" | id_ultima_classe == "12193") & dt_recebimento >= ant_0 & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | data_total_primeira_baixa >= t_0), 1 , cnm1),
        cnm1 = if_else((id_ultima_classe == "12377" | id_ultima_classe == "12193") & dt_recebimento >= t_0 & dt_recebimento < t_f & (is.na(data_total_primeiro_julgamento_sem_pronuncia) | data_total_primeiro_julgamento_sem_pronuncia >= t_f), 0 , cnm1))
    dge <- dge %>% mutate (julgadom1 = if_else(baixm1_ate != "1" & (primeirasentm1 == "1" | decm1 == "1" | baixm1 == "1" | mpum1 == "1"), 1, 0))
    dge <- dge %>% mutate(desm1 = if_else(cnm1 != "1" & dt_recebimento < t_0 & flg_dessobrestado == "1" & (dt_resolvido >= t_0 | is.na(dt_resolvido)) & dt_resolvido >= ymd(ultimo_dessobrestado), 1, 0))
    dge <- dge %>% mutate(susm1 = if_else((cnm1 == "1" | desm1 == "1") & (julgadom1 == "0" | is.na(julgadom1)) & pendente_meta == "0", 1, 0))
    
    return(dge)
  
}

Consolidado.Meta01 <- function(dge) {
    
    meta1 <- dge %>% 
      
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mesano_cnm1, mesano_sent) %>% 
        summarise(
            casos_novos_2026 = sum(cnm1,na.rm = TRUE),
            julgados_2026 = sum(julgadom1,na.rm = TRUE),
            prim_sent2026 = sum(primeirasentm1, na.rm = TRUE),
            suspensos_2026 = sum(susm1,na.rm = TRUE),
            dessobrestados_2026 = sum(desm1,na.rm = TRUE),
            cumprimento_meta1 = 100 * (julgados_2026) / (casos_novos_2026 + dessobrestados_2026 - suspensos_2026))
    
    return(meta1)
    
}
