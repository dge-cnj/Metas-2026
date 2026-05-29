# Processamento de dados do STJ

# Meta 2

STJ.Meta02 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2020-01-01"
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_a = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_a = if_else(dism2_a == "1" & (julgadom2_a=="0"|is.na(julgadom2_a)) & (susm2_a=="0"|is.na(susm2_a)) & flg_dessobrestado == "1", 1, 0))
    
    return(dge)

}

# Meta 4 

STJ.Meta04 <- function(dge) {

    t_f <- "2027-01-01"         # Julgar, até 31/12/2026
    t_0 <- "2023-01-01"         # distribuídas até 31/12/2022
        
    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
        
    return (dge)
    
}

# Meta 6 

STJ.Meta06 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01"
    
    dge <- dge %>%
        mutate(
            primeirasentm6_a = if_else(flg_amb == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm6_a = if_else(flg_amb == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm6_a_ate = if_else(flg_amb == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm6_a = if_else(flg_amb == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism6_a = if_else(flg_amb == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm6_a =="1") & (baixm6_a_ate == 0 | is.na(baixm6_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom6_a = if_else(dism6_a == "1" & (primeirasentm6_a == "1" | decm6_a == "1" | baixm6_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm6_a = if_else(dism6_a == "1" & (julgadom6_a == "0" | is.na(julgadom6_a)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Meta 7

STJ.Meta07 <- function(dge) {

    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01"
    
    # Comunidades Indígenas
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_a = if_else(flg_ind == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_a = if_else(flg_ind == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_a_ate = if_else(flg_ind == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_a = if_else(flg_ind == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_a = if_else(flg_ind == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_a =="1") & (baixm7_a_ate == 0 | is.na(baixm7_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_a = if_else(dism7_a == "1" & (primeirasentm7_a == "1" | decm7_a == "1" | baixm7_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_a = if_else(dism7_a == "1" & (julgadom7_a == "0" | is.na(julgadom7_a)) & pendente_meta == "0", 1, 0))
    
    # Quilombolas
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_b = if_else(flg_qui == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_b = if_else(flg_qui == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_b_ate = if_else(flg_qui == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_b = if_else(flg_qui == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_b = if_else(flg_qui == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_b =="1") & (baixm7_b_ate == 0 | is.na(baixm7_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_b = if_else(dism7_b == "1" & (primeirasentm7_b == "1" | decm7_b == "1" | baixm7_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_b = if_else(dism7_b == "1" & (julgadom7_b == "0" | is.na(julgadom7_b)) & pendente_meta == "0", 1, 0))

    # Racismo e injúria racial
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_c = if_else(flg_rac_inj == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_c = if_else(flg_rac_inj == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_c_ate = if_else(flg_rac_inj == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_c = if_else(flg_rac_inj == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_c = if_else(flg_rac_inj == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_c =="1") & (baixm7_c_ate == 0 | is.na(baixm7_c_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_c = if_else(dism7_c == "1" & (primeirasentm7_c == "1" | decm7_c == "1" | baixm7_c == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_c = if_else(dism7_c == "1" & (julgadom7_c == "0" | is.na(julgadom7_c)) & pendente_meta == "0", 1, 0))

    return (dge)

}

# Meta 8 

STJ.Meta08 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2025-01-01"
    
    #Violência Doméstica
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_a_ate = if_else(flag_violencia_domestica == "1" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0), 1, 0),
            decm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f | data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism8_a = if_else(flag_violencia_domestica == "1" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia)), 1, 0))
    dge <- dge %>% mutate(julgadom8_a = if_else(dism8_a == "1" & (primeirasentm8_a == "1" | decm8_a == "1" | baixm8_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_a = if_else(dism8_a == "1" & (julgadom8_a == "0" | is.na(julgadom8_a)) & pendente_meta == "0", 1, 0))
    
    #Feminicídio (a seguir, flag_feminicidio comparado com tRUEfoi trocado por flag_fem)
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_b = if_else(flag_feminicidio == "1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_b = if_else(flag_feminicidio == "1" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_b_ate = if_else(flag_feminicidio == "1" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_f), 1, 0),
            decm8_b = if_else(flag_feminicidio == "1" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism8_b = if_else(flag_feminicidio == "1" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia)), 1, 0))
    dge <- dge %>% mutate(julgadom8_b = if_else(dism8_b == "1" & (primeirasentm8_b == "1" | decm8_b == "1" | baixm8_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_b = if_else(dism8_b == "1" & (julgadom8_b == "0" | is.na(julgadom8_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Meta 10 

STJ.Meta10 <- function(dge) {

    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01"
    
    dge <- dge %>% 
        mutate(
            primeirasentm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm10_a = if_else(flg_seq == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm10_a_ate = if_else(flg_seq == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism10_a = if_else(flg_seq == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm10_a =="1") & (baixm10_a_ate == 0 | is.na(baixm10_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom10_a = if_else(dism10_a == "1" & (primeirasentm10_a == "1" | decm10_a == "1" | baixm10_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm10_a = if_else(dism10_a == "1" & (julgadom10_a == "0" | is.na(julgadom10_a)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}



Consolidado.STJ <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_ant = sum(dism2_a,na.rm = TRUE), # distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_a,na.rm = TRUE), # julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_ant = sum(susm2_a,na.rm = TRUE), # suspm2_a = sum(susm2_a,na.rm = TRUE),
            desom2_ant = sum(desm2_a,na.rm = TRUE), # desom2_a = sum(desm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_ant / (distm2_ant - desom2_ant - suspm2_ant))*(1000/10))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/10))
    
    meta6 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE)/(sum(dism6_a,na.rm = TRUE)-sum(susm6_a,na.rm = TRUE)))*(1000/7.5))
    
    meta7 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (sum(julgadom7_a,na.rm = TRUE)/(sum(dism7_a,na.rm = TRUE)-sum(susm7_a,na.rm = TRUE)))*(1000/8),
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),                
            cumprimento_meta7 = (sum(julgadom7_b,na.rm = TRUE)/(sum(dism7_b,na.rm = TRUE)-sum(susm7_b,na.rm = TRUE)))*(1000/8),            
            distm7_c = sum(dism7_c,na.rm = TRUE),
            julgm7_c = sum(julgadom7_c,na.rm = TRUE),
            suspm7_c = sum(susm7_c,na.rm = TRUE),                
            cumprimento_meta7 = (sum(julgadom7_c,na.rm = TRUE)/(sum(dism7_c,na.rm = TRUE)-sum(susm7_c,na.rm = TRUE)))*(1000/8))
    
    meta8 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm8_a = sum(dism8_a,na.rm = TRUE),
            julgm8_a = sum(julgadom8_a,na.rm = TRUE),
            suspm8_a = sum(susm8_a,na.rm = TRUE),
            cumprimento_meta8a = (sum(julgadom8_a,na.rm = TRUE)/(sum(dism8_a,na.rm = TRUE)-sum(susm8_a,na.rm = TRUE)))*(1000/10),
            distm8_b = sum(dism8_b,na.rm = TRUE),
            julgm8_b = sum(julgadom8_b,na.rm = TRUE),
            suspm8_b = sum(susm8_b,na.rm = TRUE),
            cumprimento_meta8b = (sum(julgadom8_b,na.rm = TRUE)/(sum(dism8_b,na.rm = TRUE)-sum(susm8_b,na.rm = TRUE)))*(1000/10))
    
    meta10 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE)/(sum(dism10_a,na.rm = TRUE)-sum(susm10_a,na.rm = TRUE)))*(1000/10))
    
    return (list(meta2=meta2, meta4=meta4, meta6=meta6, meta7=meta7, meta8=meta8, meta10=meta10))    
    
}






Resumo.STJ <- function(dge) {
    
    Resumo <- dge %>%
      
        group_by(sigla_tribunal) %>%
      
        summarise (
          
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2026 = sum(casos_novos_2026,na.rm = TRUE),
            julgados_2026 = sum(julgados_2026,na.rm = TRUE),
            prim_sent2026 = sum(prim_sent2026, na.rm = TRUE),
            suspensos_2026 = sum(suspensos_2026,na.rm = TRUE),
            dessobrestados_2026 = sum(dessobrestados_2026,na.rm = TRUE),
            cumprimento_meta1 = julgados_2026/(casos_novos_2026 + dessobrestados_2026 - suspensos_2026)*100,
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant / (distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a / (distm4_a - suspm4_a))*(1000/10),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a / (distm6_a - suspm6_a))*(1000/7.5),
            
            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a))*(1000/8),

            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b / (distm7_b - suspm7_b))*(1000/8),

            distm7_c = sum(distm7_c,na.rm = TRUE),
            julgm7_c = sum(julgm7_c,na.rm = TRUE),
            suspm7_c = sum(suspm7_c,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_c / (distm7_c - suspm7_c))*(1000/8),
            
            distm8_a = sum(distm8_a,na.rm = TRUE),
            julgm8_a = sum(julgm8_a,na.rm = TRUE),
            suspm8_a = sum(suspm8_a,na.rm = TRUE),
            cumprimento_meta8a = (julgm8_a / (distm8_a - suspm8_a))*(1000/10),
            
            distm8_b = sum(distm8_b,na.rm = TRUE),
            julgm8_b = sum(julgm8_b,na.rm = TRUE),
            suspm8_b = sum(suspm8_b,na.rm = TRUE),
            cumprimento_meta8b = (julgm8_b / (distm8_b - suspm8_b))*(1000/10),
            
            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a /(distm10_a - suspm10_a))*(1000/10),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}

# Hub principal, para o STJ

ProcessarDados.STJ <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do STJ"))

    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        pre_processual <- temp$pre_processual
    
        dge <- Determinar.Meta01(dge)
        dge <- STJ.Meta02(dge)
        dge <- STJ.Meta04(dge)
        dge <- STJ.Meta06(dge)
        dge <- STJ.Meta07(dge)
        dge <- STJ.Meta08(dge)
        dge <- STJ.Meta10(dge)

        dge_total <- rbind(dge_total, dge)
    
        rm (temp, pre_processual, dge)
        gc()

    } # Fim do for (i in  seq_along(aux))

    GerarArquivos(dge_total, x)

    local <- BuscarLocais()
                
    dge_total <- dge_total %>%
        mutate(
            mesano_cnm1 = formatar_mes_ano(substring(dt_recebimento,1,7)),
            mesano_senth = formatar_mes_ano(substring(dt_primeiro_julgamento_homologatorio,1,7)),
            mesano_sent = formatar_mes_ano(substring(dt_resolvido,1,7)),
            mesano_cnm1 = if_else(
                cnm1 == "1" & as.Date(substr(dt_recebimento,1,10)) < as.Date("2025-12-20"),
                "Dez 2025",
                mesano_cnm1)) # era ... 6,7
        
    dge_total <- dge_total %>% left_join(local, by=c("id_ultimo_oj"="id_orgao_julgador"))
    dge_total <- distinct(dge_total)
    
    metas <- Consolidado.STJ(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    meta4 <- metas$meta4
    meta6 <- metas$meta6
    meta7 <- metas$meta7
    meta8 <- metas$meta8
    meta10 <- metas$meta10
    list_metas = list(meta1, meta2, meta4, meta6, meta7, meta8, meta10)

    metas_compilado <- distinct(list_metas %>% reduce(bind_rows))
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.STJ(metas_compilado)

    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_STJ.csv")))
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
