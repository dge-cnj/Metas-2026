# Processamento de dados da Justiça Estadual

# Meta 2

Federal.Meta02 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026
    t_0 <- "2026-01-01"         # Início do ano
    t_12 <- "2023-01-01"        # 85% dos processos distribuídos até 31/12/2022 no 1º e 2º grau
    t_jt <- "2024-01-01"        # 100% dos processos distribuídos até 31/12/2023 nos Juizados Especiais Federais e nas Turmas Recursais
    ant <- "2011-01-01"         # todos os processos distribuídos há 15 anos (2011)
    
    # 1º e 2º graus
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else((sigla_grau == "G1" | sigla_grau == "G2") & data_total_primeiro_julgamento_sem_pronuncia >= t_12 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else((sigla_grau == "G1" | sigla_grau == "G2") & data_total_primeira_baixa >= t_12 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else((sigla_grau == "G1" | sigla_grau == "G2") & (data_total_primeira_baixa < t_12 | data_total_primeiro_julgamento_sem_pronuncia < t_12 | data_total_primeiro_procedimento_resolvido < t_12), 1, 0),
            decm2_a = if_else((sigla_grau == "G1" | sigla_grau == "G2") & data_total_primeiro_procedimento_resolvido >= t_12 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else((sigla_grau == "G1" | sigla_grau == "G2") & dt_recebimento < t_12 & (data_total_primeiro_julgamento_sem_pronuncia >= t_12 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    # Juizados e Turmas
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_b = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeiro_julgamento_sem_pronuncia >= t_jt & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_b = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeira_baixa >= t_jt & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_b_ate = if_else((sigla_grau == "JE"|sigla_grau == "TR") & (data_total_primeira_baixa < t_jt | data_total_primeiro_julgamento_sem_pronuncia < t_jt | data_total_primeiro_procedimento_resolvido < t_jt), 1, 0),
            decm2_b = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeiro_procedimento_resolvido >= t_jt & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_b = if_else((sigla_grau == "JE"|sigla_grau == "TR") & dt_recebimento < t_jt & (data_total_primeiro_julgamento_sem_pronuncia >= t_jt | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_b == "1") & (baixm2_b_ate == 0 | is.na(baixm2_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_b = if_else(dism2_b == "1" & (primeirasentm2_b == "1" | decm2_b == "1" | baixm2_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_b = if_else(dism2_b == "1" & (julgadom2_b == "0" | is.na(julgadom2_b)) & pendente_meta == "0", 1, 0))
    
    # Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= ant & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= ant & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(data_total_primeira_baixa < ant | data_total_primeiro_julgamento_sem_pronuncia < ant | data_total_primeiro_procedimento_resolvido < ant, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= ant & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < ant & (data_total_primeiro_julgamento_sem_pronuncia >= ant | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))  
    
    return (dge)
    
} 

# Meta 3

Federal.Meta03 <- function(dge, pre_processual) {
    
    t_f <- "2027-01-01"
    t_1 <- "2026-01-01"
    t_2 <- "2024-01-01"
    
    dge <- dge %>% 
        mutate(
            sent24_25 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & data_total_primeiro_julgamento_sem_pronuncia >= t_2 & data_total_primeiro_julgamento_sem_pronuncia < t_1, 1, 0),
            senth24_25 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & dt_primeiro_julgamento_homologatorio >= t_2 & dt_primeiro_julgamento_homologatorio < t_1, 1, 0),
            sent26 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & data_total_primeiro_julgamento_sem_pronuncia >= t_1 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            senth26 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & dt_primeiro_julgamento_homologatorio >= t_1 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))
    
    pre_processual <- pre_processual %>% 
        mutate(
            pre24_25 = if_else(dt_primeiro_julgamento_homologatorio >= t_2 & dt_primeiro_julgamento_homologatorio < t_1, 1, 0),
            pre26 = if_else(dt_primeiro_julgamento_homologatorio >= t_1 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))
    
    dge <- dge %>% full_join(pre_processual)
    
    return (dge)
    
} 

# Meta 4

Federal.Meta04 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026
    t_0 <- "2024-01-01"         # 85% das ações penais relacionadas a crimes contra a AP e 85% das ações de improbidade administrativa, distribuídas até 31/12/2023
    
    # Crimes contra a administração pública

    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else((flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else((flg_crim_contr_adm_pbl == "TRUE") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else((flg_crim_contr_adm_pbl == "TRUE") & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm4_a = if_else((flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else((flg_crim_contr_adm_pbl == "TRUE") & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    # Ações de improbidade administrativa
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_b = if_else((flg_imp == "TRUE") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_b_ate = if_else((flg_imp == "TRUE") & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_b = if_else((flg_imp == "TRUE") & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)
} 

# Meta 6 

Federal.Meta06 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026:
    t_0 <- "2026-01-01"         # distribuídos até 31/12/2025.
    
    # Ambiental
    
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

Federal.Meta07 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01"
    
    # Comunidades Indígenas
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_a= if_else(flg_ind == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_a= if_else(flg_ind == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_a_ate = if_else(flg_ind == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_a= if_else(flg_ind == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_a= if_else(flg_ind == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_a=="1") & (baixm7_a_ate == 0 | is.na(baixm7_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_a= if_else(dism7_a== "1" & (primeirasentm7_a== "1" | decm7_a== "1" | baixm7_a== "1"), 1, 0))
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
            dism7_c = if_else(flg_rac_inj == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_b =="1") & (baixm7_c_ate == 0 | is.na(baixm7_c_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_c = if_else(dism7_c == "1" & (primeirasentm7_c == "1" | decm7_c == "1" | baixm7_c == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_c = if_else(dism7_c == "1" & (julgadom7_c == "0" | is.na(julgadom7_c)) & pendente_meta == "0", 1, 0))
    
    # TRF1 e TRF6
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6"))fator <- 1000/2.5
    else fator <- 1000/3.8        # TRF2, 3, 4, 5
    return (dge)
    
} 

# Meta 10

Federal.Meta10 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01"

    # Subtração internacional de crianças
    
    if(unique(dge$ramo_justica) == "Justiça Federal") {
        dge <- dge %>% 
            mutate(
                primeirasentm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
                baixm10_a = if_else(flg_seq == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
                baixm10_a_ate = if_else(flg_seq == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
                decm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
                dism10_a = if_else(flg_seq == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm10_a =="1") & (baixm10_a_ate == 0 | is.na(baixm10_a_ate)), 1, 0))
        dge <- dge %>% mutate(julgadom10_a = if_else(dism10_a == "1" & (primeirasentm10_a == "1" | decm10_a == "1" | baixm10_a == "1"), 1, 0))
        dge <- dge %>% mutate(susm10_a = if_else(dism10_a == "1" & (julgadom10_a == "0" | is.na(julgadom10_a)) & pendente_meta == "0", 1, 0))
    }
    
    return (dge)
    
} 


# 2026 - Consolidando dados da Justiça Federal

Consolidado.JusticaFederal <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/8.5),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/10),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)-sum(desm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta3 <- dge %>%
        
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mesano_senth) %>% 
        
        summarise(
            
            IC24_25 = (sum(senth24_25,na.rm = TRUE)+sum(pre24_25,na.rm = TRUE))/sum(sent24_25,na.rm = TRUE),
            IC2026 = (sum(senth26,na.rm = TRUE)+sum(pre26,na.rm = TRUE))/sum(sent26,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2026 > (IC24_25+0.005),(IC2026/(IC24_25+0.005))*100,if_else(IC2026>="0.08",100,(IC2026/(IC24_25+0.005))*100)),
            quant_sent24_25 = sum(sent24_25,na.rm = TRUE),
            quant_conc24_25 = (sum(senth24_25,na.rm = TRUE)+sum(pre24_25,na.rm = TRUE)),
            quant_sent26 = sum(sent26,na.rm = TRUE),
            quant_conc26 = (sum(senth26,na.rm = TRUE)+sum(pre26,na.rm = TRUE)))
    

    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE) / (sum(dism4_a,na.rm = TRUE) - sum(susm4_a,na.rm = TRUE)))*(1000/8.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE) / (sum(dism4_b,na.rm = TRUE) - sum(susm4_b,na.rm = TRUE)))*(1000/8.5))
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6")) fator <- 1000/2.5
    else fator <- 1000/3.8        # TRF2, 3, 4, 5
    
    meta6 <- dge %>% 
        
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        
        summarise(
            
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE) / (sum(dism6_a,na.rm = TRUE) - sum(susm6_a,na.rm = TRUE)))*fator)
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6")) fator <- 1000/2.5
    else fator <- 1000/3.5        # TRF2, 3, 4, 5
    
    meta7 <- dge %>% 
      
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
      
        summarise(
          
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a)) * fator,
            
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b / (distm7_b - suspm7_b)) * fator,

            distm7_c = sum(dism7_c,na.rm = TRUE),
            julgm7_c = sum(julgadom7_c,na.rm = TRUE),
            suspm7_c = sum(susm7_c,na.rm = TRUE),
            cumprimento_meta7c = (julgm7_c / (distm7_c - suspm7_c)) * (1000/5))
    
    meta10 <- dge %>% 
      
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
      
        summarise(
          
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE) / (sum(dism10_a,na.rm = TRUE) - sum(susm10_a,na.rm = TRUE)))*(1000/10))
    
    
    return (list(meta2=meta2, meta3=meta3, meta4=meta4, meta6=meta6, meta7=meta7, meta10=meta10))    
    
}




# 2026 - Gerando resumo dos dados da Justiça Federal

Resumo.JusticaFederal <- function(dge) {
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6")) {
        fator6 <- 1000/2.5
        fator7 <- 1000/2.5
    }
    else {
        fator6 <- 1000/3.8        # TRF2, 3, 4, 5
        fator7 <- 1000/3.5
    }
    
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
            
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a /( distm2_a - suspm2_a))*(1000/8.5),

            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (julgm2_b / (distm2_b - suspm2_b))*(1000/10),

            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE), 
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant / (distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            
            quant_sent24_25 = sum(quant_sent24_25,na.rm = TRUE),
            quant_conc24_25 = sum(quant_conc24_25,na.rm = TRUE),
            quant_sent26 = sum(quant_sent26,na.rm = TRUE),
            quant_conc26 = sum(quant_conc26,na.rm = TRUE),
            IC24_25 = quant_conc24_25/quant_sent24_25,
            IC2026 = quant_conc26/quant_sent24_25,
            cumprimento_meta3 = if_else(IC2026 > (IC24_25+0.005),(IC2026/(IC24_25+0.005))*100,if_else(IC2026>="0.08",100,(IC2026/(IC24_25+0.005))*100)),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a / (distm4_a - suspm4_a))*(1000/8.5),

            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (julgm4_b / (distm4_b - suspm4_b))*(1000/8.5),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a / (distm6_a - suspm6_a)) * fator6,
            
            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a))*fator7,
            
            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b/(distm7_b-suspm7_b))*fator7,

            distm7_c = sum(distm7_c,na.rm = TRUE),
            julgm7_c = sum(julgm7_c,na.rm = TRUE),
            suspm7_c = sum(suspm7_c,na.rm = TRUE),
            cumprimento_meta7c = (julgm7_c/(distm7_c-suspm7_c))*(1000/5),

            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a/(distm10_a - suspm10_a))*(1000/10),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}



ProcessarDados.Federal <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do ", x))
    aux <- aux[grepl(paste0("Extrai_Datamart_", x), aux)]
    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        pre_processual <- temp$pre_processual
    
        dge <- Determinar.Meta01(dge)
        dge <- Federal.Meta02(dge)
        dge <- Federal.Meta03(dge, pre_processual)
        dge <- Federal.Meta04(dge)
        dge <- Federal.Meta06(dge)
        dge <- Federal.Meta07(dge)
        dge <- Federal.Meta10(dge)

        dge_total <- rbind(dge_total, dge)

        rm (temp, pre_processual, dge)
        gc()

        RegistrarLOG(aux[i])

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

    metas <- Consolidado.JusticaFederal(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    meta3 <- metas$meta3
    meta4 <- metas$meta4
    meta6 <- metas$meta6
    meta7 <- metas$meta7
    meta10 <- metas$meta10
    list_metas = list(meta1, meta2, meta3, meta4, meta6, meta7, meta10)

    metas_compilado <- distinct(list_metas %>% reduce(bind_rows))
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.JusticaFederal(metas_compilado)

    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_", x,".csv")))
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
