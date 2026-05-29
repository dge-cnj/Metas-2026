# Processamento de dados da Justiça Estadual

# Meta 2

Estadual.Meta02 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar até 31/12/2026
    t_0 <- "2026-01-01"         # Início do período
    t_1 <- "2023-01-01"         # 80% dos processos distribuídos até 31/12/2022 no 1º grau
    t_2 <- "2024-01-01"         # 90% dos processos distribuídos até 31/12/2023 no 2º grau
    t_jt <- "2024-01-01"        # 95% dos processos distribuídos até 31/12/2023 nos Juizados Especiais e Turmas Recursais
    t_ant <- "2012-01-01"       # 100% dos processos de conhecimento pendentes de julgamento há 15 anos (2011) ou mais
    
    # 1º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= t_1 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(sigla_grau == "G1" & data_total_primeira_baixa >= t_1 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(sigla_grau == "G1" & (data_total_primeira_baixa < t_1 | data_total_primeiro_julgamento_sem_pronuncia < t_1 | data_total_primeiro_procedimento_resolvido < t_1), 1, 0),
            decm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_procedimento_resolvido >= t_1 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum2_a = if_else(mpu_meta == TRUE & sigla_grau == "G1" & dt_primeira_medida_protetiva_meta >= t_1 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism2_a = if_else(sigla_grau == "G1" & dt_recebimento < t_1 & (data_total_primeiro_julgamento_sem_pronuncia >= t_1 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1" | mpum2_a =="1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    # 2º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_b = if_else(sigla_grau == "G2" & data_total_primeiro_julgamento_sem_pronuncia >= t_2 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_b = if_else(sigla_grau == "G2" & data_total_primeira_baixa >= t_2 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_b_ate = if_else(sigla_grau == "G2" & (data_total_primeira_baixa < t_2 | data_total_primeiro_julgamento_sem_pronuncia < t_2 | data_total_primeiro_procedimento_resolvido < t_2), 1, 0),
            decm2_b = if_else(sigla_grau == "G2" & data_total_primeiro_procedimento_resolvido >= t_2 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum2_b = if_else(mpu_meta == TRUE & sigla_grau == "G2" & dt_primeira_medida_protetiva_meta >= t_2 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism2_b = if_else(sigla_grau == "G2" & dt_recebimento < t_2 & (data_total_primeiro_julgamento_sem_pronuncia >= t_2 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_b == "1") & (baixm2_b_ate == 0 | is.na(baixm2_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_b = if_else(dism2_b == "1" & (primeirasentm2_b == "1" | decm2_b == "1" | baixm2_b == "1" | mpum2_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_b = if_else(dism2_b == "1" & (julgadom2_b == "0" | is.na(julgadom2_b)) & pendente_meta == "0", 1, 0))
    
    # Juizados e Turmas
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_c = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeiro_julgamento_sem_pronuncia >= t_jt & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_c = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeira_baixa >= t_jt & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_c_ate = if_else((sigla_grau == "JE"|sigla_grau == "TR") & (data_total_primeira_baixa < t_jt | data_total_primeiro_julgamento_sem_pronuncia < t_jt | data_total_primeiro_procedimento_resolvido < t_jt), 1, 0),
            decm2_c = if_else((sigla_grau == "JE"|sigla_grau == "TR") & data_total_primeiro_procedimento_resolvido >= t_jt & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum2_c = if_else(mpu_meta == TRUE & (sigla_grau == "JE"|sigla_grau == "TR") & dt_primeira_medida_protetiva_meta >= t_jt & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism2_c = if_else((sigla_grau == "JE"|sigla_grau == "TR") & dt_recebimento < t_jt & (data_total_primeiro_julgamento_sem_pronuncia >= t_jt | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_c == "1") & (baixm2_c_ate == 0 | is.na(baixm2_c_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_c = if_else(dism2_c == "1" & (primeirasentm2_c == "1" | decm2_c == "1" | baixm2_c == "1" | mpum2_c == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_c = if_else(dism2_c == "1" & (julgadom2_c == "0" | is.na(julgadom2_c)) & pendente_meta == "0", 1, 0))
    
    # Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(!func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(!func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(!func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm2_ant = if_else(!func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum2_ant = if_else(mpu_meta == TRUE & !func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism2_ant = if_else(!func.procura.array(lista=c(39,49,108),base=dge,variavel="id_ultima_classe") & dt_recebimento < t_ant & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1" | mpum2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))
    
    return(dge)
  
}

#Meta 03

Estadual.Meta03 <- function(dge, pre_processual) {
  
  t_f <- "2027-01-01"
  t_1 <- "2026-01-01" # Um ano antes de t_f (início do ano)
  t_2 <- "2025-01-01" # Dois anos antes de t_f (início do ano anterior)
  
  dge <- dge %>% 
    mutate(
      sent25 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & data_total_primeiro_julgamento_sem_pronuncia >= t_2 & data_total_primeiro_julgamento_sem_pronuncia < t_1, 1, 0),
      senth25 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & dt_primeiro_julgamento_homologatorio >= t_2 & dt_primeiro_julgamento_homologatorio < t_1, 1, 0),
      sent26 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & data_total_primeiro_julgamento_sem_pronuncia >= t_1 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
      senth26 = if_else((sigla_grau == "G1" | sigla_grau == "JE") & procedimento == "Conhecimento não criminal" & dt_primeiro_julgamento_homologatorio >= t_1 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))

  pre_processual <- pre_processual %>% 
    mutate(
      pre25 = if_else(dt_primeiro_julgamento_homologatorio >= t_2 & dt_primeiro_julgamento_homologatorio < t_1, 1, 0),
      pre26 = if_else(dt_primeiro_julgamento_homologatorio >= t_1 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))
  
  dge <- dge %>% full_join(pre_processual)
  
  return (dge)
  
}

# Meta 4 de 2025 – Priorizar o julgamento dos processos relativos aos crimes contra a administração pública, à improbidade administrativa e aos ilícitos eleitorais.

Estadual.Meta04 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026
    t_0 <- "2023-01-01"         # distribuídas até 31/12/2022
    
    # crimes contra a Administração Pública
    
    dge <- dge %>% mutate(
        primeirasentm4_a = if_else(flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
        baixm4_a = if_else(flg_crim_contr_adm_pbl == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
        baixm4_a_ate = if_else(flg_crim_contr_adm_pbl == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
        decm4_a = if_else(flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
        dism4_a = if_else(flg_crim_contr_adm_pbl == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    # ações de improbidade administrativa
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_b = if_else((flg_imp == "TRUE") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_b_ate = if_else((flg_imp == "TRUE") & data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_b = if_else((flg_imp == "TRUE") & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
  return (dge)
  
}

# Meta 6 de 2025 – Priorizar o julgamento das ações ambientais.

Estadual.Meta06 <- function(dge) {
  
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

# Meta 7 de 2025 – Priorizar o julgamento dos processos relacionados aos indígenas e quilombolas.

Estadual.Meta07 <- function(dge) {
    
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
    
    # Racismo e injúsria racial
    
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

# Meta 8 de 2025 – Priorizar o julgamento dos processos relacionados ao feminicídio e à violência doméstica e familiar contra as mulheres.

Estadual.Meta08 <- function(dge) {
    
    #'a' VD 'b' feminicídio
    
    t_f <- "2027-01-01"
    t_0 <- "2025-01-01"
    
    # Violência Doméstica
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_a = if_else(flag_violencia_domestica == TRUE & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_a = if_else(flag_violencia_domestica == TRUE & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_a_ate = if_else(flag_violencia_domestica == TRUE & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0 | dt_primeira_medida_protetiva_meta < t_0), 1, 0),
            decm8_a = if_else(flag_violencia_domestica == TRUE & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum8_a = if_else(mpu_meta == TRUE & flag_violencia_domestica == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism8_a = if_else(flag_violencia_domestica == TRUE & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | dt_primeira_medida_protetiva_meta >= t_0 | baixm8_a =="1") & (baixm8_a_ate == 0 | is.na(baixm8_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom8_a = if_else(dism8_a == "1" & (primeirasentm8_a == "1" | decm8_a == "1" | baixm8_a == "1" | mpum8_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_a = if_else(dism8_a == "1" & (julgadom8_a == "0" | is.na(julgadom8_a)) & pendente_meta == "0", 1, 0))
    
    # Feminicídio
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_b = if_else(flag_feminicidio == TRUE & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_b = if_else(flag_feminicidio == TRUE & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_b_ate = if_else(flag_feminicidio == TRUE & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0 | dt_primeira_medida_protetiva_meta < t_0), 1, 0),
            decm8_b = if_else(flag_feminicidio == TRUE & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum8_b = if_else(mpu_meta == TRUE & flag_violencia_domestica == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism8_b = if_else(flag_feminicidio == TRUE & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | dt_primeira_medida_protetiva_meta >= t_0 | baixm8_b =="1") & (baixm8_b_ate == 0 | is.na(baixm8_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom8_b = if_else(dism8_b == "1" & (primeirasentm8_b == "1" | decm8_b == "1" | baixm8_b == "1" | mpum8_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_b = if_else(dism8_b == "1" & (julgadom8_b == "0" | is.na(julgadom8_b)) & pendente_meta == "0", 1, 0))
    
  return (dge)
  
}

# Meta 10 de 2025 – Promover os direitos da criança e do adolescente.

Estadual.Meta10 <- function(dge) {
    
    t_f <- "2027-01-01"
    t_0 <- "2025-01-01"
    
    # 1º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm10_a = if_else(sigla_grau == "G1" & flg_inf == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm10_a = if_else(sigla_grau == "G1" & flg_inf == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm10_a_ate = if_else(sigla_grau == "G1" & flg_inf == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm10_a = if_else(sigla_grau == "G1" & flg_inf == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism10_a = if_else(sigla_grau == "G1" & flg_inf == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm10_a =="1") & (baixm10_a_ate == 0 | is.na(baixm10_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom10_a = if_else(dism10_a == "1" & (primeirasentm10_a == "1" | decm10_a == "1" | baixm10_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm10_a = if_else(dism10_a == "1" & (julgadom10_a == "0" | is.na(julgadom10_a)) & pendente_meta == "0", 1, 0))
    
    # 2º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm10_b = if_else(sigla_grau == "G2" & flg_inf == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm10_b = if_else(sigla_grau == "G2" & flg_inf == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm10_b_ate = if_else(sigla_grau == "G2" & flg_inf == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm10_b = if_else(sigla_grau == "G2" & flg_inf == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism10_b = if_else(sigla_grau == "G2" & flg_inf == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm10_b =="1") & (baixm10_b_ate == 0 | is.na(baixm10_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom10_b = if_else(dism10_b == "1" & (primeirasentm10_b == "1" | decm10_b == "1" | baixm10_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm10_b = if_else(dism10_b == "1" & (julgadom10_b == "0" | is.na(julgadom10_b)) & pendente_meta == "0", 1, 0))
    

  return (dge)
  
}

# 2026 - Consolidando dados do TST

Consolidado.Estadual <- function(dge) {
    
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/8),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/9),
            distm2_c = sum(dism2_c,na.rm = TRUE),
            julgm2_c = sum(julgadom2_c,na.rm = TRUE),
            suspm2_c = sum(susm2_c,na.rm = TRUE),
            cumprimento_meta2c = (sum(julgadom2_c,na.rm = TRUE)/(sum(dism2_c,na.rm = TRUE)-sum(susm2_c,na.rm = TRUE)))*(1000/9.5),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)-sum(desm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta3 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mesano_senth) %>% 
        summarise(
            quant_sent25 = sum(sent25,na.rm = TRUE),
            quant_conc25 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE)),
            quant_sent26 = sum(sent26,na.rm = TRUE),
            quant_conc26 = (sum(senth26,na.rm = TRUE)+sum(pre26,na.rm = TRUE)),
            IC2025 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE))/sum(sent25,na.rm = TRUE),
            IC2026 = (sum(senth26,na.rm = TRUE)+sum(pre26,na.rm = TRUE))/sum(sent26,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2026 > (IC2025+0.01),(IC2026/(IC2025+0.01))*100,if_else(IC2026>="0.18",100,(IC2026/(IC2025+0.01))*100)))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/6.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE) /(sum(dism4_b,na.rm = TRUE) - sum(susm4_b,na.rm = TRUE)))*(1000/10))
    
    meta6 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE) /(sum(dism6_a,na.rm = TRUE) - sum(susm6_a,na.rm = TRUE)))*(1000/5))
    
    meta7 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (sum(julgadom7_a,na.rm = TRUE)/(sum(dism7_a,na.rm = TRUE) - sum(susm7_a,na.rm = TRUE)))*(1000/5),
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),
            cumprimento_meta7b = (sum(julgadom7_b,na.rm = TRUE) /(sum(dism7_b,na.rm = TRUE) - sum(susm7_b,na.rm = TRUE)))*(1000/5),
            distm7_c = sum(dism7_c,na.rm = TRUE),
            julgm7_c = sum(julgadom7_c,na.rm = TRUE),
            suspm7_c = sum(susm7_c,na.rm = TRUE),
            cumprimento_meta7c = (sum(julgadom7_c,na.rm = TRUE) /(sum(dism7_c,na.rm = TRUE) - sum(susm7_c,na.rm = TRUE)))*(1000/5))
    
    meta8 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm8_a = sum(dism8_a,na.rm = TRUE),
            julgm8_a = sum(julgadom8_a,na.rm = TRUE),
            suspm8_a = sum(susm8_a,na.rm = TRUE),
            cumprimento_meta8a = (sum(julgadom8_a,na.rm = TRUE)/(sum(dism8_a,na.rm = TRUE) - sum(susm8_a,na.rm = TRUE)))*(1000/9),
            distm8_b = sum(dism8_b,na.rm = TRUE),
            julgm8_b = sum(julgadom8_b,na.rm = TRUE),
            suspm8_b = sum(susm8_b,na.rm = TRUE),
            cumprimento_meta8b = (sum(julgadom8_b,na.rm = TRUE) /(sum(dism8_b,na.rm = TRUE) - sum(susm8_b,na.rm = TRUE)))*(1000/7.5))
    
    meta10 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE)/(sum(dism10_a,na.rm = TRUE)-sum(susm10_a,na.rm = TRUE)))*(1000/9),
            distm10_b = sum(dism10_b,na.rm = TRUE),
            julgm10_b = sum(julgadom10_b,na.rm = TRUE),
            suspm10_b = sum(susm10_b,na.rm = TRUE),
            cumprimento_meta10b = (sum(julgadom10_b,na.rm = TRUE)/(sum(dism10_b,na.rm = TRUE)-sum(susm10_b,na.rm = TRUE)))*(1000/9.9))
    
    return (list(meta2=meta2, meta3=meta3, meta4=meta4, meta6=meta6, meta7=meta7, meta8=meta8, meta10=meta10))    
        
}
# Hub principal, para o STJ



# 2026 - Gerando resumo dos dados do TSE

Resumo.Estadual <- function(dge) {
    
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
            cumprimento_meta2a = (julgm2_a / (distm2_a - suspm2_a))*(1000/8),
            
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (julgm2_b / (distm2_b - suspm2_b))*(1000/9),
            
            distm2_c = sum(distm2_c,na.rm = TRUE),
            julgm2_c = sum(julgm2_c,na.rm = TRUE),
            suspm2_c = sum(suspm2_c,na.rm = TRUE),
            cumprimento_meta2c = (julgm2_c / (distm2_c - suspm2_c))*(1000/9.5),
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE), 
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant /(distm2_ant - suspm2_ant - desom2_ant))*(1000/10),

            quant_sent25 = sum(quant_sent25,na.rm = TRUE),
            quant_conc25 = sum(quant_conc25,na.rm = TRUE),
            quant_sent26 = sum(quant_sent26,na.rm = TRUE),
            quant_conc26 = sum(quant_conc26,na.rm = TRUE),
            IC2025 = (sum(quant_conc25,na.rm = TRUE))/sum(quant_sent25,na.rm = TRUE),
            IC2026 = (sum(quant_conc26,na.rm = TRUE))/sum(quant_sent26,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2026 > (IC2025+0.01),(IC2026/(IC2025+0.01))*100,if_else(IC2026>="0.17",100,(IC2026/(IC2025+0.01))*100)),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a/(distm4_a - suspm4_a))*(1000/6.5),
            
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (julgm4_b /(distm4_b -suspm4_b))*(1000/10),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a /(distm6_a-suspm6_a))*(1000/5),

            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a/(distm7_a - suspm7_a))*(1000/5),
            
            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b /(distm7_b - suspm7_b))*(1000/5),

            distm7_c = sum(distm7_c,na.rm = TRUE),
            julgm7_c = sum(julgm7_c,na.rm = TRUE),
            suspm7_c = sum(suspm7_c,na.rm = TRUE),
            cumprimento_meta7c = (julgm7_c /(distm7_c - suspm7_c))*(1000/5),

            distm8_a = sum(distm8_a,na.rm = TRUE),
            julgm8_a = sum(julgm8_a,na.rm = TRUE),
            suspm8_a = sum(suspm8_a,na.rm = TRUE),
            cumprimento_meta8a = (julgm8_a /(distm8_a - suspm8_a))*(1000/9),
            
            distm8_b = sum(distm8_b,na.rm = TRUE),
            julgm8_b = sum(julgm8_b,na.rm = TRUE),
            suspm8_b = sum(suspm8_b,na.rm = TRUE),
            cumprimento_meta8b = (julgm8_b /(distm8_b-suspm8_b))*(1000/7.5),

            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a /(distm10_a - suspm10_a))*(1000/9),
            
            distm10_b = sum(distm10_b,na.rm = TRUE),
            julgm10_b = sum(julgm10_b,na.rm = TRUE),
            suspm10_b = sum(suspm10_b,na.rm = TRUE),
            cumprimento_meta10b = (julgm10_b / (distm10_b - suspm10_b))*(1000/9.9),

            .groups = "drop"
        )
    print(Resumo)
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}


ProcessarDados.Estadual <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do ", x))

    aux <- aux[grepl(paste0("Extrai_Datamart_", x), aux)]

    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)

        RegistrarLOG(paste0("Flags Ok!"))
        
        temp$dge <- Determinar.Meta01(temp$dge)
        temp$dge <- Estadual.Meta02(temp$dge)
        temp$dge <- Estadual.Meta03(temp$dge, temp$pre_processual)
        temp$dge <- Estadual.Meta04(temp$dge)
        temp$dge <- Estadual.Meta06(temp$dge)
        temp$dge <- Estadual.Meta07(temp$dge)
        temp$dge <- Estadual.Meta08(temp$dge)
        temp$dge <- Estadual.Meta10(temp$dge)

        dge_total <- rbind(dge_total, temp$dge)
        
        rm (temp)
        gc()
    
        RegistrarLOG(paste0("Arquivo ", aux[i], "processado."))
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
                mesano_cnm1))

    dge_total <- dge_total %>% left_join(local, by=c("id_ultimo_oj"="id_orgao_julgador"))

    metas <- Consolidado.Estadual(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    meta3 <- metas$meta3
    meta4 <- metas$meta4
    meta6 <- metas$meta6
    meta7 <- metas$meta7
    meta8 <- metas$meta8
    meta10 <- metas$meta10
    list_metas = list(meta1, meta2, meta3, meta4, meta6, meta7, meta8, meta10)

    metas_compilado <- distinct(list_metas %>% reduce(bind_rows))
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.Estadual(metas_compilado)

    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_", x,".csv")))
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
