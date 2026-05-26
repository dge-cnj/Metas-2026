
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
            baixm8_a_ate = if_else(flag_violencia_domestica == TRUE & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm8_a = if_else(flag_violencia_domestica == TRUE & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum8_a = if_else(mpu_meta == TRUE & flag_violencia_domestica == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism8_a = if_else(flag_violencia_domestica == TRUE & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm8_a =="1") & (baixm8_a_ate == 0 | is.na(baixm8_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom8_a = if_else(dism8_a == "1" & (primeirasentm8_a == "1" | decm8_a == "1" | baixm8_a == "1" | mpum8_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_a = if_else(dism8_a == "1" & (julgadom8_a == "0" | is.na(julgadom8_a)) & pendente_meta == "0", 1, 0))
    
    # Feminicídio
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_b = if_else(flag_feminicidio == TRUE & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_b = if_else(flag_feminicidio == TRUE & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_b_ate = if_else(flag_feminicidio == TRUE & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm8_b = if_else(flag_feminicidio == TRUE & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            mpum8_b = if_else(mpu_meta == TRUE & flag_violencia_domestica == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0),
            dism8_b = if_else(flag_feminicidio == TRUE & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm8_b =="1") & (baixm8_b_ate == 0 | is.na(baixm8_b_ate)), 1, 0))
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

# Hub principal, para a Justiça Estadual

ProcessarDados.Estadual <- function(path, aux, JusticaEstadual) {
    
    for (x in JusticaEstadual) {
        
        RegistrarLOG(paste0("Processando dados do ", x))
        
        for (i in  seq_along(aux)){
          
            if (grepl(x, aux[i])) {

                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                temp <- iniciarFlags(path, aux, i)
                dge <- temp$dge
                pre_processual <- temp$pre_processual
              
                dge <- Determinar.Meta01(dge)
                dge <- Estadual.Meta02(dge)
                dge <- Estadual.Meta03(dge, pre_processual)
                dge <- Estadual.Meta04(dge)
                dge <- Estadual.Meta06(dge)
                dge <- Estadual.Meta07(dge)
                dge <- Estadual.Meta08(dge)
                dge <- Estadual.Meta10(dge)
              
                GerarArquivos(dge, x)

            }
          
        } # Fim do for (i in  seq_along(aux))
        
        RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
        
    }       
  
} # Final da função
