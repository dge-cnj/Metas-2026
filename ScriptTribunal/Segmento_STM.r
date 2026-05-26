
# Processamento de dados do STM

# Meta 2 de 2026 [Versão 13/02/2026]

STM.Meta02 <- function(dge) {
 
    
    t_0 <- "2026-01-01"       # Início do período (usado também para os processos mais antigos)
    t_f <- "2027-01-01"       # Identificar e julgar, até 31/12/2026 
    t_dist <- "2024-01-01"    # 95% dos processos distribuídos até 31/12/2023 nas Auditorias
    t_rec <- "2022-01-01"     # todos os processos de conhecimento pendentes de julgamento há 5 anos (2021) ou mais; 
    t_sup <- "2024-01-01"     # 99% dos processos distribuídos até 31/12/2024 no STM
    
    # Auditorias Militares
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= t_dist & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(sigla_grau == "G1" & data_total_primeira_baixa >= t_dist & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(sigla_grau == "G1" & (data_total_primeira_baixa < t_dist | data_total_primeiro_julgamento_sem_pronuncia < t_dist | data_total_primeiro_procedimento_resolvido < t_dist), 1, 0),
            decm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_procedimento_resolvido >= t_dist & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(sigla_grau == "G1" & dt_recebimento < t_dist & (data_total_primeiro_julgamento_sem_pronuncia >= t_dist | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    #STM
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_b = if_else(sigla_grau == "SUP" & data_total_primeiro_julgamento_sem_pronuncia >= t_sup & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_b = if_else(sigla_grau == "SUP" & data_total_primeira_baixa >= t_sup & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_b_ate = if_else(sigla_grau == "SUP" & (data_total_primeira_baixa < t_sup | data_total_primeiro_julgamento_sem_pronuncia < t_sup | data_total_primeiro_procedimento_resolvido < t_sup), 1, 0),
            decm2_b = if_else(sigla_grau == "SUP" & data_total_primeiro_procedimento_resolvido >= t_sup & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_b = if_else(sigla_grau == "SUP" & dt_recebimento < t_sup & (data_total_primeiro_julgamento_sem_pronuncia >= t_sup | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_b == "1") & (baixm2_b_ate == 0 | is.na(baixm2_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_b = if_else(dism2_b == "1" & (primeirasentm2_b == "1" | decm2_b == "1" | baixm2_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_b = if_else(dism2_b == "1" & (julgadom2_b == "0" | is.na(julgadom2_b)) & pendente_meta == "0", 1, 0))
    
    #Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_rec & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))

    return(dge)

}

# Meta 4 de 2026 [Versão 13/02/2026]

STM.Meta04 <- function(dge) {

    t_0 <- "2024-01-01"         # 95% dos processos da meta distribuídos até 31/12/2023 nas Auditorias
    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026
    t_rec <- "2025-01-01"       # 99% dos processos da meta distribuídos até 31/12/2024
    
    # Auditorias Militares
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    # STM
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else(sigla_grau == "SUP" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_rec & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_b = if_else(sigla_grau == "SUP" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeira_baixa >= t_rec & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_b_ate = if_else(sigla_grau == "SUP" & flg_crim_contr_adm_pbl == "TRUE" & (data_total_primeira_baixa < t_rec | data_total_primeiro_julgamento_sem_pronuncia < t_rec | data_total_primeiro_procedimento_resolvido < t_rec), 1, 0),
            decm4_b = if_else(sigla_grau == "SUP" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_rec & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_b = if_else(sigla_grau == "SUP" & flg_crim_contr_adm_pbl == "TRUE" & dt_recebimento < t_rec & (data_total_primeiro_julgamento_sem_pronuncia >= t_rec | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Hub principal, para o STM

ProcessarDados.STM <- function(path, aux, JusticaMilitar) {

    for (x in JusticaMilitar) {

        RegistrarLOG(paste0("Processando dados do ", x))
        
        for (i in  seq_along(aux)){
      
            if (grepl(x, aux[i])) {
        
                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                
                dge <- iniciarFlags(path, aux, i)$dge
                dge <- Determinar.Meta01(dge)
                dge <- STM.Meta02(dge)
                dge <- STM.Meta04(dge)
        
                GerarArquivos(dge, x)
            }
            
        } # Fim do for (i in  seq_along(aux))
    
    }
    
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
    
} # Final da função
