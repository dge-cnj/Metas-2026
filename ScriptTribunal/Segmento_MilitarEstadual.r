
# Processamento de dados da Justiça Militar dos Estados - Versão 2026

# Meta 2 de 2026 [Versão 13/02/2026]

MilitarEstadual.Meta02 <- function(dge) {

    
    t_f <- "2027-01-01"          # Identificar e julgar, até 31/12/2026:
    t_aud <- "2025-01-01"        # 90% dos processos distribuídos até 31/12/2024 nas Auditorias
    t_seg <- "2026-01-01"        # 95% dos processos distribuídos até 31/12/2025 no 2º Grau
    t_ant <- "2023-01-01"        # todos os processos de conhecimento pendentes de julgamento há 3 anos (2023)
    
    #Auditorias Militares
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= t_aud & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(sigla_grau == "G1" & data_total_primeira_baixa >= t_aud & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(sigla_grau == "G1" & (data_total_primeira_baixa < t_aud | data_total_primeiro_julgamento_sem_pronuncia < t_aud | data_total_primeiro_procedimento_resolvido < t_aud), 1, 0),
            decm2_a = if_else(sigla_grau == "G1" & data_total_primeiro_procedimento_resolvido >= t_aud & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(sigla_grau == "G1" & dt_recebimento < t_aud & (data_total_primeiro_julgamento_sem_pronuncia >= t_aud | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    #2º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_b = if_else(sigla_grau == "G2" & data_total_primeiro_julgamento_sem_pronuncia >= t_seg & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_b = if_else(sigla_grau == "G2" & data_total_primeira_baixa >= t_seg & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_b_ate = if_else(sigla_grau == "G2" & (data_total_primeira_baixa < t_seg | data_total_primeiro_julgamento_sem_pronuncia < t_seg | data_total_primeiro_procedimento_resolvido < t_seg), 1, 0),
            decm2_b = if_else(sigla_grau == "G2" & data_total_primeiro_procedimento_resolvido >= t_seg & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_b = if_else(sigla_grau == "G2" & dt_recebimento < t_seg & (data_total_primeiro_julgamento_sem_pronuncia >= t_seg | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_b == "1") & (baixm2_b_ate == 0 | is.na(baixm2_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_b = if_else(dism2_b == "1" & (primeirasentm2_b == "1" | decm2_b == "1" | baixm2_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_b = if_else(dism2_b == "1" & (julgadom2_b == "0" | is.na(julgadom2_b)) & pendente_meta == "0", 1, 0))
    
    #Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_ant & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_ant & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(data_total_primeira_baixa < t_ant | data_total_primeiro_julgamento_sem_pronuncia < t_ant | data_total_primeiro_procedimento_resolvido < t_ant, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_ant & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_ant & (data_total_primeiro_julgamento_sem_pronuncia >= t_ant | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))
    
    return (dge)

}

# Meta 4 de 2026 [Versão 13/02/2026]

MilitarEstadual.Meta04 <- function(dge) {

    t_f <- "2027-01-01"                  # Identificar e julgar, até 31/12/2026:
    t_aud <- "2025-01-01"                # 95% das ações penais relacionadas aos crimes contra a AP, abrangendo, inclusive, a Lei 13.491/17, distribuídas até 31/12/2024 no 1º grau
    t_seg <- "2026-01-01"                # 95% das distribuídas no 2º grau até 31/12/2025
    
    #Auditorias Militares
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_aud & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeira_baixa >= t_aud & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & (data_total_primeira_baixa < t_aud | data_total_primeiro_julgamento_sem_pronuncia < t_aud | data_total_primeiro_procedimento_resolvido < t_aud), 1, 0),
            decm4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_aud & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else(sigla_grau == "G1" & flg_crim_contr_adm_pbl == "TRUE" & dt_recebimento < t_aud & (data_total_primeiro_julgamento_sem_pronuncia >= t_aud | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    #2º grau
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else(sigla_grau == "G2" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_seg & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_b = if_else(sigla_grau == "G2" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeira_baixa >= t_seg & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_b_ate = if_else(sigla_grau == "G2" & flg_crim_contr_adm_pbl == "TRUE" & (data_total_primeira_baixa < t_seg | data_total_primeiro_julgamento_sem_pronuncia < t_seg | data_total_primeiro_procedimento_resolvido < t_seg), 1, 0),
            decm4_b = if_else(sigla_grau == "G2" & flg_crim_contr_adm_pbl == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_seg & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_b = if_else(sigla_grau == "G2" & flg_crim_contr_adm_pbl == "TRUE" & dt_recebimento < t_seg & (data_total_primeiro_julgamento_sem_pronuncia >= t_seg | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Hub principal, para a Justiça Militar Estadual

ProcessarDados.MilitarEstadual <- function(path, aux, JusticaMilitarEstadual) {

    for (x in JusticaMilitarEstadual) {

        RegistrarLOG(paste0("Processando dados do ", x))

        for (i in  seq_along(aux)){
            
            if (grepl(x, aux[i])) {

                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                
                temp <- iniciarFlags(path, aux, i)
                dge <- temp$dge
                
                dge <- Determinar.Meta01(dge)
                dge <- MilitarEstadual.Meta02(dge)
                dge <- MilitarEstadual.Meta04(dge)

                GerarArquivos(dge, x)
                
                rm (temp, dge)
                gc()
            }

        } # Fim do for (i in  seq_along(aux))

        RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
    
    }   
  
}

