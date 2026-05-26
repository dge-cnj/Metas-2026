
# Processamento de dados da Justiça Eleitoral - Versão 2026

# Meta 02

Eleitoral.Meta02 <- function(dge) {

    t_f <- "2027-01-01"         # Identificar e julgar, até 31/12/2026
    t_0 <- "2026-01-01"         # Início do ano
    t_ant <- "2021-01-01"       # 100% dos processos de conhecimento pendentes de julgamento há 6 anos (2020) ou mais
    t_rec <- "2025-01-01"       # 70% dos processos distribuídos até 31/12/2024
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_rec & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(data_total_primeira_baixa >= t_rec & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(data_total_primeira_baixa < t_rec | data_total_primeiro_julgamento_sem_pronuncia < t_rec | data_total_primeiro_procedimento_resolvido < t_rec, 1, 0),
            decm2_a = if_else(data_total_primeiro_procedimento_resolvido >= t_rec & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(dt_recebimento < t_rec & (data_total_primeiro_julgamento_sem_pronuncia >= t_rec | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    #Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_ant & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))
    
    return (dge)

}

# Meta 4

Eleitoral.Meta04 <- function(dge) {

    # Eleições 2022
    
    t_f <- "2027-01-01"
    t_0 <- "2026-01-01" # Início do ano
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else(ano_eleicao == "2022" & flg_ili_ele == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else(ano_eleicao == "2022" & flg_ili_ele == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else(ano_eleicao == "2022" & flg_ili_ele == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm4_a = if_else(ano_eleicao == "2022" & flg_ili_ele == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else(ano_eleicao == "2022" & flg_ili_ele == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    # Eleições 2024
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else(ano_eleicao == "2024" & flg_ili_ele == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_b = if_else(ano_eleicao == "2024" & flg_ili_ele == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_b_ate = if_else(ano_eleicao == "2024" & flg_ili_ele == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm4_b = if_else(ano_eleicao == "2024" & flg_ili_ele == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_b = if_else(ano_eleicao == "2024" & flg_ili_ele == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Hub principal, para a Justiça Eleitoral

ProcessarDados.Eleitoral <- function(path, aux, JusticaEleitoral) {

    for (x in JusticaEleitoral) {

        RegistrarLOG(paste0("Processando dados do ", x))

        for (i in  seq_along(aux)){
            
            if (grepl(x, aux[i])) {

                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                dge <- iniciarFlagsEleitoral(path, aux, i)$dge
                
                dge <- Determinar.Meta01(dge)
                dge <- Eleitoral.Meta02(dge)
                dge <- Eleitoral.Meta04(dge)
     
                GerarArquivos(dge, x)
                
            }

        } # Fim do for (i in  seq_along(aux))

        RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
    
    }

}

