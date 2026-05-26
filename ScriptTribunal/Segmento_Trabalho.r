
# Processamento de dados da Justiça do Trabalho

# Meta 02

Trabalho.Meta02 <- function(dge) {
    
    t_f <- "2027-01-01"         # Identificar e julgar até 31/12/2026
    t_0 <- "2026-01-01"         # 
    t_D <- "2025-01-01"         # 94% dos processos distribuídos até 31/12/2024 nos 1º e 2º graus 
    t_ant <- "2022-01-01"       # 99% dos processos pendentes de julgamento há 5 anos (2021) ou mais
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_D & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(data_total_primeira_baixa >= t_D & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(data_total_primeira_baixa < t_D | data_total_primeiro_julgamento_sem_pronuncia < t_D | data_total_primeiro_procedimento_resolvido < t_D, 1, 0),
            decm2_a = if_else(data_total_primeiro_procedimento_resolvido >= t_D & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(dt_recebimento < t_D & (data_total_primeiro_julgamento_sem_pronuncia >= t_D | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_a = if_else(dism2_a == "1" & (julgadom2_a=="0"|is.na(julgadom2_a)) & (susm2_a=="0"|is.na(susm2_a)) & flg_dessobrestado == "1", 1, 0))
    
    # Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate= if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_ant & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & flg_dessobrestado == "1", 1, 0))
    
    return (dge)

}

# Meta 03

Trabalho.Meta03 <- function(dge, pre_processual) {

    b_0 <- "2023-01-01" # Início do biênio
    b_f <- "2025-01-01" # Final do biênio
    t_0 <- "2026-01-01" # Início do período atual
    t_f <- "2027-01-01" # Final do período atual

    dge <- dge %>% 
        mutate(
            sent23_24 = if_else(sent_arq_des != "1" & !func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= b_0 & data_total_primeiro_julgamento_sem_pronuncia < b_f, 1, 0),
            senth23_24 = if_else(!func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & dt_primeiro_julgamento_homologatorio >= b_0 & dt_primeiro_julgamento_homologatorio < b_f, 1, 0),
            sent_26 = if_else(sent_arq_des != "1" & !func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            senth_26 = if_else(!func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & dt_primeiro_julgamento_homologatorio >= t_0 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))
  
    meta3 <- dge %>% 
        group_by(sigla_tribunal) %>% 
        summarise(
            IC_ant = sum(senth23_24,na.rm = TRUE)/sum(sent23_24,na.rm = TRUE),
            IC_atual = sum(senth_26,na.rm = TRUE)/sum(sent_26,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC_atual > (IC_ant+0.005),(IC_atual/(IC_ant+0.005))*100,if_else(IC_atual>="0.38",100,(IC_atual/(IC_ant+0.005))*100)))

    return (dge)

}

# Hub principal, para a JT

ProcessarDados.Trabalho <- function(path, aux, JusticaTrabalho) {
    
    for (x in JusticaTrabalho) {

        RegistrarLOG(paste0("Processando dados do ", x))
            
        for (i in  seq_along(aux)){
            
            if (grepl(x, aux[i])) {

                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                
                temp <- iniciarFlags(path, aux, i)
                dge <- temp$dge
                pre_processual <- temp$pre_processual                
                
                dge <- Determinar.Meta01(dge)
                dge <- Trabalho.Meta02(dge)
                dge <- Trabalho.Meta03(dge)

                GerarArquivos(dge, unique(dge$sigla_tribunal)[1])
                rm (temp, pre_processual, dge)
                gc()
                
            }

        } # Fim do for (i in  seq_along(aux))

        RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
    
    }   
  
}

