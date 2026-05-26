

# Processamento de dados do Tribunal Superior do Trabalho

# Meta 2

TST.Meta02 <- function(dge) {
    
    t_0 <- "2026-01-01" 
    t_f <- "2027-01-01"
    t_dist <- "2021-01-01"
    
    t_rec <- "2021-01-01" ### Verificar esta data
    
    # Mais antigos
    
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

# Hub principal, para o TST

ProcessarDados.TST <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do TST"))

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        dge <- Determinar.Meta01(dge)
        dge <- TST.Meta02(dge)
    
        GerarArquivos(dge, x)

    } # Fim do for (i in  seq_along(aux))

    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
