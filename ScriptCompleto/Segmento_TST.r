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

# 2026 - Consolidando dados do TST

Consolidado.TST <- function(dge) {
    
    colnames(dge)
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)-sum(desm2_ant,na.rm = TRUE)))*(1000/10))
    
    return (list(meta2=meta2))
    
}
# Hub principal, para o STJ



# 2026 - Gerando resumo dos dados do TSE

Resumo.TST <- function(dge) {
    
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
            cumprimento_meta2ant = ( julgm2_ant /(distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}


ProcessarDados.TST <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do TST"))
    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        pre_processual <- temp$pre_processual
    
        dge <- Determinar.Meta01(dge)
        dge <- TST.Meta02(dge)
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

    metas <- Consolidado.TST(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    list_metas = list(meta1, meta2)

    metas_compilado <- distinct(list_metas %>% reduce(bind_rows))
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.TST(metas_compilado)
    
    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_TST.csv")))
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
