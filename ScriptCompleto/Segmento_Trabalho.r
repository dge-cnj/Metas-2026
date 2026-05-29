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
            sent23_24 = sum(sent23_24,na.rm = TRUE),
            senth23_24 = sum(senth23_24,na.rm = TRUE),
            senth_26 = sum(senth_26,na.rm = TRUE),
            sent_26 = sum(sent_26,na.rm = TRUE),
            IC_ant = sum(senth23_24,na.rm = TRUE)/sum(sent23_24,na.rm = TRUE),
            IC_atual = sum(senth_26,na.rm = TRUE)/sum(sent_26,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC_atual > (IC_ant+0.005),(IC_atual/(IC_ant+0.005))*100,if_else(IC_atual>="0.38",100,(IC_atual/(IC_ant+0.005))*100)))

    return (dge)

}

# 2026 - Consolidando dados da Justiça do Trabalho

Consolidado.JusticaTrabalho <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(

            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            desom2_a = sum(desm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a/(distm2_a - desom2_a - suspm2_a))*(1000/9.4),
            
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant / (distm2_ant - suspm2_ant - desom2_ant )) * (1000/9.9))
        
    meta3 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mesano_senth) %>% 
        summarise(
            quant_sent23_24 = sum(sent23_24,na.rm = TRUE),
            quant_conc23_24 = sum(senth23_24,na.rm = TRUE),
            quant_sent26 = sum(sent_26,na.rm = TRUE),
            quant_conc26 = sum(senth_26,na.rm = TRUE),
            IC23_24 = quant_conc23_24 / quant_sent23_24,
            IC2026 = quant_conc26 / quant_sent26,
            cumprimento_meta3 = if_else(
                IC2026 > (IC23_24+0.005),
                (IC2026/(IC23_24+0.005))*100,
                if_else(
                    IC2026>="0.38",
                    100,
                    (IC2026/(IC23_24+0.005))*100)))  

    return (list(meta2=meta2, meta3=meta3))    
    
} 




# 2026 - Gerando resumo dos dados da Justiça do Trabalho

Resumo.JusticaTrabalho <- function(dge) {
    
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
            desom2_a = sum(desom2_a,na.rm = TRUE),

            cumprimento_meta2a = (julgm2_a/(distm2_a - desom2_a - suspm2_a))*(1000/9.4),

            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),

            cumprimento_meta2ant = julgm2_ant/( distm2_ant - desom2_ant - suspm2_ant) * (1000/9.9),

            quant_sent23_24 = sum(quant_sent23_24,na.rm = TRUE),
            quant_conc23_24 = sum(quant_conc23_24,na.rm = TRUE),
            quant_sent26 = sum(quant_sent26,na.rm = TRUE),
            quant_conc26 = sum(quant_conc26,na.rm = TRUE),
            IC23_24 = (sum(quant_conc23_24,na.rm = TRUE))/sum(quant_sent23_24,na.rm = TRUE),
            IC2026 = (sum(quant_conc26,na.rm = TRUE))/sum(quant_sent26,na.rm = TRUE),
            
            cumprimento_meta3 = if_else(
                IC2026 > (IC23_24+0.005),
                (IC2026/(IC23_24+0.005))*100,
                if_else(
                    IC2026>="0.38",
                    100,
                    (IC2026/(IC23_24+0.005))*100)),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}




ProcessarDados.Trabalho <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do ", x))
    aux <- aux[grepl(paste0("Extrai_Datamart_", x, "(_|\\.csv$|$)"), aux)]
    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Lendo dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)

        dge <- temp$dge
        dge <- Determinar.Meta01(dge)
        dge <- Trabalho.Meta02(dge)
        dge <- Trabalho.Meta03(dge, temp$pre_processual)
        dge_total <- rbind(dge_total, dge)

        rm (temp, dge)
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
    dge_total <- distinct(dge_total)

    metas <- Consolidado.JusticaTrabalho(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    meta3 <- metas$meta3
    list_metas = list(meta1, meta2, meta3)

    metas_compilado <- list_metas %>% reduce(bind_rows)
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.JusticaTrabalho(metas_compilado)    

    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_", x,".csv")))
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
