

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







#Gerando consolidado


Consolidado.JusticaEleitoral <- function(dge) {
    
    meta2 <- dge %>% 
      
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
      
        summarise(
          
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a /(distm2_a - suspm2_a))*(1000/7),
            
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)-sum(desm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta4 <- dge %>% 
      
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
      
        summarise(
          
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/10),
            
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/7))
    
    return (list(meta2=meta2, meta4=meta4))
    
}


# 2026 - Gerando resumo dos dados da Justiça Militar dos Estados [Versão 13/02/2026]


Resumo.JusticaEleitoral <- function(dge) {
    
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
            cumprimento_meta2a = (julgm2_a / (distm2_a-suspm2_a)) * (1000/7),
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant /(distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgm4_a,na.rm = TRUE)/(sum(distm4_a,na.rm = TRUE)-sum(suspm4_a,na.rm = TRUE)))*(1000/10),
            
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgm4_b,na.rm = TRUE)/(sum(distm4_b,na.rm = TRUE)-sum(suspm4_b,na.rm = TRUE)))*(1000/7),
            
            .groups = "drop"
            
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}

# Hub principal, para o STJ

ProcessarDados.Eleitoral <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do ", x))

    aux <- aux[grepl(paste0("Extrai_Datamart_", x), aux)]

    dge_total <- data.frame()

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        dge <- iniciarFlagsEleitoral(path, aux, i)
        dge <- dge$dge
    
        dge <- Determinar.Meta01(dge)
        dge <- Eleitoral.Meta02(dge)
        dge <- Eleitoral.Meta04(dge)

        dge_total <- rbind(dge_total, dge)
    
        rm (dge)
        gc()

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
    
    metas <- Consolidado.JusticaEleitoral(dge_total)
    meta1 <- Consolidado.Meta01(dge_total)
    meta2 <- metas$meta2
    meta4 <- metas$meta4
    list_metas = list(meta1, meta2, meta4)

    metas_compilado <- distinct(list_metas %>% reduce(bind_rows))
    metas_compilado <- eliminar_colunas(metas_compilado)

    Resumo.JusticaEleitoral(metas_compilado)

    fwrite(metas_compilado,file = file.path("output/Teste", paste0("teste_", x,".csv")))
    
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
