# Processamento de dados do STM

# Filtrando processos que estão incluídos na meta 02

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

# Filtrando processos que estão incluídos na meta 04

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

# Gerando sub-tabela de consolidados por OJ, que comporá o arquivo CSV que alimentará o Painel de Metas

Consolidado.STM <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/9.5),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/9.9),
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
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/9.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/9.9))
    
    return (list(meta2=meta2, meta4=meta4))
    
}

# Gerando resumo dos dados da Justiça Militar da União [Versão 13/02/2026]

Resumo.STM <- function(dge) {

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
            cumprimento_meta2a = (julgm2_a /( distm2_a - suspm2_a))*(1000/9.5),
            
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgm2_b,na.rm = TRUE)/(sum(distm2_b,na.rm = TRUE)-sum(suspm2_b,na.rm = TRUE)))*(1000/9.9),
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgm2_ant,na.rm = TRUE)/(sum(distm2_ant,na.rm = TRUE)-sum(suspm2_ant,na.rm = TRUE)-sum(desom2_ant,na.rm = TRUE)))*(1000/10),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgm4_a,na.rm = TRUE)/(sum(distm4_a,na.rm = TRUE)-sum(suspm4_a,na.rm = TRUE)))*(1000/9.5),
            
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgm4_b,na.rm = TRUE)/(sum(distm4_b,na.rm = TRUE)-sum(suspm4_b,na.rm = TRUE)))*(1000/9.9),
            
            .groups = "drop"
        )

    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}

# Hub principal do STM

ProcessarTribunal <- function(path, aux, segmento, tribunal) {

    RegistrarLOG(paste0("Processando dados do ", tribunal))
        
    for (i in  seq_along(aux)){
      
        if (grepl(x, aux[i])) {
        
            RegistrarLOG(paste0("Lendo arquivo", aux[i]))
            dge <- iniciarFlags(path, aux, i)$dge
            dge <- Determinar.Meta01(dge)

            if (segmento == "STM") dge = Processar.STM(dge)


        } # Fim do if



    } # Fim do for (i in  seq_along(aux))

    
    GerarResumo(metas_compilado, tribunal)

    
    
    RegistrarLOG(paste0("Encerrando processamento dos dados do ", tribunal))
    
} # Final da função
