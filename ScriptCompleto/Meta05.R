
Determinar.Meta05 <- function() {
  
  if (opcao_local == TRUE) {
    outputFolder <- "output/Painel 2026/"
    if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)      
  }
  else {
    outputFolder <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Saneamento/Painel de Metas/Painel 2026/"
  }  


    tbl_fato <- fread("//fluor02.cnj.jus.br/SEP/DPJ Pnud/Eixo04/painel_estatistica/BI/producao/tbl_fato_R.csv")
    tbl_fato <- tbl_fato %>% select(c(id_orgao_julgador, sigla_tribunal, ramo_justica, sigla_grau, procedimento, anomes, ano, mes, ind3, ind5))
    tbl_fato <- tbl_fato %>% filter(ano == "2024" | ano == "2025" | ano == "2026")
    tbl_fato <- tbl_fato %>% filter(!procedimento %in% c("Outros", "Fase investigatória", "Pré-processual", "Administrativo eleitoral"))
    tbl_fato <- tbl_fato %>% filter(sigla_tribunal != "CJF")
    
    
    unique(tbl_fato$procedimento)

    setDT(tbl_fato)

    tbl_fato <- tbl_fato %>% dtplyr::lazy_dt()
    tbl_fato <- tbl_fato %>% 
        group_by(id_orgao_julgador, sigla_tribunal, ramo_justica, sigla_grau, procedimento, anomes, ano, mes) %>% 
        summarise(
            ind3=sum(ind3, na.rm = TRUE),
            ind5=sum(ind5, na.rm = TRUE),
            .groups = "drop")
    tbl_fato <- tbl_fato %>% collect() %>% data.frame()

    # Flags da meta 5

    tbl_fato <- tbl_fato %>% 
        mutate(
            flg_m5 = if_else(sigla_tribunal == "STJ", 1,
                     if_else(sigla_tribunal == "TST" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça Federal" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça do Trabalho" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça Estadual" & (sigla_grau == "G1" | sigla_grau == "JE") & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1,
                     if_else(sigla_tribunal == "STM" & sigla_grau == "G1" & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1,
                     if_else(ramo_justica == "Justiça Militar Estadual" & sigla_grau == "G1" & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1, 0))))))))

    tbl_fato <- tbl_fato %>% filter(flg_m5 == "1")

    # uf, nome_municipio

    nome_oj <- fread("//fluor02.cnj.jus.br/SEP/DPJ Pnud/Eixo04/painel_estatistica/BI/desenvolvimento/Orgaos_julgadores_R.csv")
    nome_oj <- nome_oj %>% select(c("id_orgao_julgador","orgao_julgador", "uf_oj", "municipio_oj"))
    nome_oj <- distinct(nome_oj)

    tbl_fato <- tbl_fato %>% left_join(nome_oj, by=c("id_orgao_julgador"))
    tbl_fato <- tbl_fato %>% mutate(data_referencia = "31/12/2026")

    setDT(tbl_fato)

    meta5 <- tbl_fato %>% dtplyr::lazy_dt()
    meta5 <- meta5 %>% 
        group_by(ramo_justica, sigla_tribunal) %>% 
        summarise(
            pendente_liquido_2024 = sum(ind5[which(anomes == "202412")], na.rm = TRUE),
            baixados_2024 = sum(ind3[which(ano == "2024")], na.rm = TRUE),
            pendente_liquido_2025 = sum(ind5[which(anomes == "202512")], na.rm = TRUE),
            baixados_2025 = sum(ind3[which(ano == "2025")], na.rm = TRUE),
            pendente_liquido_2026 = sum(ind5[which(anomes == "202601")], na.rm = TRUE),
            baixados_2026 = sum(ind3[which(ano == "2026")], na.rm = TRUE) + sum(ind3[which(ano == "2025")], na.rm = TRUE) - sum(ind3[which(anomes == "202501")], na.rm = TRUE),
            TCLC2026_trt = sum(ind5[which(anomes == "202601" & procedimento == "Conhecimento não criminal")], na.rm = TRUE)/(sum(ind5[which(anomes == "202601" & procedimento == "Conhecimento não criminal")], na.rm = TRUE)+sum(ind3[which(ano == "2026" & procedimento == "Conhecimento não criminal")], na.rm = TRUE) + sum(ind3[which(ano == "2025" & procedimento == "Conhecimento não criminal")], na.rm = TRUE)-sum(ind3[which((anomes == "202501") & procedimento == "Conhecimento não criminal")], na.rm = TRUE)),
            TCLNFISC2026_trt = sum(ind5[which(anomes == "202601" & procedimento != "Execução fiscal")], na.rm = TRUE)/(sum(ind5[which(anomes == "202601" & procedimento != "Execução fiscal")], na.rm = TRUE)+sum(ind3[which(ano == "2026" & procedimento != "Execução fiscal")], na.rm = TRUE) + sum(ind3[which(ano == "2025" & procedimento != "Execução fiscal")], na.rm = TRUE)- sum(ind3[which((anomes == "202501") & procedimento != "Execução fiscal")], na.rm = TRUE)),
            .groups = "drop")

    meta5 <- meta5 %>%
        mutate(
            TCL2024 = pendente_liquido_2024/(pendente_liquido_2024 + baixados_2024),
            TCL2025 = pendente_liquido_2025/(pendente_liquido_2025 + baixados_2025),
            TCL2026 = pendente_liquido_2026/(pendente_liquido_2026 + baixados_2026))
    meta5 <- meta5 %>% collect() %>% data.frame()
    meta5 <- meta5 %>% 
        mutate(
            cumprimento_meta5 = if_else(sigla_tribunal == "STJ",((TCL2025-0.005)/TCL2026)*100,
                if_else(sigla_tribunal == "TST",((TCL2025-0.005)/TCL2026)*100,
                if_else(ramo_justica == "Justiça Federal",if_else((TCL2025-0.005)/TCL2026>=1,((TCL2025-0.005)/TCL2026)*100,if_else(TCL2026<=0.46,100,((TCL2025-0.005)/TCL2026)*100)),
                if_else(ramo_justica == "Justiça do Trabalho",if_else((TCL2025-0.005)/TCL2026>=1,((TCL2025-0.005)/TCL2026)*100,if_else((TCLC2026_trt<=0.4 & TCLNFISC2026_trt <= 0.45),100,((TCL2025-0.005)/TCL2026)*100)),
                if_else(ramo_justica == "Justiça Estadual",if_else((TCL2025-0.005)/TCL2026>=1,((TCL2025-0.005)/TCL2026)*100,if_else(TCL2026<=0.52,100,((TCL2025-0.005)/TCL2026)*100)),
                if_else(sigla_tribunal == "STM",((TCL2025-0.005)/TCL2026)*100,
                if_else(ramo_justica == "Justiça Militar Estadual",((TCL2025-0.005)/TCL2026)*100,0))))))))

    data.table::fwrite(tbl_fato, file = paste0(outputFolder, "meta5_2026.csv"), sep = ";", dec = ",", bom = T)
    data.table::fwrite(meta5, file = paste0(outputFolder, "meta5_2026_compilado.csv"), sep = ";", dec = ",", bom = T)

}




