GerarPlanilhaFinal <- function(DataReferencia) {

    RegistrarLOG("Gerando planilha final")
    
    if (opcao_local) {
        path <- "output/Teste/"
        outputFolder <- "output/Painel 2026/"
    } 
    else {
        path <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/"
        outputFolder <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Saneamento/Painel de Metas/Painel 2026/"
    }
  
    if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)
    
    RegistrarLOG("Carregando arquivos")

    arquivos <- list.files(
        path = path,
        pattern = "\\.csv$",
        full.names = TRUE,
        recursive = TRUE
    )
  
    if (length(arquivos) == 0) stop("Nenhum CSV encontrado em: ", path)
    
    RegistrarLOG("Concatenando arquivos")

    Painel <- rbindlist(
        lapply(arquivos, fread),
        use.names = TRUE,
        fill = TRUE
    )
  
    Painel[, data_referencia := DataReferencia]

    RegistrarLOG("Gravando resultados")
  
    save(Painel, file = file.path(outputFolder, "Painel_26.RData"))
    fwrite(Painel, file = file.path(outputFolder, "Painel_26.csv"))
  
    return(Painel)

}