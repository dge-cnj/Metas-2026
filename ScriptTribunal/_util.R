
# Operador personalizado '%!in%': verifica se os elementos de x não estão em y, desempenhando papel de "não pertence a"

# Se x estiver em y, o valor do operador será FALSE
# Se x NÃO estiver em y, o valor do operador será TRUE


'%!in%' <- function(x,y)!('%in%'(x,y))

# Função pendente_meta (acessa cpp)

pendente_meta <- function(array_pendente, data_limite = ref) {  
  array_pendente <- gsub("[{}]", "", array_pendente)
  intervalos <- unlist(strsplit(array_pendente, ","))
  pendente_meta_cpp(intervalos, data_limite)
}

# Função BasePublicaDPJ: Busca, na base do DPJ, os nomes dos arquivos de RamoJustica específico

BasePublicaDPJ <- function(RamoJustica, path) {
  
  RegistrarLOG(paste0("Registrando dados do(a) ", RamoJustica))
  
  Temp <- list.files(path, pattern = "csv")
  Temp <- Temp[substr(Temp,1,18) == "1)Extrai_Datamart_"]
  
  if (RamoJustica == "Justiça Estadual") {
    Temp <- Temp[substr(Temp,1,23) != "1)Extrai_Datamart_TJMMG"]
    Temp <- Temp[substr(Temp,1,23) != "1)Extrai_Datamart_TJMRS"]
    Temp <- Temp[substr(Temp,1,23) != "1)Extrai_Datamart_TJMSP"]
    Temp <- Temp[substr(Temp,1,20) == "1)Extrai_Datamart_TJ"]
  } 
  else if (RamoJustica == "Justiça Militar Estadual") {
    Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_TJM"]
    Temp <- Temp[17:20]
    Temp <- Temp[!substr(Temp, 1, 23) == "1)Extrai_Datamart_TJMS."]
  }
  else if (RamoJustica == "Justiça Federal") {
      Temp <- Temp[substr(Temp,1,18) == "1)Extrai_Datamart_"]
      Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_TRF"]
      print(Temp)
  }
  else if (RamoJustica == "Justiça do Trabalho") Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_TRT"]
  else if (RamoJustica == "Justiça Eleitoral") Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_TRE"]
  else if (RamoJustica == "Superior Tribunal de Justiça") Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_STJ"]
  else if (RamoJustica == "Tribunal Superior do Trabalho") Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_TST"]
  else if (RamoJustica == "Justiça Militar da União") Temp <- Temp[substr(Temp,1,21) == "1)Extrai_Datamart_STM"]
  
  return(Temp)
  
}

# Função func.procura.array:  verificar se elementos de uma lista aparecem dentro de uma coluna de um dataframe (base)

func.procura.array <- function(lista, base, variavel) {
  grepl(paste0(",",lista,",", collapse="|"),paste0(",",gsub(" |\\}|\\{|\\[|\\]","",as.character(base[,variavel])),","))
}

RegistrarLOG <- function(Mensagem) {
    pid <- Sys.getpid()
    datahora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # Monta registro como data.frame
    log_entry <- data.frame(
        pid      = pid,
        datahora = datahora,
        mensagem = Mensagem,
        stringsAsFactors = FALSE
    )
    
    # Nome do arquivo de log
    log_file <- "Paralelo_ref_20260131.csv"
    
    #lock_file <- paste0(log_file, ".lock")
    #lock <- filelock::lock(lock_file, timeout = 1000)
    #on.exit(filelock::unlock(lock), add = TRUE)
    
    # Cria ou acrescenta no CSV
    if (!file.exists(log_file)) {
        write.table(log_entry, file = log_file, sep = ";", row.names = FALSE, col.names = TRUE)
    } else {
        write.table(log_entry, file = log_file, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
    
    # Mostra no console
    print(log_entry)
}




GerarArquivos <- function(dge, trib) {
  
    path <- "output/"
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    print(paste0(path, trib,".csv"))
    
    data.table::fwrite(dge, file = paste0(path, trib,".csv"), append = TRUE, sep = ";",dec=",",bom=T)


}

GerarResumo <- function(metas, trib) {
    
    
    print(trib)
    
path <- "output/Resumo/"
if (!dir.exists(path)) dir.create(path, recursive = TRUE)      

    # Verificações de sanidade
    if (!is.character(trib) || length(trib) != 1L || is.na(trib)) {
        stop("Erro: 'trib' precisa ser uma string única e não-NA.")
    }
    
    if (!is.data.frame(metas) || nrow(metas) == 0) {
        stop("Erro: 'metas' está vazio ou não é um data.frame.")
    }
    
    
    if (!grepl("/$", path)) path <- paste0(path, "/") # Garantir que o caminho termina com barra    
    arquivo <- paste0(path, trib, ".csv") # Caminho final do arquivo
    print(paste("Salvando em:", arquivo)) # Print para depuração
    data.table::fwrite(metas, file = arquivo, sep = ";", dec = ",", bom = TRUE) # Exportar
    
    RegistrarLOG(paste0("Resumo do ", trib, " gerado."))

}

# Função: CompactarArquivos (Versão 06.2025)

CompactarArquivos <- function() {
    
    Pasta <- "output/"
    PastaCompactados <- "output/Zip/"

    if (!dir.exists(PastaCompactados)) dir.create(PastaCompactados, recursive = TRUE)
    
    aux <- list.files(Pasta, pattern = "\\.csv$", ignore.case = TRUE)
    
    if (length(aux) == 0L) {
        RegistrarLOG("⚠️ Nenhum CSV encontrado para compactar.")
        return(invisible(NULL))
    }
    
    # Detectar núcleos (com salvaguarda) e iniciar cluster
    
    n_cores <- max(1L, parallel::detectCores() - 1L)
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    doParallel::registerDoParallel(cl)
    
    # Exportar objetos necessários aos workers
    
    parallel::clusterExport(cl, varlist = c("Pasta", "PastaCompactados"), envir = environment())
    
    foreach::foreach(i = aux,
                     .packages = "utils",
                     .export   = c("RegistrarLOG")) %dopar% {
                         trib <- sub("\\.csv$", "", i, ignore.case = TRUE)
                         
                         # Caminhos fonte e destino
                         arq_csv <- file.path(Pasta, i)
                         arq_zip <- file.path(PastaCompactados, paste0(trib, ".zip"))
                         
                         # zip() com flag -j para não incluir a estrutura de pastas
                         # (Em alguns ambientes, "-j9X" ou "-j" já basta; aqui mantemos só "-j")
                         res <- try(utils::zip(zipfile = arq_zip,
                                               files   = arq_csv,
                                               flags   = "-j"),
                                    silent = TRUE)
                         
                         if (inherits(res, "try-error") || !file.exists(arq_zip)) {
                             RegistrarLOG(paste0("❌ Falha ao compactar: ", i))
                         } else {
                             RegistrarLOG(paste0("✅ Arquivo compactado: ", trib))
                         }
                     }
    
} # Fim da função

# Função: GerarConsolidados()

ConcatenarConsolidados <- function() {
    
    path <- "output/Resumo/"
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)      
    
    data_referencia <- "31/05/2025"
    
    # O nome da planilha será a data, em formato americano, com barras trocadas por pontos.
    
    sheet_name <- format(as.Date(data_referencia, format = "%d/%m/%Y"), "%Y.%m.%d")
    
    arquivos <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
    
    # Ler os arquivos e adicionar colunas
    
    lista_arquivos <- lapply(arquivos, function(arquivo) {
        dt <- fread(arquivo)
        dt[, arquivo_origem := basename(arquivo)]
        dt[, data_referencia := data_referencia]
        return(dt)
    })
    
    # Consolidar os dados
    
    consolidado <- rbindlist(lista_arquivos, fill = TRUE)
    
    # Colocar 'data_referencia' no início
    
    setcolorder(consolidado, c("data_referencia", setdiff(names(consolidado), "data_referencia")))
    
    # Função para classificar o segmento
    
    identificar_segmento <- function(sigla) {
        if (sigla %in% c("STJ", "TST")) return("Tribunais Superiores")
        if (sigla == "STM") return("Justiça Militar da União")
        if (sigla %in% c("TJMMG", "TJMRS", "TJMSP")) return("Justiça Militar Estadual")
        if (grepl("^TRE", sigla)) return("Justiça Eleitoral")
        if (grepl("^TRT", sigla)) return("Justiça do Trabalho")
        if (grepl("^TRF", sigla)) return("Justiça Federal")
        if (grepl("^TJ", sigla)) return("Justiça Estadual")        
        return("Segmento Desconhecido")
    }
    
    # Adicionar coluna Segmento após sigla_tribunal
    
    if ("sigla_tribunal" %in% names(consolidado)) {
        
        consolidado[, Segmento := sapply(sigla_tribunal, identificar_segmento)]
        colunas <- names(consolidado)
        pos_sigla <- which(colunas == "sigla_tribunal")
        novas_ordem <- append(colunas[colunas != "Segmento"], "Segmento", after = pos_sigla)
        setcolorder(consolidado, novas_ordem)
    } 
    else {
        warning("Coluna 'sigla_tribunal' não encontrada. 'Segmento' não será adicionado.")
    }
    
    # Caminho do Excel final
    
    arquivo_saida <- file.path(path, "Consolidado_Final.xlsx")
    
    # Abrir ou criar workbook
    
    if (file.exists(arquivo_saida)) wb <- loadWorkbook(arquivo_saida)
    else wb <- createWorkbook()
    
    # Remover aba existente com o mesmo nome (para sobrescrever)
    
    if (sheet_name %in% names(wb)) removeWorksheet(wb, sheet_name)
    
    # Criar nova aba
    
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = consolidado)
    
    # Aplicar estilo às colunas numéricas
    
    numeric_cols <- which(sapply(consolidado, is.numeric))
    number_style <- createStyle(numFmt = "#,##0.00")
    
    # Criar estilo em negrito
    
    bold_decimal_style <- createStyle(
        textDecoration = "bold",
        numFmt = "#,##0.00"
    )
    
    # Identificar colunas que começam com "cumprimento" (case-insensitive)
    
    cols_cumprimento <- which(grepl("^cumprimento", names(consolidado), ignore.case = TRUE))
    
    # Aplicar estilo nas células (exceto cabeçalho)
    for (col in cols_cumprimento) {
        addStyle(
            wb,
            sheet = sheet_name,
            style = bold_decimal_style,
            cols = col,
            rows = 2:(nrow(consolidado) + 1),
            gridExpand = TRUE,
            stack = TRUE
        )
    }
    # Salvar arquivo
    saveWorkbook(wb, arquivo_saida, overwrite = TRUE)
    
    cat("Excel atualizado com aba:", sheet_name, "\n")
    
}

