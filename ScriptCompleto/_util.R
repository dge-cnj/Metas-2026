

BuscarLocais <- function() {

    nome_oj <- fread("//fluor02.cnj.jus.br/SEP/DPJ Pnud/Eixo04/painel_estatistica/BI/desenvolvimento/Orgaos_julgadores_R.csv")
    nome_oj <- nome_oj %>% select(c("id_orgao_julgador","orgao_julgador", "uf_oj", "municipio_oj"))
    nome_oj <- distinct(nome_oj)
    names(nome_oj)[names(nome_oj) == 'orgao_julgador'] <- 'nome'
    nome_oj$id_orgao_julgador[is.na(nome_oj$id_orgao_julgador)] <- -1
    local <- nome_oj[, c("uf_oj", "nome", "municipio_oj", "id_orgao_julgador")]

    return (local)

}

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
# Parâmetro: RamoJustica, cujos valores são considerados nos ifs
# Valor de retorno: Temp, com os nomes dos arquivos inerentes ao segmento passado como parâmetro
# Versão: 04/02/2025

BasePublicaDPJ <- function(RamoJustica, path) {
    
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
# Parâmetros: lista, base, variavel
# Valor de retorno:
# Versão: Anterior a 2025


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
    log_file <- "Paralelo_ref_20260430.csv"
    
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
  
    if (opcao_local == TRUE) {
        path <- "output/"
        if (!dir.exists(path)) dir.create(path, recursive = TRUE)      
    }
    else {
        path <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/"
    }
  
  print(paste0(path, trib,".csv"))
  
  

    data.table::fwrite(dge, file = paste0(path, trib,".csv"), append = TRUE, sep = ";",dec=",",bom=T)

}

GerarResumo <- function(metas, trib) {
    
    print(trib)
    
    if (opcao_local == TRUE) {
        path <- "output/Resumo/"
        if (!dir.exists(path)) dir.create(path, recursive = TRUE)      
    } else {
        path <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/Consolidado tribunal/"    
    }
    
    # Verificações de sanidade
    if (!is.character(trib) || length(trib) != 1L || is.na(trib)) {
        stop("Erro: 'trib' precisa ser uma string única e não-NA.")
    }
    
    if (!is.data.frame(metas) || nrow(metas) == 0) {
        stop("Erro: 'metas' está vazio ou não é um data.frame.")
    }
    
    # Garantir que o caminho termina com barra
    if (!grepl("/$", path)) path <- paste0(path, "/")
    
    # Caminho final do arquivo
    arquivo <- paste0(path, trib, ".csv")
    
    # Print para depuração
    print(paste("Salvando em:", arquivo))
    
    # Exportar
    data.table::fwrite(metas, file = arquivo, sep = ";", dec = ",", bom = TRUE)
    
    RegistrarLOG(paste0("Resumo do ", trib, " gerado."))
}


EnviarCompactados <- function() {

    if (!requireNamespace("paws.storage", quietly = TRUE)) install.packages("paws")
    library(paws.storage)

    # Configurando cliente S3 com as credenciais e endpoint CNJ
    
    s3 <- paws.storage::s3(
        config = list(
            credentials = list(
                creds = list(
                    access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
                    secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")
                )
            ),
            endpoint = "https://prd.s3.cnj.jus.br",
            region = "us-east-1",
            disable_ssl = FALSE,
            s3_force_path_style = TRUE
        )
    )

    if (opcao_local == TRUE) {
        path <- "output/Zip/"
    }
    else {
        path <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/Zip"
    }

    objs <- s3$list_objects_v2(Bucket = "dge", Prefix = "Metas 2026/")
    
    if (!is.null(objs$Contents)) {
        for (o in objs$Contents) {
            s3$delete_object(Bucket = "dge", Key = o$Key)
            message(sprintf("🗑️  Excluído do S3: %s", o$Key))
        }
    } else {
        message("Nenhum objeto encontrado para excluir.")
    }
    
    arq <- list.files(path, pattern = "zip")

    for (X in arq) {
  
        arquivo <- paste0(path, X)
        bucket <- "dge"
        objeto_s3 <- paste0("Metas 2026/", X)
  
        # Leitura do conteúdo
        
        conteudo <- readBin(arquivo, "raw", n = file.info(arquivo)$size)
  
        # Upload com `paws`

        res <- s3$put_object(
            Bucket = bucket,
            Key = objeto_s3,
            Body = conteudo
        )
  
        if (is.null(res$ETag)) message("❌ Falha no upload com `paws`. Verifique as permissões e o endpoint.")
        else message(paste0("✅ Upload realizado para: ", X))
  
    } # Fim do "for" que envia, individualmente, cada arquivo "zip" para o S3

} # Fim da função enviar compactados


# Função: CompactarArquivos (Versão 06.2025)

CompactarArquivos <- function() {
    
    if (isTRUE(opcao_local)) {
        Pasta <- "output/"
        PastaCompactados <- "output/Zip/"
    } else {
        Pasta <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/"
        PastaCompactados <- file.path(Pasta, "Zip")
    }
    
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


LiberarVariaveisHistorico <- function() {
    variaveis <- c(
        "S_CasosNovos", "S_Julgados", "S_PrimeiraSentenca", "S_Suspensos", "S_Dessobrestados",
        "S_distm2_a", "S_julgm2_a", "S_suspm2_a", "S_distm2_b", "S_julgm2_b", "S_suspm2_b",
        "S_distm2_c", "S_julgm2_c", "S_suspm2_c", "S_distm2_ant", "S_julgm2_ant", "S_suspm2_ant",
        "S_desom2_ant", "S_IC_a", "S_IC_b", "S_sentencas_2023", "S_sentencas_homologatorias_2023",
        "S_sentencas_2024", "S_sentencas_homologatorias_2024", "S_sentencas_2025", "S_sentencas_homologatorias_2025",
        "S_conc_pre_2023", "S_conc_pre_2024", "S_conc_pre_2025", "S_distm4_a", "S_julgm4_a", "S_suspm4_a",
        "S_distm4_b", "S_julgm4_b", "S_suspm4_b", "S_distm", "S_julgm6", "S_suspm6",
        "S_distm6_a", "S_julgm6_a", "S_suspm6_a", "S_distm7_a", "S_julgm7_a", "S_suspm7_a",
        "S_distm7_b", "S_julgm7_b", "S_suspm7_b", "S_distm8_a", "S_julgm8_a", "S_suspm8_a",
        "S_distm8_b", "S_julgm8_b", "S_suspm8_b", "S_distm10_a", "S_julgm10_a", "S_suspm10_a",
        "S_distm10_b", "S_julgm10_b", "S_suspm10_b", "S_distm10_c", "S_julgm10_c", "S_suspm10_c",
        "S_distm11_a", "S_julgm11_a", "S_suspm11_a", "S_distm11_b", "S_julgm11_b", "S_suspm11_b"
    )
    
    vars_exist <- intersect(variaveis, ls(envir = .GlobalEnv))
    if (length(vars_exist) > 0) {
        rm(list = vars_exist, envir = .GlobalEnv)
        message(length(vars_exist), " variáveis removidas do Global Environment.")
    } else {
        message("Nenhuma das variáveis especificadas estava no Global Environment.")
    }
}




# Função: GerarConsolidados()

ConcatenarResumos <- function() {
    
    if (opcao_local == TRUE) {
        path <- "output/Resumo/"
        if (!dir.exists(path)) dir.create(path, recursive = TRUE)      
    }
    else {
        path <- "//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Datajud/Painel Estatística/2026/Consolidado tribunal/"    
    }
    
    data_referencia <- "30/04/2026"
    
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
    
    RegistrarLOG(paste0("Excel atualizado com aba:", sheet_name, "\n"))
    
}


# Calculo do tempo serial

calcular_tempo_total_processado <- function(arquivo) {
    library(data.table)
    
    dt <- fread(
        arquivo,
        sep = ";",
        header = FALSE,
        fill = TRUE,
        quote = "\"",
        col.names = c("pid", "timestamp", "mensagem")
    )
    
    dt[, mensagem := as.character(mensagem)]
    
    dt_proc <- dt[grepl("processado em", mensagem, ignore.case = TRUE)]
    
    dt_proc[, tempo := as.numeric(
        sub(".*processado em ([0-9.]+).*", "\\1", mensagem)
    )]
    
    total <- sum(dt_proc$tempo, na.rm = TRUE)
    
    return(total)
}




# Função auxiliar

formatar_mes_ano <- function(x) {
    meses <- c("Jan","Fev","Mar","Abr","Mai","Jun",
               "Jul","Ago","Set","Out","Nov","Dez")
    
    ano <- substr(x, 1, 4)
    mes <- as.integer(substr(x, 6, 7))
    
    resultado <- paste(meses[mes], ano)
    
    return(resultado)
}

# Função para eliminar, das sub-tabelas da planilha final, as linhas em que as colunas de dados são nulas ou vazia

eliminar_colunas <- function(metas_compilado) {
    
    Colunas <- names(metas_compilado) [match("casos_novos_2026", names(metas_compilado)):ncol(metas_compilado)]

    metas_compilado <- metas_compilado %>%
        filter(
            !if_all(
                all_of(Colunas),
                ~ is.na(.) | . == 0
            )
        )
        
    metas_compilado <- distinct(metas_compilado)

}