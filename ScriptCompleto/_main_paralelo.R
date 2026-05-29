# Usar data.table ???

suppressWarnings(suppressMessages({
  pacman::p_load(
    "data.table", 
    "dplyr", "dtplyr", "stringr", "lubridate", 
    "tidyverse", "winch", "foreach", "doParallel", "writexl", 
    "processx", "future", "Rcpp", "parallel", "openxlsx"
  )
}))

readRenviron(".Renviron.sh")

n_cores <- detectCores() - 2
n_cores <- 6
cl <- makeCluster(n_cores)

ProcessID <- Sys.getpid() # Obter o ID do processo atual (PID)

# Usar PowerShell para definir a prioridade do processo como "alta"

cmd <- paste0("powershell -Command \"(Get-Process -Id ", ProcessID, ").PriorityClass = 'High'\"")

tryCatch({
  system(cmd)  # Executa o comando PowerShell
  cat("Prioridade do processo definida como 'alta'.\n")
}, error = function(e) {
  cat("Erro ao definir a prioridade do processo:", e$message, "\n")
})

# Variáveis globais (outras variáveis)

path <- "//fluor02.cnj.jus.br/SEP/DPJ Pnud/Eixo04/painel_estatistica/BI/download/base_publica/"   # Arquivos do DPJ

ref <<- 20260101  ### Alterado

#setwd("//fluor02.cnj.jus.br/presidencia/SEP/DGE/_Restrito/DGPJ/METAS NACIONAIS DO PODER JUDICIÁRIO/Metas 2025/Scripts - painel")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Rcpp::sourceCpp("pendente_meta.cpp")

source("_util.r")
source("_flags.r")
source("_GerarPlanilhaFinal.r")

source("Meta01.r")
source("Meta05.r")

source("Segmento_STM.r")              
source("Segmento_MilitarEstadual.r")
source("Segmento_TST.r")
source("Segmento_Federal.r")
source("Segmento_Trabalho.r")
source("Segmento_STJ.r")
source("Segmento_Estadual.r")
source("Segmento_Eleitoral.r")

processar_STM <- FALSE # Ok
processar_STJ <- FALSE # Ok
processar_JME <- FALSE # Ok
processar_TRE <- FALSE # Ok
processar_TRF <- FALSE # Ok
processar_TJs <- FALSE # Ok
processar_TRT <- FALSE # Ok
processar_TST <- FALSE # Ok

opcao_EnviarCompactados <- FALSE
opcao_CompactarArquivos <- FALSE
opcao_ProcessarMeta05 <- TRUE
opcao_gerarPlanilhaPainel <- FALSE
opcao_gerarConsolidadoAgrupado <- FALSE
opcao_ExcluirPasta <- FALSE

opcao_local <<- TRUE # Caso esta variável seja TRUE, todos os arquivos gerados serão armazenados localmente

if (opcao_local == TRUE) {
    if (opcao_ExcluirPasta == TRUE) unlink("output", recursive = TRUE)
    if (!dir.exists("output/")) dir.create("output/", recursive = TRUE)      
    if (!dir.exists("output/Teste")) dir.create("output/Teste", recursive = TRUE)      
    if (!dir.exists("output/Zip")) dir.create("output/Zip", recursive = TRUE)      
    if (!dir.exists("output/Resumo")) dir.create("output/Resumo", recursive = TRUE)      
    if (!dir.exists("output/Painel 2026")) dir.create("output/Painel 2026", recursive = TRUE)      
}

criar_tarefa_processamento <- function(nome_base, tribunal, func_processar, path) {
    
    force(nome_base)
    force(tribunal)
    force(func_processar)
    force(path)
    
    function() {
        Sys.sleep(runif(1, 0, 0.5))
        
        inicio <- proc.time()
        temp <- BasePublicaDPJ(nome_base, path)
        func_processar(path, temp, tribunal)
        fim <- proc.time()
        
        tempo <- round((fim - inicio)[["elapsed"]], 2)
        RegistrarLOG(sprintf("%s processado em %s", tribunal, tempo))
        
        invisible(NULL)
    }
    
}

tarefas<- list()


if (processar_TJs) {
    
    JusticaEstadual <- c("TJSP", "TJTO", "TJAC", "TJDFT", "TJBA", "TJAP", "TJCE", "TJRJ", "TJES", "TJGO", "TJMA", "TJMS", "TJMT", "TJPA", "TJPB", "TJPE", "TJPI", "TJRN", "TJRO", "TJRR", "TJSE", "TJMG", "TJPR", "TJAM", "TJAL", "TJRS", "TJSC")
    
    for (t in JusticaEstadual) {
        tarefas[[t]] <- criar_tarefa_processamento(
            nome_base = "Justiça Estadual",
            tribunal = t,
            func_processar = ProcessarDados.Estadual,
            path = path
        )
    }
    
}

if (processar_TRE) {
    
    JusticaEleitoral <- c("TRE-AC", "TRE-AL", "TRE-AM", "TRE-AP", "TRE-BA", "TRE-CE", "TRE-DF", "TRE-ES", "TRE-GO", "TRE-MA", "TRE-MG", "TRE-MS", "TRE-MT", "TRE-PA", "TRE-PB", "TRE-PE", "TRE-PI", "TRE-PR", "TRE-RJ", "TRE-RN", "TRE-RO", "TRE-RR", "TRE-RS", "TRE-SC", "TRE-SE", "TRE-SP", "TRE-TO")
    for (t in JusticaEleitoral) {
        tarefas[[t]] <- criar_tarefa_processamento(
            nome_base = "Justiça Eleitoral",
            tribunal = t,
            func_processar = ProcessarDados.Eleitoral,
            path = path
        )
    }
    
}

if (processar_STJ) {
    
    tarefas[["STJ"]] <- criar_tarefa_processamento(
        nome_base = "Superior Tribunal de Justiça",
        tribunal = "STJ",
        func_processar = ProcessarDados.STJ,
        path = path
    )
    
}

if (processar_TST) {
    
    tarefas[["TST"]] <- criar_tarefa_processamento(
        nome_base = "Tribunal Superior do Trabalho",
        tribunal = "TST",
        func_processar = ProcessarDados.TST,
        path = path
    )
    
}


if (processar_STM) {
    
    tarefas[["STM"]] <- criar_tarefa_processamento(
        nome_base = "Justiça Militar da União",
        tribunal = "STM",
        func_processar = ProcessarDados.STM,
        path = path
    )
    
}



if (processar_TRF) {
    
    JusticaFederal <- c("TRF1", "TRF2", "TRF3", "TRF4", "TRF5", "TRF6")
    for (t in JusticaFederal) {
        tarefas[[t]] <- criar_tarefa_processamento(
            nome_base = "Justiça Federal",
            tribunal = t,
            func_processar = ProcessarDados.Federal,
            path = path
        )
    }
    
}


if (processar_JME) {
    
    JusticaMilitar <- c("TJMMG", "TJMSP", "TJMRS")
    for (t in JusticaMilitar) {
        tarefas[[t]] <- criar_tarefa_processamento(
            nome_base = "Justiça Militar Estadual",
            tribunal = t,
            func_processar = ProcessarDados.MilitarEstadual,
            path = path
        )
    }
    
}



if (processar_TRT) {
    
    JusticaTrabalho <- c("TRT1", "TRT2", "TRT3", "TRT4", "TRT5", "TRT6", "TRT7", "TRT8", "TRT9", "TRT10", "TRT11", "TRT12", "TRT13", "TRT14", "TRT15", "TRT16", "TRT17", "TRT18", "TRT19", "TRT20", "TRT21", "TRT22", "TRT23", "TRT24")

    for (t in JusticaTrabalho) {
        tarefas[[t]] <- criar_tarefa_processamento(
            nome_base = "Justiça do Trabalho",
            tribunal = t,
            func_processar = ProcessarDados.Trabalho,
            path = path
        )
    }
    
}


### Processando dados da meta 05, para todos os tribunais

if (opcao_ProcessarMeta05) {
    
    T <- c("Meta 05")
    
    for (t in T) {
        tarefas[[t]] <- local({
            meta05 <- t  # captura por valor
            function() {
                Sys.sleep(runif(1, 0, 0.5))
                inicio <- proc.time()
                Determinar.Meta05()
                fim <- proc.time()
                tempo <- round((fim - inicio)[["elapsed"]], 2)
                RegistrarLOG(sprintf("%s processado em %s", meta05, tempo))
                invisible(NULL)
                
            }
        })
    }
}


RegistrarLOG("Construindo ambiente clusterizado")

clusterEvalQ(cl, {

    source("_util.r")
    source("_flags.r")
    source("Meta01.r")
    source("Meta05.r")
    source("Segmento_STJ.r")
    source("Segmento_STM.r")
    source("Segmento_TST.r")
    source("Segmento_Estadual.r")
    source("Segmento_MilitarEstadual.r")
    source("Segmento_Eleitoral.r")
    source("Segmento_Federal.r")
    source("Segmento_Trabalho.r")    
    
    Rcpp::sourceCpp("pendente_meta.cpp")

    suppressWarnings(suppressMessages({
        pacman::p_load(
            "data.table", "dplyr", "dtplyr", "stringr", "lubridate", 
            "tidyverse", "winch", "foreach", "doParallel", "writexl", 
            "processx", "future", "Rcpp", "parallel"
        )
    }))
    
    # etc.
})

RegistrarLOG("Iniciando processamento paralelo.")

clusterExport(
    cl, 
    varlist = c(
        "path", 
        "ref", 
        "opcao_EnviarCompactados", 
        "opcao_CompactarArquivos", 
        "opcao_ProcessarMeta05", 
        "opcao_gerarPlanilhaPainel", 
        "opcao_local"))

parLapplyLB(cl, tarefas, function(f) f()) 

#parLapplyLB(cl, seq_along(tarefas), function(i) {
#    delay <- (i - 1) * 1  # 1 segundo entre tarefas
#    Sys.sleep(delay)
#    tarefas[[i]]()
#})


stopCluster(cl)

RegistrarLOG("Finalizando processamento paralelo.")

if (opcao_gerarPlanilhaPainel == TRUE) {
    
    DataReferencia <- "30/04/2026"
    
    inicio <- proc.time()
    GerarPlanilhaFinal(DataReferencia)
    fim <- proc.time()
    tempo <- round((fim - inicio)[["elapsed"]], 2)
    RegistrarLOG(paste0("Planilha do painel gerada em ", tempo, " seg."))
}

if (opcao_CompactarArquivos == TRUE) {
    inicio <- proc.time()
    CompactarArquivos()
    fim <- proc.time()
    tempo <- round((fim - inicio)[["elapsed"]], 2)
    RegistrarLOG(paste0("Arquivos compactados em ", tempo, " seg."))
}

if (opcao_EnviarCompactados == TRUE) {
    inicio <- proc.time()
    EnviarCompactados()
    fim <- proc.time()
    tempo <- round((fim - inicio)[["elapsed"]], 2)
    RegistrarLOG(paste0("Arquivos enviados para o S3 em ", tempo, " seg."))
}

if (opcao_gerarConsolidadoAgrupado == TRUE) {
    inicio <- proc.time()
    ConcatenarResumos()
    fim <- proc.time()
    tempo <- round((fim - inicio)[["elapsed"]], 2)
    RegistrarLOG(paste0("Consolidados agrupados em ", tempo, " seg."))
}

RegistrarLOG("Dados processados com sucesso!")






