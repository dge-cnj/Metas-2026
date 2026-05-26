
suppressWarnings(
    
    suppressMessages({
    
        if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
        if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
        if (!requireNamespace("dtplyr", quietly = TRUE)) install.packages("dtplyr")
        if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
        if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
        if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
        if (!requireNamespace("winch", quietly = TRUE)) install.packages("winch")
        if (!requireNamespace("foreach", quietly = TRUE)) install.packages("foreach")
        if (!requireNamespace("doParallel", quietly = TRUE)) install.packages("doParallel")
        if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
        if (!requireNamespace("processx", quietly = TRUE)) install.packages("processx")
        if (!requireNamespace("future", quietly = TRUE)) install.packages("future")
        if (!requireNamespace("Rcpp", quietly = TRUE)) install.packages("Rcpp")
        if (!requireNamespace("parallel", quietly = TRUE)) install.packages("parallel")
        if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
        if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
      
        library("data.table")
        library("dtplyr")
        library("stringr")
        library("lubridate")
        library("tidyverse")
        library("winch")
        library("foreach")
        library("doParallel")
        library("writexl")
        library("processx")
        library("future")
        library("Rcpp")
        library("parallel")
        library("openxlsx")
        library("tibble")
    
    }))

readRenviron(".Renviron.sh")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Rcpp::sourceCpp("pendente_meta.cpp")

source("_util.r")
source("_flags.r")
source("_gerarPlanilhaFinal.r")
source("_gerarResumo.r")
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
source("ProcessosPorMeta.r")

RegistrarLOG("Iniciando processo ")

unlink("output", recursive = TRUE)

if (!dir.exists("output/")) dir.create("output/", recursive = TRUE)
if (!dir.exists("output/Painel 2026/")) dir.create("output/Painel 2026/", recursive = TRUE)      
if (!dir.exists("output/Resumo/")) dir.create("output/Resumo/", recursive = TRUE) 

file.create(".Renviron.sh")

RegistrarLOG("Estrutura de pastas construída! ")

ProcessID <- Sys.getpid() # Obter o ID do processo atual (PID)
cmd <- paste0("powershell -Command \"(Get-Process -Id ", ProcessID, ").PriorityClass = 'High'\"")
system(cmd)  # Executa o comando PowerShell

STJ <- c("STJ")
STM <- c("STM")
TST <- c("TST")
JusticaMilitar <- c("TJMMG", "TJMSP", "TJMRS")
JusticaEstadual <- c("TJSP", "TJTO", "TJAC", "TJRJ", "TJBA", "TJAP", "TJCE", "TJDFT", "TJES", "TJGO", "TJMA", "TJMS", "TJMT", "TJPA", "TJPB", "TJPE", "TJPI", "TJRN", "TJRO", "TJRR", "TJSE", "TJMG", "TJPR", "TJAM", "TJAL", "TJRS", "TJSC")
JusticaFederal <- c("TRF1", "TRF2", "TRF3", "TRF4", "TRF5", "TRF6")
JusticaTrabalho <- c("TRT1", "TRT2", "TRT3", "TRT4", "TRT5", "TRT6", "TRT7", "TRT8", "TRT9", "TRT10", "TRT11", "TRT12", "TRT13", "TRT14", "TRT15", "TRT16", "TRT17", "TRT18", "TRT19", "TRT20", "TRT21", "TRT22", "TRT23", "TRT24")
JusticaEleitoral <- c("TRE-AC", "TRE-AL", "TRE-AM", "TRE-AP", "TRE-BA", "TRE-CE", "TRE-DF", "TRE-ES", "TRE-GO", "TRE-MA", "TRE-MG", "TRE-MS", "TRE-MT", "TRE-PA", "TRE-PB", "TRE-PE", "TRE-PI", "TRE-PR", "TRE-RJ", "TRE-RN", "TRE-RO", "TRE-RR", "TRE-RS", "TRE-SC", "TRE-SE", "TRE-SP", "TRE-TO")

path <- ""   # Arquivos que serão processados para gerar dados do painel

ref <<- 20260101  ### Alterado
temp <- list.files(pattern = "1)Extrai_Datamart.*\\.csv$",full.names = TRUE)

# Determinando o tribunal

Tribunal <- fread(temp[1], nrows = 1)$sigla_tribunal

RegistrarLOG(sprintf("Iniciando processamento dos dados do %s.", Tribunal))

Inicio <- proc.time()

if (Tribunal %in% STJ) {
    ProcessarDados.STJ(path, temp, Tribunal)
} else if (Tribunal %in% STM) {
    ProcessarDados.STM(path, temp, Tribunal)
} else if (Tribunal %in% TST) {
    ProcessarDados.TST(path, temp, Tribunal)
} else if (Tribunal %in% JusticaMilitar) {
    ProcessarDados.MilitarEstadual(path, temp, Tribunal)
} else if (Tribunal %in% JusticaEstadual) {
    ProcessarDados.Estadual(path, temp, Tribunal)
} else if (Tribunal %in% JusticaFederal) {
    ProcessarDados.Federal(path, temp, Tribunal)
} else if (Tribunal %in% JusticaTrabalho) {
    ProcessarDados.Trabalho(path, temp, Tribunal)
} else if (Tribunal %in% JusticaEleitoral) {
    ProcessarDados.Eleitoral(path, temp, Tribunal)
} else {
    stop(sprintf("Sigla de tribunal desconhecida: '%s'", Tribunal))
}

Fim <- proc.time()

Tempo <- round((Fim - Inicio)[["elapsed"]], 2)

RegistrarLOG(sprintf("%s processado em %s seg.", Tribunal, Tempo))

Determinar.Meta05(Tribunal)
GerarPlanilhaFinal()
CompactarArquivos()
ConcatenarConsolidados()
ProcessosPorMeta(Tribunal)

RegistrarLOG("Dados processados cpom sucesso!")

