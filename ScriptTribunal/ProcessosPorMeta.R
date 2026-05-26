
tribunais_segmento <<- tribble(

    ~sigla_tribunal, ~segmento,
    
    # Justiça Estadual
    
    "TJAC", "Justiça Estadual",
    "TJAL", "Justiça Estadual",
    "TJAM", "Justiça Estadual",
    "TJAP", "Justiça Estadual",
    "TJBA", "Justiça Estadual",
    "TJCE", "Justiça Estadual",
    "TJDFT", "Justiça Estadual",
    "TJES", "Justiça Estadual",
    "TJGO", "Justiça Estadual",
    "TJMA", "Justiça Estadual",
    "TJMG", "Justiça Estadual",
    "TJMS", "Justiça Estadual",
    "TJMT", "Justiça Estadual",
    "TJPA", "Justiça Estadual",
    "TJPB", "Justiça Estadual",
    "TJPE", "Justiça Estadual",
    "TJPI", "Justiça Estadual",
    "TJPR", "Justiça Estadual",
    "TJRJ", "Justiça Estadual",
    "TJRN", "Justiça Estadual",
    "TJRO", "Justiça Estadual",
    "TJRR", "Justiça Estadual",
    "TJRS", "Justiça Estadual",
    "TJSC", "Justiça Estadual",
    "TJSE", "Justiça Estadual",
    "TJSP", "Justiça Estadual",
    "TJTO", "Justiça Estadual",
    
    # Justiça do Trabalho
    
    "TRT1", "Justiça do Trabalho",
    "TRT2", "Justiça do Trabalho",
    "TRT3", "Justiça do Trabalho",
    "TRT4", "Justiça do Trabalho",
    "TRT5", "Justiça do Trabalho",
    "TRT6", "Justiça do Trabalho",
    "TRT7", "Justiça do Trabalho",
    "TRT8", "Justiça do Trabalho",
    "TRT9", "Justiça do Trabalho",
    "TRT10", "Justiça do Trabalho",
    "TRT11", "Justiça do Trabalho",
    "TRT12", "Justiça do Trabalho",
    "TRT13", "Justiça do Trabalho",
    "TRT14", "Justiça do Trabalho",
    "TRT15", "Justiça do Trabalho",
    "TRT16", "Justiça do Trabalho",
    "TRT17", "Justiça do Trabalho",
    "TRT18", "Justiça do Trabalho",
    "TRT19", "Justiça do Trabalho",
    "TRT20", "Justiça do Trabalho",
    "TRT21", "Justiça do Trabalho",
    "TRT22", "Justiça do Trabalho",
    "TRT23", "Justiça do Trabalho",
    "TRT24", "Justiça do Trabalho",
    
    # Justiça Eleitoral
    
    "TRE-AC", "Justiça Eleitoral",
    "TRE-AL", "Justiça Eleitoral",
    "TRE-AM", "Justiça Eleitoral",
    "TRE-AP", "Justiça Eleitoral",
    "TRE-BA", "Justiça Eleitoral",
    "TRE-CE", "Justiça Eleitoral",
    "TRE-DF", "Justiça Eleitoral",
    "TRE-ES", "Justiça Eleitoral",
    "TRE-GO", "Justiça Eleitoral",
    "TRE-MA", "Justiça Eleitoral",
    "TRE-MG", "Justiça Eleitoral",
    "TRE-MS", "Justiça Eleitoral",
    "TRE-MT", "Justiça Eleitoral",
    "TRE-PA", "Justiça Eleitoral",
    "TRE-PB", "Justiça Eleitoral",
    "TRE-PE", "Justiça Eleitoral",
    "TRE-PI", "Justiça Eleitoral",
    "TRE-PR", "Justiça Eleitoral",
    "TRE-RJ", "Justiça Eleitoral",
    "TRE-RN", "Justiça Eleitoral",
    "TRE-RO", "Justiça Eleitoral",
    "TRE-RR", "Justiça Eleitoral",
    "TRE-RS", "Justiça Eleitoral",
    "TRE-SC", "Justiça Eleitoral",
    "TRE-SE", "Justiça Eleitoral",
    "TRE-SP", "Justiça Eleitoral",
    "TRE-TO", "Justiça Eleitoral",
    
    # Justiça Federal
    
    "TRF1", "Justiça Federal",
    "TRF2", "Justiça Federal",
    "TRF3", "Justiça Federal",
    "TRF4", "Justiça Federal",
    "TRF5", "Justiça Federal",
    "TRF6", "Justiça Federal",
    
    # Justiça Militar Estadual
    
    "TJMMG", "Justiça Militar Estadual",
    "TJMSP", "Justiça Militar Estadual",
    "TJMRS", "Justiça Militar Estadual",
    
    # Tribunais Superiores
    
    "STJ", "Superior Tribunal de Justiça",
    "TST", "Tribunal Superior do Trabalho",
    "TSE", "Tribunal Superior Eleitoral",
    "STM", "Justiça Militar da União"
    
)


# Detecta núcleos e cria cluster
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)


# Arquivos disponibilizados para download

path <<- "output/"

# Filtrando processos

ProcessosMeta01 <- function(DataBase) {
  temp <- DataBase %>%
    filter(cnm1 == "1" | desm1 == "1" | julgadom1 == "1" )
  return(temp)
}

PendentesMeta01 <- function(DataBase) {
  temp <- DataBase %>% filter((susm1 == "0" | is.na(susm1)) & (julgadom1 == "0" | is.na(julgadom1)))
  return(temp)
}

ProcessosMeta02A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_a == "1")
  return(temp)
}

PendentesMeta02A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_a == "1" & (susm2_a == "0" | is.na(susm2_a)) & (julgadom2_a == "0" | is.na(julgadom2_a)))
  return(temp)
}

ProcessosMeta02B <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_b == "1")
  return (temp)
}

PendentesMeta02B <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_b == "1" & (susm2_b == "0" | is.na(susm2_b)) & (julgadom2_b == "0" | is.na(julgadom2_b)))
  return(temp)
}

ProcessosMeta02C <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_c == "1")
  return (temp)
}

PendentesMeta02C <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_c == "1" & (susm2_c == "0" | is.na(susm2_c)) & (julgadom2_c == "0" | is.na(julgadom2_c)))
  return(temp)
}

ProcessosMeta02Ant <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_ant == "1")
  return (temp)
}

PendentesMeta02Ant <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism2_ant == "1" & (susm2_ant == "0" | is.na(susm2_ant)) & (julgadom2_ant == "0" | is.na(julgadom2_ant)))
  return(temp)
}

ProcessosMeta04A <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism4_a == "1")
  return (temp)
}

PendentesMeta04A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism4_a == "1" & (susm4_a == "0" | is.na(susm4_a)) & (julgadom4_a == "0" | is.na(julgadom4_a)))
  return(temp)
}

ProcessosMeta04B <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism4_b == "1")
  return (temp)
}

PendentesMeta04B <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism4_b == "1" & (susm4_b == "0" | is.na(susm4_b)) & (julgadom4_b == "0" | is.na(julgadom4_b)))
  return(temp)
}

ProcessosMeta06A <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism6_a == "1")
  return (temp)
}

PendentesMeta06A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism6_a == "1" & (susm6_a == "0" | is.na(susm6_a)) & (julgadom6_a == "0" | is.na(julgadom6_a)))
  return(temp)
}

ProcessosMeta07A <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism7_a == "1")
  return (temp)
}

PendentesMeta07A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism7_a == "1" & (susm7_a == "0" | is.na(susm7_a)) & (julgadom7_a == "0" | is.na(julgadom7_a)))
  return(temp)
}

ProcessosMeta07B <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism7_b == "1")
  return (temp)
}


PendentesMeta07B <- function(DataBase) {
    temp <- DataBase %>%
        filter(dism7_b == "1" & (susm7_b == "0" | is.na(susm7_b)) & (julgadom7_b == "0" | is.na(julgadom7_b)))
    return(temp)
}

ProcessosMeta07C <- function(DataBase) {
    temp <- DataBase %>%  
        filter(dism7_c == "1")
    return (temp)
}

PendentesMeta07C <- function(DataBase) {
    temp <- DataBase %>%
        filter(dism7_c == "1" & (susm7_c == "0" | is.na(susm7_c)) & (julgadom7_c == "0" | is.na(julgadom7_c)))
    return(temp)
}


ProcessosMeta08A <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism8_a == "1")
  return (temp)
}

PendentesMeta08A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism8_a == "1" & (susm8_a == "0" | is.na(susm8_a)) & (julgadom8_a == "0" | is.na(julgadom8_a)))
  return(temp)
}

ProcessosMeta08B <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism8_b == "1")
  return (temp)
}

PendentesMeta08B <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism8_b == "1" & (susm8_b == "0" | is.na(susm8_b)) & (julgadom8_b == "0" | is.na(julgadom8_b)))
  return(temp)
}

ProcessosMeta10A <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism10_a == "1")
  return (temp)
}


PendentesMeta10A <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism10_a == "1" & (susm10_a == "0" | is.na(susm10_a)) & (julgadom10_a == "0" | is.na(julgadom10_a)))
  return(temp)
}

ProcessosMeta10B <- function(DataBase) {
  temp <- DataBase %>%  
    filter(dism10_b == "1")
  return (temp)
}

PendentesMeta10B <- function(DataBase) {
  temp <- DataBase %>%
    filter(dism10_b == "1" & (susm10_b == "0" | is.na(susm10_b)) & (julgadom10_b == "0" | is.na(julgadom10_b)))
  return(temp)
}


# Função: GravarArquivos (grava arquivos com processos que foram considerados em uma meta específica)

GravarArquivos <- function(Nome, Processos) {

    local <- "output/Detalhamento/"
    if (!dir.exists(local)) dir.create(local, recursive = TRUE)      
    data.table::fwrite(Processos, file = paste0(local, Nome), sep = ";", dec = ",", bom = T)

}

ProcessosPorMeta <- function(Tribunal) {
  
    Segmento <- tribunais_segmento %>% filter(sigla_tribunal == Tribunal) %>% pull(segmento)
    
    Arquivo <- paste0 ("output/", Tribunal, ".csv")
    DataBase <- fread(Arquivo, sep = ";", dec = ",")
    
    Resultado <- switch(
        Segmento,
        "Justiça Estadual" = {
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02B.csv"), ProcessosMeta02B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02B.csv"), PendentesMeta02B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02C.csv"), ProcessosMeta02C(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02C.csv"), PendentesMeta02C(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04B.csv"), ProcessosMeta04B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04B.csv"), PendentesMeta04B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta06A.csv"), ProcessosMeta06A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta06A.csv"), PendentesMeta06A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07A.csv"), ProcessosMeta07A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07B.csv"), ProcessosMeta07B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07B.csv"), PendentesMeta07B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta08A.csv"), ProcessosMeta08A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta08A.csv"), PendentesMeta08A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta08B.csv"), ProcessosMeta08B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta08B.csv"), PendentesMeta08B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta10A.csv"), ProcessosMeta10A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta10A.csv"), PendentesMeta10A(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta10B.csv"), ProcessosMeta10B(DataBase))
            GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta10A.csv"), PendentesMeta10A(DataBase))
            
            return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JE) foi gerado!"))   
            
      },
                      
    "Justiça do Trabalho" = {
      
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JT) foi gerado!"))      
        
    },
                      
    "Justiça Federal" = {
      
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02B.csv"), ProcessosMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02B.csv"), PendentesMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04B.csv"), ProcessosMeta04B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04B.csv"), PendentesMeta04B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta06A.csv"), ProcessosMeta06A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta06A.csv"), PendentesMeta06A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07A.csv"), ProcessosMeta07A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07A.csv"), PendentesMeta07A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07B.csv"), ProcessosMeta07B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07B.csv"), PendentesMeta07B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta10A.csv"), ProcessosMeta10A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta10A.csv"), PendentesMeta10A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta10B.csv"), ProcessosMeta10B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta10B.csv"), PendentesMeta10B(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JF) foi gerado!"))      
    },
                      
    "Justiça Eleitoral" = {
      
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04B.csv"), ProcessosMeta04B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04B.csv"), PendentesMeta04B(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JE) foi gerado!"))
        
    },
                      
    "Justiça Militar Estadual" = {
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02B.csv"), ProcessosMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02B.csv"), PendentesMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04B.csv"), ProcessosMeta04B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04B.csv"), PendentesMeta04B(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JME) foi gerado!"))
        
    },
          
    "Justiça Militar da União" = {
      
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02B.csv"), ProcessosMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02B.csv"), PendentesMeta02B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04B.csv"), ProcessosMeta04B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04B.csv"), PendentesMeta04B(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JMU) foi gerado!"))
        
    },
                      
    "Superior Tribunal de Justiça" = {
      
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02A.csv"), ProcessosMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02A.csv"), PendentesMeta02A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta04A.csv"), ProcessosMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta04A.csv"), PendentesMeta04A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta06A.csv"), ProcessosMeta06A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta06A.csv"), PendentesMeta06A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07A.csv"), ProcessosMeta07A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07A.csv"), PendentesMeta07A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07B.csv"), ProcessosMeta07B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07B.csv"), PendentesMeta07B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta07C.csv"), ProcessosMeta07C(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta07C.csv"), PendentesMeta07C(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta08A.csv"), ProcessosMeta08A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta08A.csv"), PendentesMeta08A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta08B.csv"), ProcessosMeta08B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta08B.csv"), PendentesMeta08B(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta10A.csv"), ProcessosMeta10A(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta10A.csv"), PendentesMeta10A(DataBase))
        
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " foi gerado!"))
    
      },
    
    "Tribunal Superior do Trabalho" = {
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta01.csv"), ProcessosMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta01.csv"), PendentesMeta01(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Considerados_Meta02Ant.csv"), ProcessosMeta02Ant(DataBase))
        GravarArquivos(paste0(Tribunal, "_Processos_Pendentes_Meta02Ant.csv"), PendentesMeta02Ant(DataBase))
        return(paste("✅ O conjunto de arquivos do ", Tribunal, " (da JT) foi gerado!"))      
    },
                      
                      {
                        paste("Segmento desconhecido para o tribunal", Tribunal)
                      }
  )
  
  return(Resultado)
}

