# Módulo: flags (versão )

#part <<- NULL # Carga inicial da base do DPJ
#dge <<- NULL # Dados que serão utilizados para o cômputo das metas
#tribunal <<- NULL # Tribunal cujas metas estão sendo calculadas
#pre_processual <<- NULL

#Flags metas temáticas

rac_inj_assunto <<- c(3613, 9873, 11844, 12861, 14101, 14102, 14100, 15135, 14111, 14110, 14108, 14697,14699, 15128)


imp_assunto <<- c(10011,10012,10013,10014)
imp_classe <<- c(64)
adm_pub_assunto <<- c(5874, 3580, 3577, 3576, 3595, 3585, 11797, 3593, 3579, 3582, 3592, 3594, 3588, 3578, 3587, 3581,
                      3583, 3584, 3582, 3586, 3589, 3590, 3591, 3575, 3523, 3535, 11006, 11007, 3533, 10995, 3536, 3531,
                      10998, 3529, 11005, 3530, 3541, 3534, 3524, 3545, 10993, 3527, 10994, 3540, 3539,3546, 10992, 3528,
                      3542, 3537, 10996, 10997, 3543, 14731, 3538, 3532, 11004, 5875, 5905, 5909, 5903, 10991, 5904, 10988,
                      10989, 10990, 5908, 5910, 5906, 5907, 3430, 14698, 3432, 3547, 10982, 3561, 3559, 3553, 3558, 3555,
                      3552, 3554, 3562, 3551, 3556, 3596, 3597, 3548, 3550, 3557, 3564, 3563, 3560, 5872, 10983, 3574,
                      3568, 3573, 3572, 3569, 3570, 3566, 3598, 3571, 3567, 3565, 10984, 5876, 5877, 3651, 3614, 10985,
                      10986, 3612, 3642, 3628, 3606, 3604, 3650, 3603, 5893, 12015, 3655, 12013, 3605, 3615, 14800, 14801,
                      14802, 15397, 15403, 15072, 11069, 11070, 11071, 11072, 11358, 11359, 11360, 11361, 11362, 11363,
                      11364, 11365, 11366, 11367, 11368, 11369, 11370, 11371, 11372, 11308, 11355, 11356, 11357, 11309,
                      11352, 11353, 11354, 11310, 11336, 11337, 11338, 11339, 11340, 11341, 11342, 11343, 11344, 11345,
                      11346, 11347, 11348, 11349, 11350, 11351, 11311, 11331, 11332, 11333, 11334, 11335, 11312, 11326,
                      11327, 11328, 11329, 11330, 11313, 11318, 11319, 11320, 11321, 11322, 11323, 11324, 11325, 11314,
                      11315, 11316, 11317, 11166, 11167, 11152, 11143, 11140, 11165, 11153, 11173, 11171, 11169, 11174)
ili_ele_classe <<- c(11528, 11526, 11527, 12627, 12630, 14209, 11548, 1346, 12627, 11533)
ili_ele_assunto <<- c(11470, 12634, 15020, 11479, 11473, 14221, 11472, 11478, 11476, 11471, 11474, 11475, 11436, 15143,
                      11466, 11455, 11438, 11440, 11464, 11465, 11504, 11501, 11510, 11505, 11506, 11500, 11509, 11503,
                      15399, 11507, 11522, 11596, 12395, 11598, 11599, 12396, 12393, 11604, 12392, 11605, 11607, 11700,
                      10803, 11718, 11719, 11720, 12597, 11721, 12062, 12063, 11722, 11723, 15537)
amb_assunto <<- c(3429, 3511, 3618, 3619, 9792, 9878, 9879, 9880, 9881, 9882, 9883,9884, 9887, 11779, 11780,14779, 14780,
                  14785, 14783, 14781, 14782, 14784, 3620, 14790, 14792, 14786, 14788, 14787, 14789, 14791, 14793, 3621,
                  14798, 14795, 14799, 14796, 14797, 14794, 3622, 3623, 14805, 14804, 14803, 3624, 3626, 3627, 10986, 14800,
                  14801, 14802, 5962, 10016, 10108, 10396, 10110, 9994, 15302, 15301, 15008, 15300, 10111, 10112, 10113,
                  10114, 10115, 10116, 10118, 10119, 10438, 11822, 11823, 11824, 11825, 11826, 11827, 11828, 11829, 11830,
                  11862, 11869)
ind_assunto <<- c(9989, 12824, 13199, 9901, 15220, 10102, 3647, 15114, 10105, 10104, 10103, 15595, 15614, 15609, 15592,
                  15114, 15597, 15610, 15604, 15615, 15611)


qui_assunto <<- c(12031, 12825, 15618, 15613, 15687, 15688)
vd_assunto <<- c(10948, 10949, 11979, 12194, 12196, 14097, 14098, 14226, 14227, 14228, 14229, 14943, 14945, 14942, 14944, 15511)
vd_classe <<- c(1268, 15309, 12423)
fem_assunto <<- c(12091,12358)
seq_assunto <<- c(10921)
inf_assunto <<- c(9633, 9634, 9932, 9740, 9755, 9770, 9756, 9760, 9766, 9807, 9655, 9658, 12547, 9773, 9666, 9664, 9671, 9659,
                  9722, 9803, 9720, 9635, 9955, 9674, 9734, 15385, 9641, 9647, 9648, 9828, 9917, 9929, 9847, 15134, 9848, 9878,
                  9858, 9895, 15181, 14228, 15525, 9964, 12341, 11818, 9916, 9915, 9946, 9936, 9938, 9935, 9934, 9953, 11939, 9945,
                  9939, 9943, 9951, 9937, 9941, 9942, 9940, 9944, 9947, 9948, 9949, 9933, 9950, 9952, 9749, 9744, 9743, 9751, 9745,
                  12385, 9742, 11456, 9750, 11457, 11458, 12384, 9747, 9748, 9752, 11459, 9741, 9753, 11460, 9754, 9746, 11461,
                  9771, 9772, 9758, 9757, 9759, 9761, 9762, 9763, 9765, 9764, 9767, 9768, 9769, 9827, 9817, 9810, 9824, 9819, 9815,
                  9818, 9814, 9813, 9811, 9812, 9823, 9816, 9825, 9808, 9826, 9809, 9820, 9822, 9821, 9656, 9657, 12546, 12545,
                  14109, 14111, 14110, 14115, 14114, 14108, 14112, 14113, 15133, 9783, 9784, 9782, 9791, 9792, 9793, 9778, 11970,
                  9781, 11973, 11971, 9780, 9787, 9794, 9786, 9790, 9775, 11964, 9800, 9789, 9795, 9774, 11962, 9796, 9785, 9777,
                  11968, 9788, 9802, 9797, 9798, 9799, 9779, 11965, 9776, 9801, 9668, 9669, 9667, 9670, 9665, 9672, 9673, 9661,
                  14944, 9660, 15484, 15485, 14686, 9663, 9662, 9732, 9731, 9725, 9724, 9723, 9730, 9729, 9733, 9728, 9727, 9726,
                  9806, 14687, 14688, 9805, 9804, 9721, 11987, 12358, 12131, 9637, 9638, 15178, 9636, 9639, 9640, 9958, 9962, 9956,
                  9957, 9961, 9963, 9959, 9960, 9714, 9708, 9682, 9688, 9706, 9696, 9705, 9695, 9689, 9685, 9687, 9704, 14700, 9709,
                  9707, 9713, 9718, 9684, 9690, 14693, 9691, 9679, 9681, 9702, 9680, 9701, 9693, 9710, 9716, 9712, 9711, 9717, 15411,
                  9675, 9677, 9698, 9697, 9676, 9715, 9686, 9700, 9692, 9694, 11960, 9719, 9678, 9699, 12009, 9703, 9683, 9735, 9736,
                  9739, 9737, 9738, 9914, 15392, 15386, 15394, 15393, 15387, 15395, 15389, 15396, 15388, 15390, 15391, 14945, 12196,
                  12197, 9644, 9645, 9642, 9646, 9643, 9651, 14691, 15310, 9652, 9649, 9654, 9650, 9841, 9839, 9833, 9838, 9835, 9832,
                  9834, 9842, 9831, 9836, 9845, 9846, 9829, 9830, 9837, 9844, 9843, 9840, 9927, 9921, 9926, 9925, 9922, 9923, 9919,
                  9928, 9924, 9920, 9918, 9930, 9931, 9904, 15139, 15138, 15137, 15135, 15136, 9913, 9856, 9875, 9874, 9852, 9851,
                  9853, 9850, 9849, 9854, 9871, 9876, 9889, 9882, 9883, 9884, 9879, 9880, 11779, 9881, 9886, 9887, 9896, 9870, 9872,
                  9899, 12022, 12020, 9900, 12010, 12024, 12018, 12016, 9888, 15398, 9857, 9897, 9894, 9907, 9855, 9891, 9864, 9866,
                  9868, 9861, 9865, 9862, 9863, 9860, 9867, 9859, 9892, 9908, 9893, 9906, 15473, 15471, 15460, 15458, 15462, 15457,
                  15478, 15463, 15470, 15464, 15479, 15459, 15468, 15466, 15467, 15477, 15472, 15465, 15475, 15469, 15474, 15476,
                  15461, 9909, 12026, 9911, 9912, 9910, 9901, 9890, 15407, 15182, 15402, 14229, 11896, 14699, 9903, 12014, 9905, 9877,
                  9869, 9873, 9898, 12012, 15404, 15073, 15400, 9653, 15526, 15527, 15528, 15529, 11979, 15175, 15197, 15198, 15199,
                  9966, 9965, 9968, 9975, 9974, 9972, 9973, 14672, 14675, 14673, 14674, 11819, 11821, 11820, 9977, 12006, 9969, 11981, 
                  12007, 11817, 11816, 9967, 12002, 12004, 11996, 12003, 11999, 12001, 11998, 11997, 12005, 12000, 12090, 9970, 9979,
                  9971, 9978, 10941, 15176, 15549, 15550, 15551, 15552, 15553, 15554, 15555, 15556, 15557, 15558, 15559, 15560)
data_dessobrestado <<- seq(from=as.Date("2025-12-20"), to=as.Date("2026-12-19"), by="day") # Período da Meta 1
data_dessobrestado <<- as.numeric(format(data_dessobrestado, "%Y%m%d"))

mpu_meta_classe <- c(1268,15309,12423, 15170, 15171, 14734, 15172, 10967, 12424, 12070)# Medidas protetivas de urgência (considerados nas metas 1, 2 e 8)

ref <<- 20270101

iniciarFlags <- function(path, aux, i) {

    file_path <- paste0(path, aux[i])
    part <- NULL

    tryCatch({
        part <- fread(file = file_path)
        if (nrow(part) == 0) stop("O arquivo foi carregado, mas está vazio.")
    }, error = function(e) {
        message("Erro ao abrir o arquivo: ", e$message)
    })
        

    unique(part$procedimento)
    
    pre_processual <- part %>% filter(procedimento == "Pré-processual")
    pre_processual <- pre_processual %>% 
        mutate(
            senth_pre22 = if_else(dt_primeiro_julgamento_homologatorio >= "2022-01-01" & dt_primeiro_julgamento_homologatorio < "2023-01-01", 1, 0),
            senth_pre23 = if_else(dt_primeiro_julgamento_homologatorio >= "2023-01-01" & dt_primeiro_julgamento_homologatorio < "2024-01-01", 1, 0),
            senth_pre24 = if_else(dt_primeiro_julgamento_homologatorio >= "2024-01-01" & dt_primeiro_julgamento_homologatorio < "2025-01-01", 1, 0),
            senth_pre25 = if_else(dt_primeiro_julgamento_homologatorio >= "2025-01-01" & dt_primeiro_julgamento_homologatorio < "2026-01-01", 1, 0),
            senth_pre26 = if_else(dt_primeiro_julgamento_homologatorio >= "2026-01-01" & dt_primeiro_julgamento_homologatorio < "2027-01-01", 1, 0)
            ) %>% 
        filter(!is.na(senth_pre22) | !is.na(senth_pre23) | !is.na(senth_pre24) | !is.na(senth_pre25) | !is.na(senth_pre26)) %>% 
        select(
            c(id_processo, sigla_tribunal, sigla_grau, procedimento, recurso, id_tribunal, numero_sigilo, id_ultimo_oj,
              dt_recebimento, data_total_primeira_baixa, dt_pendente_liquido, dt_dessobrestamento, dt_julgamento, dt_julgamento_movimento,
              dt_julgamento_homologatorio, data_total_primeiro_julgamento_sem_pronuncia, dt_primeiro_julgamento_homologatorio, flag_racismo,
              data_total_primeiro_procedimento_resolvido, dt_decisao_movimento, dt_redistribuido_entrada, dt_redistribuido_saida,
              data_total_ultima_suspensao, ramo_justica, id_ultima_classe, dt_pendente, data_ultima_movimentacao, dt_primeira_medida_protetiva_meta, flag_quilombolas, flag_indigenas,
              id_assunto, flag_violencia_domestica, flag_feminicidio, flag_ambiental, flag_infancia, senth_pre22, senth_pre23, senth_pre24, senth_pre25, senth_pre26))
    
    part <- part %>% filter(procedimento %in% c("Conhecimento criminal", "Conhecimento não criminal"))
    gc()
    
    ### Eliminando colunas não necessárias para as metas
    
    part <- part %>% 
        select(
            c(id_processo, sigla_tribunal, sigla_grau, procedimento, recurso, id_tribunal, numero_sigilo, id_ultimo_oj,
              dt_recebimento, data_total_primeira_baixa, dt_pendente_liquido, dt_dessobrestamento, dt_pendente, dt_julgamento, dt_julgamento_movimento,
              dt_julgamento_homologatorio, data_total_primeiro_julgamento_sem_pronuncia, dt_primeiro_julgamento_homologatorio, 
              data_total_primeiro_procedimento_resolvido, dt_decisao_movimento, dt_redistribuido_entrada, dt_redistribuido_saida,
              data_ultima_movimentacao, data_total_ultima_suspensao, ramo_justica, id_ultima_classe,
              dt_primeira_medida_protetiva_meta, flag_quilombolas, flag_indigenas, flag_infancia,flag_racismo,
              id_assunto, flag_violencia_domestica, flag_feminicidio, flag_ambiental, flag_infancia))

    ### Flags ----
    part[, dt_pendente_meta := sapply(dt_pendente_liquido, pendente_meta)]
    setDT(part)
    part <- part %>% dtplyr::lazy_dt()
    part <- part %>% collect() %>% data.frame()    

    part <- part %>% mutate(
        penultimo_pendente_liq = substr(dt_pendente_meta,1,8),
        ultimo_pendente_liq = if_else(grepl(":0", dt_pendente_meta), "0", substr(dt_pendente_meta,10,18)),
        comp_penultimo = substr(data_ultima_movimentacao,1,10),
        pronuncia = case_when(grepl(":72:10953}",dt_julgamento_movimento) ~ 1, TRUE ~ 0),
        decisao = if_else(grepl(":14702}",dt_decisao_movimento), substr(dt_decisao_movimento, nchar(dt_decisao_movimento)-18, nchar(dt_decisao_movimento)-11), "0"),
        sent_arq_des = if_else(grepl(":463}|:473}|:472}", dt_julgamento_movimento), 1, 0),
        pendente_ano_meta = if_else(penultimo_pendente_liq < ref & (ultimo_pendente_liq >= ref | ultimo_pendente_liq == "0"), "1", "0"),
        penultimo_pendente_liq = if_else(pendente_ano_meta == "0", "100", penultimo_pendente_liq),
        ultimo_pendente_liq = if_else(pendente_ano_meta == "0", "100", ultimo_pendente_liq),
        flg_dessobrestado = func.procura.array(data_dessobrestado, base = part, variavel = "dt_dessobrestamento"),
        ultimo_dessobrestado = substr(dt_dessobrestamento, nchar(dt_dessobrestamento)-8, nchar(dt_dessobrestamento)),
        ultimo_dessobrestado = substr(ultimo_dessobrestado,1,8),
        ultimo_dessobrestado = if_else(ultimo_dessobrestado > max(data_dessobrestado), "0", ultimo_dessobrestado))

    dge <- part
    colnames(dge)
        
    #GROUP BY
        
    setDT(dge)
    dge <- dge %>% dtplyr::lazy_dt()
    dge <- dge %>% 
        group_by(id_processo, numero_sigilo, sigla_grau, procedimento, ramo_justica, sigla_tribunal, id_tribunal, recurso, id_ultimo_oj) %>%
        summarise(
            dt_recebimento = min(dt_recebimento, na.rm = TRUE),
            data_total_primeira_baixa = min(data_total_primeira_baixa, na.rm = TRUE),
            dt_julgamento = min(dt_julgamento, na.rm = TRUE),
            dt_julgamento_homologatorio = min(dt_julgamento_homologatorio, na.rm = TRUE),
            data_total_primeiro_julgamento_sem_pronuncia = min(data_total_primeiro_julgamento_sem_pronuncia, na.rm = TRUE),
            dt_primeiro_julgamento_homologatorio = min(dt_primeiro_julgamento_homologatorio, na.rm = TRUE),
            data_total_primeiro_procedimento_resolvido = min(data_total_primeiro_procedimento_resolvido, na.rm = TRUE),
            dt_redistribuido_entrada = min(dt_redistribuido_entrada, na.rm = TRUE),
            dt_redistribuido_saida = min(dt_redistribuido_saida, na.rm = TRUE),
            dt_primeira_medida_protetiva_meta = min(dt_primeira_medida_protetiva_meta, na.rm = TRUE),
            #data_total_ultimo_pendente = max(data_total_ultimo_pendente, na.rm = TRUE),
            data_total_ultima_suspensao = max(data_total_ultima_suspensao, na.rm = TRUE),
            id_ultima_classe = min(id_ultima_classe, na.rm = TRUE),
            id_assunto = min(id_assunto, na.rm = TRUE),
            flag_violencia_domestica = max(flag_violencia_domestica, na.rm = TRUE),
            flag_feminicidio = max(flag_feminicidio, na.rm = TRUE),
            flag_ambiental = max(flag_ambiental, na.rm = TRUE),
            flag_quilombolas = max(flag_quilombolas, na.rm = TRUE),
            flag_indigenas = max(flag_indigenas, na.rm = TRUE),
            flag_infancia = max(flag_infancia, na.rm = TRUE),
            flag_rac_inj = max(flag_racismo, na.rm = TRUE),
            ultimo_pendente_liq = max(ultimo_pendente_liq, na.rm = TRUE),
            dt_dessobrestamento = max(dt_dessobrestamento, na.rm = TRUE),
            ultimo_dessobrestado = max(ultimo_dessobrestado, na.rm = TRUE),
            flg_dessobrestado = max(flg_dessobrestado, na.rm = TRUE),
            pronuncia = min(pronuncia, na.rm = TRUE),
            decisao = min(decisao, na.rm = TRUE),
            penultimo_pendente_liq = max(penultimo_pendente_liq, na.rm = TRUE),
            pendente_meta = max(pendente_ano_meta, na.rm = TRUE),
            sent_arq_des = max(sent_arq_des, na.rm = TRUE)
        ) %>% collect() %>% data.frame()
    
    dge <- dge %>% 
        mutate(
            dt_resolvido = pmin(data_total_primeiro_julgamento_sem_pronuncia, data_total_primeira_baixa, data_total_primeiro_procedimento_resolvido, dt_primeira_medida_protetiva_meta, na.rm = TRUE))

     #Flags metas temáticas
    
    dge <- dge %>% 
        mutate(
            flg_imp = func.procura.array(imp_classe, base = dge, variavel = "id_ultima_classe") 
            | func.procura.array(imp_assunto, base=dge, variavel = "id_assunto"),
            flg_crim_contr_adm_pbl = func.procura.array(adm_pub_assunto, base = dge, variavel = "id_assunto"), flg_ili_ele = func.procura.array(ili_ele_classe, base = dge, variavel = "id_ultima_classe") &
                func.procura.array(ili_ele_assunto, base = dge, variavel = "id_assunto"),
            flg_amb = func.procura.array(amb_assunto, base = dge, variavel = "id_assunto"),
            flg_ind = func.procura.array(ind_assunto, base = dge, variavel = "id_assunto"),
            flg_qui = func.procura.array(qui_assunto, base = dge, variavel = "id_assunto"),
            flg_vd = func.procura.array(vd_assunto, base = dge, variavel = "id_assunto"),
            flg_fem = func.procura.array(fem_assunto, base = dge, variavel = "id_assunto"),
            flg_seq = func.procura.array(seq_assunto, base = dge, variavel = "id_assunto"),
            flg_inf = func.procura.array(inf_assunto, base = dge, variavel = "id_assunto"),
            flg_rac_inj = func.procura.array(rac_inj_assunto, base = dge, variavel = "id_assunto"),
            mpu_meta = func.procura.array(mpu_meta_classe, base = dge, variavel = "id_ultima_classe"))

    return (list(dge=dge, pre_processual=pre_processual))
  
}







iniciarFlagsEleitoral <- function(path, aux, i) {
    
    file_path <- paste0(path, aux[i])
    
    tryCatch({
        part <- fread(file = file_path)
        if (nrow(part) == 0) stop("O arquivo foi carregado, mas está vazio.")
    }, error = function(e) {
        message("Erro ao abrir o arquivo: ", e$message)
    })
    
    ### Filtrando conhecimento 

    unique(part$procedimento)
    
    pre_processual <- part %>% filter(procedimento == "Pré-processual")
    pre_processual <- pre_processual %>% 
        mutate(
            senth_pre22 = if_else(dt_primeiro_julgamento_homologatorio >= "2022-01-01" & dt_primeiro_julgamento_homologatorio < "2023-01-01", 1, 0),
            senth_pre23 = if_else(dt_primeiro_julgamento_homologatorio >= "2023-01-01" & dt_primeiro_julgamento_homologatorio < "2024-01-01", 1, 0),
            senth_pre24 = if_else(dt_primeiro_julgamento_homologatorio >= "2024-01-01" & dt_primeiro_julgamento_homologatorio < "2025-01-01", 1, 0),
            senth_pre25 = if_else(dt_primeiro_julgamento_homologatorio >= "2025-01-01" & dt_primeiro_julgamento_homologatorio < "2026-01-01", 1, 0),
            senth_pre26 = if_else(dt_primeiro_julgamento_homologatorio >= "2026-01-01" & dt_primeiro_julgamento_homologatorio < "2027-01-01", 1, 0)) %>% 
        filter(!is.na(senth_pre22) | !is.na(senth_pre23) | !is.na(senth_pre24) | !is.na(senth_pre25)) %>% 
        select(
            c(id_processo, sigla_tribunal, sigla_grau, procedimento, recurso, id_tribunal, numero_sigilo, id_ultimo_oj,
              dt_recebimento, data_total_primeira_baixa, dt_pendente_liquido, dt_dessobrestamento, dt_julgamento, dt_julgamento_movimento,
              dt_julgamento_homologatorio, data_total_primeiro_julgamento_sem_pronuncia, dt_primeiro_julgamento_homologatorio, 
              data_total_primeiro_procedimento_resolvido, dt_decisao_movimento, dt_redistribuido_entrada, dt_redistribuido_saida,
              data_total_ultima_suspensao, ramo_justica, id_ultima_classe, ano_eleicao, 
              dt_pendente, data_ultima_movimentacao, dt_primeira_medida_protetiva_meta, flag_quilombolas, flag_indigenas, flag_racismo,
              id_assunto, flag_violencia_domestica, flag_feminicidio, flag_ambiental, flag_infancia, senth_pre22, senth_pre23, senth_pre24, senth_pre25, senth_pre26))
    
        part <- part %>% filter(procedimento %in% c("Conhecimento criminal", "Conhecimento não criminal"))
    
    # Eliminando colunas não necessárias para as metas

    part <- part %>% 
        select(
            c(id_processo, sigla_tribunal, sigla_grau, procedimento, recurso, id_tribunal, numero_sigilo, id_ultimo_oj,
              dt_recebimento, data_total_primeira_baixa, dt_pendente, dt_pendente_liquido, dt_dessobrestamento, dt_julgamento, data_ultima_movimentacao, dt_julgamento_movimento,
              dt_julgamento_homologatorio, data_total_primeiro_julgamento_sem_pronuncia, dt_primeiro_julgamento_homologatorio, 
              data_total_primeiro_procedimento_resolvido, dt_decisao_movimento, dt_redistribuido_entrada, dt_redistribuido_saida,
              data_total_ultima_suspensao, ramo_justica, id_ultima_classe, ano_eleicao,
              dt_primeira_medida_protetiva_meta, flag_quilombolas, flag_indigenas,
              id_assunto, flag_violencia_domestica, flag_feminicidio, flag_ambiental, flag_infancia, flag_racismo))
    
    # Flags ----
    
    part[, dt_pendente_meta := sapply(dt_pendente_liquido, pendente_meta)]
    setDT(part)
    part <- part %>% dtplyr::lazy_dt()
    part <- part %>% collect() %>% data.frame()
    
    part <- part %>% mutate(
        penultimo_pendente_liq = substr(dt_pendente_meta,1,8),
        ultimo_pendente_liq = if_else(grepl(":0", dt_pendente_meta), "0", substr(dt_pendente_meta,10,18)),
        comp_penultimo = substr(data_ultima_movimentacao,1,10),
        pronuncia = case_when(grepl(":72:10953}",dt_julgamento_movimento) ~ 1, TRUE ~ 0),
        decisao = if_else(grepl(":14702}",dt_decisao_movimento), substr(dt_decisao_movimento, nchar(dt_decisao_movimento)-18, nchar(dt_decisao_movimento)-11), "0"),
        sent_arq_des = if_else(grepl(":463}|:473}|:472}", dt_julgamento_movimento), 1, 0),
        pendente_ano_meta = if_else(penultimo_pendente_liq < ref & (ultimo_pendente_liq >= ref | ultimo_pendente_liq == "0"), "1", "0"),
        penultimo_pendente_liq = if_else(pendente_ano_meta == "0", "100", penultimo_pendente_liq),
        ultimo_pendente_liq = if_else(pendente_ano_meta == "0", "100", ultimo_pendente_liq),
        flg_dessobrestado = func.procura.array(data_dessobrestado, base = part, variavel = "dt_dessobrestamento"),
        ultimo_dessobrestado = substr(dt_dessobrestamento, nchar(dt_dessobrestamento)-8, nchar(dt_dessobrestamento)),
        ultimo_dessobrestado = substr(ultimo_dessobrestado,1,8),
        ultimo_dessobrestado = if_else(ultimo_dessobrestado > max(data_dessobrestado), "0", ultimo_dessobrestado))
    




    dge <- part
    setDT(dge)
    dge <- dge %>% dtplyr::lazy_dt()  
    dge <- dge %>% 
        group_by(id_processo, numero_sigilo, sigla_grau, procedimento, ramo_justica, sigla_tribunal, id_tribunal, recurso, id_ultimo_oj) %>%
        summarise(
            dt_recebimento = min(dt_recebimento, na.rm = TRUE),
            data_total_primeira_baixa = min(data_total_primeira_baixa, na.rm = TRUE),
            dt_julgamento = min(dt_julgamento, na.rm = TRUE),
            dt_julgamento_homologatorio = min(dt_julgamento_homologatorio, na.rm = TRUE),
            data_total_primeiro_julgamento_sem_pronuncia = min(data_total_primeiro_julgamento_sem_pronuncia, na.rm = TRUE),
            dt_primeiro_julgamento_homologatorio = min(dt_primeiro_julgamento_homologatorio, na.rm = TRUE),
            data_total_primeiro_procedimento_resolvido = min(data_total_primeiro_procedimento_resolvido, na.rm = TRUE),
            dt_redistribuido_entrada = min(dt_redistribuido_entrada, na.rm = TRUE),
            dt_redistribuido_saida = min(dt_redistribuido_saida, na.rm = TRUE),
            dt_primeira_medida_protetiva_meta = min(dt_primeira_medida_protetiva_meta, na.rm = TRUE),
            #data_total_ultimo_pendente = max(data_total_ultimo_pendente, na.rm = TRUE),
            data_total_ultima_suspensao = max(data_total_ultima_suspensao, na.rm = TRUE),
            id_ultima_classe = min(id_ultima_classe, na.rm = TRUE),
            id_assunto = min(id_assunto, na.rm = TRUE),
            flag_violencia_domestica = max(flag_violencia_domestica, na.rm = TRUE),
            flag_feminicidio = max(flag_feminicidio, na.rm = TRUE),
            flag_ambiental = max(flag_ambiental, na.rm = TRUE),
            flag_quilombolas = max(flag_quilombolas, na.rm = TRUE),
            flag_indigenas = max(flag_indigenas, na.rm = TRUE),
            flag_infancia = max(flag_infancia, na.rm = TRUE),
            flag_rac_inj = max(flag_racismo, na.rm = TRUE),
            ultimo_pendente_liq = max(ultimo_pendente_liq, na.rm = TRUE),
            dt_dessobrestamento = max(dt_dessobrestamento, na.rm = TRUE),
            flg_dessobrestado = max(flg_dessobrestado, na.rm = TRUE),
            ultimo_dessobrestado = max(ultimo_dessobrestado, na.rm = TRUE),
            pronuncia = min(pronuncia, na.rm = TRUE),
            decisao = min(decisao, na.rm = TRUE),
            penultimo_pendente_liq = min(penultimo_pendente_liq, na.rm = TRUE),
            pendente_meta = max(pendente_ano_meta, na.rm = TRUE),
            sent_arq_des = max(sent_arq_des, na.rm = TRUE),
            ano_eleicao = max(ano_eleicao, na.rm = TRUE)
        ) %>% collect() %>% data.frame()
    
    dge <- dge %>% 
        mutate(
            dt_resolvido = pmin(
                data_total_primeiro_julgamento_sem_pronuncia, data_total_primeira_baixa, data_total_primeiro_procedimento_resolvido, dt_primeira_medida_protetiva_meta, na.rm = TRUE))
        
    #Flags metas temáticas
      
    dge <- dge %>% 
        mutate(
            flg_imp = func.procura.array(imp_classe, base = dge, variavel = "id_ultima_classe") | 
                func.procura.array(imp_assunto, base=dge, variavel = "id_assunto"),
            flg_crim_contr_adm_pbl = func.procura.array(adm_pub_assunto, base = dge, variavel = "id_assunto"),
            flg_ili_ele = func.procura.array(ili_ele_classe, base = dge, variavel = "id_ultima_classe") &
                func.procura.array(ili_ele_assunto, base = dge, variavel = "id_assunto"),
            flg_amb = func.procura.array(amb_assunto, base = dge, variavel = "id_assunto"),
            flg_ind = func.procura.array(ind_assunto, base = dge, variavel = "id_assunto"),
            flg_qui = func.procura.array(qui_assunto, base = dge, variavel = "id_assunto"),
            flg_vd = func.procura.array(vd_assunto, base = dge, variavel = "id_assunto"),
            flg_fem = func.procura.array(fem_assunto, base = dge, variavel = "id_assunto"),
            flg_seq = func.procura.array(seq_assunto, base = dge, variavel = "id_assunto"),
            flg_inf = func.procura.array(inf_assunto, base = dge, variavel = "id_assunto"),
            flg_rac_inj = func.procura.array(rac_inj_assunto, base = dge, variavel = "id_assunto"),

            mpu_meta = func.procura.array(mpu_meta_classe, base = dge, variavel = "id_ultima_classe"))
    
    return (list(dge=dge, pre_processual=pre_processual))
    
}