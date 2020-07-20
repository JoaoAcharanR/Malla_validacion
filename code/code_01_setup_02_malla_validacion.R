##### ANALYZE
#### Setup

source("code/code_01_setup_01_libraries.R")

#### Load raw data

data_enut = read_sav("raw/BASE_USUARIO_corregida.sav")
data_enut %>% names()
data_enut %>% View()

####ERRORES DE DATOS PERDIDOS####

data_perdidos <- data_enut     %>% select(id_vivienda,id_hogar,id_persona)
data_perdidos <- data_perdidos %>% add_column(ERRORMISsING_c12_1_1   = data_enut %>% transmute(ERRORMISsING_c12_1_1 = case_when(c12_1_1 = is.na(c12_1_1) ~ 1, TRUE ~ 0)))
data_perdidos <- data_perdidos %>% add_column(ERRORMISsING_c13_1_1   = data_enut %>% transmute(ERRORMISsING_c13_1_1 = case_when(c13_1_1 = is.na(c13_1_1) ~ 1, TRUE ~ 0)))
data_perdidos <- data_perdidos %>% add_column(ERRORMISsING_c14_1_1   = data_enut %>% transmute(ERRORMISsING_c14_1_1 = case_when(c14_1_1 = is.na(c14_1_1) ~ 1, TRUE ~ 0)))
data_perdidos <- data_perdidos %>% add_column(ERRORMISsING_c15_1_1   = data_enut %>% transmute(ERRORMISsING_c15_1_1 = case_when(c15_1_1 = is.na(c15_1_1) ~ 1, TRUE ~ 0)))

##REPORTE##
#resumen de errores#
reporte_errormissing <-  tibble(variable = data_perdidos %>% colnames)
reporte_errormissing <-  reporte_errormissing %>% mutate(errores = data_perdidos %>% colSums(na.rm = TRUE))
reporte_errormissing <-  reporte_errormissing %>% filter(variable != "id_vivienda" & variable != "id_hogar" & variable != "id_persona")

####ERRORES DE PROGRAMACION####

data_errorpro <- data_enut      %>% select(id_vivienda,id_hogar,id_persona)

##ERRORPRO_1: Responde que la semana pasada si trabajó personal servicio domestico y luego no responde cuantas personas
data_errorpro   <- data_errorpro %>% add_column(ERRORPRO_1  = data_enut %>% transmute(errorpro = case_when(f11_1_1 == 1 & is.na(f12_1_1) ~ 1, TRUE ~ 0)))
data_ERRORPRO_1 <- data_errorpro %>% filter(ERRORPRO_1 == 1)
data_ERRORPRO_1 <- left_join(data_ERRORPRO_1,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_ERRORPRO_1 <- data_ERRORPRO_1 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_1,f11_1_1,f12_1_1)
data_ERRORPRO_1

##ERRORPRO_2: Responde que la semana pasada NO trabajó personal servicio domestico y luego responde cuantas personas trabajaron
data_errorpro   <- data_errorpro %>% add_column(ERRORPRO_2  = data_enut %>% transmute(errorpro = case_when(f11_1_1 == 2 & !is.na(f12_1_1) ~ 1, TRUE ~ 0)))
data_ERRORPRO_2 <- data_errorpro %>% filter(ERRORPRO_2 == 1)
data_ERRORPRO_2 <- left_join(data_ERRORPRO_2,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_ERRORPRO_2 <- data_ERRORPRO_2 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_2,f11_1_1,f12_1_1)
data_ERRORPRO_2

##ERRORPRO_3: Responde que la semana pasada si trabajó personal servicio domestico y luego no responde sexo de personal domestico
data_errorpro   <- data_errorpro %>% add_column(ERRORPRO_3  = data_enut %>% transmute(errorpro = case_when(f11_1_1 == 1 & is.na(f13_1_1) ~ 1, TRUE ~ 0)))
data_ERRORPRO_3 <- data_errorpro %>% filter(ERRORPRO_3 == 1)
data_ERRORPRO_3 <- left_join(data_ERRORPRO_3,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_ERRORPRO_3 <- data_ERRORPRO_3 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_3,f11_1_1,f13_1_1)
data_ERRORPRO_3

##ERRORPRO_4: Responde que la semana pasada NO trabajó personal servicio domestico y luego responde sexo de personal domestico
data_errorpro   <- data_errorpro %>% add_column(ERRORPRO_4  = data_enut %>% transmute(errorpro = case_when(f11_1_1 == 2 & !is.na(f13_1_1) ~ 1, TRUE ~ 0)))
data_ERRORPRO_4 <- data_errorpro %>% filter(ERRORPRO_4 == 1)
data_ERRORPRO_4 <- left_join(data_ERRORPRO_4,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_ERRORPRO_4 <- data_ERRORPRO_4 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_4,f11_1_1,f13_1_1)
data_ERRORPRO_4

##REPORTE##
#resumen de errores#
reporte_errorpro <-  tibble(variable = data_errorpro %>% colnames)
reporte_errorpro <-  reporte_errorpro %>% mutate(errores = data_errorpro %>% colSums(na.rm = TRUE))
reporte_errorpro <-  reporte_errorpro %>% filter(variable != "id_vivienda" & variable != "id_hogar" & variable != "id_persona")

####ERRORES DE CONSISTENCIA####
data_errorcon <- data_enut      %>% select(id_vivienda,id_hogar,id_persona)

##ERRORCON_1: Responde que tiene estudios de magister o doctorado y edad inferior a 21 años
data_errorcon   <- data_errorcon %>% add_column(ERRORCON_1 = data_enut %>% transmute(errorcon = case_when(c14_1_1 <= 21 & (d12_1_1 == 14 | d12_1_1 == 15) ~ 1, TRUE ~ 0)))
data_errorcon_1 <- data_errorcon %>% filter(ERRORCON_1 == TRUE)
data_errorcon_1 <- left_join(data_errorcon_1,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_errorcon_1 <- data_errorcon_1 %>% select(id_vivienda,id_hogar,id_persona,ERRORCON_1,f11_1_1,f13_1_1)
data_errorcon_1

##ERRORCON_2: Errores de rango
data_errorcon <- data_errorcon %>% add_column(ERRORCON_2   = data_enut %>% transmute(errorcon = case_when(c14_1_1 = between(c14_1_1,0,99) ~ 0, TRUE ~ 1)))
data_errorcon_2 <- data_errorcon %>% filter(ERRORCON_2 == TRUE)
data_errorcon_2 <- left_join(data_errorcon_2,data_enut, by = c("id_vivienda", "id_hogar", "id_persona"))
data_errorcon_2 <- data_errorcon_2 %>% select(id_vivienda,id_hogar,id_persona,ERRORCON_2,c14_1_1)
data_errorcon_2

##REPORTE##
#resumen de errores#
reporte_errorcon <-  tibble(variable = data_errorcon %>% colnames)
reporte_errorcon <-  reporte_errorcon %>% mutate(errores = data_errorcon %>% colSums(na.rm = TRUE))
reporte_errorcon <-  reporte_errorcon %>% filter(variable != "id_vivienda" & variable != "id_hogar" & variable != "id_persona")
