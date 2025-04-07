mplusdata <- cognitiondata %>% 
  dplyr::filter(Year < 2021) %>% 
  dplyr::left_join(dem_sr, by = c("HRS_ID", "Year")) %>% 
  dplyr::filter(!HRS_ID %in% id_convert$HRS_ID) %>%  
  left_join(demos, by = "HRS_ID") %>% 
  tidyr::unite(SECU_R, c("SECU", "STRATUM"), sep = ".", remove = FALSE) %>%
  dplyr::mutate(nhw = dplyr::case_when(vdhisp == 1 ~ 0,
                                       vdothrac == 1 ~ 0,
                                       vdblack == 1 ~ 0,
                                       TRUE ~ 1),
                nhwc = nhw - mean(nhw, na.rm = T),
                femc = vdfem - mean(vdfem, na.rm = T),
                hispc = vdhisp - mean(vdhisp, na.rm = T),
                othc = vdothrac - mean(vdothrac, na.rm = T),
                blkc = vdblack - mean(vdblack, na.rm = T),
                educ = vdedu - mean(vdedu, na.rm = T),
                agec = vdage - mean(vdage, na.rm = T)) %>% 
  dplyr::select(mplusid, DR, femc, nhwc, educ, vdage, yrsinstudy,
                person0webphone1, FWGTR, SECU_R, STRATUM) %>% 
  dplyr::rename(yrsfu = yrsinstudy,
                mode = person0webphone1) %>% 
  dplyr::mutate(yrsd = yrsfu/10) %>% 
  dplyr::select(-yrsfu) %>% 
  tidyr::pivot_wider(names_from = yrsd, values_from = c(DR, mode)) %>% 
  dplyr::rename(y0 = DR_0,
                y2 = DR_0.2,
                y4 = DR_0.4,
                y6 = DR_0.6,
                y8 = DR_0.8,
                y10 = DR_1,
                y12 = DR_1.2,
                y14 = DR_1.4,
                y16 = DR_1.6,
                y18 = DR_1.8,
                y20 = DR_2,
                y22 = DR_2.2,
                x0 = mode_0,
                x2 = mode_0.2,
                x4 = mode_0.4,
                x6 = mode_0.6,
                x8 = mode_0.8,
                x10 = mode_1,
                x12 = mode_1.2,
                x14 = mode_1.4,
                x16 = mode_1.6,
                x18 = mode_1.8,
                x20 = mode_2,
                x22 = mode_2.2) %>% 
  dplyr::mutate(x0 = dplyr::if_else(is.na(x0), 0, x0),
                x2 = dplyr::if_else(is.na(x2), 0, x2),
                x4 = dplyr::if_else(is.na(x4), 0, x4),
                x6 = dplyr::if_else(is.na(x6), 0, x6),
                x8 = dplyr::if_else(is.na(x8), 0, x8),
                x10 = dplyr::if_else(is.na(x10), 0, x10),
                x12 = dplyr::if_else(is.na(x12), 0, x12),
                x14 = dplyr::if_else(is.na(x14), 0, x14),
                x16 = dplyr::if_else(is.na(x16), 0, x16),
                x18 = dplyr::if_else(is.na(x18), 0, x18),
                x20 = dplyr::if_else(is.na(x20), 0, x20),
                x22 = dplyr::if_else(is.na(x22), 0, x22)) %>% 
  labelled::remove_labels() %>% 
  dplyr::mutate(y0 = dplyr::if_else(y0 > 7, 7, y0),
                y2 = dplyr::if_else(y2 > 7, 7, y2),
                y4 = dplyr::if_else(y4 > 7, 7, y4),
                y6 = dplyr::if_else(y6 > 7, 7, y6),
                y8 = dplyr::if_else(y8 > 7, 7, y8),
                y10 = dplyr::if_else(y10 > 7, 7, y10),
                y12 = dplyr::if_else(y12 > 7, 7, y12),
                y14 = dplyr::if_else(y14 > 7, 7, y14),
                y16 = dplyr::if_else(y16 > 7, 7, y16),
                y18 = dplyr::if_else(y18 > 7, 7, y18),
                y20 = dplyr::if_else(y20 > 7, 7, y20),
                y22 = dplyr::if_else(y22 > 7, 7, y22)) %>% 
  dplyr::mutate(baseagec = (vdage - 70) / 10) %>% 
  dplyr::select(-vdage)


fs::dir_create(Mplus_path, "180_no_dem")
setwd(here::here(Mplus_path, "180_no_dem"))


longcog_modelwt <- mplusObject(
  MODEL = "
           i s q | y0@0 y2@.2 y4@.4 y6@.6 y8@.8 y10@1;
           i s q | y12@1.2 y14@1.4 y16@1.6 y18@1.8 y20@2 y22@2.2;
           
           [s] (ms);
           [q] (mq);
           
           ! time-varying mode effect
           y0 on x0 (mode);
           y2 on x2 (mode);
           y4 on x4 (mode);
           y6 on x6 (mode);
           y8 on x8 (mode);
           y10 on x10 (mode);
           y12 on x12 (mode);
           y14 on x14 (mode);
           y16 on x16 (mode);
           y18 on x18 (mode);
           y20 on x20 (mode);
           y22 on x22 (mode);
           
           ! covariates
            i ON femc nhwc educ baseagec (i1-i4);
            s ON femc nhwc educ baseagec (s1-s4);
            q ON femc nhwc educ baseagec (q1-q4);
           
           
           !fix thresholds and mean intercept
           [i@0];
           
     [ y0$1*-3.49185 ] (18);
     [ y0$2*-2.83233 ] (19);
     [ y0$3*-1.95847 ] (20);
     [ y0$4*-0.77504 ] (21);
     [ y0$5*0.56545 ] (22);
     [ y0$6*2.01021 ] (23);
     [ y0$7*3.36149 ] (24);
     [ y2$1*-3.49185 ] (18);
     [ y2$2*-2.83233 ] (19);
     [ y2$3*-1.95847 ] (20);
     [ y2$4*-0.77504 ] (21);
     [ y2$5*0.56545 ] (22);
     [ y2$6*2.01021 ] (23);
     [ y2$7*3.36149 ] (24);
     [ y4$1*-3.49185 ] (18);
     [ y4$2*-2.83233 ] (19);
     [ y4$3*-1.95847 ] (20);
     [ y4$4*-0.77504 ] (21);
     [ y4$5*0.56545 ] (22);
     [ y4$6*2.01021 ] (23);
     [ y4$7*3.36149 ] (24);
     [ y6$1*-3.49185 ] (18);
     [ y6$2*-2.83233 ] (19);
     [ y6$3*-1.95847 ] (20);
     [ y6$4*-0.77504 ] (21);
     [ y6$5*0.56545 ] (22);
     [ y6$6*2.01021 ] (23);
     [ y6$7*3.36149 ] (24);
     [ y8$1*-3.49185 ] (18);
     [ y8$2*-2.83233 ] (19);
     [ y8$3*-1.95847 ] (20);
     [ y8$4*-0.77504 ] (21);
     [ y8$5*0.56545 ] (22);
     [ y8$6*2.01021 ] (23);
     [ y8$7*3.36149 ] (24);
     [ y10$1*-3.49185 ] (18);
     [ y10$2*-2.83233 ] (19);
     [ y10$3*-1.95847 ] (20);
     [ y10$4*-0.77504 ] (21);
     [ y10$5*0.56545 ] (22);
     [ y10$6*2.01021 ] (23);
     [ y10$7*3.36149 ] (24);
     [ y12$1*-3.49185 ] (18);
     [ y12$2*-2.83233 ] (19);
     [ y12$3*-1.95847 ] (20);
     [ y12$4*-0.77504 ] (21);
     [ y12$5*0.56545 ] (22);
     [ y12$6*2.01021 ] (23);
     [ y12$7*3.36149 ] (24);
     [ y14$1*-3.49185 ] (18);
     [ y14$2*-2.83233 ] (19);
     [ y14$3*-1.95847 ] (20);
     [ y14$4*-0.77504 ] (21);
     [ y14$5*0.56545 ] (22);
     [ y14$6*2.01021 ] (23);
     [ y14$7*3.36149 ] (24);
     [ y16$1*-3.49185 ] (18);
     [ y16$2*-2.83233 ] (19);
     [ y16$3*-1.95847 ] (20);
     [ y16$4*-0.77504 ] (21);
     [ y16$5*0.56545 ] (22);
     [ y16$6*2.01021 ] (23);
     [ y16$7*3.36149 ] (24);
     [ y18$1*-3.49185 ] (18);
     [ y18$2*-2.83233 ] (19);
     [ y18$3*-1.95847 ] (20);
     [ y18$4*-0.77504 ] (21);
     [ y18$5*0.56545 ] (22);
     [ y18$6*2.01021 ] (23);
     [ y18$7*3.36149 ] (24);
     [ y20$1*-3.49185 ] (18);
     [ y20$2*-2.83233 ] (19);
     [ y20$3*-1.95847 ] (20);
     [ y20$4*-0.77504 ] (21);
     [ y20$5*0.56545 ] (22);
     [ y20$6*2.01021 ] (23);
     [ y20$7*3.36149 ] (24);
     [ y22$1*-3.49185 ] (18);
     [ y22$2*-2.83233 ] (19);
     [ y22$3*-1.95847 ] (20);
     [ y22$4*-0.77504 ] (21);
     [ y22$5*0.56545 ] (22);
     [ y22$6*2.01021 ] (23);
     [ y22$7*3.36149 ] (24); ",
  MODELCONSTRAINT = "
  new(fem75 fem85 fem95
      nhw75 nhw85 nhw95
      edu75 edu85 edu95
      age75 age85 age95);
  fem75 = s1 + q1;
  fem85 = s1 + 3*q1;
  fem95 = s1 + 5*q1;
  
  nhw75 = s2 + q2;
  nhw85 = s2 + 3*q2;
  nhw95 = s2 + 5*q2;
  
  edu75 = s3 + q3;
  edu85 = s3 + 3*q3;
  edu95 = s3 + 5*q3;
  
  age75 = s4 + q4;
  age85 = s4 + 3*q4;
  age95 = s4 + 5*q4;
  ",
  ANALYSIS = "ESTIMATOR = MLR;
              TYPE = COMPLEX;
              COVERAGE = .071;
              !PARAMETERIZATION = THETA;",
  VARIABLE = "IDVARIABLE = mplusid;
              WEIGHT = FWGTR;
              CLUSTER = SECU_R;
              STRATIFICATION = STRATUM;
              CATEGORICAL = y0-y22;",
  OUTPUT = "TECH4; TECH10; SVALUES; STDYX; CINTERVAL; RESIDUAL;",
  usevariables = colnames(mplusdata),
  rdata = mplusdata
)

dr_longwt <- mplusModeler(longcog_modelwt, modelout = "quad_time_metric.inp", run = TRUE)


longcog_modelwt <- mplusObject(
  MODEL = "
           i s | y0@0 y2@.2 y4@.4 y6@.6 y8@.8 y10@1;
           i s | y12@1.2 y14@1.4 y16@1.6 y18@1.8 y20@2 y22@2.2;
           
           [s] (ms);

           
           ! time-varying mode effect
           y0 on x0 (mode);
           y2 on x2 (mode);
           y4 on x4 (mode);
           y6 on x6 (mode);
           y8 on x8 (mode);
           y10 on x10 (mode);
           y12 on x12 (mode);
           y14 on x14 (mode);
           y16 on x16 (mode);
           y18 on x18 (mode);
           y20 on x20 (mode);
           y22 on x22 (mode);
           
           ! covariates
           i s ON femc nhwc educ baseagec;
           
           
           !fix thresholds and mean intercept
           [i@0];
           
     [ y0$1*-3.49185 ] (18);
     [ y0$2*-2.83233 ] (19);
     [ y0$3*-1.95847 ] (20);
     [ y0$4*-0.77504 ] (21);
     [ y0$5*0.56545 ] (22);
     [ y0$6*2.01021 ] (23);
     [ y0$7*3.36149 ] (24);
     [ y2$1*-3.49185 ] (18);
     [ y2$2*-2.83233 ] (19);
     [ y2$3*-1.95847 ] (20);
     [ y2$4*-0.77504 ] (21);
     [ y2$5*0.56545 ] (22);
     [ y2$6*2.01021 ] (23);
     [ y2$7*3.36149 ] (24);
     [ y4$1*-3.49185 ] (18);
     [ y4$2*-2.83233 ] (19);
     [ y4$3*-1.95847 ] (20);
     [ y4$4*-0.77504 ] (21);
     [ y4$5*0.56545 ] (22);
     [ y4$6*2.01021 ] (23);
     [ y4$7*3.36149 ] (24);
     [ y6$1*-3.49185 ] (18);
     [ y6$2*-2.83233 ] (19);
     [ y6$3*-1.95847 ] (20);
     [ y6$4*-0.77504 ] (21);
     [ y6$5*0.56545 ] (22);
     [ y6$6*2.01021 ] (23);
     [ y6$7*3.36149 ] (24);
     [ y8$1*-3.49185 ] (18);
     [ y8$2*-2.83233 ] (19);
     [ y8$3*-1.95847 ] (20);
     [ y8$4*-0.77504 ] (21);
     [ y8$5*0.56545 ] (22);
     [ y8$6*2.01021 ] (23);
     [ y8$7*3.36149 ] (24);
     [ y10$1*-3.49185 ] (18);
     [ y10$2*-2.83233 ] (19);
     [ y10$3*-1.95847 ] (20);
     [ y10$4*-0.77504 ] (21);
     [ y10$5*0.56545 ] (22);
     [ y10$6*2.01021 ] (23);
     [ y10$7*3.36149 ] (24);
     [ y12$1*-3.49185 ] (18);
     [ y12$2*-2.83233 ] (19);
     [ y12$3*-1.95847 ] (20);
     [ y12$4*-0.77504 ] (21);
     [ y12$5*0.56545 ] (22);
     [ y12$6*2.01021 ] (23);
     [ y12$7*3.36149 ] (24);
     [ y14$1*-3.49185 ] (18);
     [ y14$2*-2.83233 ] (19);
     [ y14$3*-1.95847 ] (20);
     [ y14$4*-0.77504 ] (21);
     [ y14$5*0.56545 ] (22);
     [ y14$6*2.01021 ] (23);
     [ y14$7*3.36149 ] (24);
     [ y16$1*-3.49185 ] (18);
     [ y16$2*-2.83233 ] (19);
     [ y16$3*-1.95847 ] (20);
     [ y16$4*-0.77504 ] (21);
     [ y16$5*0.56545 ] (22);
     [ y16$6*2.01021 ] (23);
     [ y16$7*3.36149 ] (24);
     [ y18$1*-3.49185 ] (18);
     [ y18$2*-2.83233 ] (19);
     [ y18$3*-1.95847 ] (20);
     [ y18$4*-0.77504 ] (21);
     [ y18$5*0.56545 ] (22);
     [ y18$6*2.01021 ] (23);
     [ y18$7*3.36149 ] (24);
     [ y20$1*-3.49185 ] (18);
     [ y20$2*-2.83233 ] (19);
     [ y20$3*-1.95847 ] (20);
     [ y20$4*-0.77504 ] (21);
     [ y20$5*0.56545 ] (22);
     [ y20$6*2.01021 ] (23);
     [ y20$7*3.36149 ] (24);
     [ y22$1*-3.49185 ] (18);
     [ y22$2*-2.83233 ] (19);
     [ y22$3*-1.95847 ] (20);
     [ y22$4*-0.77504 ] (21);
     [ y22$5*0.56545 ] (22);
     [ y22$6*2.01021 ] (23);
     [ y22$7*3.36149 ] (24); ",
     MODELCONSTRAINT = "new(sdec);
    sdec = ms / 2.14;",
  ANALYSIS = "ESTIMATOR = MLR;
              TYPE = COMPLEX;
              COVERAGE = .071;
              !PARAMETERIZATION = THETA;",
  VARIABLE = "IDVARIABLE = mplusid;
              WEIGHT = FWGTR;
              CLUSTER = SECU_R;
              STRATIFICATION = STRATUM;
              CATEGORICAL = y0-y22;",
  OUTPUT = "TECH4; TECH10; SVALUES; STDYX; CINTERVAL; RESIDUAL;",
  usevariables = colnames(mplusdata),
  rdata = mplusdata
)

dr_longwt <- mplusModeler(longcog_modelwt, modelout = "time_metric.inp", run = TRUE)

