get_inputs <- function(paper_input){
 
  ## -- obtain inputs
  business_input_id <- unlist(paper_input$`business-input-ids`)
  representative_input_id <- unlist(paper_input$`representative-input-ids`)
  manager_input_id <- unlist(paper_input$`manager-input-ids`)
  
  
  # - business input
  db_BusinessInput <- mongo(collection = "Businessinput",
                            db = options()$mongodb$db, 
                            url = options()$mongodb$host)
  business_input_id_array <- paste0('{"$oid":"', business_input_id, '"}')
  
  business_input <- db_BusinessInput$find(query = paste0('{"_id": {"$in": [',
                                                         paste(business_input_id_array, 
                                                               collapse = ","),
                                                         ']}}'))
  
  # - representative input
  db_RepresentativeInput <- mongo(collection = "Representativeinput",
                                  db = options()$mongodb$db, 
                                  url = options()$mongodb$host)
  representative_input_id_array <- paste0('{"$oid":"', representative_input_id, '"}')
  representative_input <- db_RepresentativeInput$find(query = paste0('{"_id": {"$in": [',
                                                                     paste(representative_input_id_array,
                                                                           collapse = ","),
                                                                     ']}}'))
  # - manager input
  db_manager_input <- mongo(collection = "Managerinput",
                            db = options()$mongodb$db, 
                            url = options()$mongodb$host)
  manager_input_id_array <- paste0('{"$oid":"', manager_input_id, '"}')
  manager_input <- db_manager_input$find(query = paste0('{"_id": {"$in": [',
                                                        paste(manager_input_id_array,
                                                              collapse = ","),
                                                        ']}}'))
  out <- list("business_input" = business_input,
              "representative_input" = representative_input,
              "manager_input" = manager_input)
  
  return(out)
}

get_id_mapping_table <- function(proposal_id) {
  
  db_scenario <- mongo(collection = "Scenario",
                       db = options()$mongodb$db, 
                       url = options()$mongodb$host)
  scenario_info <- db_scenario$find(query = paste0('{"proposal-id": "', 
                                                   proposal_id,
                                                   '"}'),
                                    field = '{}')
  scenario_id <- scenario_info$`_id`
  scenario_id_array <- paste0('"', scenario_id, '"')
  
  db_GoodsConfig <- mongo(collection = "GoodsConfig",
                          db = options()$mongodb$db, 
                          url = options()$mongodb$host)
  GoodsConfig_info <- db_GoodsConfig$find(query = paste0('{"scenario-id": {"$in": [',
                                                         paste(scenario_id_array,
                                                               collapse = ","),
                                                         ']}}'),
                                          field = '{}')
  GoodsConfig_id <- unique(GoodsConfig_info$`goods-id`)
  GoodsConfig_id_array <- paste0('{"$oid":"', GoodsConfig_id, '"}')
  
  db_ProductConfig <- mongo(collection = "ProductConfig",
                            db = options()$mongodb$db, 
                            url = options()$mongodb$host)
  productconfig_info <- db_ProductConfig$find(query = paste0('{"_id": {"$in": [',
                                                             paste(GoodsConfig_id_array,
                                                                   collapse = ","),
                                                             ']}}'),
                                              field = '{}')
  
  product_mapping_table <- productconfig_info %>%
    dplyr::rename(ProductConfig_id = `_id`) %>%
    select(ProductConfig_id, `product-id`) %>%
    left_join(GoodsConfig_info, by = c("ProductConfig_id" = "goods-id")) %>%
    dplyr::rename(GoodsConfig_id = `_id`) %>%
    select(GoodsConfig_id, `product-id`) %>%
    distinct()
  
  
  db_DestConfig <- mongo(collection = "DestConfig",
                         db = options()$mongodb$db, 
                         url = options()$mongodb$host)
  DestConfig_info <- db_DestConfig$find(query = paste0('{"scenario-id": {"$in": [',
                                                       paste(scenario_id_array,
                                                             collapse = ","),
                                                       ']}}'),
                                        field = '{}')
  
  dest_id <- unique(DestConfig_info$`dest-id`)
  dest_id_array <- paste0('{"$oid":"', dest_id, '"}')
  
  db_HospitalConfig <- mongo(collection = "HospitalConfig",
                             db = options()$mongodb$db, 
                             url = options()$mongodb$host)
  hospitalconfig_info <- db_HospitalConfig$find(query = paste0('{"_id": {"$in": [',
                                                               paste(dest_id_array,
                                                                     collapse = ","),
                                                               ']}}'),
                                                field = '{}')
  
  hospital_mapping_table <- hospitalconfig_info %>%
    rename(hospitalconfig_id = `_id`) %>%
    select(hospitalconfig_id, `hospital-id`) %>%
    left_join(DestConfig_info, by = c("hospitalconfig_id" = "dest-id")) %>%
    rename(destconfig_id = `_id`) %>%
    select(destconfig_id, `hospital-id`) %>%
    distinct()
  
  db_ResourceConfig <- mongo(collection = "ResourceConfig",
                             db = options()$mongodb$db, 
                             url = options()$mongodb$host)
  
  resourceconfig_info <- db_ResourceConfig$find(query = paste0('{"scenario-id": {"$in": [',
                                                               paste(scenario_id_array,
                                                                     collapse = ","),
                                                               ']}}'),
                                                field = '{}')
  resource_id <- resourceconfig_info$`resource-id`
  resource_id_array <- paste0('{"$oid":"', resource_id, '"}')
  
  db_RepresentativeConfig <- mongo(collection = "RepresentativeConfig",
                                   db = options()$mongodb$db, 
                                   url = options()$mongodb$host)
  representativeconfig_info <- db_RepresentativeConfig$find(query = paste0('{"_id": {"$in": [',
                                                                           paste(resource_id_array,
                                                                                 collapse = ","),
                                                                           ']}}'),
                                                            field = '{}')
  representative_mapping_table <- representativeconfig_info %>%
    rename(representativeconfig_id = `_id`) %>%
    select(representativeconfig_id, `representative-id`) %>%
    left_join(resourceconfig_info, by = c("representativeconfig_id" = "resource-id")) %>%
    rename(resourceconfig_id = `_id`) %>%
    select(resourceconfig_id, `representative-id`)
  
  out <- list("hosp_mappping" = hospital_mapping_table,
              "prod_mappipng" = product_mapping_table,
              "rep_mappings" = representative_mapping_table)
  return(out)
}

get_sales_bakground <- function(paper_info,
                                current_phase,
                                hospital_mapping_table,
                                product_mapping_table,
                                representative_mapping_table){
  
  sales_report_id <- tail(unlist(paper_info$`sales-report-ids`), n = 2)
  cp_sales_report_id <- sales_report_id[2]
  
  if (current_phase == 1) {
    
    pp_sales_report_id <- sales_report_id[1]
  } else {
    pp_sales_report_id <- unlist(paper_info$`sales-report-ids`)[4]
  }
  
  db_sales_report <- mongo(collection = "SalesReport",
                           db = options()$mongodb$db, 
                           url = options()$mongodb$host)
  # - pp report
  pp_sales_report_info <- db_sales_report$find(query = paste0('{"_id": {"$oid":"', 
                                                              pp_sales_report_id,
                                                              '"}}'))
  pp_hospital_sales_report_id <- unlist(pp_sales_report_info$`hospital-sales-report-ids`)
  
  db_HospitalSalesReport <- mongo(collection = "HospitalSalesReport",
                                  db = options()$mongodb$db, 
                                  url = options()$mongodb$host)
  
  pp_hospital_sales_report_id_array <- paste0('{"$oid":"', pp_hospital_sales_report_id, '"}')
  pp_hospital_sales_report <- db_HospitalSalesReport$find(query = paste0('{"_id": {"$in": [',
                                                                         paste(pp_hospital_sales_report_id_array,
                                                                               collapse = ","),
                                                                         ']}}'))
  pp_hospital_sales_report$phase <- "pp"
  
  cp_sales_report_info <- db_sales_report$find(query = paste0('{"_id": {"$oid":"', 
                                                              cp_sales_report_id,
                                                              '"}}'))
  cp_hospital_sales_report_id <- unlist(cp_sales_report_info$`hospital-sales-report-ids`)
  
  cp_hospital_sales_report_id_array <- paste0('{"$oid":"', cp_hospital_sales_report_id, '"}')
  cp_hospital_sales_report <- db_HospitalSalesReport$find(query = paste0('{"_id": {"$in": [',
                                                                         paste(cp_hospital_sales_report_id_array,
                                                                               collapse = ","),
                                                                         ']}}'))
  cp_hospital_sales_report$phase <- "cp"
  
  hospital_sales_report <- bind_rows(cp_hospital_sales_report,
                                     pp_hospital_sales_report) %>%
    left_join(hospital_mapping_table, by = c("dest-config-id" = "destconfig_id")) %>%
    left_join(product_mapping_table, by = c("goods-config-id" = "GoodsConfig_id")) %>%
    left_join(representative_mapping_table, by = c("resource-config-id" = "resourceconfig_id")) %>%
    select(- `dest-config-id`, -`goods-config-id`, -`resource-config-id`) %>%
    gather(metric, value, -c("hospital-id", 
                             "product-id",
                             "potential", "phase")) %>%
    dcast(formula = `hospital-id` + `product-id` +
            potential ~ phase + metric,
          value.var = "value")
  
  return(hospital_sales_report)
}

get_rep_bakground <- function(paper_info,
                              current_phase){
  
  personnel_assessment_id <- tail(unlist(paper_info$`personnel-assessment-ids`), n = 2)
  cp_personnel_assessment_id<- personnel_assessment_id[2]
  
  if (current_phase == 1) {
    
    pp_personnel_assessment_id <- personnel_assessment_id[1]
  } else {
    pp_personnel_assessment_id <- unlist(paper_info$`personnel-assessment-ids`)[1]
  }
  
  db_PersonnelAssessment <- mongo(collection = "PersonnelAssessment",
                                  db = options()$mongodb$db, 
                                  url = options()$mongodb$host)
  # cp
  cp_personnel_assess_info <- db_PersonnelAssessment$find(query = paste0('{"_id": {"$oid":"', 
                                                                         cp_personnel_assessment_id,
                                                                         '"}}'))
  cp_representative_ability_id <- unlist(cp_personnel_assess_info$`representative-ability-ids`)
  
  db_RepresentativeAbility <- mongo(collection = "RepresentativeAbility",
                                    db = options()$mongodb$db, 
                                    url = options()$mongodb$host)
  cp_representative_ability_id_array <- paste0('{"$oid":"', cp_representative_ability_id, '"}')
  
  cp_representative_ability_info <- db_RepresentativeAbility$find(query = paste0('{"_id": {"$in": [',
                                                                                 paste(cp_representative_ability_id_array,
                                                                                       collapse = ","),
                                                                                 ']}}'))
  cp_representative_ability_info$phase <- "cp"
  # pp
  pp_personnel_assess_info <- db_PersonnelAssessment$find(query = paste0('{"_id": {"$oid":"', 
                                                                         pp_personnel_assessment_id,
                                                                         '"}}'))
  pp_representative_ability_id <- unlist(pp_personnel_assess_info$`representative-ability-ids`)
  
  pp_representative_ability_id_array <- paste0('{"$oid":"', pp_representative_ability_id, '"}')
  
  pp_representative_ability_info <- db_RepresentativeAbility$find(query = paste0('{"_id": {"$in": [',
                                                                                 paste(pp_representative_ability_id_array,
                                                                                       collapse = ","),
                                                                                 ']}}'))
  pp_representative_ability_info$phase <- "pp"

  out <- list("cp_representative_ability_info" = cp_representative_ability_info,
              "pp_representative_ability_info" = pp_representative_ability_info)
  
  return(out)
}


get_eva_scores <- function(business_input,
                           representative_input,
                           manager_input,
                           hospital_mapping_table,
                           product_mapping_table,
                           representative_mapping_table,
                           hospital_sales_report,
                           cp_representative_ability_info,
                           pp_representative_ability_info,
                           representative_ability_report){
  
  ## -- 区域划分
  # - 区域潜力平衡
  ab1_tmp <- hospital_sales_report %>%
    group_by(`cp_representative-id`) %>%
    summarise(potential = sum(potential, na.rm = T),
              pp_sales = sum(pp_sales, na.rm = T)) %>%
    mutate(potential_as = sd(potential)/mean(potential),
           pp_sales_as = sd(pp_sales)/mean(pp_sales, na.rm = T))
  
  ab1_score <- 0.6*unique(ab1_tmp$potential_as)+ 0.4*unique(ab1_tmp$pp_sales_as)
  
  ## -- 指标分配
  # - 3:7原则 潜力与历史销售额
  ab2_tmp <- hospital_sales_report %>%
    mutate(potential_contri = potential/sum(potential, na.rm = T),
           pp_sales_contri = pp_sales/sum(pp_sales, na.rm = T),
           quota_contri = 0.3*potential_contri +0.7*pp_sales_contri,
           user_quota_contri = `cp_sales-quota`/sum(`cp_sales-quota`, na.rm = T),
           chk_diff = abs(quota_contri- user_quota_contri)*100)
  
  ab2_1 <- sd(ab2_tmp$chk_diff)
  
  # - 历史增长率
  ab2_tmp2 <- hospital_sales_report %>%
    mutate(quota_growth = `cp_sales-quota`/pp_sales,
           pp_growth_rank = dense_rank(-`pp_sales-growth`),
           user_quota_growth_rank = dense_rank(-quota_growth),
           chk_diff = abs(pp_growth_rank- user_quota_growth_rank)/pp_growth_rank) 
  
  ab2_2 <- sum(ab2_tmp2$chk_diff)
  
  ab2_score <- 0.7*ab2_1 +0.3*ab2_2
  
  ## -- 资源优化分配
  ab3_tmp <- sum(hospital_sales_report$cp_sales)/sum(hospital_sales_report$`cp_sales-quota`)
  
  ab3_score <- ab3_tmp
  
  
  ## -- 自我时间管理
  standard_manage_time <- data.frame(
    "client-management-time" = c(8, 0.2),
    "kpi-analysis-time" = c(8, 0.1),
    "team-meeting-time" = c(6, 0.1),
    "assist-access-time" = c(34, 0.2),
    "ability-coach" = c(30, 0.2),
    "strategy-analysis-time" = c(8, 0.1),
    "admin-work-time" = c(6, 0.1),
    "type" = c("std", "wgt"),
    stringsAsFactors = F)
  
  ab4_tmp1 <- manager_input %>%
    mutate(type = "user") %>%
    select(client.management.time = `client-management-time`, 
           kpi.analysis.time = `kpi-analysis-time`,
           team.meeting.time = `team-meeting-time`,
           strategy.analysis.time = `strategy-analysis-time`,
           admin.work.time = `admin-work-time`) %>%
    mutate("assist.access.time" = sum(representative_input$`assist-access-time`, na.rm = T),
           "ability.coach" = sum(representative_input$`ability-coach`, na.rm = T),
           type = "user") %>%
    bind_rows(.,
              standard_manage_time) %>%
    gather(variables, value, - type) %>%
    spread(type, value) %>%
    mutate(chk_diff = abs(user-std)/std*wgt)
  
  representative_input_m <- representative_input %>%
    left_join(representative_mapping_table, by = c("resource-config-id" = "resourceconfig_id")) 
  
  ab4_tmp2 <- hospital_sales_report %>%
    group_by(`cp_representative-id`) %>%
    summarise(`cp_sales-quota` = sum(`cp_sales-quota`, na.rm = T)) %>%
    left_join(representative_mapping_table, by = c("cp_representative-id" = "representative-id")) %>%
    rename(`representative-id` = `cp_representative-id`) %>%
    left_join(representative_input_m, by = c("representative-id")) %>%
    select(- resourceconfig_id) %>%
    distinct() %>%
    left_join(pp_representative_ability_info,
              by = "representative-id") %>%
    mutate(`assist-access-time-prop` = ifelse(sum(`assist-access-time`, na.rm = T) == 0,
                                              0,
                                              `assist-access-time`/sum(`assist-access-time`, na.rm = T)),
           `cp_sales-quota-prop` =  ifelse(sum(`cp_sales-quota`, na.rm = T) == 0,
                                           0,
                                           `cp_sales-quota`/sum(`cp_sales-quota`, na.rm = T)),
           user_rank = dense_rank(-`assist-access-time-prop`),
           std_rank = dense_rank(-`cp_sales-quota-prop`),
           chk_diff = abs(user_rank - std_rank)/std_rank)
  
  ab4_score <- 0.4*sum(ab4_tmp1$chk_diff)+ 0.6*sum(ab4_tmp2$chk_diff)
  
  
  ## -- 人才培养
  ab5_tmp1 <- representative_ability_report %>%
    group_by(phase) %>%
    summarise_if(is.numeric, c("mean", "var")) %>%
    gather(variable, value, -phase) %>%
    separate(variable, into = c("variables", "index"), sep = "_") %>%
    dcast(variables~ phase+index) %>%
    mutate(chk_mean = abs(cp_mean - pp_mean)/pp_mean,
           chk_var = abs(cp_var - pp_var)/pp_var)
  
  ab5_1 <- sum(ab5_tmp1$chk_mean)
  ab5_2 <- sum(ab5_tmp1$chk_var)
  ab5_score <- 0.5* ab5_1 + 0.5*ab5_2
  
  
  ## -- aggregation
  overall_score <- data.frame(
    ab1 = ab1_score,
    ab2 = ab2_score,
    ab3 = ab3_score,
    ab4 = ab4_score,
    ab5 = ab5_score,
    stringsAsFactors = F
  ) %>%
    gather(variables, user)
  
  return(overall_score)
}

get_eva_bak_info <- function(overall_score,
                             ab_dimen_mapping) {
  
  db_intermedia <- mongo(collection = "Intermedia",
                         db = options()$mongodb$db, 
                         url = options()$mongodb$host)
  
  eva_table <- db_intermedia$find(query = '{"type":"evaluation"}')
  eva_table_m <- eva_table$`eva_table`[[1]] %>%
    left_join(overall_score, by = "variables") %>%
    mutate(score = ifelse(user < level1,
                          "B", ifelse( user < level2,
                                       "A",
                                       "S"))) %>%
    mutate(score_m = ifelse(user < level1,
                          1, ifelse( user < level2,
                                       2,
                                       3)))
  general_score <- mean(eva_table_m$score_m, na.rm = T)
  general_per <- data.frame(variables = "general",
                            user = general_score,
                            score = ifelse(general_score < 1.5,
                                           "B",
                                           ifelse(general_score < 2.5,
                                                  "A",
                                                  "S")),
                            stringsAsFactors = F)
  eva_table_m <- bind_rows(eva_table_m,
                           general_per) %>%
    left_join(ab_dimen_mapping, by = "variables")
  
  db_level <- mongo(collection = "Level",
                    db = options()$mongodb$db, 
                    url = options()$mongodb$host)
  
  level_info <- db_level$find(field = '{}') %>%
    select(`level-id` = `_id`, level)
  
  db_levelconfig <- mongo(collection = "LevelConfig",
                          db = options()$mongodb$db, 
                          url = options()$mongodb$host)
  
  levelconfig_info <- db_levelconfig$find(field = '{"_id": 1, "level-id":1, "code" :1}') %>%
    rename("level-config-id" = `_id`) %>%
    left_join(level_info, by = "level-id")
  
  db_AssessmentReportDescribe <- mongo(collection = "AssessmentReportDescribe",
                                       db = options()$mongodb$db, 
                                       url = options()$mongodb$host)
  assessmentReportDescribe_info <- db_AssessmentReportDescribe$find(field = '{"_id":1, "code":1}') %>%
    rename("assessment-report-describe" = `_id`)
  
  
  eva_table_m1 <- eva_table_m %>%
    left_join(levelconfig_info, by = c("score" = "level",
                                       "code")) %>%
    left_join(assessmentReportDescribe_info, by = "code")
  
  ## -- 1
  db_RegionalDivisionResult <- mongo(collection = "RegionalDivisionResult",
                                     db = options()$mongodb$db, 
                                     url = options()$mongodb$host)
  tmp1 <- eva_table_m1 %>%
    filter(variables == 'ab1')
  
  db_RegionalDivisionResult$insert(list("level-config-id" = unique(tmp1$`level-config-id`),
                                        "assessment-report-describe-ids" = tmp1$`assessment-report-describe`),
                                   na = "string", 
                                   auto_unbox = TRUE)
  tmp1_1 <- tail(unlist(db_RegionalDivisionResult$find(fields = '{"_id":1}')), n = 1)
  
  
  ## -- 2
  db_TargetAssignsResult <- mongo(collection = "TargetAssignsResult",
                                  db = options()$mongodb$db, 
                                  url = options()$mongodb$host)
  tmp2 <- eva_table_m1 %>%
    filter(variables == 'ab2')
  
  db_TargetAssignsResult$insert(list("level-config-id" = unique(tmp2$`level-config-id`),
                                     "assessment-report-describe-ids" = tmp2$`assessment-report-describe`),
                                na = "string", 
                                auto_unbox = TRUE)
  
  tmp2_1 <- tail(unlist(db_TargetAssignsResult$find(field = '{"_id" :1}')), n = 1)
  
  ## -- 3
  db_ResourceAssignsResult <- mongo(collection = "ResourceAssignsResult",
                                    db = options()$mongodb$db, 
                                    url = options()$mongodb$host)
  tmp3 <- eva_table_m1 %>%
    filter(variables == 'ab3')
  
  db_ResourceAssignsResult$insert(list("level-config-id" = unique(tmp3$`level-config-id`),
                                       "assessment-report-describe-ids" = tmp3$`assessment-report-describe`),
                                  na = "string", 
                                  auto_unbox = TRUE)
  
  tmp3_1 <- tail(unlist(db_ResourceAssignsResult$find(field = '{"_id" :1}')), n = 1)
  
  ## -- 4
  db_ManageTimeResult <- mongo(collection = "ManageTimeResult",
                               db = options()$mongodb$db, 
                               url = options()$mongodb$host)
  tmp4 <- eva_table_m1 %>%
    filter(variables == 'ab4')
  
  db_ManageTimeResult$insert(list("level-config-id" = unique(tmp4$`level-config-id`),
                                  "assessment-report-describe-ids" = tmp4$`assessment-report-describe`),
                             na = "string", 
                             auto_unbox = TRUE)
  
  tmp4_1 <- tail(unlist(db_ManageTimeResult$find(field = '{"_id" :1}')), n = 1)
  
  ## -- 5
  db_ManageTeamResult <- mongo(collection = "ManageTeamResult",
                               db = options()$mongodb$db, 
                               url = options()$mongodb$host)
  tmp5 <- eva_table_m1 %>%
    filter(variables == 'ab5')
  
  db_ManageTeamResult$insert(list("level-config-id" = unique(tmp5$`level-config-id`),
                                  "assessment-report-describe-ids" = tmp5$`assessment-report-describe`),
                             na = "string", 
                             auto_unbox = TRUE)
  
  tmp5_1 <- tail(unlist(db_ManageTeamResult$find(field = '{"_id" :1}')), n = 1)
  
  ## -- general performance
  db_GeneralPerformanceResult <- mongo(collection = "GeneralPerformanceResult",
                                       db = options()$mongodb$db, 
                                       url = options()$mongodb$host)
  tmp6 <- eva_table_m1 %>%
    filter(variables == 'general')
  
  db_GeneralPerformanceResult$insert(list("level-config-id" = unique(tmp6$`level-config-id`),
                                  "assessment-report-describe-ids" = NULL),
                             na = "string", 
                             auto_unbox = TRUE)
  
  tmp6_1 <- tail(unlist(db_GeneralPerformanceResult$find(field = '{"_id" :1}')), n = 1)
  
  ## -- overall
  db_AssessmentReport <- mongo(collection = "AssessmentReport",
                               db = options()$mongodb$db, 
                               url = options()$mongodb$host)
  db_AssessmentReport$insert(list("regional-division-result-id" = tmp1_1,
                                  "target-assigns-result-id" = tmp2_1,
                                  "resource-assigns-result-id" = tmp3_1,
                                  "manage-time-result-id" = tmp4_1,
                                  "manage-team-result-id" = tmp5_1,
                                  "general-performance-id" = tmp6_1,
                                  "scenario-id" = paper_input$`scenario-id`,
                                  "paper-input-id" = input_id,
                                  "time" = round(as.numeric(Sys.time())*1000)),
                             na = "string", 
                             auto_unbox = TRUE)
  
  assessment_report_id <- tail(unlist(db_AssessmentReport$find(field = '{"_id" :1}')), n = 1)
  
  return(assessment_report_id)
}
