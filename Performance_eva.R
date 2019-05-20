#' @get /ntm/<proposal_id>/<account_id>

function(proposal_id, account_id) {
  
  options(scipen = 200, 
          mongodb = list(
            "host" = "mongodb://localhost:27017", 
            "db" = "pharbers-ntm-client"), 
          digits = 13, 
          digits.secs = 3)
  
  library(DT)
  library(dplyr)
  library(tidyr)
  library(mongolite)
  library(data.table)
  library(jsonlite)
  
  # proposal_id <- "5cc018a2f4ce4374c23cece6"
  # account_id <- "5cc3fb57ceb3c45854b80e57"
  
  source("./functions.R")
  
  
  if (!missing(account_id) & !missing(proposal_id)) {
    
    
    ## -- obtain inputs
    db_Paper <- mongo(collection = "Paper",
                      db = options()$mongodb$db, 
                      url = options()$mongodb$host)
    ## -- obtain inputs
    paper_info <- db_Paper$find(query = paste0('{"proposal-id":"', proposal_id, '",', 
                                               '"account-id":"', account_id, '"}')) 
    
    input_id <- tail(unlist(paper_info$`input-ids`), n = 1)
    
    db_PaperInput <- mongo(collection = "Paperinput",
                           db = options()$mongodb$db, 
                           url = options()$mongodb$host)
    
    paper_input <- db_PaperInput$find(query = paste0('{"_id": {"$oid":"', 
                                                     input_id,
                                                     '"}}'))
    
    current_phase <- paper_input$phase
    
    input_info <- get_inputs(paper_input)
    
    business_input <- input_info[["business_input"]]
    representative_input <- input_info[["representative_input"]]
    manager_input <- input_info[["manager_input"]]
    
    
    ## -- obtain background data
    # - hospital& other background
    mapping_tables <- get_id_mapping_table(proposal_id)
    hospital_mapping_table <- mapping_tables[["hosp_mappping"]]
    product_mapping_table <- mapping_tables[["prod_mappipng"]]
    representative_mapping_table <- mapping_tables[["rep_mappings"]] 
    
    
    # - sales background
    hospital_sales_report <- get_sales_bakground(paper_info,
                                                 current_phase,
                                                 hospital_mapping_table,
                                                 product_mapping_table,
                                                 representative_mapping_table)
    hospital_sales_report <- hospital_sales_report %>%
      group_by(`hospital-id`,
               `product-id`,
               `cp_representative-id`,
               `pp_representative-id`) %>%
      mutate_all(funs(as.numeric(.))) %>%
      ungroup()
    
    # - representatives' ability
    representative_ability_info <- get_rep_bakground(paper_info,
                                                     current_phase)
    cp_representative_ability_info <- representative_ability_info[["cp_representative_ability_info"]]
    pp_representative_ability_info <- representative_ability_info[["pp_representative_ability_info"]]
    representative_ability_report <- bind_rows(cp_representative_ability_info,
                                               pp_representative_ability_info)
    
    ## -- eva process
    overall_score <- get_eva_scores(business_input,
                                    representative_input,
                                    manager_input,
                                    hospital_mapping_table,
                                    product_mapping_table,
                                    representative_mapping_table,
                                    hospital_sales_report,
                                    cp_representative_ability_info,
                                    pp_representative_ability_info,
                                    representative_ability_report)
    
    
    
    ab_dimen_mapping <- data.frame(variables = c("ab1", "ab2", "ab3", "ab4", "ab5", "general"),
                                   code = 0:5,
                                   stringsAsFactors = F)
    
    
    assessment_report_id <- get_eva_bak_info(overall_score,
                                             ab_dimen_mapping)
    assessment_report_id_list <- unlist(paper_info$`assessment-report-ids`)
    db_Paper$update(query = paste0('{"proposal-id" : "', proposal_id, '", "account-id" : "', account_id, '"}'), 
                    update = paste0('{"$set" : {"assessment-report-ids" : ', toJSON(c(assessment_report_id_list, assessment_report_id), auto_unbox = TRUE), '}}'), 
                    upsert = FALSE)
    
    
    
    
    ## output
    return(list(status = unbox("Success")))
    
  } else {
    
    return(list(status = unbox("Failed")))
  } }

