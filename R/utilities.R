CreateFolders = function(path=getwd()){
  #' CreateFolders
  #' @description This function generates the folders the script needs to run.
  #' @param name the path
  #' @return True if the folders are created correctly.

  message('\n -----------------------------------')
  message('Running: CreateFolders')


  out <- tryCatch(
    {
      ### ----- Create folder to storage logs ----- ###
      dir.create(path = paste0(path,'/log'),showWarnings = FALSE)
      ##### ---- Create folder to storage data ---- ###
      dir.create(path = paste0(path,'/data'),showWarnings = FALSE)
      ##### ---- Create folder to storage outputs ---- ###
      dir.create(path = paste0(path,'/outputs'),showWarnings = FALSE)
      print("Initial requirements: O.K.")
      message('\n -----------------------------------')
      return(T)

    },
    error=function(cond) {
      message("please check your overwriting permissions.")
      message(cond)
      return(NA)
    }
  )

  return(out)


}


SummaryReport = function(concordances_POA,concordances_SA3,metrics_POA,metrics_SA3){
  #' SummaryReport
  #' @description Create the summary report
  #' @param concordances_POA Concordances used during the first stage: POA to SA3.
  #' @param concordances_SA3 Concordances used during the second stage: SA3 to SA3.
  #' @param metrics_POA Metrics used during the first stage: POA to SA3.
  #' @param metrics_SA3 Metrics used during the second stage: SA3 to SA3.
  #' @return Returns a list with all the information about the data linkage.
  ##### ------- Write ------ #####
  Concordances_log = SummaryConcordances(concordances_POA = concordances_POA,concordances_SA3=concordances_SA3)
  ######### ---------- Summary metrics -------- ############
  Metric_log = SummaryMetrics(metrics_POA =metrics_POA , metrics_SA3=metrics_SA3)
  #### ------ Create a table for TSP variables ---- ####
  TSP_variables = as.data.frame(table(TSP_2021_metadata$variable))
  TSP_variables = as.data.frame(TSP_2021_metadata %>% group_by(variable, year) %>%
                                  summarise(length(variable)))
  colnames(TSP_variables)  = c('Variable','ABS Year','categories')
  summary_results <- list('Conversion' = Metric_log,
                          'Concordances' = Concordances_log,
                          'TSP variables' = TSP_variables,
                          'Ratio Postcodes to SA3' = SurveyResponses_SA3_ratio,
                          'Ratio SA3 to SA3 2021' = SurveyResponses_SA3_TSP_ratio)


  return(summary_results)
}


SummaryLog = function(summaryResults){
  #' SummaryLog
  #' @description Consolidate a log with all the information on the data linkage.
  #' @param summaryResults List which contains the output after the data linkage.
  #### ------ Cross sectional ------ #####
  if (length(LSAY_waves)==1) {
    linkage_message = paste0('Cross-sectional: The round (',toString(unique(summaryResults[['TSP variables']]$`ABS Year`)),') of the TSP (2021) was linked with the wave ' ,toString(unique(LSAY_waves)),' of the Longitudinal survey used in the linkage.')
  } else {
    linkage_message = paste0('Longitudinal: The rounds ',toString(unique(summaryResults[['TSP variables']]$`ABS Year`)),' of the TSP (2021) was linked with the waves: ',toString(unique(LSAY_waves)),' of the Longitudinal survey used in the linkage.')
  }
  message(linkage_message)
  message("\n")
  #### ---- Variables
  message(paste0("The LSAY ",LSAY_cohort," was filtered  using the following sub-topics:"))
  for (j in unique(LSAY_topics)){
    message(paste0('- ',j))
  }
  message("\n")
  message("The Geographical unit used to do the data linkage was SA3 2021. More information about this: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/latest-release")
  ###### ----- Concordances log -------- ####
  message("\n")
  message("Concordances: The data linkage uses the following concordances: ")
  summaryResults_short = unique(summaryResults[['Concordances']])
  ### -------- Header
  message(paste0(colnames(summaryResults_short),collapse = " | ") )
  for (j in c(1:dim(summaryResults_short)[1])){
    message(paste0(unique(summaryResults[['Concordances']])[j,],collapse = " | "))
  }
  message("\n")
  message("The number of cases and associated postcodes that could not be linked:")
  message(paste0("- ",sum(summaryResults[['Linkage']][5]) ," postcodes, with ",sum(summaryResults[['Linkage']][6])," individuals could not be linked"))
  message("\n")
  message("The Census information linked to the longitudinal survey are:")

  for (j in unique(TSP_2021_metadata$variable)){
    message(paste0('- ',j))
  }
  # Warning flag for cross-sectional spatial data linkage:
  # A general warning if the type of linkage executed does not warrant
  # an analysis that considers spatial characteristics longitudinally
  # (due to inconsistencies in spatial boundaries, potential changes to Census data definitions over time etc.).
  message("\n")
  message(paste0("The average ratio of the data linkage was: ",  round(mean(colMeans(summaryResults[['Ratio Postcodes to SA3']][-1],na.rm = T)),2)))
  message(paste0("Please refers to the summary file: Summary.xlsx, which contains more details about the implementation."))
}







WriteStata = function(DataJoined,SurveyResponses,waves,path){
  #' WriteStata
  #' @description Write the outcome of the data linkage.
  #' @param DataJoined List which contains the output after the data linkage.
  #' @param SurveyResponses: Dataframe which contains the survey responses.
  #' @param waves vector of years. (For example, wave = [2012,2013,2014])
  #' @param path Location where the files are going to be written.
  #' @return True if the folders are created correctly.

  ##### --------- Longitudinal ------- #######
  if (length(waves)==1){
    ##### --------- Cross-sectional ------- #######
    print(paste0("Writing: SurveyResponses.dta"))
    sjlabelled::write_stata(DataJoined,paste0(path,"LSAY09_TSP_",waves,".dta"))

  } else {

    ##### -------- Create multiple files ------- #####
    for (wave in names(DataJoined)){
      print(paste0("Writing:",path,"SA3_",wave,'.dta'))
      sjlabelled::write_stata(DataJoined[[wave]],paste0(path,"SA3_",wave,'.dta'))
    }
    #### ------- Survey responses ------ ###
    print(paste0("Writing:",path,"SurveyResponses.dta"))
    colnames(SurveyResponses) = gsub(colnames(SurveyResponses),replacement = '',pattern='\\.')
    sjlabelled::write_stata(SurveyResponses,paste0(path,"LSAY09_responses_",paste0(unique(waves),collapse = '_'),".dta"))

  }
}




