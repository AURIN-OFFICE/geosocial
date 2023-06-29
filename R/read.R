LoadTSP2021 = function(year=NULL,variables=NULL){
  #' LoadTSP2021
  #' @description Load and filter the TSP 2021 by the demonstrator variables and the closest year published census.
  #' For example, if the year is [2017,2018,2019], the TSP2021 only will have the closet census to this years that is 2016.
  #' @param year Vector of years.
  #' @return a list which contains the data and metadata
  message('\n -----------------------------------')

  #### ---- Get data from the R library ---- ####
  TSP_2021_metadata = TSP2021[['metadata']]
  TSP_2021 = TSP2021[['data']]

  #### ------ If is cross sectional filtered by the year ----- ######
  if (length(year)!=0){
    year = unique(year)
    ####### ------ If the user select a different year of the census ----- ####
    ###### ----- Extract only the ABS year ----- ######
    year = as.character(lapply(year, function(x){strsplit(PotentialCensus(year = as.character(x)),"_")[[1]][1]}))
    ##### ----- Filter only by unique ----- #####
    year = unique(year)
    TSP_2021_metadata = TSP_2021_metadata[grep(TSP_2021_metadata$year,pattern=paste0("^",paste0(year,collapse = "$|^"),"$")),]
    TSP_2021 = TSP_2021[,c('SA3',TSP_2021_metadata$column)]
    message(paste0('Loading: TSP 2021: ',paste0(year,collapse = ',') ))

  } else {
    message('Loading: TSP 2021: 2011, 2016 and 2021')
  }


  ##### -------- Filtering by variables ------- ########
  if (length(variables)!=0){
    variables = unique(variables)
    TSP_2021_metadata = TSP_2021_metadata[grep(TSP_2021_metadata$variable,pattern=paste0("^",paste0(variables,collapse = "$|^"),"$")),]
    TSP_2021 = TSP_2021[,c('SA3',TSP_2021_metadata$column)]
    message(paste0('Filtering the following variables: ',toString(variables) ))

  } else {
    message('Loading all the variables of the demonstrator.')

  }

  results = list()
  results[['metadata']] = TSP_2021_metadata
  results[['data']] = TSP_2021
  return(results)
}


LoadLSAY = function(cohort,wave,LSAY_topics) {
  #' LoadLSAY
  #' @description Load or download the LSAY data, given a cohort and wave. (The demonstrator only supports LSAY cohort 2009)
  #' @param cohort year of the cohort (For example, LSAY 2009, cohort = 2009)
  #' @param wave vector of years. (For example, wave = [2012,2013,2014])
  #' @param LSAY_topics vector with the sub-topics that would be included in the analysis, (For example["School transition" "Current",..]).
  #' @return a list which contains the survey data and the geospatial data.

  #### ----- Verify cohort ----- #####
  if (as.numeric(cohort)==2009) {
    withAPI = as.logical(ParsingParameter('withAPI'))
    ############ ---------- Online -------- #################
    if (withAPI){
      ### -- Get the dataverse data - ###
      SurveyFiles = downloadDataverseData(id = ParsingParameter('DATAVERSE_ID'),
                                          file = ParsingParameter('DATAVERSE_NAME'))
      SurveyFiles = paste0('data/',SurveyFiles)
      ############ ---------- Offline -------- #################
    } else {
      ######### ---------- A vector with the path of each file ------- @@@@@@@@@@
      SurveyFiles = ParsingParameter('SurveyFiles')
    }

    ##### ------ Load all the files of the survey -------- ######
    SurveyData = lapply(SurveyFiles, function(x){haven::read_stata(x)})
    ######## ---------- Geospatial responses-------- ########
    GeospatialResponses = SpatiallyLSAY(survey = SurveyData,wave=wave,cohort = cohort)
    ######## ---------- Survey responses -------- ########
    SurveyResponses = SurveyLSAY(survey = SurveyData, wave = wave,LSAY_topics=LSAY_topics,cohort=cohort)

    ###### ------ consolidate resutls ------ #######
    results = list('GeospatialResponses' = GeospatialResponses,'SurveyResponses' = SurveyResponses)
    return(results)

  } else {
    message('This demonstrator only supports LSAY 2009.')
  }

}



SpatiallyLSAY = function(survey,cohort,wave=NULL){
  #' SpatiallyLSAY
  #' @description Identify the geospatial table, and filter by the wave and cohorts.
  #' @param survey Dataset
  #' @param cohort year of the cohort (For example, LSAY 2009, cohort = 2009).
  #' @param wave vector of years. (For example, wave = [2012,2013,2014])
  #' @return A dataframe with stata labels intact.
  #' @keywords internal

  message('\n -----------------------------------')
  ###### ------- identify which dataset variation on the schpcode: To discovered if is the geospatial or survey dta----- #####
  tryCatch(
    expr = {
      geospatial = which.max(unlist(lapply(survey, function(x){length(unlist(unique(x[grep(tolower(colnames(x)),pattern = 'schpcode')])))})))
      ###### ----- Choose only the geospatial values ----- ####
      geospatial = as.data.frame(survey[[geospatial]])
      ###### ----- Successfully  ----- ####
    },
    error = function(e){
      stop("The dataset is not valid. Please load the correct dataset and try again.")
      file.copy(paste0('log/', logNm), paste0(output_location, 'report.log'))

      print(e)
    },
    finally = {
      message(paste0('Running: Reading LSAY 2009'))
    }
  )


  ###### ----- Check if the variable have duplicated names ---- ####
  duplicated_columns = checkNamesDuplicates(geospatial)
  ###### ----- check if values contains the correct postcode structure ---- ######
  postcode_structure = checkPostcodeStructure(geospatial)


  if (postcode_structure==TRUE){
    ###### ------- Replace homes postcodes ---- #####
    ###### ----- Identify the year of the selection ------ #####
    colnames(geospatial) = gsub(toupper(colnames(geospatial)), pattern='PC',repl='')
    ###### ----- Replace SCHODE by the year of LSAY
    colnames(geospatial)[grep(colnames(geospatial),pattern='SCHODE')] = paste0(cohort)
    ### ------ If is a cross-sectional ----- ######
    if (length(wave)!=0){
      ##### ------ Only unique years ------ #####
      wave = unique(wave)
      ### ------- Include STIDSTD and school data ------- ######
      geospatial = geospatial[,c('STIDSTD',colnames(geospatial)[grep(colnames(geospatial),pattern=paste0("^",paste0(c('2009',wave),collapse = "$|^"),"$"))])]
      message(paste0('Loading: Geospatial data - LSAY 09 - Waves: ',paste0(wave,collapse = ',') ))
    } else {
      message('Loading: Geospatial data - LSAY 09 - All waves')
    }
    #### ---- Replace names of the years by closest ABS year ------ ######
    colnames(geospatial)[grep(colnames(geospatial),pattern='[0-9]')] = unlist(lapply(colnames(geospatial)[grep(colnames(geospatial),pattern='[0-9]')],function(x){PotentialCensus(year = x)}))

    return(geospatial)
  } else {
    stop("Postcode column values not valid.")
  }
}
###### -------- Read data survey, and filter if is a cross sectional analysis ------- ####
SurveyLSAY = function(survey,wave=NULL,LSAY_topics=NULL,cohort){
  #' SurveyLSAY
  #' @description Identify the survey responses, and filter them by the wave and cohorts.
  #' @param cohort year of the cohort (For example, LSAY 2009, cohort = 2009).
  #' @param wave vector of years. (For example, wave = [2012,2013,2014]).
  #' @return A dataframe with stata labels intact.
  #' @keywords internal

  message('\n -----------------------------------')
  ###### ------- identify which dataset variation on the schpcode: To discovered if is the geospatial or survey dta----- #####
  SurveyResponses = which.min(unlist(lapply(survey, function(x){length(unlist(unique(x[grep(tolower(colnames(x)),pattern = 'schpcode')])))})))
  ###### ----- Choose only the geospatial values ----- ####
  SurveyResponses = survey[[SurveyResponses]]
  ##### ----- If not, the code will stop ----- #####
  checkLSAY(dataset = SurveyResponses,cohort=cohort)
  ##### ------ Filter only with the metadadata of the cohort ------ #####
  cohort = substr(cohort,3,4)
  LSAY_metadata = LSAY_metadata[grep(LSAY_metadata$Cohort,pattern=cohort),]

  if (length(wave)!=0) {
    ##### ------ Only unique years ------ #####
    wave = unique(wave)
    ##### ------- LSAY 2009 metadata -------- #######
    #LSAY_metadata = readRDS('parameters/LSAY_metadata.RDS')
    #### ------- Filter by the wave ------ ######
    LSAY_metadata = LSAY_metadata[grep(LSAY_metadata$Year,pattern=paste0("^",paste0(c('2009',wave),collapse = "$|^"),"$")),]
    SurveyResponses = SurveyResponses[,c(LSAY_metadata$Variable)]
    message(paste0('Loading: Survey data - LSAY 09 - Waves: ',paste0(wave,collapse = ',') ))
  } else {
    message('Loading: Survey data - LSAY 09 - All waves')
  }

  ###### ------ filtering by topic ------ ######
  if  (length(LSAY_topics)!=0) {
    ##### ------ Only unique years ------ #####
    LSAY_topics = unique(LSAY_topics)
    #### ------- Filter by the wave ------ ######
    metadata_wave = LSAY_metadata[grep(LSAY_metadata[['Sub-major topic area']],pattern=paste0("^",paste0(LSAY_topics,collapse = "$|^"),"$")),]
    SurveyResponses = SurveyResponses[,c(metadata_wave$Variable)]
    message(paste0('Loading: Survey data - LSAY 09 - Topic area: ',paste0(LSAY_topics,collapse = ',') ))
  } else {
    message('Loading: Survey data - LSAY 09 - All Topics')
  }



  return(SurveyResponses)
}



