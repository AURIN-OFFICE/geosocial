############### ---------------- Geospatial functions -------------- ################

############### ---------------- Auxiliary functions -------------- ################
PotentialCensus= function(year){
  #' PotentialCensus
  #' @description Given a year, find the closest census year.
  #' @param year year of analysis.
  #' @return String which contains the ABS_year and the original year.
  #' @keywords internal.
  #### ------- Census intervals ABS ------ ######
  year = as.numeric(year)
  #### ----- SA3 starts with 2011: Assumption that after 2009 is possible make the link ---- ##
  census_intervals = list(ABS_2011=c(2009,2015),
                          ABS_2016=c(2016,2020),
                          ABS_2021=c(2021,2023))
  #### -------- Evaluate year in each interval of the census ------- #####
  Evaluation = lapply(census_intervals, function(x){all((year >= x[[1]]) & ((year <= x[[2]])))})
  #### ------ Find the interval which is true------ ####
  year_associate = as.numeric(gsub(names(Evaluation[Evaluation==TRUE]),replacement = '',pattern='ABS_'))
  ### ------ Return the year_associate_And the original year ----- #######
  return(paste0(year_associate,'_',year))
}


FilterConcordance = function(concordances,id,ratio_threshold=NULL){
  #' FilterConcordance
  #' @description Narrow down the concordances and select the highest
  #' ratio that meets the threshold set by the user.
  #' @param concordances Concorcondaces file.
  #' @param id Geospatial identificator in que concordances table.
  #' @param ratio_threshold ratio.
  #' @return True if the file is storage correctly.
  #'
  ### ----- Filter the table using the criteria search -- ###
  concordances_filtered= concordances[grep(as.numeric(concordances$origin),pattern = paste0('^',as.numeric(id),'$')),]
  ### ----- Identified the best match: Max ratio ----- ###
  max_ratio = which.max(concordances_filtered$ratio)

  ### ----- Verify that the concordance exists ---- ###
  if (dim(concordances_filtered)[1]!=0) {
    ### ----- Filtered by the max ratio ---- ###
    concordances_filtered = concordances_filtered[max_ratio,]
    ##### --------- Evaluate if ratio_threshold is defined -------- #######
    if (!is.null(ratio_threshold)) {
      ### ----- If the ratio doesn’t satisfy the threshold ---- ###
      if (concordances_filtered$ratio<ratio_threshold) {
        ##### ------- Given that doesn’t satisfy the threshold is imputed as Nan -------- #########
        concordances_filtered[c('origin','destination','ratio','origin_areasqkm','destination_areasqkm','correspondence')] = c(NaN,NaN,NaN,0,0,0)
      }
    }
  } else {
    #### ------- To preserve the structure of the original data ------ #####
    concordances_filtered = data.frame(origin=NaN,destination=NaN,ratio=NaN,origin_areasqkm=0,destination_areasqkm=0,correspondence=0)
  }


  return(concordances_filtered)
}




###### -------- QualityIndicator ------- ########
#' QualityIndicator
#' @description Implementation of the ABS quality indicator which provides a considered measure of the quality of the correspondence.
#' in relation to the weighting unit. (ABS, 2021).
#' @param ratio This field describes the Ratio of the FROM region that is being donated to the TO region. The Ratio is a figure between 0 and 1. (ABS, 2021).
#' @return Returns the equivalent SA3 for each row.
QualityIndicator=function(ratio){
  QualityClassification=function(ratio){
    if (!is.na(ratio)) {
      ratio = as.numeric(ratio)
        #### ---- Good ------ ####
      if (ratio>0.9) {
        return('Good')
      } else if ((ratio<=0.9) & (ratio>=0.75)) {
        #### ---- Acceptable ------ ####
        return('Acceptable')
      } else if (ratio<0.75) {
        #### ---- Poor ------ ####
        return('Poor')
      }
    } else {
      return(ratio)
    }}

  #### ------ Calculate the quality indicator - Vector ----- ######
  quality_indicator = table(as.character(lapply(ratio, function(x){QualityClassification(ratio = x)})))
  #### ------ Frecuency ------- ######
  quality_indicator = as.data.frame(t(data.frame(as.numeric(quality_indicator),row.names = names(quality_indicator))))
  rownames(quality_indicator) = NULL
  #### ------ Verify categories ------ ####
  difference_names = setdiff(c('Good','Acceptable','Poor'),colnames(quality_indicator))
  #### ------ add misssing categories ------ ####
  if (length(difference_names)!=0) {
    for (category in difference_names) {
      quality_indicator[category]=0
    }
  }
  #### ----- Organise the structure ----- ####
  quality_indicator = quality_indicator[,c('Good','Acceptable','Poor')]

  return(quality_indicator)
}



###### -------- Find the pontential census year ------- ########
#' TransformPOA
#' @description Recieve a vector of POAS, This function transformate POA to SA3.
#' @param year Year.
#' @return Returns the equivalent SA3 for each row.
#' @return the metric of missing values, miss matching and POAS that doenst match.

TransformPOA = function(data,concordances,year,ratio_threshold=NULL){
  #### ----- Identify how many missing values have the original data ------ #####
  miss_values_before = sum(is.na(data))
  #### ----- POA to SA3: For the same year ------ #####
  concordances_search = concordances[(concordances$origin_unit=='POA') & (concordances$year_in==as.character(year)) & (concordances$year_out==as.character(year)),]
  ###### ----- Filter the concordances ------- #####
  data = as.character(data)
  print(paste0('Transforming POA to SA3 - ABS - ',year))
  #### ---- first identify possible type errors or postcodes that are not mapping in the ABS --- ###
  miss_matchings = setdiff(as.numeric(data[!is.na(as.numeric(data))]),as.numeric(concordances_search$origin))
  miss_matchings = miss_matchings[!is.na(miss_matchings)]
  #### ------ Change POA to SA3 for each row ----- #####
  poa_result = lapply(as.numeric(data), function(x){if (is.na(x))  {data.frame(origin=NaN,destination=NaN,ratio=NaN,origin_areasqkm=0,destination_areasqkm=0,correspondence=0)} else { FilterConcordance(concordances = concordances_search,id = as.character(x),ratio_threshold=ratio_threshold)[,c('origin','destination','ratio','origin_areasqkm','destination_areasqkm','correspondence')] }})
  poa_result = do.call('rbind',poa_result)
  #### ---- Area metric  (overlap ratio*Area origin/Area detination)----- ####
  poa_result$representation = apply(poa_result,1,function(x){ifelse(test = is.na(x['ratio']),yes = x,no = round((((as.numeric(x['origin_areasqkm'])*as.numeric(x['ratio']))/as.numeric(x['destination_areasqkm']))*100),2))})
  #### ------- Calculated metrics ------- ######
  metric = data.frame(N=length(data),MissingValues= miss_values_before,NotLinkedAreas=length(miss_matchings),NotLinkedIndividuals = sum(is.na(poa_result$destination))-(miss_values_before))
  #### ------ Average ratio ----- ######
  metric$avg_ratio = round(mean(poa_result$ratio,na.rm = TRUE),2)
  #### ------ Quality indicator ----- ######
  quality_indicator = QualityIndicator(ratio = poa_result$ratio)
  metric = cbind(metric,quality_indicator)
  #### ---- Return ----- ###
  outputs = list()
  outputs[['match']] = poa_result
  outputs[['metric']] = metric
  return(outputs)
}

###### -------- SA3 year to SA3 end year ------- ########
#' TransformSA3
#' @description Recieve a vector of SA3 ABS year,  This function transformate POA to SA3.
#' @param data Year.
#' @param concordances Year.
#' @return Returns the equivalent SA3 for each row.
#' @return the metric of missing values, miss matching and POAS that doenst match.

######## --------- Stage 2: SA3 year to SA3 end year -------- #########
TransformSA3 = function(data,concordances,year_in,year_out,ratio_threshold){
  #### ----- Identify how many missing values have the original data ------ #####
  miss_values_before = sum(is.na(data))
  #### ----- SA3 to SA3: For the same year ------ #####
  concordances_search = concordances[(concordances$origin_unit=='SA3') & (concordances$destination_unit=='SA3') & (concordances$year_in==year_in) & (concordances$year_out==(year_out)),]
  #### ----- transformate data ------ ####
  data = as.character(data)
  print(paste0('Transforming SA3 ',year_in, ' to SA3 ', year_out))
  #### ---- first identify possible type errors or postcodes that are not mapping in the ABS --- ###
  miss_matchings = setdiff(as.numeric(data[!is.na(as.numeric(data))]),as.numeric(concordances_search$origin))
  miss_matchings = miss_matchings[!is.na(miss_matchings)]
  #### ------ Change the year for SA3----- #####
  sa3_result = lapply(as.numeric(data), function(x){if (is.na(x))  { data.frame(origin=NaN,destination=NaN,ratio=NaN,origin_areasqkm=0,destination_areasqkm=0,correspondence=0)} else { FilterConcordance(concordances = concordances_search,id = as.character(x),ratio_threshold=ratio_threshold)[,c('origin','destination','ratio','origin_areasqkm','destination_areasqkm','correspondence')] }})
  sa3_result = do.call('rbind',sa3_result)
  #### ---- Area metric  (overlap ratio*Area origin/Area detination)----- ####
  sa3_result$representation = apply(sa3_result,1,function(x){ifelse(test = is.na(x['ratio']),yes = x,no = round((((as.numeric(x['origin_areasqkm'])*as.numeric(x['ratio']))/as.numeric(x['destination_areasqkm']))*100),2))})
  metric = data.frame(N = length(data), MissingValues= miss_values_before,NotLinkedAreas=length(miss_matchings),NotLinkedIndividuals =  sum(is.na(sa3_result$destination))-(miss_values_before))
  #### ------ Average ratio ----- ######
  metric$avg_ratio = round(mean(sa3_result$ratio,na.rm = TRUE),2)
  #### ------ Quality indicator ----- ######
  quality_indicator = QualityIndicator(ratio = sa3_result$ratio)
  metric = cbind(metric,quality_indicator)
  #### ---- Ouputs ----- ###
  outputs = list()
  outputs[['match']] = sa3_result
  outputs[['metric']] = metric
  return(outputs)
}





LSAY_POA_SA3 = function(data,concordances,ratio_threshold=NULL){
  data = as.data.frame(data)
  #### ------ Get the list of years available to transform ----- ####
  years_available = colnames(data[grep(colnames(data),pattern='[0-9]{4}_[0-9]{4}')])
  ###### ------ Execute the match for each cohort ---- ######
  SA3_results = lapply(years_available, function(x){TransformPOA(data= data[[x]],concordances=concordances,year = strsplit(x = x,split = "_")[[1]][1],ratio_threshold=ratio_threshold)})
  #### ---- Replace years ---- ####
  names(SA3_results)=years_available

  ##### ----- Extract the new SA3 after the match ------ ######
  SA3_data =  do.call(cbind,lapply(years_available, function(x){SA3_results[[x]][[1]][[2]]}))
  colnames(SA3_data) = years_available
  SA3_data = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_data)))]),as.data.frame(SA3_data))
  colnames(SA3_data) = colnames(data)

  ##### ----- Extract ratios ------ ######
  SA3_ratios =  do.call(cbind,lapply(years_available, function(x){SA3_results[[x]][[1]][[3]]}))
  colnames(SA3_ratios) = years_available
  SA3_ratios = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_ratios)))]),as.data.frame(SA3_ratios))
  colnames(SA3_ratios) = colnames(data)

  ##### ----- Extract Concordances ------ ######
  SA3_concordances = do.call(cbind,lapply(years_available, function(x){SA3_results[[x]][[1]][[6]]}))
  colnames(SA3_concordances) = years_available
  SA3_concordances = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_concordances)))]),as.data.frame(SA3_concordances))
  colnames(SA3_concordances) = colnames(data)

  ##### ----- Extract the representation information after the match ------ ######
  SA3_representation =  do.call(cbind,lapply(years_available, function(x){SA3_results[[x]][[1]][[7]]}))
  colnames(SA3_representation) = years_available

  ##### ----- Extract the metrics ------ ######
  SA3_metric =  do.call(rbind,lapply(years_available, function(x){SA3_results[[x]][[2]]}))
  SA3_metric$year = years_available
  ######### --------- Prepare outputs ------ ##########
  outputs = list()
  outputs[['sa3']] = SA3_data
  outputs[['ratio']] = SA3_ratios
  outputs[['representation']] = SA3_representation
  outputs[['concordances']] = SA3_concordances
  outputs[['metric']] = SA3_metric

  return(outputs)
}





LSAY_PSA3_SA3 = function(data,concordances,year_out,ratio_threshold=NULL){
  #### ------ Get the list of years available to transform ----- ####
  years_available = colnames(data[grep(colnames(data),pattern='[0-9]{4}_[0-9]{4}')])
  ###### ------ Execute the match for each cohort ---- ######
  SA3_transformation = lapply(years_available, function(x){TransformSA3(data= data[[x]],concordances=concordances,year_in = strsplit(x = x,split = "_")[[1]][1],year_out = year_out, ratio_threshold=ratio_threshold)})
  #### ---- Replace years ---- ####
  names(SA3_transformation)=years_available

  ##### ----- Extract the new SA3 after the match ------ ######
  SA3_out =  do.call(cbind,lapply(years_available, function(x){SA3_transformation[[x]][[1]][[2]]}))
  colnames(SA3_out) = years_available
  SA3_out = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_out)))]),as.data.frame(SA3_out))
  colnames(SA3_out) = colnames(data)

  ##### ----- Extract the ratios information after the match ------ ######
  SA3_out_ratios =  do.call(cbind,lapply(years_available, function(x){SA3_transformation[[x]][[1]][[3]]}))
  colnames(SA3_out_ratios) = years_available
  SA3_out_ratios = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_out_ratios)))]),as.data.frame(SA3_out_ratios))
  colnames(SA3_out_ratios) = colnames(data)

  ##### ----- Extract Concordances ------ ######
  SA3_out_concordances = do.call(cbind,lapply(years_available, function(x){SA3_transformation[[x]][[1]][[6]]}))
  colnames(SA3_out_concordances) = years_available
  SA3_out_concordances = cbind(as.data.frame(data[,c(setdiff(colnames(data),colnames(SA3_out_concordances)))]),as.data.frame(SA3_out_concordances))
  colnames(SA3_out_concordances) = colnames(data)

  ##### ----- Extract the representation information after the match ------ ######
  SA3_out_representation =  do.call(cbind,lapply(years_available, function(x){SA3_transformation[[x]][[1]][[7]]}))
  colnames(SA3_out_representation) = years_available

  ##### ----- Extract the metrics ------ ######
  SA3_out_metric =  do.call(rbind,lapply(years_available, function(x){SA3_transformation[[x]][[2]]}))
  SA3_out_metric$year = years_available

  ##### ----- Outputs ------ ######
  outputs = list()
  outputs[['sa3']] = SA3_out
  outputs[['ratio']] = SA3_out_ratios
  outputs[['representation']] = SA3_out_representation
  outputs[['concordances']] = SA3_out_concordances
  outputs[['metric']] = as.data.frame(SA3_out_metric)

  return(outputs)
}



######## --------- The output need to be defined -------- #########
######## ------- So far is the list of TSP atributes merge with geospatial wave ------- ######
#### ------ Get the list of years available to transform ----- ####
GeoSpatialJoin = function(year,GeospatialResponses,SurveyResponses,TSP_data,TSP_metadata){
  message('\n -----------------------------------')

  ########## ---------------- Longitudinal format ---------- ############
  if (length(year)==1){
    ##### -------- Cross-sectional format: Only one file ------- #####
    colnames(GeospatialResponses) = c('STIDSTD','SCHPSA3CODE',"SA3")
    #paste0(strsplit(colnames(SA3_OUT[3]),"_")[[1]][2],
    ##### --------- Merge SA3 home with SA3 -------- #########
    colnames(TSP_data)= gsub(colnames(TSP_data),replacement = '',pattern='\\.')
    SA3_TSP = merge(GeospatialResponses,TSP_data,by='SA3',all.x =TRUE)
    ###### -------- Matadata ------ ##########
    SA3_TSP = set_label(SA3_TSP,c(paste0("Home SA3 ",year," CODE"),"Student ID 5-digit",paste0("SCHP SA3 ",year," CODE"),TSP_metadata$Long))
    ###### -------- Write stata ------- ########
    ###### -------- Need to be using dplyr ------- ########
    SA3_TSP = left_join(x = SA3_TSP, y = SurveyResponses, by = "STIDSTD")
    message('\n -----------------------------------')
    return(SA3_TSP)
  } else {
    ##### -------- Create multiple files ------- #####
    years_available_out = colnames(GeospatialResponses[grep(colnames(GeospatialResponses),pattern='[0-9]{4}_[0-9]{4}')])
    longitudinal = list()
    for (variable in c(2:length(years_available_out))) {
      j = years_available_out[variable]
      wave_year = strsplit(j,split = '_')[[1]][2]
      wave_ABS = strsplit(j,split = '_')[[1]][1]

      ###### ------- Extract the ID and the wave data ------ #######
      wave_i = GeospatialResponses[,c('STIDSTD',j)]
      colnames(wave_i) = c('STIDSTD','SA3')
      ##### ------- Merge with SA3 census ----- ######
      stata_data = merge(wave_i,TSP_data,by='SA3',all.x =TRUE)
      ##### ------ Change name of the first col ------- ####
      colnames(stata_data)[1] = c(paste0('SA3_', strsplit(j,split = '_')[[1]][2]))
      ##### ----- Clean titles ------- ####
      colnames(stata_data) = gsub(colnames(stata_data),replacement = '',pattern='\\.')
      ##### ----- Sort data by STIDSTD ------- #######
      stata_data = stata_data[order(as.numeric(stata_data[,'STIDSTD'])),]
      ##### ----- add the labels ----- #######
      stata_data = set_label(stata_data,label = c(paste0('School wave ', wave_year, ' SA3 ',wave_ABS),'Student ID 5-digit',TSP_metadata$Long))
      ##### ---- Storage in a list ------ #####
      longitudinal[[wave_year]] = stata_data

    }
    message('Joining: LSAY 2009: Waves: ', paste0(paste0(unique(year),collapse = '|'),' with TSP 2021: ',paste0(unique( as.character(lapply(year, function(x){strsplit(PotentialCensus(year = as.character(x)),"_")[[1]][1]}))),collapse = '|')))
    message('\n -----------------------------------')

    return(longitudinal)

  }

}


