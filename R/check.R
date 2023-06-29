checkLSAY= function(dataset,cohort){
  #' checkLSAY
  #' @description Check that the dataset is an LSAY dataset
  #' and the dataset does not have modifications affecting the linkage process.
  #' @param dataset The dataset that would be analysed.
  #' @param cohort year of the cohort (For example, LSAY 2009, cohort = 2009).
  #' @return True if the dataset does not have modifications that affect the linkage process.

  message('\n -----------------------------------')
  message('Running: Checking LSAY structure')
  ##### ------- LSAY 2009 metadata -------- #######
  ###### ----- Comparison names ----- #####
  cohort = substr(cohort,3,4)
  LSAY_metadata = LSAY_metadata[grep(LSAY_metadata$Cohort,pattern=cohort),]
  missing_lsay = setdiff( as.character(LSAY_metadata$Variable), as.character(colnames(dataset)))
  if (length(missing_lsay)!=0){
    message(paste0("Please check the LSAY data; the variables ",toString(missing_lsay), ' are missing.'))
    file.copy(paste0('log/', logNm), paste0(output_location, 'report.log'))
    stop("Please adjust input data and start again")
    return(FALSE)
  } else {
    return(TRUE)
  }
}


checkVariableNames = function(dataset){
  #' checkVariableNames
  #' @description Check that the variable name is accepted by Stata: Stata variable names must adhere to the following rules:
  #' • Contain 1-32 characters.
  #' • Only contain the characters A-Z, 0-9, and underscore (_).
  #' • Begin with a letter or an underscore.
  #' @param dataset The dataset that would be analysed
  #' @return if the variable names are valid. Prints a message describing problem and specific variable that is the problem if invalid.

  message('\n -----------------------------------')
  message(paste0('Running: checkVariableNames on dataset: '))
  names<-colnames(dataset)
  pass = TRUE
  message = list()
  #### ------- for each column test ------- #####
  for (name in names){
    #### ------ check the number of characters ----- ####
    if (nchar(name)>32){
      pass = FALSE
      message = append(message, paste0("Variable ", name, " contains too many characters. Only 32 characters allowed in Stata."))
    }
    #### ------ check that only letters, numbers and underscore are included ----- ####
    if(grepl("[^[:alnum:]_]", name)){
      pass = FALSE
      message = append(message, paste0("Variable ", name, " contains invalid characters. Only letters, numbers, and underscore are allowed in Stata"))
    }

    #### ------ check that the first character is not a number ----- ####
    if(grepl("^\\d", name)){
      pass = FALSE
      message = append(message, paste0("Variable ", name, " starts with a number. In Stata variables must start with a letter or an underscore"))
    }

  }

  #### ------ Error ------- ####
  if (!pass){
    message("Error:")
    message(message)
    file.copy(paste0('log/', logNm), paste0(output_location, 'report.log'))
    stop("Variable names don't adhere to stata rules. Please adjust input data and start again")
    return(F)

  }

  return(pass)
}


checkNamesDuplicates = function(dataset){
  #' checkNamesDuplicates
  #' @description It's important to check for duplicate names to avoid
  #' errors during data linkage and ensure accurate results.
  #' Stata does not allow duplicate variable names, so this
  #' process ensures the joined datasets don't have any variables with the same name.
  #' @param dataset The dataset that would be analysed.
  #' @return True if none of the variable names are duplicates, otherwise false if overlap exists.

    names_data = colnames(dataset)
  message('\n -----------------------------------')
  message(paste0('Running: checkVariableNames_duplicates'))
  if (length(names_data) > length(unique(names_data))){
    message(paste0("Duplicate variables - the variable(/s): ", toString(names_data[duplicated(names_data)]),  " is duplicated."))
    stop("Please remove duplicates and try again.")
    file.copy(paste0('log/', logNm), paste0(output_location, 'report.log'))
    return(FALSE)
  }
  else{
    print('No duplicates found.')
    return(TRUE)
  }
}

checkPostcodeStructure<- function(dataset){
  #' checkPostcodeStructure
  #' @description Checks that the postcodes are valid values.
  #' @param dataset The dataset that would be analysed.
  #' @return True if all the postcodes are valid.
  message('\n -----------------------------------')
  message(paste0('Running: checkPostcodeColumn on '))
  dataset = dataset[,c(-1)]
  all_postcodes = unlist(dataset)
  all_postcodes = unique(all_postcodes)
  ##### ---- Replace ---- #####
  all_postcodes[all_postcodes==0]=NaN
  all_postcodes = all_postcodes[!is.na(all_postcodes)]
  all_postcodes = all_postcodes[all_postcodes!='NaN']
  ###### ----- Search postcodes ------ ####
  search_postcodes = grep(all_postcodes,pattern='^[[:digit:]]{4}$|^[[:digit:]]{3}$')
  if (length(all_postcodes[search_postcodes])==0){
   stop("Postcode column values not valid.")
  } else if (length(all_postcodes[search_postcodes])<length(all_postcodes)){
   warning(paste("Some values in postcode column are invalid. see values: ", toString(all_postcodes[-search_postcodes])))
  } else{
   return(TRUE)
  }

}



