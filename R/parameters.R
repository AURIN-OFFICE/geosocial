
##### ----- Internal function to parsing the parameters that are storage in the global env
ParsingParameter = function(parameter){
  #' ParsingParameter
  #' @description Returns the parameters from the global environment.
  #' @param parameter String with the name of the parameter.
  #' @return The value of the parameter.
  #' @keywords internal

  ##### ------- Get from then main environment ------- ########
  parameter = unique(trimws(strsplit(Sys.getenv(parameter),',')[[1]]))
  return(parameter)
}


LoadParameters <- function(file) {
  #' LoadParameters
  #' @description Loads the parameters and sets the global environment.
  #' @param file is the path where the JSON file is located.
  #' @return True if the parameters are valid

  #### -------- Load file ------- ####
  parameters = fromJSON(file = file)
  ##### ------ Try loading each parameter of the file ------ ######
  out <- tryCatch(
    {
      ##### ---- Set up environment ------ ######
      Sys.setenv("LSAY_cohort" =  parameters$LSAY_cohort)
      Sys.setenv("LSAY_waves" = toString(parameters$LSAY_waves))
      Sys.setenv("TSP_year" =  toString(parameters$TSP_year))
      Sys.setenv("TSP_variables" = toString(parameters$TSP_variables))
      Sys.setenv("withAPI" =  parameters$with_API)
      Sys.setenv("LSAY_topics" = toString(parameters$LSAY_topics))

      ####### ------- Test the connection -------- ######
      if (parameters$with_API) {
        message('\n -----------------------------------')
        message('Running: LoadCredentials')
        ##### ---- Set up Dataverse environment ------ ######
        Sys.setenv("DATAVERSE_SERVER" =  'dataverse.ada.edu.au')
        Sys.setenv("DATAVERSE_KEY" =  parameters$dataverse$token)
        Sys.setenv("DATAVERSE_ID" =  parameters$dataverse$ADA_ID)
        Sys.setenv("DATAVERSE_NAME" =  parameters$dataverse$name)
        #### ----- Test connection: Aurin ---- #
        connection = TestDataverseConnection()
        print("Dataverse connection: Correct")
        print("Longitudinal survey: Dataverse")

        return(T)

      } else {
        path_folder = toString(parameters$Survey_files)
        ## Search file in the folder
        Survey_files = list.files(path_folder)
        ## Only choose stata fomat
        Survey_files = Survey_files[grep(Survey_files,pattern=".dta")]
        ## Add path
        Survey_files = paste0(path_folder,Survey_files)
        Sys.setenv("SurveyFiles" = toString(Survey_files))
        print("Longitudinal survey: Local")
        return(T)
      }
    },
    error=function(cond) {
      print("Please check the parameters file.")
      message(cond)
      return(NA)
    }
  )

}




