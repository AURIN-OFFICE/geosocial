TestDataverseConnection <- function(){
  #' TestDataverseConnection
  #' @description Tests connection to ADA dataverse. Requires dataverse token to be loaded in the system environment.
  #' @return True if the connection is correct
  out <- tryCatch(
    {
      message('\n -----------------------------------')
      message('Running: TestDataverseConnection')

      #### ---- Get a random search to evaluate the retrieval data ----- ####
      results = dataverse_search("data")
      results = length(results$url)
      #### ---- If the server return at least more than 1 ----- #####
      results = dim(results)[1] >= 0
      print("Dataverse connection: O.K.")
      return(T)

    },
    error=function(cond) {
      print("Please check your credentials: Dataverse")
      message(cond)
      return(NA)
    }
  )

  return(out)

}


downloadDataverseData = function(id, file){
  #' downloadDataverseData
  #' @description Function takes inputs doi (get from dataverse listing of desired data) and the year of the data
  #INPUT:
  #' @param id unique doi (get from dataverse listing of desired data)
  #' @param file where the file would be storage

  #RETURN:
  # downloaded dataset in .dta format in data folder with name 'AuSSA_[year].dta'
  #' @return True if the download is correct

  out <- tryCatch(
    {
      message('\n -----------------------------------')
      message(paste0('Running: getDataverseData - ',id))

      ## ACCESSING - Get files
      # get file from ADA dv, and write it to a binary format location (writeBin)
      flist <- dataset_files(id)
      #### ------ Get the names and lower ------ #####
      files_names = unlist(lapply(c(1:length(flist)), function(x){tolower(flist[[x]]$label)}))
      #### ---- Get the file that are in STATA format: .dta ------ ###
      files_downloads = lapply(grep(files_names,pattern = 'stata|.dta'),function(x) {flist[[x]]})
      ##### ---- Download all the stata files ----- ####
      files_sucess = c()
      for (file_download in files_downloads) {
        #### ---- Get the format of the file: zip or stata --- ####
        format_file = gsub(pattern = '.+\\.',x = file_download$label,replacement = '')
        ##### ---- Get the raw data ---- ###3
        file_raw <- get_file(file_download)
        clean_name = gsub(x = tolower(file_download$label),pattern = '[0-9]{1}\\.|stata.+|version.+|\\(', replacement='')
        clean_name = gsub(x = tolower(clean_name),pattern = '-', replacement='')
        clean_name = gsub("^ *|(?<= ) | *$", "", clean_name, perl = TRUE)
        clean_name = gsub(" ", "_", clean_name)
        files_sucess = c(files_sucess,clean_name)
        #### ----- Decompressing file ------ ####
        if (format_file=='zip') {
          print(paste0('ZIP file - Decompressing: ', file_download$label))
          #### ----- We need to download the file in a temp folder ----- #####
          #### ----- Create a temporal folder ----- ####
          temp = tempdir()
          temp_name = paste0(temp,'/',file_download$label)
          folder_unzip = paste0(temp,'/unzip')
          ### ---- In cases that the folder already exist - Delete ------ ####
          unlink(folder_unzip, recursive = TRUE, force = TRUE)

          ### ------ When is zip we download the zip in a temporal folder ----- ###
          writeBin(file_raw, temp_name)
          #### ---- Storage data ---- ###
          unzip(temp_name, exdir = folder_unzip)
          ### --- Get the file that was extracted ---- ###
          file_move = list.files(folder_unzip)
          ### ---- in case that the file already exist -- delete --- #
          unlink(file, recursive = TRUE, force = TRUE)

          ### ----- Rename and storage in the new folder --- ####
          file.copy(from = paste0(folder_unzip,'/',file_move),
                    to   = paste0('data/',clean_name,'.dta'))

        } else {
          #### ---- In case that the file is already stata format ---- ####
          writeBin(file_raw, paste0('data/',clean_name,'.dta'))
        }

      }


      print(paste0(id, ' has been downloaded correctly'))
      return(paste0(files_sucess,'.dta'))
    },
    error=function(cond) {
      message("Something is wrong:")
      message(cond)
      return(NA)
    }
  )

  return(out)

}


