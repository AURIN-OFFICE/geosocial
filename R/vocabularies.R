######## ---------- Read RVA vocabs: -------- ######
SearchConcept = function(keywords){
  #' SearchConcept
  #' @description Based on a keyword, query the ARDC vocabs server and return related terms. For example: SearchConcept(keywords='AGED').
  #' @param keywords A vector of strings that contains keywords.
  #' @return Terms related to the keywords.

  #### -------- Transformate white spaces to API format ------- ##
  FillSpaces = function(str){
    str = tolower(str)
    str = gsub(pattern = " ",replacement = '+',x = str)
  }
  baseurl = 'https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/concept.json?labelcontains='
  ### --- Parsing spaces --- ###
  keywords = FillSpaces(keywords)
  mainUrl = paste0(baseurl,keywords)

  #### ------- Function  ------ ######
  result <- fromJSON(file = mainUrl,simplify = TRUE )
  result_parsing = do.call(rbind, lapply(result$result$items, as.data.frame))
  # delete duplicates
  result_parsing = result_parsing[!duplicated(result_parsing),]
  return(result_parsing)
}


###### ------ Get the metadata of a particular concept ---- #####
GetTerm = function(term){
  #' GetTerm
  #' @description Based on a term, get the metadata associated to it. For example: GetTerm(keywords='http://example.com/census/IFAGEP').
  #' @param term A String with the direction of the term in the ARDC server.
  #' @return Return label, dcterms_created, dcterms_modified, creator, dc_publisher, dc_source, dcterms_title, has_top_concept, history_note, scope_note, type and identifier.

  mainUrl = 'https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/resource.json?uri='
  about = 'http://example.com/census/AGEP'
  mainUrl = paste0(mainUrl,about)
  result <- fromJSON(file = mainUrl,simplify = TRUE )
  ##### ------ Parsing ------ #####
  results = list(label = as.character(result$result$primaryTopic[["label"]][1]),
                 dcterms_created = as.character(result$result$primaryTopic$dctermsCreated),
                 dcterms_modified = as.character(result$result$primaryTopic$dctermsModified),
                 creator = as.character(result$result$primaryTopic$creator),
                 dc_publisher =  as.character(result$result$primaryTopic$dcPublisher),
                 dc_source =  as.character(result$result$primaryTopic$dcSource),
                 dcterms_title =  as.character(result$result$primaryTopic$dctermsTitle),
                 has_top_concept = do.call(rbind, lapply(c(1:length(result$result$primaryTopic$hasTopConcept)), function(x){as.data.frame(result$result$primaryTopic$hasTopConcept[[x]])})) ,
                 history_note = result$result$primaryTopic$historyNote,
                 scope_note = result$result$primaryTopic$scopeNote,
                 type = result$result$primaryTopic$type,
                 identifier = result$result$primaryTopic$identifier)


  return(results)
}

