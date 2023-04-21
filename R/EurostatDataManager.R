#' @importFrom stringr str_locate


get_eurostat_data = function(endpoint)
{
  res = GET(endpoint)

  content = fromJSON(rawToChar(res$content))

  if(length(content$value)==0){

    selectedYear = substr(endpoint,str_locate(endpoint,"&time=")[1,2]+1,str_locate(endpoint,"&time=")[1,2]+4)
    requestedTable = substr(endpoint,str_locate(endpoint,"1.0/data/")[1,2]+1,str_locate(endpoint,"\\?")[1,1]-1)

    stop(paste0("Donnees eurostat indisponibles pour ",selectedYear," (table ", requestedTable, ")"))
  }

  data = data.frame(matrix(nrow = length(content$value), ncol = (length(content$id))))
  colnames(data) = content$id

  for (i in 1:length(content$value))
  {
    id_value = strtoi(names(content$value)[i])

    for (j in length(content$id):1)
    {
      param = content$id[j]
      x_param = id_value %% content$size[j]
      indexes = content$dimension[[param]]$category$index
      data[[param]][i] = names(indexes)[x_param+1]
      id_value = id_value %/% content$size[j]
    }

    data$value[i] = content$value[i]
  }

  data$value = as.numeric(data$value)

  return(data)
}

