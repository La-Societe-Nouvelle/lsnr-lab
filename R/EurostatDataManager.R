
get_eurostat_data = function(endpoint)
{
  res = GET(endpoint)

  content = fromJSON(rawToChar(res$content))

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

  return(data)
}