

composicao <- read.csv("c:/Users/Usuario/Downloads/BSMoutcorre/composicao.csv", sep=";")
composicao

idtocol = function(id) paste0("X", id, ".2010")

valores <- read.csv("c:/Users/Usuario/Downloads/BSMoutcorre/valores.csv", sep=";") %>%
  mutate(across(paste0(idtocol(2:62)), stringr::str_replace, "\\,", ".")) %>%
  mutate(across(paste0(idtocol(2:62)), as.numeric))

composicao

result <- valores %>% dplyr::select(1)

pais = unique(composicao$codigo_pai)[-1]

for(pai in pais){
  pai=3
  cat(paste0("Processing ", pai, "\n"))
  filhos = composicao %>%
    dplyr::filter(codigo_pai == !!pai) %>%
    .$codigo_filho
  
  if(length(filhos) > 1){
    vfilhos = valores[,idtocol(filhos)]
    row_sum = rowSums(vfilhos)
  
    mresult <- vfilhos %>% 
      mutate_all(~ ./row_sum)
    
    colnames(mresult) <- paste(colnames(mresult), ".", pai, sep="")
    head(mresult)
    result <- cbind(result, mresult)
  }
  else{
  }
}

head(result)
