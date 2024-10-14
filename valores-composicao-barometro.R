
composicao <- read.csv("c:/Users/Usuario/Downloads/BSMoutcorre/composicao.csv", sep=";")

# convert from id to column name
idtocol = function(id) paste0("X", id, ".2010")

valores <- read.csv("c:/Users/Usuario/Downloads/BSMoutcorre/valores.csv", sep=";") %>%
  mutate(across(paste0(idtocol(2:62)), stringr::str_replace, "\\,", ".")) %>%
  mutate(across(paste0(idtocol(2:62)), as.numeric))

result <- valores %>% dplyr::select(1)

pais = unique(composicao$codigo_pai)[-1]

for(pai in pais){
  cat(paste0("Processing ", pai, "\n"))
  filhos = composicao %>%
    dplyr::filter(codigo_pai == !!pai) %>%
    .$codigo_filho

  vfilhos = valores %>% dplyr::select(idtocol(filhos))

  if(length(filhos) > 1){
    row_sum = rowSums(vfilhos)
  
    vfilhos <- vfilhos %>% 
      mutate_all(~ ./row_sum) %>%
      round(3)
  }
  else
    vfilhos[, 1] = 1
  
  colnames(vfilhos) <- paste(colnames(vfilhos), ".", pai, sep="")
  result <- cbind(result, vfilhos)
}

x = names(result)

# pai vai para a primeira linha do csv de saida
# if vai para a segunda linha do csv de saida
pai = as.numeric(gsub("X([0-9]+)\\.2010\\.([0-9]+).*$", "\\2", x))
pai = ifelse(duplicated(pai), NA, pai)

nas = is.na(pai)
pai = paste0(pai)
pai[nas] = ""

id = as.numeric(gsub("X([0-9]+).*$", "\\1", x))

id = paste0(id, "-2010", sep = "")
id[1] = ""

colnames(result) = c("id", idtocol(4:62))

result <- mutate(result, across(paste0(idtocol(4:62)), as.character, "")) %>%
  mutate(, across(paste0(idtocol(4:62)), tidyr::replace_na, "DI"))

result <- rbind(result, id)
result <- rbind(result, pai)
result <- rbind(tail(result, 2)[2:1, ], head(result, -2))

head(result)



