# script para gerar as proporcionalidades a partir das tabelas
# de valores e composicao, supondo que as proporcionalidades
# sao simplesmente as proporcoes dos valores filhos de um 
# determinado pai. por exemplo, se um pai tem 3 filhos com valores
# 20, 20 e 10, as proporcionalidades deles serao 0.4, 0.4 e 0.2,
# respectivamente.

require(dplyr)

#pasta = "c:/Users/Usuario/Downloads/BSMoutcorre/"
#extensao = "csv"

pasta = "C:/Users/Usuario/Downloads/se_bio_1610/"
extensao = "xlsx"
year = 2017

arquivo = function(nome) paste0(pasta, nome, sep = "")

if(extensao == "csv")
  composicao <- read.csv(arquivo("composicao.csv"), sep = ";") else
  composicao <- xlsx::read.xlsx(arquivo("composicao.xlsx"), 1)

# convert from id to column name
idtocol <- function(id) paste0("X", id, ".", year)

if(extensao == "csv")
  valores <- read.csv(arquivo("valores.csv"), sep = ";") else
  valores <- xlsx::read.xlsx(arquivo("valores.xlsx"), 1)

# se os valores estiverem com virgula ao inves de ponto para 
# delimitar casas decimais
valores <- valores %>%
#  mutate(across(paste0(idtocol(2:62)), ~ stringr::str_replace(.x, "\\,", "."))) %>%
  mutate(across(paste0(idtocol(2:35)), as.numeric)) %>%
  replace(is.na(.), 0)

result <- valores %>% dplyr::select(1)

pais <- unique(composicao$codigo_pai)[-1]
pais <- pais[!is.na(pais)]

for(pai in pais){
#  pai=pais[1]
  cat(paste0("Processing ", pai, "\n"))

  filhos <- composicao %>%
    dplyr::filter(codigo_pai == !!pai) %>%
    .$codigo_filho

  vfilhos <- valores %>% dplyr::select(idtocol(filhos))

  if(length(filhos) > 1){
    row_sum <- rowSums(vfilhos)
  
    vfilhos <- vfilhos %>% 
      mutate_all(~ ./row_sum) %>%
      round(3)
  }
  else
    vfilhos[, 1] <- 1
  
  colnames(vfilhos) <- paste(colnames(vfilhos), ".", pai, sep="")
  result <- cbind(result, vfilhos)
}

x = names(result)

# pai vai para a primeira linha do csv de saida
# if vai para a segunda linha do csv de saida
str <- paste0("X([0-9]+)\\.", year, "\\.([0-9]+).*$")
pai <- suppressWarnings(as.numeric(gsub(str, "\\2", x)))
pai <- ifelse(duplicated(pai), NA, pai)

nas <- is.na(pai)
pai <- paste0(pai, "-", year, sep = "")
pai[nas] = ""

id <- suppressWarnings(as.numeric(gsub("X([0-9]+).*$", "\\1", x)))

id <- paste0(id, "-", year, sep = "")
id[1] <- "id"

colnames(result) <- c("id", idtocol(4:35))

View(result)

result <- result[, -34]

result <- result %>%
  replace(is.na(.), 0) %>%
  mutate(result, across(paste0(idtocol(4:35)), ~as.character(.x, ""))) %>%
  mutate(, across(paste0(idtocol(4:35)), ~ tidyr::replace_na(.x, "DI")))

dim(result)
id <- id[-34]
pai <- pai[-34]
result <- rbind(result, id)
result <- rbind(result, pai)
result <- rbind(tail(result, 2)[2:1, ], head(result, -2))

write.table(result, "proporcionalidades.csv", row.names = FALSE, col.names = FALSE, sep="|")
xlsx::write.xlsx(result, "proporcionalidades.xls", row.names = FALSE, col.names = FALSE)
