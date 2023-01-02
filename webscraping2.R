#webscraping

#começo
comeco <- Sys.time()

#bibliotecas
library(rvest)
library(dplyr)

#pagina utilizada
link <- 'https://www.imdb.com/search/title/?title_type=video_game&genres=horror,adventure&genres=Adventure&explore=title_type,genres&ref_=adv_explore_rhs'

page <- read_html(link)

##funcao extrai avaliacao
pega_valor <- function(link_avaliacao){
  jogo_page <- read_html(link_avaliacao)
  valor <- jogo_page %>% html_nodes('td+ td .leftAligned') %>% 
    html_text() %>%  paste(collapse = '|')
}

##funcao extrai diretor
pega_valor2 <- function(link_avaliacao){
  jogo_page <- read_html(link_avaliacao)
  valor <- jogo_page %>% html_nodes('#director+ .simpleCreditsTable a') %>% 
    html_text() %>%  paste(collapse = '|')
}


##dataset final
Banco_jogos <- data.frame()

#contador
j=1

for(i in 1:17){
  #contador para remover depois
  print(j)
  j=j+1
  
  #variaveis coletadas
  
  nome_jogo <- page %>% html_nodes('.lister-item-header a') %>% 
    html_text()
  
  ##########################################################
  
  link_jogo <- page %>% html_nodes('.lister-item-header a') %>% 
    html_attr('href') %>% paste0('https://www.imdb.com',.)
  
  ##########################################################
  
  #ano jogo - extraído de string
  ano_jogo <- page %>% html_nodes('.text-muted.unbold') %>% 
    html_text()
  ano_jogo <- as.numeric(gsub("\\(([0-9]+).*$", "\\1", ano_jogo))
  
  ##########################################################
  
  genero_jogo <- page %>% html_nodes('.genre') %>% 
    html_text()
  
  
  ##########################################################
  
  link_avaliacao <- gsub('\\?ref_=adv_li_tt','',link_jogo)
  link_avaliacao <- paste0(link_avaliacao,'ratings/?ref_=tt_ov_rt')
  
  ##extrair avaliacoes 10 a 1, separador |
  
  avaliacao_jogo <- sapply(link_avaliacao,pega_valor,USE.NAMES = F)
  
  ##########################################################
  
  nota_jogo <- page %>% html_nodes('.ratings-imdb-rating strong') %>% 
    html_text()
  
  #inserir NA nas posicoes sem avalicao
  indices <- c(which(avaliacao_jogo==''))
  
  if(length(indices)!=0){
    for(i in 1:length(indices)){
      nota_jogo <- append(nota_jogo,'NA', after = indices[i]-1)
    }
  }
  ##########################################################
  
  link_fullcredit <- gsub('\\?ref_=adv_li_tt','',link_jogo)
  link_fullcredit <- paste0(link_fullcredit,'fullcredits/?ref_=tt_ql_cl')
  
  ##extrair diretor do jogo
  
  diretor_jogo <- sapply(link_fullcredit,pega_valor2,USE.NAMES = F)
  
  ##########################################################
  
  Banco_jogos <- rbind(Banco_jogos,data.frame(nome_jogo,nota_jogo,link_jogo,ano_jogo,genero_jogo,link_avaliacao,avaliacao_jogo,diretor_jogo))
  
  ##########################################################
  #indo para a próxima página
  
  link <- unique(page %>% html_nodes('.next-page') %>% 
                   html_attr('href') %>% paste0('https://www.imdb.com',.))
  
  page <- read_html(link)
  
  #tempo iteracao
  interacao <- Sys.time()
}

#final
final <- Sys.time()

print(final-comeco)
