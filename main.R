rm(list=ls())

require(tidyverse)
require(rvest)

source("utils.R")

## Moema
# url = "https://www.lelloimoveis.com.br/aluguel/residencial/moema-sao_paulo-regioes/"

## Vila Clementino
# url = "https://www.lelloimoveis.com.br/aluguel/residencial/vila_clementino-sao_paulo-regioes/"

# Vila Mariana
# https://www.lelloimoveis.com.br/venda/residencial/sim-mobiliado/vila_mariana-sao_paulo-regioes/

bairros = c('saude'
            ,'vila_clementino'
            ,'vila_mariana'
            ,'paraiso'
            ,'moema'
            ,'pinheiros'
            ,'vila_madalena'
            ,'brooklin'
            ,'chacara_klabin'
            ,'campo_belo'
            #,'jardim_paulista'
            ,'vila_olimpia'
            ,'itaim_bibi'
            ,'aclimacao'
            ,'ipiranga')

nhood_lists=c()
for(i_bairro in bairros){
  message(paste0("> listing neighborhood: ",i_bairro))
  
  aux_realty_id_list = c()
  for(dorms in 2:3){
    # Varios Bairros
    url = paste0('https://www.lelloimoveis.com.br/venda/residencial/'
                 ,dorms,'-dormitorios/'
                 ,paste(
                   i_bairro
                   ,collapse='-sao_paulo')
                 ,'-regioes/'
                 ,'ate-1250000-r$/'
                 ,'XYX-pagina/'
                 ,'de-70-ate-150-metros/'
                 ,'ordenar-por-menor-valor/'
    )
    #url %>%
    #  message
    
    n_pages = 20
    
    realty_id_list = lapply(1:n_pages,function(x) get_list_by_page(url,x)) %>%
      unlist
    
    realty_id_list = realty_id_list[realty_id_list != ""]
    
    aux_realty_id_list = c(aux_realty_id_list,realty_id_list) %>% unique
    
  }
  nhood_lists = rbind(nhood_lists,data.frame(i_bairro,aux_realty_id_list))

}

final_realty_id_list = nhood_lists$aux_realty_id_list

df_realty = lapply(final_realty_id_list,function(x) extract_from_realty(x)) %>%
  do.call(rbind,.) 

df_realty2 = df_realty  %>%
  merge(.,nhood_lists,by.x="id_realty_input",by.y="aux_realty_id_list") %>%
  filter(realty_condition == "comprar") %>%
  mutate(value_area = realty_value/realty_area)

df_realty2$realty_parking[is.na(df_realty2$realty_parking)]=0

df_realty2 = df_realty2[complete.cases(df_realty2[,c('realty_iptu','realty_cond')]),] 




# write.csv(df_realty2,"output.csv",row.names = F)

df_realty_plot = df_realty2 %>%
  filter(realty_type == "Apartamento Padrão") %>%
  filter(realty_cond <1250) %>%
  filter(!i_bairro %in% c("chacara_klabin","vila_madalena"))

df_realty_plot %>%
  select(i_bairro,realty_nhood) %>%
  table()

ggplot(df_realty_plot,aes(x=realty_area,y=realty_value,color=i_bairro)) +
  geom_point(size=2)



df_realty_plot %>%
  group_by(i_bairro) %>%
  summarise(count = n()) %>%
  arrange(count)

mean_nhood = df_realty_plot %>%
  group_by(i_bairro) %>%
  summarise(mean_v_a = mean(value_area)) %>%
  arrange(-mean_v_a)

mean_nhood_label = mean_nhood %>% select(i_bairro) %>% as.matrix()

mean_nhood %>%
  ggplot(aes(i_bairro,mean_v_a)) +
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=mean_nhood_label) +
  xlab("Bairro") +
  ylab("Preço do m2") +
  coord_flip() 


library(ggridges)
ggplot(df_realty_plot, aes(x = value_area, y = i_bairro,fill=stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "R$/m2", option = "C") + 
  scale_y_discrete(limits=mean_nhood_label) + 
  xlab("Valor do m2") +
  ylab("Bairro")


ggplot(df_realty_plot, aes(x = value_area, y = i_bairro, fill = 0.5 - abs(0.5 - stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1) + 
  scale_y_discrete(limits=mean_nhood_label) + 
  xlab("Valor do m2") +
  ylab("Bairro")

df_realty_plot %>%
  select(value_area) %>%
  as.matrix() %>%
  density() %>%
  plot()

fit_lm = lm(realty_value ~ realty_area +
              #I(realty_area^2)+
     realty_rooms +
     realty_baths +
     realty_parking +
     realty_iptu + 
     realty_cond + 
    i_bairro +
      i_bairro*realty_area , data = df_realty_plot)

fit_lm %>%
  summary

fit_lm %>%
  plot

score_data = data.frame("realty_area" = 60
                        ,"realty_rooms"= 2
                        ,"realty_baths"= 2
                        ,"realty_parking"= 0
                        ,"realty_iptu"= 101
                        ,"realty_cond"= 1000
                        ,"i_bairro"= 'aclimacao')

fit_lm %>%
  predict(.,newdata=score_data)

df_realty_plot %>%
  filter(i_bairro == 'aclimacao') %>%
  sumarise(mean = mean(realty_iptu))

df_realty_plot %>%
     filter(i_bairro == 'aclimacao') %>%
  summarise(mean=mean(realty_iptu))



ix_lev = c(342,74,413)
ix_qq = c(453,58)

ggplot(df_realty_plot,aes(realty_area,realty_value)) +
  geom_point() +
  geom_point(data=df_realty_plot[ix_lev,]
             ,aes(x=realty_area,y=realty_value)
             ,color="red"
             ,size=3
  ) +
  geom_point(data=df_realty_plot[ix_qq,]
             ,aes(x=realty_area,y=realty_value)
             ,color="blue"
             ,size=3
  )

require(Boruta)

fit_bor = Boruta(realty_value ~ realty_area +
                   realty_rooms +
                   realty_baths +
                   realty_parking +
                   realty_iptu + 
                   realty_cond +
                   as.factor(realty_nhood), data = df_realty_plot)



df_realty_plot[ix_lev,] %>%
  View




plot(fit_bor)

df_realty_plot %>%
  arrange(total_cost) %>%
  View

"220289" %>%
  paste0("https://www.lelloimoveis.com.br/imovel/",.)

######################################3

base_url = paste0('https://www.lelloimoveis.com.br/aluguel/residencial/moema-sao_paulo-regioes/',pages[1],'-pagina/#')

source = url %>%
  read_html()


# //*[@id="realty-list"]/div[2]/div/div[3]/div/div[2]/a/span
# //*[@id="realty-list"]/div[21]/div/div[3]/div/div[2]/a/span

i_realty = 2

sapply(2:21,function(x) get_realty_id(source,x)) %>%
  unlist

source %>%
  html_nodes(xpath = '//*[@id="realty-list"]/div[21]/div/div[3]/div/div[2]/a/span') %>%
  html_text() %>%
  str_replace_all(.,'[^0-9]','')


"150551" %>%
  extract_from_realty
