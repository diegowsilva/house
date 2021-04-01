get_realty_id = function(source_input,ix){
  source_input %>%
    html_nodes(xpath = paste0('//*[@id="realty-list"]/div[',ix,']/div/div[3]/div/div[2]/a/span')) %>%
    html_text() %>%
    str_replace_all(.,'[^0-9]','') %>% return
}

extract_from_realty = function(id_realty_input){
  #id_realty_input = "1130"
  if(file.exists(paste0("data/",id_realty_input,".rds"))){
    message(paste0("> old realty: ",id_realty_input))
    readRDS(paste0("data/",id_realty_input,".rds")) %>% return
  }
  else{
    closeAllConnections()
    message(paste0("> new realty: ",id_realty_input))
    source_input = paste0("https://www.lelloimoveis.com.br/imovel/",id_realty_input) %>%
      read_html()
    closeAllConnections()
    realty_type = source_input %>%
      html_node(.,".title") %>%
      html_text() %>%
      str_split(.,"\\|") %>%
      .[[1]] %>% .[1] %>%
      str_replace_all("\\s{2,}","") %>%
      str_split(" para ") %>%
      .[[1]] %>%
      trimws()
    
    realty_type = data.frame(t(realty_type))
    if(length(realty_type)==2){
      names(realty_type) = c("realty_type","realty_condition") 
    }
    
    realty_nhood = source_input %>%
      html_node(.,".title") %>%
      html_text() %>%
      str_split(.,"(\\|)|(\n)") %>%
      .[[1]] %>% .[3] %>%
      trimws()
    if(length(realty_nhood)==0){
      realty_nhood=NA
    }
    
    ## Valor
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[2]/div/p
    realty_value = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[2]/div/p') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric %>%
      '/'(100)
    
    if(length(realty_value)==0){
      realty_value=NA
    }
    
    ## Condominio
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[3]/div[1]/p/b[2]
    realty_cond = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[3]/div[1]/p/b[2]') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric %>%
      '/'(100)
    
    if(length(realty_cond)==0){
      realty_cond=NA
    }
    
    ## IPTU
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[3]/div[2]/p/b[2]
    realty_iptu = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[3]/div[2]/p/b[2]') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric %>%
      '/'(100)
    
    if(length(realty_iptu)==0){
      realty_iptu=NA
    }
    
    ## Area
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[1]/p/text()[1]
    realty_area = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[1]/p/text()[1]') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9\\.]','') %>%
      as.numeric
    
    if(length(realty_area)==0){
      realty_area=NA
    }
    
    ## Quartos 
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[2]/p/text()
    realty_rooms = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[2]/p/text()[1]') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric
    
    if(length(realty_rooms)==0){
      realty_rooms=NA
    }
    
    ## Banheiros
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[3]/p
    realty_baths = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[3]/p') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric
    
    if(length(realty_baths)==0){
      realty_baths=NA
    }
    
    ## Vagas
    # /html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[4]/p
    realty_parking = source_input %>%
      html_nodes(xpath = '/html/body/app-root/main/app-realty-detail-component/div/div[1]/div[2]/div[1]/div[2]/div[5]/div[4]/p') %>%
      html_text() %>%
      str_replace_all(.,'[^0-9]','') %>%
      as.numeric
    
    if(length(realty_parking)==0){
      realty_parking=NA
    }
    
    realty_data = c(realty_value
                    ,realty_cond
                    ,realty_iptu
                    ,realty_area
                    ,realty_rooms
                    ,realty_baths
                    ,realty_parking)
    
    if(sum(is.na(realty_data))==7){
    } else {
      out_df = data.frame(id_realty_input
                 ,realty_type[1]
                 ,realty_type[2]
                 ,realty_nhood
                 ,realty_value
                 ,realty_cond
                 ,realty_iptu
                 ,realty_area
                 ,realty_rooms
                 ,realty_baths
                 ,realty_parking)
      saveRDS(out_df,paste0("data/",id_realty_input,".rds"))
      out_df %>% return
    }
    
  }

  
}

get_list_by_page = function(base_url,n_page){
  #base_url=url
  #n_page = 6
  local_url = str_replace(base_url,"XYX",n_page %>% as.character())
  closeAllConnections()
  #local_url %>% message
  source = local_url %>%
    read_html()
  closeAllConnections()
  sapply(2:21,function(x) get_realty_id(source,x)) %>%
    unlist %>%
    return
}
