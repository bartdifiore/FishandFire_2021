fit_trout <- function(y, data){
  response = as.name(y)
  form = substitute(response ~ trout * scale(log.discharge) + (1|code) + (1|year)) # model form
  if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the response is integer data, but a linear mixed effects model if the response is non-integer
    
    # run the model
    mod1 = glmer(form, data, family = "poisson")
    mod2 = update(mod1, . ~ trout + scale(log.discharge) + (1|code) + (1|year))
    mod3 = update(mod1, . ~ trout + (1|code) + (1|year))
    
  } else{
    
    # run the model
    mod1 = lmer(form, data, REML = F)
    mod2 = update(mod1, . ~ trout + scale(log.discharge) + (1|code) + (1|year))
    mod3 = update(mod1, . ~ trout + (1|code) + (1|year))
    
  }
  
  # compare by AIC
  temp.list = list(mod1 = mod1, mod2 = mod2, mod3 = mod3)
  Aic_tab <- AICcmodavg::aictab(temp.list)
  
  best_models = data.frame(models = Aic_tab$Modnames[Aic_tab$Delta_AICc < 4]) %>% 
    separate(models, into = c("junk", "number"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
    filter(number == max(number)) %>%
    mutate(mod = paste(junk, number, sep = ""))
  
  best_model <- best_models$mod

  candidate_model = temp.list[[best_model]]
  
  out <- list(aic_tab = Aic_tab, 
              best_model = candidate_model, 
              model_name = best_model)
  out
}


plot_trout <- function(response, list, ylab){
  
  if(list[[response]]$model_name == "mod2" | list[[response]]$model_name == "mod3"){
    
    sig <- summary(list[[response]]$best_model)$coefficients[2,4]
    
    df.temp <- as.data.frame(ggpredict(list[[response]]$best_model, terms = ~trout)) %>% 
      rename(trout = x)
    
    var <- sym(response)
    
    p <- ggplot(df, aes(x = trout, y = !!var))+
      geom_jitter(aes(color = trout), show.legend = F, alpha = 0.5)+
      scale_color_manual(values = c("#d72a36", "#387fb7"))+
      geom_errorbar(data = df.temp, aes(ymin = conf.low, ymax = conf.high, y = predicted, x = trout), color = "gray30", width = 0.2)+
      geom_point(data = df.temp, aes(x = trout, y = predicted), color = "black", size = 3)+
      annotate("text", x = 1, y = max(df[, c(response)])*0.8, label = ifelse(sig > 0.05, "NS", "p < 0.05"))+
      labs(x = "Trout", y = ylab, title = "")+
      theme_bd()
    
    
    return(p)
    
  } 
  
  if(list[[response]]$model_name == "mod1"){
    
    sig <- summary(list[[response]]$best_model)$coefficients[4,4]
    
    df.temp2 <- ggpredict(list[[response]]$best_model, terms = ~log.discharge*trout)
    p2 <- plot(df.temp2, add.data = T)+
      labs(x = "Log (Discharge)", y = "", title = "", color = "Trout")+
      annotate("text", x = -7.5, y = max(df[, c(response)])*0.8, label = ifelse(sig > 0.05, "NS", "p < 0.05"))+
      theme_bd()
    
    lims = layer_scales(p2)$y$range$range
    
    df.temp1 <- as.data.frame(ggpredict(list[[response]]$best_model, terms = ~trout)) %>% 
      rename(trout = x)
    
    var <- sym(response)
    
    p1 <- ggplot(df, aes(x = trout, y = !!var))+
      geom_jitter(aes(color = trout), show.legend = F, alpha = 0.5)+
      scale_color_manual(values = c("#d72a36", "#387fb7"))+
      geom_errorbar(data = df.temp1, aes(ymin = conf.low, ymax = conf.high, y = predicted, x = trout), color = "gray30", width = 0.2)+
      geom_point(data = df.temp1, aes(x = trout, y = predicted), color = "black", size = 3)+
      coord_cartesian(ylim = lims)+
      labs(x = "Trout", y = ylab, title = "")+
      theme_bd()
    
    p  <- cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.33, 0.66))
    return(p)
  }
  
}






















