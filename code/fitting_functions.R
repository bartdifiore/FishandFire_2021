q.glmer <- quietly(glmer)
q.lmer <- quietly(lmer)

compile_results <- function(y, data){
  response = as.name(y)
  form = substitute(response ~ treatment * (scale(discharge.log) + scale(drying.sqrt)) + (1|code) + (1|year)) # model form
  if(y %in% c("mollusks", "no_wings", "turbellaria") == T){
    mod.temp = q.glmer(form, data[data$burn_debris != "bdf", ], family = "poisson")
    mod.df = tidy(mod.temp$result) %>% 
      mutate(response = y, .before = effect) %>% 
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
    mod.df
  } else {
    if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the repsonse is integer data, but a linear mixed effects model if the response is non-integer
      mod.temp = q.glmer(form, data, family = "poisson")
      mod.df = tidy(mod.temp$result) %>% 
        mutate(response = y, .before = effect) %>% 
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
      mod.df
    } else{
      mod.temp = q.lmer(form, data)
      mod.df = tidy(mod.temp$result) %>% 
        mutate(response = y, .before = effect) %>% 
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
      mod.df
    }}
}



predict_glmer <- function(y, data){
    response = as.name(y)
    
    form = substitute(response ~ treatment * (scale(discharge.log) + scale(drying.sqrt)) + (1|code) + (1|year)) # model form
    
    if(y == c("mollusks")){
      # run the model
      mod.temp = q.glmer(form, data[data$treatment != "BDFTL", ], family = "poisson")
      
      # estimate post hoc comparisons      
      post.hoc = summary(glht(mod.temp$result, mcp(treatment=c("UNT - UNTL = 0", 
                                                                 "BRITL - UNTL = 0", 
                                                                 "BRBTL - UNTL = 0"))))
      df.post = data.frame(response = y,
                           x = c("UNT", "BRITL", "BRBTL"),
                           comparison = names(post.hoc$test$coefficients), 
                           difference = post.hoc$test$coefficients,
                           pvalue = round(post.hoc$test$pvalues,8)
      )
      
      dummy.row <- data.frame(response = y, x = "BDFTL", predicted = 0, std.error = NA, 
                              conf.low = NA, conf.high = NA, group = as.factor("1"), warning = "NO", comparison = "NA", difference = NA, pvalue = NA)
      # run the model predictions and join the posthoc comparisons
      ggpredict(mod.temp$result, terms = ~ treatment) %>% 
        as.data.frame() %>% 
        mutate(response = y, .before = x) %>%
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
        left_join(df.post) %>%
        bind_rows(dummy.row)
      
    } else{
      if(is.integer(data[,y]) == T ){ # this just fits a poisson glmer if the response is integer data, but a linear mixed effects model if the response is non-integer
        
        # run the model
        mod.temp = q.glmer(form, data, family = "poisson")
        
        # estimate post hoc comparisons      
        post.hoc = summary(glht(mod.temp$result, mcp(treatment=c("UNT - UNTL = 0", 
                                                                 "BRITL - UNTL = 0", 
                                                                 "BRBTL - UNTL = 0", 
                                                                 "BDFTL - UNTL = 0"))))
        df.post = data.frame(response = y,
                             x = c("UNT", "BRITL", "BRBTL", "BDFTL"),
                             comparison = names(post.hoc$test$coefficients), 
                             difference = post.hoc$test$coefficients,
                             pvalue = round(post.hoc$test$pvalues,8)
        )
        
        # run the model predictions and join the posthoc comparisons
        ggpredict(mod.temp$result, terms = ~ treatment) %>% 
          as.data.frame() %>% 
          mutate(response = y, .before = x) %>%
          mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
          left_join(df.post)
      } else{
        
        # run the model
        mod.temp = q.lmer(form, data)
        
        # estimate post hoc comparisons      
        post.hoc = summary(glht(mod.temp$result, mcp(treatment=c("UNT - UNTL = 0", 
                                                                 "BRITL - UNTL = 0", 
                                                                 "BRBTL - UNTL = 0", 
                                                                 "BDFTL - UNTL = 0"))))
        df.post = data.frame(response = y,
                             x = c("UNT", "BRITL", "BRBTL", "BDFTL"),
                             comparison = names(post.hoc$test$coefficients), 
                             difference = post.hoc$test$coefficients,
                             pvalue = round(post.hoc$test$pvalues,8)
        )
        ggpredict(mod.temp$result, terms = ~ treatment) %>% 
          as.data.frame() %>% 
          mutate(response = y, .before = x) %>%
          mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
          left_join(df.post)
      }}
}
``