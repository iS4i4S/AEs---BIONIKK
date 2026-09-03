###Utility functions
library(ggplot2)
library(reshape2)
is.numeric0<-function(x) {
  identical(x, numeric(0))
}
plot_contingency<-function(data,
         variables = c("TLS_pos", "HBV"),
         hide_legend = F,
         text_size=6,
         create_labels = T,
         inverse_factor2=F,
         order_factor=NULL,
         palette = NA,
         test_name = c("chisq.test", "fisher.test")[1],
         labs = c(xlab = "",
                  ylab = "",
                  legend_title = ""),
         hide_test = F,
         title = ""){
  if(inverse_factor2){
    data[,variables[2]] <- factor(data[,variables[2]],levels=rev(levels(data[,variables[2]])))
  }
  cont_table <- table(data[,variables[1]],data[,variables[2]])
  #clean data table to avoid empty rows
  non_empty_rows <- rowSums(cont_table)!=0
  cont_table <- cont_table[non_empty_rows,]
  
  if(create_labels){
    #sapply(levels(factor(data[,variables[2]])), function(x) paste(variables[2],x,sep="_"))
    colnames(cont_table) <- sapply(levels(factor(data[,variables[2]])), function(x) paste(variables[2],x,sep="_"))
    rownames(cont_table) <- sapply(levels(factor(data[,variables[1]])), function(x) paste(variables[1],x,sep="_"))
  }
  if(test_name != "fisher.test" & test_name != "chisq.test"){
    stop("incorrect test, valid options are fisher.test or chisq.test")
  }
  if(test_name == "fisher.test"){
    error_test <-  if(class(try(fisher.test(cont_table),silent=T)) == "try-error"){
      test_res <- fisher.test(cont_table,simulate.p.value = T)
    }else{
      test_res <- fisher.test(cont_table)
    }
  }
  if(test_name == "chisq.test"){
    test_res <- chisq.test(cont_table)
  }
  #make sure there is a matrix (enough values in both sides)
  if(!is.null(dim(cont_table))){
    cont_table_perc <-apply(cont_table,1,function(x) {
      res <- x / sum(x) *100
    })
    cont_table_m <- melt(cont_table_perc)
    cont_table_m$Var1 <- factor(cont_table_m$Var1)
    p <- ggplot(data=cont_table_m, aes(x=Var2,y=value,fill=Var1,label=value))
    p <- p + geom_bar(stat="identity",width=0.95)
    #to reverse order of bars
    #p <- p + geom_bar(stat="identity",width=0.95,position = position_stack(reverse = TRUE))
    p <- p + geom_text(label = paste0(as.vector(t(cont_table)),"\n(",format(cont_table_m$value,digits = 1),"%)"),
                       position = position_stack(vjust = .5),size=text_size)
    p <- p + theme_minimal()
    
    if(any(!is.na(palette))){
      p <- p + scale_fill_manual(values = palette)
    }
    #p <- p + theme(legend.title = element_blank())#,axis.text.x = element_text(angle = 25, hjust = 1))
    p <- p + guides(fill=guide_legend(title=labs[3]))
    if(hide_test){
      p <- p + labs(subtitle=paste("p = ",format(test_res$p.value,digits=3),sep=" "),title=title,x=labs[1],y=labs[2])
      if(test_res$p.value < 0.05 ) p <- p + theme(plot.subtitle=element_text(face="bold"))
    }else{
      p <- p + labs(subtitle=paste(test_name, " p = ",format(test_res$p.value,digits=3),sep=" "),title=title,x=labs[1],y=labs[2])
      if(test_res$p.value < 0.05 ) p <- p + theme(plot.subtitle=element_text(face="bold"))
    }
    
    if(hide_legend){
      p <- p + guides(fill=FALSE)
    }
  }else{
    p <- ggplot()
    test_res <-1
  }
  return(list(p,test_res,cont_table))
  
}

############""
library(dplyr)
library(tidyr)

melt_df <- function(df, var_to_group, value_cols = NULL) {
  if (is.null(value_cols)) {
    value_cols <- names(df)[sapply(df, is.numeric)]
  }
  df %>%
    rename(groups = all_of(var_to_group)) %>%
    pivot_longer(cols = all_of(value_cols), names_to = "variable", values_to = "value") %>%
    as.data.frame()
}

# Arm order matches the figure's top-to-bottom panel order throughout.
comp_list <- c("Nivo-Ipi", "Nivo", "TKI")


# ============================================================
# Fig1/code/utils_plot_group_boxplot.R

# ============================================================
library(dplyr)
library(ggplot2)
library(ggsignif)
library(dunn.test)
plot_group_boxplot<-function(data.m,
         variable,
         plot_only_signif=FALSE,
         pval_thresh=0.05,
         specify_col=NA,
         palette="YlOrRd",
         title="",
         title_size=NA,
         show_paired=F,
         add_jitter=T,
         color_points_id=NULL,
         log_scale=F,
         violin=F,
         trim=F,
         size_point=2,
         plot_outlier=T,
         compare_groups=T,
         add_hline=NA,
         alpha=0.3,
         jitter_mode="none",
         hide_test =F,
         paired=F,
         do_test=T,
         ylim=NA,
         labs=c("title","value","value")){
  return_plot <- FALSE
  dodge <- position_dodge(width=0.9)
  p <- NULL
  ylim1 <- NULL
  row_id <- which(variable == data.m[,"variable"])
  id_per_group <- lapply(unique(data.m$groups),function(x) which(data.m[row_id,"groups"] == x))
  names(id_per_group) <- unique(data.m$groups)
  data_to_sub <- data.m[row_id,]
  value_per_group <- lapply(id_per_group,function(x) data_to_sub[x,"value"])
  
  #Get num id to colour points
  #num_ids <- color_points_id[which(unique(IHC_melted$variable) == variable),]
  
  #statistical test
  if(do_test){
    if(length(value_per_group) == 2 & !paired){
      test = suppressWarnings(wilcox.test(value_per_group[[1]],value_per_group[[2]]))
      test_name <- "Mann-Whitney test"
    }
    if(length(value_per_group) > 2) {
      remove_if_num0 <- !sapply(value_per_group,is.numeric0)
      test <- kruskal.test(value_per_group[remove_if_num0])
      test_name <-"Kruskal-Wallis test"
    }
    if(paired){
      #add Ids for strips
      data.m$IDs <- c(id_per_group[[1]],id_per_group[[1]])
      test = wilcox.test(value_per_group[[1]],value_per_group[[2]],paired=T)
      test_name <- "Paired Mann-Whitney test"
    }
    pval <- test$p.value
  }else{
    pval <- 1
    test_name <- "No test"
  }
  if(is.na(pval)){
    pval <- 1
    test_name <- "test failed "
  }
  
  if(plot_only_signif & pval <= pval_thresh) return_plot <-  TRUE
  if(plot_only_signif == FALSE ) return_plot <- TRUE
  if(return_plot){
    p <- ggplot(data = data.m[row_id,], aes(x=groups, y=value))
    p <- p + theme_bw()
    if(!is.na(title_size)){
      p <- p +   theme(plot.title = element_text(size = title_size))
    }
    if(all(!is.na(ylim))){
      p <- p + ylim(ylim)
    }
    if(any(is.na(specify_col))){
      p <- p + scale_fill_brewer(palette = palette)
    }else{
      p <- p + scale_fill_manual(values = specify_col)
    }
    if(violin){
      p <- p + geom_violin(aes(fill=groups), position = dodge,trim=trim) + geom_boxplot(outlier.alpha = 0.003,width=0.1, fill="white")
    }else{
      p <- p + geom_boxplot(aes(fill=groups), width=0.3,position = dodge,outlier.shape=NA)
    }
    if(show_paired){
      p <- p + geom_line(aes(group=IDs),colour=alpha("grey50", alpha = 0.7),linetype="11")
      p <- p + geom_point(data=data.m[row_id,],aes(x =groups, y =value),alpha=0.03, colour ="black",show.legend = F)
    }
    if(add_jitter){
      if(jitter_mode=="big"){
        p <- p + geom_jitter(position = position_jitter(height = 0.5,width = 0.3),size=size_point, alpha =alpha)
      }else{
        p <- p + geom_jitter(position = position_jitter(width = .05),size=size_point, alpha =alpha)
      }
    }
    if(!(is.na(add_hline))){
      p <- p + geom_hline(yintercept=add_hline, linetype="dashed", color = "red")
    }
    if(hide_test){
      if(pval < 1e-16){
        p <- p + labs(subtitle=paste(" p < 10e-16",sep=" "),title=paste(variable,labs[1]),x=labs[2],y=labs[3]) +  guides(fill=FALSE)
        p <- p + theme(plot.subtitle=element_text(face="bold"))
      }else{
        p <- p + labs(subtitle=paste(" p = ",format(pval,digits = 3),sep=" "),title=paste(variable,labs[1]),x=labs[2],y=labs[3]) +  guides(fill=FALSE)
        if(pval < 0.05 ) p <- p + theme(plot.subtitle=element_text(face="bold"))
      }
    }else{
      if(pval < 1e-16){
        p <- p + labs(subtitle=paste(test_name, " p < 10e-16",sep=" "),title=paste(variable,labs[1]),x=labs[2],y=labs[3]) +  guides(fill=FALSE)
        p <- p + theme(plot.subtitle=element_text(face="bold"))
      }else{
        p <- p + labs(subtitle=paste(test_name, " p = ",format(pval,digits = 3),sep=" "),title=paste(variable,labs[1]),x=labs[2],y=labs[3]) +  guides(fill=FALSE)
        if(pval < 0.05 ) p <- p + theme(plot.subtitle=element_text(face="bold"))
      }
    }
    p <- p + theme(plot.subtitle = element_text(size = 9))
    if(!is.null(color_points_id)){
      p <- p + geom_point(data=data.m[num_ids,],aes(x =groups, y =value, colour ="red"))
    }
    if(log_scale){
      p <- p + scale_y_continuous(trans = "log10",breaks = base_breaks(), labels = prettyNum) + theme(panel.grid.minor = element_blank())
      
    }
    if(!plot_outlier){
      ylim1[1] <- quantile(data.m[row_id,"value"],0.02,na.rm=T)
      ylim1[2] <- quantile(data.m[row_id,"value"],0.95,na.rm=T)
      p <- p + coord_cartesian(ylim =ylim1)
    }
    if(compare_groups & length(value_per_group) > 2){
      #remove cat from dunn.test
      dunn_test <- dunn.test(x=data.m[row_id,"value"],g=data.m[row_id,'groups'],method = "bh")
      comparisons <- sapply(dunn_test$comparisons,function(x) strsplit(x,split =" - "))
      adjp_dunn <- format(dunn_test$`P.adjusted`,digits = 2,trim = T)
      to_bold <- ifelse(as.numeric(adjp_dunn) < 0.05, paste( "bold('" , adjp_dunn,"')"),adjp_dunn)
      p <- p + geom_signif(annotations = to_bold,parse = T, comparisons = comparisons, step_increase = 0.1)
      
      names(adjp_dunn) <- dunn_test$comparisons
      res <- list(plot=p,main_pval=pval,main_test=test_name,comparisons_pval=adjp_dunn)
      return(res)
    }
  }
  res <- list(plot=p,main_pval=pval,main_test=test_name)
  return(res)
}

####
base_breaks<-function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = T, n = n)
  }
}


#####
run_survival_cutpoint_analysis<-function(
    data,
    time_col,
    event_col,
    vars,
    cut_method = c("optimal", "percentile"),
    percentile = 50,
    minprop = 0.2,
    multivariable = FALSE,
    controls = NULL
) {
  
  library(dplyr)
  library(survival)
  library(survminer)
  library(coxphf)
  library(ezcox)
  library(rlang)
  process_mv_fit <- function(mv_fit, var_name, cutoff_percentile, is_firth = FALSE) {
    if (is_firth) {
      df <- data.frame(
        Variable = var_name,
        Term = names(mv_fit$coefficients),
        HR = exp(mv_fit$coefficients),
        HR_low = mv_fit$ci.lower,
        HR_high = mv_fit$ci.upper,
        P_value = mv_fit$prob,
        Model = "Firth multivariable",
        Cutoff_percentile = round(cutoff_percentile, 1),
        # fill NA for columns present in ezcox
        # is_control = NA,
        #contrast_level = NA,
        #ref_level = NA,
        #n_contrast = NA,
        #n_ref = NA,
        stringsAsFactors = FALSE
      )
    } else {
      # Standard Cox / ezcox: use contrast_level as Term
      if (!"contrast_level" %in% colnames(mv_fit)) {
        mv_fit$contrast_level <- mv_fit$Term %||% mv_fit$covariate
      }
      df <- mv_fit %>%
        dplyr::mutate(
          Term = contrast_level,
          HR = exp(beta),
          HR_low = lower_95,
          HR_high = upper_95,
          P_value = p.value,
          Model = "Standard multivariable",
          Cutoff_percentile = round(cutoff_percentile, 1),
          Variable = !!var_name
        ) %>%
        dplyr::select(Variable, Term, HR, HR_low, HR_high, P_value, Model, Cutoff_percentile)
    }
    return(df)
  }
  
  
  
  cut_method <- match.arg(cut_method)
  
  time_col  <- enquo(time_col)
  event_col <- enquo(event_col)
  
  results_list <- list()
  km_plots <- list()
  mv_results <- list()
  
  for (v in vars) {
    
    message("Processing: ", v)
    
    # -----------------------------
    # Prepare data
    # -----------------------------
    df <- data %>%
      dplyr::select(
        !!time_col,
        !!event_col,
        all_of(v),
        all_of(controls)
      ) %>%
      dplyr::rename(
        time = !!time_col,
        event = !!event_col
      ) %>%
      filter(!is.na(.data[[v]]))
    
    if (nrow(df) < 10) next
    if (length(unique(df[[v]])) < 5) next
    
    # -----------------------------
    # Define cut-off
    # -----------------------------
    if (cut_method == "optimal") {
      
      cut <- tryCatch(
        surv_cutpoint(
          df,
          time = "time",
          event = "event",
          variables = v,
          minprop = minprop
        ),
        error = function(e) NULL
      )
      
      if (is.null(cut)) next
      
      #cut_val <- cut$cutpoint[[v]]
      cut_val <- cut$cutpoint$cutpoint;print(cut_val)
      cutoff_percentile <- ecdf(df[[v]])(cut_val) * 100
      
      # --- manually create group column ---
      df_cat <- df %>%
        mutate(
          group = factor(
            ifelse(.data[[v]] <= cut_val, "Low", "High"),
            levels = c("Low", "High")
          )
        )
      
    } else {
      
      cut_val <- as.numeric(
        quantile(df[[v]], probs = percentile / 100, na.rm = TRUE)
      )
      
      cutoff_percentile <- percentile
      
      df_cat <- df %>%
        dplyr::mutate(
          group = factor(
            ifelse(.data[[v]] <= cut_val, "Low", "High"),
            levels = c("Low", "High")
          )
        )
    }
    
    if (!"group" %in% colnames(df_cat)) next
    
    # -----------------------------
    # Group sizes & events
    # -----------------------------
    n_table <- table(df_cat$group)
    if (length(n_table) < 2) next
    
    event_table <- table(df_cat$group, df_cat$event)
    separation_detected <- any(event_table[, "1"] == 0)
    
    # -----------------------------
    # Log-rank test
    # -----------------------------
    surv_diff <- tryCatch(
      survdiff(Surv(time, event) ~ group, data = df_cat),
      error = function(e) NULL
    )
    
    p_val <- if (!is.null(surv_diff)) {
      1 - pchisq(surv_diff$chisq, df = 1)
    } else {
      NA
    }
    
    # -----------------------------
    # Univariate Cox
    # -----------------------------
    if (separation_detected) {
      
      cox <- tryCatch(
        coxphf(Surv(time, event) ~ group, data = df_cat),
        error = function(e) NULL
      )
      
      if (is.null(cox)) next
      
      HR <- exp(cox$coefficients)
      HR_low <- cox$ci.lower
      HR_high <- cox$ci.upper
      p_cox <- cox$prob
      model_type <- "Firth"
      
    } else {
      
      cox <- coxph(Surv(time, event) ~ group, data = df_cat)
      cox_sum <- summary(cox)
      
      HR <- cox_sum$coefficients[, "exp(coef)"]
      HR_low <- cox_sum$conf.int[, "lower .95"]
      HR_high <- cox_sum$conf.int[, "upper .95"]
      p_cox <- cox_sum$coefficients[, "Pr(>|z|)"]
      model_type <- "Standard"
    }
    
    # -----------------------------
    # KM fit
    # -----------------------------
    fit <- survfit(Surv(time, event) ~ group, data = df_cat)
    
    km_plots[[v]] <- ggsurvplot(
      fit,
      data = df_cat,
      risk.table = TRUE,
      pval = TRUE,
      legend.title = v,
      legend.labs = names(n_table),
      ggtheme = theme_classic(),
      title = paste0(
        "KM curve: ", v,
        " | ", cut_method,
        " (", round(cutoff_percentile, 1), "th pct)"
      )
    )
    
    # -----------------------------
    # Store univariate results
    # -----------------------------
    results_list[[v]] <- data.frame(
      Variable = v,
      Cut_method = cut_method,
      Cutoff = round(cut_val, 4),
      Cutoff_percentile = round(cutoff_percentile, 1),
      N_low = as.numeric(n_table["Low"]),
      N_high = as.numeric(n_table["High"]),
      P_value = signif(p_val, 3),
      HR = round(HR, 3),
      HR_low = round(HR_low, 3),
      HR_high = round(HR_high, 3),
      Cox_model = model_type,
      stringsAsFactors = FALSE
    )
    
    # -----------------------------
    # Multivariable Cox
    # -----------------------------
    if (multivariable && !is.null(controls)) {
      
      mv_formula <- as.formula(
        paste(
          "Surv(time, event) ~ group +",
          paste(controls, collapse = " + ")
        )
      )
      
      if (separation_detected) {
        
        mv_fit <- tryCatch(
          coxphf(mv_formula, data = df_cat[complete.cases(df_cat),]),
          error = function(e) NULL
        )
        
        if (!is.null(mv_fit)) {
          mv_results[[v]] <- process_mv_fit(mv_fit, v, cutoff_percentile, is_firth = TRUE)
        }
        
      } else {
        
        mv_fit <- tryCatch(
          ezcox(
            df_cat,
            covariates = "group",
            time = "time",
            status = "event",
            controls = controls
          ),
          error = function(e) NULL
        )
        
        if (!is.null(mv_fit)) {
          mv_results[[v]] <- process_mv_fit(mv_fit, v, cutoff_percentile, is_firth = FALSE)
        }
      }
    }
  }
  
  return(list(
    results = bind_rows(results_list),
    km_plots = km_plots,
    multivariable = bind_rows(mv_results)
  ))
}

