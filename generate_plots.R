library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(kableExtra)
library(ggpubr)

read_one_file <- function(fname){
fname2 <- paste("plotting_data",fname,sep="/")
D <- read_csv(
  fname2
) %>%
  mutate_at(
    vars(
      starts_with("FV")
    ),
    funs(as.character)
  ) %>%
  mutate_at(
    vars(
    starts_with("DI")
    ),
    ~sub(
      "False|AskAPatient", NA, .
    )
  ) %>%
  pivot_longer(
    starts_with(c("DI","FV")),
    names_to = "Metric",
    values_to="Value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    Metric = str_replace(Metric, "_bin", "")
  ) %>%
  mutate(
    finetune=str_split_fixed(model, "_", 5)[,4]
  ) %>%
  mutate(
    model=str_split_fixed(model, "_", 6)[,5]
  ) %>%
  mutate(
    Metric = replace( 
      Metric,
      (str_detect(DV,"AskAPatient")) & (str_detect(Metric, "Race")),
      "DI_Age_Gender"
    )
  ) %>%
  rowwise() %>%
  mutate(
    num_demos=length(strsplit(Metric, "_")[[1]])-1
  ) %>%
  ungroup() %>%
  filter(
    str_detect(Metric,"Gender")
  ) %>%
  select(
    !c(DV)
  ) %>%
  rename(
    "DV" = "wordlists",
    "Dataset"="debiasing"
  ) %>%
  mutate(
    Dataset = replace( 
      Dataset,
      is.na(Dataset),
      "AskAPatient"
    )
  ) %>%
  mutate(
    DV = replace( 
      DV,
      is.na(DV),
      "AskAPatient"
    )
  ) 
  return(D)
}

fnames <- c(
  "results_bert_acl22_AAP.csv",
  "results_bert_acl22_HS.csv",
  "results_bert_acl22_Psych_FIPI.csv",
  "results_bert_acl22_MBTI.csv",
  "results_bert_acl22_MBTI_part2.csv",
  "results_bert_acl22_Psych_alternates.csv",
  "acl_AAP_CNN.csv",
  "acl_HS_CNN.csv"
)

D2 <- lapply(fnames, read_one_file)

D <- do.call(rbind,D2)



z <- D %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(
    DV = str_replace(DV, "AskAPatient", "Sentiment")
  ) %>%
  mutate(
    DV = str_replace(DV, "Phys", "")
  ) %>%
  mutate(
    DV = str_replace(DV, "SubjectiveLit", "Literacy")
  ) %>%
  mutate(
    FairnessMetric = str_sub(Metric,end=2)
  ) %>%
  group_by(
    model, MSE, `Pearson R`, F1, AUC, Dataset, DV, finetune, num_demos, fullN, adjustedDI, FairnessMetric
  ) %>%
  summarize(
    ci = list(mean_cl_normal(Value) %>% 
            rename(mean=y, lwr=ymin, upr=ymax)),
    n=n(),
    min_val = min(Value),
    max_val= max(Value)
  ) %>%
  mutate(
    furthest1 = if_else(
      abs(1-min_val) > abs(1-max_val),
      min_val,
      max_val
    )
    )%>% 
  mutate(
    furthest1TABLE = if_else(
      abs(min_val-1) > abs(max_val-1),
      min_val-1,
      max_val-1
    )
  )%>%
  mutate(
    furthest1 = if_else(
      FairnessMetric=="FV",
      max_val,
      furthest1
    )
  )%>%
  unnest %>%
  rename(
    "Task" = "DV"
  ) %>%
  mutate(
    Task = str_replace(
      Task, "HateSpeech", "Hatespeech"
    )
  )



plot_one_dataset <- function(dataset.name, aDI, fN){
  cols <- c("PT" = "red","BERTPTD" = "blue","CNNPTD" = "darkgreen","RoBERTaPTD"="blue", "RoBERTaPT"="red")
  shapes <- c("PT" = 1,"BERTPTD" = 2,"CNNPTD" = 3, "RoBERTaPTD"=2, "RoBERTaPT"=1)
  
  
  data.plot <- z %>%
    mutate(
      gridname = str_glue("{Task}")
    ) %>%
    mutate(
      debias = str_glue("{model}{finetune}")
    ) %>%
    mutate(
      debias = str_replace(debias, "CNNPT$", "PT")
    ) %>%
    mutate(
      debias = str_replace(debias, "BERTPT$", "PT")
    ) %>%
    filter(FairnessMetric=="DI") %>%
    filter(model == dataset.name) %>%
    filter(fullN==fN) %>%
    filter(
      Dataset !="FIPIShort"
    )%>%
    filter(
      !(Dataset =="FIPI"& str_detect(model, "BERT"))
    )%>%
    filter(
      Dataset !="MBTIShort"
    )%>%
    filter(
      finetune %in% c("PT", "PTD")
    ) %>%
    mutate(
      Task = str_to_title(Task)
    ) %>%
    filter(adjustedDI == aDI) %>%
    ggplot(
      aes(
        x=num_demos,
        y=mean,
        color=debias,
        shape=debias
      )
    ) + 
    geom_point(
      position=position_dodge(width=0.5)
    ) + 
    geom_line(
      position=position_dodge(width=0.5)
    ) +
    geom_errorbar(
      aes(ymin=min_val, ymax=max_val),
      position=position_dodge(width=0.5)
    )  +
    facet_wrap(
        ~ Task,
       ncol=3,
      scales="free_y"
    ) +
    geom_hline(
      yintercept=1.2, 
      linetype="dashed", 
      color = "gray"
    ) + 
    geom_hline(
      yintercept=0.8, 
      linetype="dashed", 
      color = "gray"
    ) +
    theme_bw() 
  
  if(dataset.name == "BERT"){
    data.plot <- data.plot +
      theme(legend.position="top") +
      scale_color_manual(
        name = "Debiasing", 
        values=cols,
        labels = c("None", "ContextED", "WordED"),
        limits = c("PT", "BERTPTD", "CNNPTD"),
      ) + 
      scale_shape_manual(
        name = "Debiasing", 
        values=shapes,
        labels = c("None", "ContextED", "WordED"),
        limits = c("PT", "BERTPTD", "CNNPTD"),
        
      ) +
      xlab("") + 
      ylab("")
  } else if (dataset.name == "RoBERTa"){
    data.plot <- data.plot +
      theme(legend.position="bottom") +
      scale_color_manual(
        name = "Debiasing", 
        values=cols,
        labels = c("None", "ContextED"),
        limits = c("RoBERTaPT", "RoBERTaPTD"),
        ) + 
      scale_shape_manual(
        name = "Debiasing", 
        values=shapes,
        labels = c("None", "ContextED"),
        limits = c("RoBERTaPT", "RoBERTaPTD"),
        
      ) +
      xlab("") + 
      ylab("Disparate Impact")
  } else{
    data.plot <- data.plot +
      theme(legend.title=element_blank()) +
      scale_color_manual(
        name = "Debiasing", 
        values=cols,
        labels = c("None", "ContextED", "WordED"),
        limits = c("PT", "BERTPTD", "CNNPTD"),
      ) + 
      scale_shape_manual(
        name = "Debiasing", 
        values=shapes,
        labels = c("None", "ContextED", "WordED"),
        limits = c("PT", "BERTPTD", "CNNPTD"),
        
      ) +
      xlab("Number of Demographic Characteristics Considered") + 
      ylab("")
  }
    #ggtitle(str_glue("Effect of intersectionality on Disparate Impact: {dataset.name}")) + 
  return(data.plot)
}

BERT.DI <- plot_one_dataset("BERT", F, FALSE)
BERT.ADI <- plot_one_dataset("BERT", T, FALSE)

RoBERTa.DI <- plot_one_dataset("RoBERTa", F, F)
RoBERTa.ADI<- plot_one_dataset("RoBERTa", T, F)
CNN.DI <- plot_one_dataset("CNN", T, F)
CNN.ADI <- plot_one_dataset("CNN", F, F)

ggarrange(
  BERT.ADI, 
#  RoBERTa.ADI,
  CNN.ADI,
  nrow=3,
  labels = c("BERT", "GloVe"),
  common.legend = TRUE, legend = "top",
  vjust=0.1
)

ggsave(
  "merged_ADI_new.pdf", 
  width=8,
  height=18
)
knitr::plot_crop("merged_ADI_new.pdf")


ggarrange(
  BERT.DI, 
  #RoBERTa.DI,
  CNN.DI,
  nrow=3,
  labels = c("BERT", "word2vec"),
  common.legend = TRUE, legend = "top",
  vjust=0.1
)

ggsave(
  "merged_DI_new.pdf", 
  width=8,
  height=18
)
knitr::plot_crop("merged_DI_new.pdf")

ggsave(
  "RoBERTa_TRUE_FALSE_new.pdf", 
  RoBERTa.ADI,
  width=8,
  height=8
)
knitr::plot_crop("RoBERTa_TRUE_FALSE_new.pdf")


# Results Table (see notes for columns)

write_table <- function(adi){
z %>%
    mutate(
      num_demos=
      case_when(
        Task=="Hatespeech" & num_demos==3 ~ 5,
        Task=="Hatespeech" & num_demos==2 ~ 3,
        Task %in% c("perceiving", "thinking", "Sentiment") & num_demos==2 ~ 3,
        TRUE ~ num_demos
      )
    )%>%
  filter(
    num_demos %in% c(1,3,5)
  ) %>%
  filter(adjustedDI==adi) %>%
  filter(
    !(Dataset =="FIPI" & str_detect(model, "BERT"))
  )%>%
    filter(
      Dataset !="FIPIShort"
    )%>%
  filter(
    Dataset !="MBTIShort"
  )%>%
    filter(
      !str_detect(finetune,"(CDA)|(Dropout)")
    ) %>%
  ungroup() %>%
    mutate(
      mean = if_else(
        FairnessMetric=="DI",
        furthest1,
        mean
      )
    ) %>%
  mutate(
    ModelName = str_glue("{model}-{finetune}")
  ) %>%
  mutate(
    ModelName = str_replace(
      ModelName,
      "PTD",
      "debiased"
    )
  ) %>%
  mutate(
    ModelName = str_replace(
      ModelName,
      "PTDCDA",
      "CDA"
    )
  ) %>%
  mutate(
    ModelName = str_replace(
      ModelName,
      "PTDDropout",
      "Dropout"
    )
  ) %>%
  mutate(
    ModelName = str_replace(
      ModelName,
      "PT",
      "base"
    )
  ) %>%
  select(
    c(
      Task,
      ModelName,
#      model,
      MSE,
      `Pearson R`,
      F1,
      AUC,
      FairnessMetric,
      num_demos,
      mean
    )
  ) %>% 
  pivot_wider(
    names_from = c(FairnessMetric, num_demos),
    values_from = mean,
    names_glue = "{FairnessMetric} (Gender) + {num_demos-1}"
  ) %>%
  mutate_if(
    is.numeric,
    round,
    digits=2
  ) %>%
  rename(
    "DI" = "DI (Gender) + 0",
    "FV" = "FV (Gender) + 0",
    "DI+" = "DI (Gender) + 2",
    "FV+" = "FV (Gender) + 2",
    "DI++" ="DI (Gender) + 4",
    "FV++" = "FV (Gender) + 4",
    `Pearson\'s r` = `Pearson R`
  ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "CNN",
        "word2vec"
      )
    ) %>%
  arrange(
    Task,
    ModelName
  ) %>%
  relocate(
    starts_with("FV"),
    .after = last_col()
  ) %>%
  group_by(Task) %>%
  mutate(
    across(
      c(MSE)
    ,
    ~ cell_spec(
      ., 
      "latex", 
      bold=case_when(
        .==min(.)~T, 
        TRUE~F
      )
    )
    )
  ) %>%
  mutate(
    across(
      c(
        `Pearson\'s r`, F1, AUC
      ), 
      ~ cell_spec(
        ., 
        "latex", 
        bold=case_when(
          .==max(.)~T, 
          TRUE ~ F
          )
        )
      )
    ) %>%
  mutate(
    across(
      starts_with(
        "DI"
      ), 
      ~ cell_spec(
        ., 
        "latex", 
        bold=case_when(
          abs(.-1)==max(abs(.-1))~T, 
          TRUE ~ F
        )
      )
    )
  ) %>%
  mutate(
    across(
      starts_with(
        "FV"
      ), 
      ~ cell_spec(
        ., 
        "latex", 
        bold=case_when(
          .==max(.)~T, 
          TRUE ~ F
        )
      )
    )
  ) %>%
  mutate_all(
    ~case_when(
      .==0 ~ "-",
      .=="\\textbf{\\textbf{0}}"~"-",
      .=="\\textbf{0}"~"-",
      .=="NA" ~ "-",
      TRUE ~ .
    )
  ) %>%
    mutate(
      Task = str_to_title(Task)
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "-base",
        ""
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "-debiased",
        "-D"
      )
    ) %>%
  kbl(., 
    format="latex",
    booktabs = T, 
    align = "c",
    escape=F
  ) %>%
  column_spec(
    c(1,2), 
    bold=T
  ) %>%
  collapse_rows(
    columns = c(1,2), 
    latex_hline = "major", 
    valign = "middle"
    #target=2
  ) %>%
  save_kable(
    str_glue(
    "table1_{adi}.tex"
    )
  )
}

write_table(TRUE)
write_table(FALSE)




plot_BERT_Psych <- function(){
  data.plot <- z %>%
    mutate(
      gridname = str_glue("{Task}")
    ) %>%
    filter(FairnessMetric=="DI") %>%
    filter(model == "BERT") %>%
    filter(fullN==FALSE) %>%
    filter(
      Dataset =="Psychometric"
    )%>%
     mutate(
      Task = str_to_title(Task)
    ) %>%
    filter(adjustedDI == TRUE) %>%
    ggplot(
      aes(
        x=num_demos,
        y=mean,
        color=finetune,
        shape=finetune
      )
    ) + 
    geom_point(
      position=position_dodge(width=0.5)
    ) + 
    geom_line(
      position=position_dodge(width=0.5)
    ) +
    geom_errorbar(
      aes(ymin=min_val, ymax=max_val),
      position=position_dodge(width=0.5)
    )  +
    facet_wrap(
      ~ gridname,
      ncol=2,
      scales="free_y"
    ) +
    geom_hline(
      yintercept=1.2, 
      linetype="dashed", 
      color = "gray"
    ) + 
    geom_hline(
      yintercept=0.8, 
      linetype="dashed", 
      color = "gray"
    ) +
    theme_bw() 
  
    data.plot <- data.plot +
      theme(legend.position="bottom") +
      scale_color_discrete(name = "Debiasing", labels = c("None", "ContextED", "CDA", "Dropout")) + 
      scale_shape_discrete(name = "Debiasing", labels = c("None", "ContextED", "CDA", "Dropout")) +
      xlab("Number of Demographic Characteristics Considered") + 
      ylab("Disparate Impact") +
      ggtitle("Effect of debiasing and intersectionality on Disparate Impact: BERT") 
  return(data.plot)
}

p <- plot_BERT_Psych()

ggsave(
  "BERT_debias_ADI_new.pdf", 
  p,
height=6,
width=8
)



write_tablecsv <- function(adi){
  z %>%
    mutate(
      num_demos=
        case_when(
          Task=="Hatespeech" & num_demos==3 ~ 5,
          Task=="Hatespeech" & num_demos==2 ~ 3,
          Task %in% c("perceiving", "thinking", "Sentiment") & num_demos==2 ~ 3,
          TRUE ~ num_demos
        )
    )%>%
    filter(
      num_demos %in% c(1,3,5)
    ) %>%
    filter(adjustedDI==adi) %>%
    filter(
      !(Dataset =="FIPI" & str_detect(model, "BERT"))
    )%>%
    filter(
      Dataset !="FIPIShort"
    )%>%
    filter(
      Dataset !="MBTIShort"
    )%>%
    filter(
      !str_detect(finetune,"(CDA)|(Dropout)")
    ) %>%
    ungroup() %>%
    mutate(
      mean = if_else(
        FairnessMetric=="DI",
        furthest1,
        mean
      )
    ) %>%
    mutate(
      ModelName = str_glue("{model}-{finetune}")
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName,
        "PTD",
        "debiased"
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName,
        "PTDCDA",
        "CDA"
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName,
        "PTDDropout",
        "Dropout"
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName,
        "PT",
        "base"
      )
    ) %>%
    select(
      c(
        Task,
        ModelName,
        #      model,
        MSE,
        `Pearson R`,
        F1,
        AUC,
        FairnessMetric,
        num_demos,
        mean
      )
    ) %>% 
    pivot_wider(
      names_from = c(FairnessMetric, num_demos),
      values_from = mean,
      names_glue = "{FairnessMetric} (Gender) + {num_demos-1}"
    ) %>%
    mutate_if(
      is.numeric,
      round,
      digits=2
    ) %>%
    rename(
      "DI" = "DI (Gender) + 0",
      "FV" = "FV (Gender) + 0",
      "DI+" = "DI (Gender) + 2",
      "FV+" = "FV (Gender) + 2",
      "DI++" ="DI (Gender) + 4",
      "FV++" = "FV (Gender) + 4",
      `Pearson r` = `Pearson R`
    ) %>%
    arrange(
      Task,
      ModelName
    ) %>%
    relocate(
      starts_with("FV"),
      .after = last_col()
    ) %>%
    group_by(Task) %>%
    mutate(
      Task = str_to_title(Task)
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "CNN",
        "word2vec"
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "-base",
        ""
      )
    ) %>%
    mutate(
      ModelName = str_replace(
        ModelName, 
        "-debiased",
        "-D"
      )
    ) %>%
    write_csv("table_data.csv")
}

write_tablecsv(TRUE)
write_tablecsv(FALSE)

