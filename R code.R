#WOMEN, TENURE, AND PERFORMANCE EXPLORING LEADERSHIP DYNAMICS IN BRAZILIAN CREDIT COOPERATIVES

#By 
#Arthur Frederico Lerner - arthurlerner_@hotmail.com
#Leonardo Flach
#Francisco Bravo Urquiza


#### INSTALLING PACKAGES ####
pacotes <- c("huxreg", "systemfit", "ivpack","robustbase", "tsibbledata","timetk", "magrittr", "base", "basictabler", "bibtex", "BiocManager", "bookdown", "caper", "car", "caret", "conflicted", "correlation", "corrplot", "cowplot", "DescTools", "distill", "dplyr", "encrptr", "factoextra", "FactoMineR", "faraway", "fastDummies", "flextable", "foreign", "gdata", "ggrepel", "ggtree", "ggplot2", "ggpubr", "graphics", "grid", "gridExtra", "gtsummary", "Hmisc", "httr2", "jsmodule", "jtools", "knitr", "kableExtra", "knitLatex", "lmtest", "lubridate", "magick", "margins", "marginaleffects", "MASS", "MatchIt", "mfx", "mgcv", "minqa", "modelr", "nnet", "nortest", "OddsPlotty", "papaja", "pandoc", "palmerpenguins", "performance", "pglm", "plm", "plotly", "pROC", "pscl", "psych", "rddtools", "readr", "regclass", "rJava", "readxl", "RefManageR", "remotes", "reshape2", "repos", "report", "ReporteRs", "rgl", "rlang", "rmarkdown", "Rmisc", "ROCR", "RSelenium", "scales", "sjlabelled", "stargazer", "stats", "stringr", "stringi", "texreg", "tidyr", "tidyverse", "tinytex", "tseries", "truncnorm", "visreg", "viridis", "xfun", "xlsx", "xtable", "wesanderson", "writexl", "vdr", "sandwich", "openintro", "OIdata", "doBy", "ivpack", "vtable", "summarytools",
             
             "htmltools", "DescTools", "sandwich", "lmtest", "car", "dplyr", "stargazer", "ggplot2", "foreign",
             
             "openintro","OIdata", "gdata", "doBy","ivpack", "psych","plm", "readxl", "vtable", "summarytools", "gtsummary", "AER","base", "basictabler", "bibtex","BiocManager", "bookdown","caper", "car","caret", "conflicted","correlation","corrplot","cowplot","DescTools","distill","dplyr","encrptr","factoextra","FactoMineR", "faraway","fastDummies","flextable","foreign","gdata", "ggrepel","ggtree", "ggplot2","ggpubr","graphics", "grid","gridExtra","gtsummary","Hmisc","httr2","jsmodule", "jtools","knitr","kableExtra","knitLatex","lmtest", "lubridate","magick","margins", "marginaleffects", "MASS", "MatchIt","mfx","mgcv","minqa","modelr" ,"mgcv","nnet","nortest","OddsPlotty","papaja", "pandoc","palmerpenguins","performance", "pglm","plm","plotly","plotly", "pROC","pscl","psych","rddtools","readr", "regclass","rJava", "readxl","RefManageR", "remotes", "reshape2", "repos", "report","ReporteRs","reshape2","rgl","rlang","rmarkdown","Rmisc","ROCR","RSelenium", "scales","sjlabelled", "stargazer","stats","stringr", "stringi","texreg","tidyr","tidyverse", "tinytex","tseries","truncnorm", "visreg","viridis", "xfun","xlsx","xtable","wesanderson", "writexl","vdr","gtsummary",
             
             "AER","base", "basictabler", "bibtex","BiocManager", "bookdown","caper", "car","caret", "conflicted","correlation","corrplot","cowplot","DescTools","distill","dplyr","encrptr","factoextra","FactoMineR", "faraway","fastDummies","flextable","foreign","gdata", "ggrepel","ggtree", "ggplot2","ggpubr","graphics", "grid","gridExtra","gtsummary","Hmisc","httr2","jsmodule", "jtools","knitr","kableExtra","knitLatex","lmtest", "lubridate","magick","margins", "marginaleffects", "MASS", "MatchIt","mfx","mgcv","minqa","modelr" ,"mgcv","nnet","nortest","OddsPlotty","papaja", "pandoc","palmerpenguins","performance", "pglm","plm","plotly","plotly", "pROC","pscl","psych","rddtools","readr", "regclass","rJava", "readxl","RefManageR", "remotes", "reshape2", "repos", "report","ReporteRs","reshape2","rgl","rlang","rmarkdown","Rmisc","ROCR","RSelenium", "scales","sjlabelled", "stargazer","stats","stringr", "stringi","texreg","tidyr","tidyverse", "tinytex","tseries","truncnorm", "visreg","viridis", "xfun","xlsx","xtable","wesanderson", "writexl","vdr", "modelsummary")
options(repos = "https://cran.rstudio.com/")
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

library(conflicted)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::filter)

#### IMPORTING FILE ####

library(readxl)
Data <- read_excel("C:/Users/Arthur/Desktop/Dados - Tese/Article 2 - thesis/Data.xlsx", 
                   col_types = c("date", "numeric", "numeric", 
                                 "text", "numeric", "text", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))


#### Winsorization ####

attach(Data)
Data$ROE_NW<-Winsorize(Data$ROE, probs = c(0.01,0.99), na.rm=T)
#Data$ROE_NW_2.5<-Winsorize(Data$ROE, probs = c(0.025,0.975), na.rm=T)
#Data$ROE_NW_5<-Winsorize(Data$ROE, probs = c(0.05,0.95), na.rm=T)

Data$ROA_NW<-Winsorize(Data$ROA, probs = c(0.01,0.99), na.rm=T)
#Data$ROA_NW_2.5<-Winsorize(Data$ROA, probs = c(0.025,0.975), na.rm=T)
#Data$ROA_NW_5<-Winsorize(Data$ROA, probs = c(0.05,0.95), na.rm=T)

Data$LLPRatio_NW<-Winsorize(Data$LLPRatio, probs = c(0.01,0.99), na.rm=T)
Data$NIM_NW<-Winsorize(Data$NIM, probs = c(0.01,0.99), na.rm=T)
Data$CostToIncome_NW<-Winsorize(Data$CostToIncome, probs = c(0.01,0.99), na.rm=T)
Data$PER_NW<-Winsorize(Data$PER, probs = c(0.01,0.99), na.rm=T)

Data$BoardSizeBOD_NW<-Winsorize(Data$BoardSizeBOD, probs = c(0.01,0.99), na.rm=T)
Data$BoardSizeEM_NW<-Winsorize(Data$BoardSizeEM, probs = c(0.01,0.99), na.rm=T)

Data$TenureBOD_NW<-Winsorize(Data$TenureBOD, probs = c(0.01,0.99), na.rm=T)
Data$TenureEM_NW<-Winsorize(Data$TenureEM, probs = c(0.01,0.99), na.rm=T)


detach(Data)

summary(Data)



######COVID######
# Supondo que seu dataframe seja chamado 'Data' e que você tenha uma coluna chamada 'Year' com os anos

# Criar a variável dummy 'covid' para os anos de 2020, 2021 e 2022
Data$Covid <- ifelse(Data$Year %in% c(2020, 2021, 2022), 1, 0)


# Carregando apenas o dplyr
library(dplyr)


#####CRIANDO LAGS########

Data <- Data %>%
  dplyr::arrange(Codigo, Date) %>%  # Ordena por Codigo e Data
  group_by(Codigo) %>%
  dplyr::mutate(
    PWOM_BOD_Lag1 = dplyr::lag(PWOM_BOD, n = 1, default = NA),
    PWOM_BOD_Lag2 = dplyr::lag(PWOM_BOD, n = 2, default = NA),
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    PWOM_SB_Lag1 = dplyr::lag(PWOM_SB, n = 1, default = NA),
    PWOM_SB_Lag2 = dplyr::lag(PWOM_SB, n = 2, default = NA),
    
    PWOM_BOD_Lag3 = dplyr::lag(PWOM_BOD, n = 3, default = NA),
    PWOM_BOD_Lag4 = dplyr::lag(PWOM_BOD, n = 4, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    PWOM_SB_Lag3 = dplyr::lag(PWOM_SB, n = 3, default = NA),
    PWOM_SB_Lag4 = dplyr::lag(PWOM_SB, n = 4, default = NA),
    
    BLAU_BOD_Lag1 = dplyr::lag(BLAU_BOD, n = 1, default = NA),
    BLAU_BOD_Lag2 = dplyr::lag(BLAU_BOD, n = 2, default = NA),
    BLAU_BOD_Lag3 = dplyr::lag(BLAU_BOD, n = 3, default = NA),
    BLAU_BOD_Lag4 = dplyr::lag(BLAU_BOD, n = 4, default = NA),
    
    BLAU_EM_Lag1 = dplyr::lag(BLAU_EM, n = 1, default = NA),
    BLAU_EM_Lag2 = dplyr::lag(BLAU_EM, n = 2, default = NA),
    BLAU_EM_Lag3 = dplyr::lag(BLAU_EM, n = 3, default = NA),
    BLAU_EM_Lag4 = dplyr::lag(BLAU_EM, n = 4, default = NA),
    
    
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    
    PWOM_SB_Lag1 = dplyr::lag(PWOM_SB, n = 1, default = NA),
    PWOM_SB_Lag2 = dplyr::lag(PWOM_SB, n = 2, default = NA),
    PWOM_SB_Lag3 = dplyr::lag(PWOM_SB, n = 3, default = NA),
    PWOM_SB_Lag4 = dplyr::lag(PWOM_SB, n = 4, default = NA),
    
    ROE_NW_Lag1 = dplyr::lag(ROE_NW, n = 1, default = NA),
    ROE_NW_Lag2 = dplyr::lag(ROE_NW, n = 2, default = NA),
    ROE_NW_Lag3 = dplyr::lag(ROE_NW, n = 3, default = NA),
    ROE_NW_Lag4 = dplyr::lag(ROE_NW, n = 4, default = NA),
    
    ROA_NW_Lag1 = dplyr::lag(ROA_NW, n = 1, default = NA),
    ROA_NW_Lag2 = dplyr::lag(ROA_NW, n = 2, default = NA),
    ROA_NW_Lag3 = dplyr::lag(ROA_NW, n = 3, default = NA),
    ROA_NW_Lag4 = dplyr::lag(ROA_NW, n = 4, default = NA),
    
    LLPRatio_NW_Lag1 = dplyr::lag(LLPRatio_NW, n = 1, default = NA),
    LLPRatio_NW_Lag2 = dplyr::lag(LLPRatio_NW, n = 2, default = NA),
    LLPRatio_NW_Lag3 = dplyr::lag(LLPRatio_NW, n = 3, default = NA),
    LLPRatio_NW_Lag4 = dplyr::lag(LLPRatio_NW, n = 4, default = NA),
    
    NIM_NW_Lag1 = dplyr::lag(NIM_NW, n = 1, default = NA),
    NIM_NW_Lag2 = dplyr::lag(NIM_NW, n = 2, default = NA),
    NIM_NW_Lag3 = dplyr::lag(NIM_NW, n = 3, default = NA),
    NIM_NW_Lag4 = dplyr::lag(NIM_NW, n = 4, default = NA),
    
    CostToIncome_NW_Lag1 = dplyr::lag(CostToIncome_NW, n = 1, default = NA),
    CostToIncome_NW_Lag2 = dplyr::lag(CostToIncome_NW, n = 2, default = NA),
    CostToIncome_NW_Lag3 = dplyr::lag(CostToIncome_NW, n = 3, default = NA),
    CostToIncome_NW_Lag4 = dplyr::lag(CostToIncome_NW, n = 4, default = NA),
    
    PER_NW_Lag1 = dplyr::lag(PER_NW, n = 1, default = NA),
    PER_NW_Lag2 = dplyr::lag(PER_NW, n = 2, default = NA),
    PER_NW_Lag3 = dplyr::lag(PER_NW, n = 3, default = NA),
    PER_NW_Lag4 = dplyr::lag(PER_NW, n = 4, default = NA),
    
    SHAN_BOD_Lag1 = dplyr::lag(SHAN_BOD, n = 1, default = NA),
    SHAN_BOD_Lag2 = dplyr::lag(SHAN_BOD, n = 2, default = NA),
    SHAN_BOD_Lag3 = dplyr::lag(SHAN_BOD, n = 3, default = NA),
    SHAN_BOD_Lag4 = dplyr::lag(SHAN_BOD, n = 4, default = NA),
    
    SHAN_EM_Lag1 = dplyr::lag(SHAN_EM, n = 1, default = NA),
    SHAN_EM_Lag2 = dplyr::lag(SHAN_EM, n = 2, default = NA),
    SHAN_EM_Lag3 = dplyr::lag(SHAN_EM, n = 3, default = NA),
    SHAN_EM_Lag4 = dplyr::lag(SHAN_EM, n = 4, default = NA)
  
  )


summary(Data)

#######excluindo os anos 2005, 2006 e 2007######
# Carregar o pacote dplyr
library(dplyr)

# Criar um dataframe excluindo os anos 2005, 2006 e 2007
Data_Tenure <- Data[!(Data$Year %in% c(2005, 2006, 2007)), ]

summary(Data_Tenure)


# Calcular médias regionais por ano
Data_Tenure <- Data_Tenure %>%
  group_by(RegiaoDeAtuacao, Year) %>%
  mutate(
    media_regional_pwon_bod = mean(PWOM_BOD, na.rm = TRUE),
    media_regional_pwon_em = mean(PWOM_EM, na.rm = TRUE)
  ) %>%
  ungroup()


#view(Data_Tenure)

summary(Data_Tenure)


####YEAR DUMMIES####

# Assumindo que Data_Tenure já está carregado e tem uma coluna 'Year' de 2008 a 2022

# Passo 1: Converter o ano para fator (caso ainda não seja)
Data_Tenure$Year <- as.factor(Data_Tenure$Year)

# Passo 2: Criar as dummies
library(fastDummies)
Data_Tenure <- dummy_cols(Data_Tenure, select_columns = "Year", remove_first_dummy = TRUE)



# Exiba os primeiros dados para verificar
summary(Data_Tenure)


# removendo os NA das colunas especificadas
Data_Tenure <- Data_Tenure %>%
  plotly::filter(!is.na(CCLeverage) & !is.na(CCSize))


Data_Tenure <- Data_Tenure %>%
  filter(Mouth == 12)


# Função para aplicar transformação logarítmica com tratamento de zeros e negativos
transform_log_variables <- function(df) {
  # Criando uma cópia do dataframe original
  df_transformed <- df
  
  # Lista de variáveis para transformação
  variables <- c("BoardSizeBOD", "BoardSizeEM", "TenureBOD", "TenureEM")
  
  # Aplicando ln para cada variável
  for(var in variables) {
    # Verificando se a variável existe no dataframe
    if(var %in% names(df)) {
      # Adicionando 1 para evitar log(0) e preservar a nova variável
      df_transformed[[paste0("ln_", var)]] <- log(df[[var]] + 1)
      
      # Imprimindo sumário da transformação
      cat("\nTransformação para", var, ":\n")
      cat("Mínimo original:", min(df[[var]], na.rm = TRUE), "\n")
      cat("Máximo original:", max(df[[var]], na.rm = TRUE), "\n")
      cat("Mínimo após ln:", min(df_transformed[[paste0("ln_", var)]], na.rm = TRUE), "\n")
      cat("Máximo após ln:", max(df_transformed[[paste0("ln_", var)]], na.rm = TRUE), "\n")
    } else {
      warning(paste("Variável", var, "não encontrada no dataframe"))
    }
  }
  
  return(df_transformed)
}

# Aplicando a transformação
Data_Tenure <- transform_log_variables(Data_Tenure)

# Verificação da estrutura do novo dataframe
str(Data_Tenure[,c("BoardSizeBOD", "BoardSizeEM", "TenureBOD", "TenureEM",
                               "ln_BoardSizeBOD", "ln_BoardSizeEM", "ln_TenureBOD", "ln_TenureEM")])

# Criando um sumário estatístico das variáveis originais e transformadas
summary_stats <- function(df) {
  vars_original <- c("BoardSizeBOD", "BoardSizeEM", "TenureBOD", "TenureEM")
  vars_transformed <- paste0("ln_", vars_original)
  
  stats <- data.frame(
    Variable = c(vars_original, vars_transformed),
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )
  
  for(i in 1:nrow(stats)) {
    var <- stats$Variable[i]
    if(var %in% names(df)) {
      stats$Mean[i] <- mean(df[[var]], na.rm = TRUE)
      stats$SD[i] <- sd(df[[var]], na.rm = TRUE)
      stats$Min[i] <- min(df[[var]], na.rm = TRUE)
      stats$Max[i] <- max(df[[var]], na.rm = TRUE)
      stats$Skewness[i] <- moments::skewness(df[[var]], na.rm = TRUE)
    }
  }
  
  return(stats)
}

# Gerando o sumário estatístico
stats <- summary_stats(Data_Tenure)
print(stats)


summary(Data_Tenure)


# Função para calcular estatísticas descritivas, incluindo o número de observações
summary_stats <- function(df) {
  # Lista das variáveis de interesse
  vars <- c("ROE_NW", "ROA_NW", "BLAU_BOD", "BLAU_EM", "PWOM_BOD", "PWOM_EM",   
            "ln_TenureBOD", "ln_TenureEM", "BoardSizeBOD", "BoardSizeEM", 
            "CCSize", "CCLeverage")
  
  # Criando um data frame para armazenar as estatísticas
  stats <- data.frame(
    Variable = vars,
    Observations = NA, # Adiciona a coluna para número de observações
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )
  
  # Calculando as estatísticas para cada variável
  for(i in 1:nrow(stats)) {
    var <- stats$Variable[i]
    if(var %in% names(df)) {
      # Removendo os NAs
      cleaned_data <- df[[var]][!is.na(df[[var]])]
      
      # Calculando as estatísticas descritivas
      stats$Observations[i] <- length(cleaned_data) # Número de observações não nulas
      stats$Mean[i] <- mean(cleaned_data, na.rm = TRUE)
      stats$SD[i] <- sd(cleaned_data, na.rm = TRUE)
      stats$Min[i] <- min(cleaned_data, na.rm = TRUE)
      stats$Max[i] <- max(cleaned_data, na.rm = TRUE)
      stats$Skewness[i] <- moments::skewness(cleaned_data, na.rm = TRUE)
    }
  }
  
  return(stats)
}



# Aplicando a função para calcular as estatísticas descritivas
summary_stats_result <- summary_stats(Data_Tenure)


# Gerando o sumário estatístico
stats <- summary_stats(Data_Tenure)
print(stats)


# Salvando os resultados em um arquivo Excel
library(openxlsx)

# Definindo o caminho de destino
#output_path <- "F:/BACKUP GERAL 01.06.2020/UFSC/2024/Francisco Bravo Urquiza/summary_stats_FINAL.xlsx"

# Salvando os resultados
openxlsx::write.xlsx(summary_stats_result, output_path)

# Confirmando que o arquivo foi salvo
cat("Arquivo Excel salvo em:", output_path, "\n")


# Conta os valores distintos na coluna 'Codigo'
quantidade_distintos <- n_distinct(Data_Tenure$Instituicao)

# Exibe o resultado
print(quantidade_distintos)


# Conta os valores distintos na coluna 'Codigo'
quantidade_distintos <- n_distinct(Data_Tenure$Codigo)

# Exibe o resultado
print(quantidade_distintos)


quantidade_observacoes <- nrow(Data_Tenure)
print(quantidade_observacoes)

# Contar quantos PWOM_BOD = 1 e PWOM_BOD = 0 no data frame Data_Tenure_BOD
count_PWOM_BOD_1 <- sum(Data_Tenure$PWOM_BOD == 1, na.rm = TRUE)
count_PWOM_BOD_0 <- sum(Data_Tenure$PWOM_BOD == 0, na.rm = TRUE)

# Contar quantos WON1_BOD = 1 no data frame Data_Tenure_BOD
count_WON1_BOD_1 <- sum(Data_Tenure$WON1_BOD == 1, na.rm = TRUE)


# Imprimir os resultados
cat("Count of PWOM_BOD = 1:", count_PWOM_BOD_1, "\n")
cat("Count of PWOM_BOD = 0:", count_PWOM_BOD_0, "\n")

cat("Count of WON1_BOD = 1 (at least one woman):", count_WON1_BOD_1, "\n")


##### Data_Tenure_BOD Criar um novo DataFrame com as colunas especificadas #####
Data_Tenure_BOD <- Data_Tenure %>%
  dplyr::select(Date, Mouth, Year, Instituicao, Codigo, TCB, Cidade, UF, 
         AtivoTotal, PatrimonioLiquido, LucroLiquido, BoardSizeBOD, ln_BoardSizeBOD, BoardSizeBOD_NW,
         FemaleBOD,TenureBOD, ln_TenureBOD, TenureBOD_NW,
         CCSize, CCLeverage, NumberFemaleBOD, NumberFemaleSB,
         DummyCentralEConfederacao, DummyLivreAdmissao, BLAU_EM, BLAU_SB,
         RegiaoDeAtuacao, BigFour, PWOM_BOD, PWOM_EM, PWOM_SB,
         WON1_BOD, W_BOD_33,BLAU_BOD, #BLAU_BOD_Lag1, BLAU_BOD_Lag2, BLAU_BOD_Lag3, BLAU_BOD_Lag4, 
         SHAN_BOD, #SHAN_BOD_Lag1, SHAN_BOD_Lag2, SHAN_BOD_Lag3, SHAN_BOD_Lag4, 
         NumberMenBOD, NumberMenEM, 
         ROE_NW, ROA_NW, LLPRatio_NW, NIM_NW, 
         CostToIncome_NW, PER_NW, Covid,# PWOM_BOD_Lag1, PWOM_EM_Lag1, PWOM_BOD_Lag2, PWOM_EM_Lag2, PWOM_BOD_Lag3, PWOM_EM_Lag3, PWOM_BOD_Lag4, PWOM_EM_Lag4, 
         #NIM_NW_Lag1, NIM_NW_Lag2,NIM_NW_Lag3, NIM_NW_Lag4, ROE_NW_Lag1,
         Year_2009, Year_2010, Year_2011, Year_2012, 
         Year_2013, Year_2014, Year_2015, Year_2016, 
         Year_2017, Year_2018, Year_2019, Year_2020, 
         Year_2021, Year_2022, #media_regional_pwon_bod
         , WON1_BOD)

# Verificar as primeiras linhas do novo DataFrame
summary(Data_Tenure_BOD)

#tibble::view(Data_Tenure_BOD)

Data_Tenure_BOD <- Data_Tenure_BOD %>%
  filter(Mouth == 12)


Data_Tenure_BOD <- Data_Tenure_BOD %>%
  dplyr::arrange(Codigo, Date) %>%  # Ordena por Codigo e Data
  group_by(Codigo) %>%
  dplyr::mutate(
    PWOM_BOD_Lag1 = dplyr::lag(PWOM_BOD, n = 1, default = NA),
    PWOM_BOD_Lag2 = dplyr::lag(PWOM_BOD, n = 2, default = NA),
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    
    PWOM_BOD_Lag3 = dplyr::lag(PWOM_BOD, n = 3, default = NA),
    PWOM_BOD_Lag4 = dplyr::lag(PWOM_BOD, n = 4, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    
    BLAU_BOD_Lag1 = dplyr::lag(BLAU_BOD, n = 1, default = NA),
    BLAU_BOD_Lag2 = dplyr::lag(BLAU_BOD, n = 2, default = NA),
    BLAU_BOD_Lag3 = dplyr::lag(BLAU_BOD, n = 3, default = NA),
    BLAU_BOD_Lag4 = dplyr::lag(BLAU_BOD, n = 4, default = NA),
    
    BLAU_EM_Lag1 = dplyr::lag(BLAU_EM, n = 1, default = NA),
    BLAU_EM_Lag2 = dplyr::lag(BLAU_EM, n = 2, default = NA),
    BLAU_EM_Lag3 = dplyr::lag(BLAU_EM, n = 3, default = NA),
    BLAU_EM_Lag4 = dplyr::lag(BLAU_EM, n = 4, default = NA),
    
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    
    ROE_NW_Lag1 = dplyr::lag(ROE_NW, n = 1, default = NA),
    ROE_NW_Lag2 = dplyr::lag(ROE_NW, n = 2, default = NA),
    ROE_NW_Lag3 = dplyr::lag(ROE_NW, n = 3, default = NA),
    ROE_NW_Lag4 = dplyr::lag(ROE_NW, n = 4, default = NA),
    
    ROA_NW_Lag1 = dplyr::lag(ROA_NW, n = 1, default = NA),
    ROA_NW_Lag2 = dplyr::lag(ROA_NW, n = 2, default = NA),
    ROA_NW_Lag3 = dplyr::lag(ROA_NW, n = 3, default = NA),
    ROA_NW_Lag4 = dplyr::lag(ROA_NW, n = 4, default = NA),
    
    LLPRatio_NW_Lag1 = dplyr::lag(LLPRatio_NW, n = 1, default = NA),
    LLPRatio_NW_Lag2 = dplyr::lag(LLPRatio_NW, n = 2, default = NA),
    LLPRatio_NW_Lag3 = dplyr::lag(LLPRatio_NW, n = 3, default = NA),
    LLPRatio_NW_Lag4 = dplyr::lag(LLPRatio_NW, n = 4, default = NA),
    
    NIM_NW_Lag1 = dplyr::lag(NIM_NW, n = 1, default = NA),
    NIM_NW_Lag2 = dplyr::lag(NIM_NW, n = 2, default = NA),
    NIM_NW_Lag3 = dplyr::lag(NIM_NW, n = 3, default = NA),
    NIM_NW_Lag4 = dplyr::lag(NIM_NW, n = 4, default = NA),
    
    CostToIncome_NW_Lag1 = dplyr::lag(CostToIncome_NW, n = 1, default = NA),
    CostToIncome_NW_Lag2 = dplyr::lag(CostToIncome_NW, n = 2, default = NA),
    CostToIncome_NW_Lag3 = dplyr::lag(CostToIncome_NW, n = 3, default = NA),
    CostToIncome_NW_Lag4 = dplyr::lag(CostToIncome_NW, n = 4, default = NA),
    
    PER_NW_Lag1 = dplyr::lag(PER_NW, n = 1, default = NA),
    PER_NW_Lag2 = dplyr::lag(PER_NW, n = 2, default = NA),
    PER_NW_Lag3 = dplyr::lag(PER_NW, n = 3, default = NA),
    PER_NW_Lag4 = dplyr::lag(PER_NW, n = 4, default = NA),
    
  )

# Criar um novo DataFrame chamado Data_Tenure_BOD, removendo os NA das colunas especificadas
Data_Tenure_BOD <- Data_Tenure_BOD %>%
  plotly::filter(!is.na(FemaleBOD) & !is.na(BoardSizeBOD) & !is.na(TenureBOD) & !is.na(CCSize))

# Conta os valores distintos na coluna 'Codigo'
quantidade_distintos <- n_distinct(Data_Tenure_BOD$Codigo)

# Exibe o resultado
print(quantidade_distintos)


quantidade_observacoes <- nrow(Data_Tenure_BOD)
print(quantidade_observacoes)

# Calcular médias por Região e Ano
medias_regionais <- Data_Tenure_BOD %>%
  group_by(RegiaoDeAtuacao, Year) %>%
  dplyr::summarise(  # <- FORÇA O USO DO SUMMARISE DO DPLYR
    media_regional_pwon_bod = mean(PWOM_BOD, na.rm = TRUE),
    media_regional_pwon_em = mean(PWOM_EM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()


# Calcular médias por Região e Ano
medias_regionais_BLAU <- Data_Tenure_BOD %>%
  group_by(RegiaoDeAtuacao, Year) %>%
  dplyr::summarise(  # <- FORÇA O USO DO SUMMARISE DO DPLYR
    media_regional_blau_bod = mean(BLAU_BOD, na.rm = TRUE),
    media_regional_blau_em = mean(BLAU_EM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()


# Juntar com o dataset original
Data_Tenure_BOD <- Data_Tenure_BOD %>%
  left_join(medias_regionais, by = c("RegiaoDeAtuacao", "Year"))

# Juntar com o dataset original
Data_Tenure_BOD <- Data_Tenure_BOD %>%
  left_join(medias_regionais_BLAU, by = c("RegiaoDeAtuacao", "Year"))

#view(medias_regionais)
#view(Data_Tenure_BOD)

nrow(Data_Tenure_BOD)

# Conta os valores distintos na coluna 'Codigo'
quantidade_distintos <- n_distinct(Data_Tenure_BOD$Codigo)

# Exibe o resultado
print(quantidade_distintos)


# Contar quantos PWOM_BOD = 1 e PWOM_BOD = 0 no data frame Data_Tenure_BOD
count_PWOM_BOD_1 <- sum(Data_Tenure_BOD$PWOM_BOD == 1, na.rm = TRUE)
count_PWOM_BOD_0 <- sum(Data_Tenure_BOD$PWOM_BOD == 0, na.rm = TRUE)

# Contar quantos WON1_BOD = 1 no data frame Data_Tenure_BOD
count_WON1_BOD_1 <- sum(Data_Tenure_BOD$WON1_BOD == 1, na.rm = TRUE)


# Imprimir os resultados
cat("Count of PWOM_BOD = 1:", count_PWOM_BOD_1, "\n")
cat("Count of PWOM_BOD = 0:", count_PWOM_BOD_0, "\n")

cat("Count of WON1_BOD = 1 (at least one woman):", count_WON1_BOD_1, "\n")


# Função para calcular estatísticas descritivas, incluindo o número de observações
summary_stats <- function(df) {
  # Lista das variáveis de interesse
  vars <- c("ROA_NW",
    "ROE_NW", #"NIM_NW", "LLPRatio_NW", 
            "BLAU_BOD",
            "PWOM_BOD", #"PWOM_EM", 
            "ln_TenureBOD", #"ln_TenureEM", 
            "BoardSizeBOD", #"ln_BoardSizeEM", 
            "CCSize", "CCLeverage")
  
  # Criando um data frame para armazenar as estatísticas
  stats <- data.frame(
    Variable = vars,
    Observations = NA, # Adiciona a coluna para número de observações
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )
  
  # Calculando as estatísticas para cada variável
  for(i in 1:nrow(stats)) {
    var <- stats$Variable[i]
    if(var %in% names(df)) {
      # Removendo os NAs
      cleaned_data <- df[[var]][!is.na(df[[var]])]
      
      # Calculando as estatísticas descritivas
      stats$Observations[i] <- length(cleaned_data) # Número de observações não nulas
      stats$Mean[i] <- mean(cleaned_data, na.rm = TRUE)
      stats$SD[i] <- sd(cleaned_data, na.rm = TRUE)
      stats$Min[i] <- min(cleaned_data, na.rm = TRUE)
      stats$Max[i] <- max(cleaned_data, na.rm = TRUE)
      stats$Skewness[i] <- moments::skewness(cleaned_data, na.rm = TRUE)
    }
  }
  
  return(stats)
}



# Aplicando a função para calcular as estatísticas descritivas
summary_stats_result <- summary_stats(Data_Tenure_BOD)


# Gerando o sumário estatístico
stats <- summary_stats(Data_Tenure_BOD)
print(stats)


# Salvando os resultados em um arquivo Excel
library(openxlsx)

# Definindo o caminho de destino
#output_path <- "F:/BACKUP GERAL 01.06.2020/UFSC/2024/Francisco Bravo Urquiza/summary_stats_BOD.xlsx"

# Salvando os resultados
openxlsx::write.xlsx(summary_stats_result, output_path)

# Confirmando que o arquivo foi salvo
cat("Arquivo Excel salvo em:", output_path, "\n")


# Criar um novo DataFrame com as colunas especificadas
Data_Tenure_EM <- Data_Tenure %>%
  dplyr::select(Date, Mouth, Year, Instituicao, Codigo, TCB, Cidade, UF, 
                AtivoTotal, PatrimonioLiquido, LucroLiquido, BoardSizeEM, ln_BoardSizeEM, BoardSizeEM_NW,
                FemaleEM,TenureEM, ln_TenureEM, TenureEM_NW,
                CCSize, CCLeverage, NumberFemaleBOD, NumberFemaleSB, NumberFemaleEM,
                DummyCentralEConfederacao, DummyLivreAdmissao, BLAU_BOD, BLAU_SB,
                RegiaoDeAtuacao, BigFour, PWOM_BOD, PWOM_EM, PWOM_SB,
                WON1_EM, W_EM_33,BLAU_EM, BLAU_EM_Lag1, BLAU_EM_Lag2, BLAU_EM_Lag3, BLAU_EM_Lag4, 
                SHAN_EM, SHAN_EM_Lag1, SHAN_EM_Lag2, SHAN_EM_Lag3, SHAN_EM_Lag4, 
                NumberMenBOD, NumberMenEM, 
                ROE_NW, ROA_NW, LLPRatio_NW, NIM_NW, 
                CostToIncome_NW, PER_NW, Covid, PWOM_EM_Lag1, PWOM_BOD_Lag1, PWOM_EM_Lag2, PWOM_BOD_Lag2, PWOM_EM_Lag3, PWOM_BOD_Lag3, PWOM_EM_Lag4, PWOM_BOD_Lag4,
                NIM_NW_Lag1, NIM_NW_Lag2,NIM_NW_Lag3, NIM_NW_Lag4, ROE_NW_Lag1,
                Year_2009, Year_2010, Year_2011, Year_2012, 
                Year_2013, Year_2014, Year_2015, Year_2016, 
                Year_2017, Year_2018, Year_2019, Year_2020, 
                Year_2021, Year_2022,# media_regional_pwon_em,
                WON1_EM)

# Verificar as primeiras linhas do novo DataFrame
summary(Data_Tenure_EM)

Data_Tenure_EM <- Data_Tenure_EM %>%
  filter(Mouth == 12)


Data_Tenure_EM <- Data_Tenure_EM %>%
  dplyr::arrange(Codigo, Date) %>%  # Ordena por Codigo e Data
  group_by(Codigo) %>%
  dplyr::mutate(
    PWOM_BOD_Lag1 = dplyr::lag(PWOM_BOD, n = 1, default = NA),
    PWOM_BOD_Lag2 = dplyr::lag(PWOM_BOD, n = 2, default = NA),
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    #  PWOM_SB_Lag1 = dplyr::lag(PWOM_SB, n = 1, default = NA),
    #  PWOM_SB_Lag2 = dplyr::lag(PWOM_SB, n = 2, default = NA),
    
    PWOM_BOD_Lag3 = dplyr::lag(PWOM_BOD, n = 3, default = NA),
    PWOM_BOD_Lag4 = dplyr::lag(PWOM_BOD, n = 4, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    # PWOM_SB_Lag3 = dplyr::lag(PWOM_SB, n = 3, default = NA),
    #  PWOM_SB_Lag4 = dplyr::lag(PWOM_SB, n = 4, default = NA),
    
    BLAU_BOD_Lag1 = dplyr::lag(BLAU_BOD, n = 1, default = NA),
    BLAU_BOD_Lag2 = dplyr::lag(BLAU_BOD, n = 2, default = NA),
    BLAU_BOD_Lag3 = dplyr::lag(BLAU_BOD, n = 3, default = NA),
    BLAU_BOD_Lag4 = dplyr::lag(BLAU_BOD, n = 4, default = NA),
    
    BLAU_EM_Lag1 = dplyr::lag(BLAU_EM, n = 1, default = NA),
    BLAU_EM_Lag2 = dplyr::lag(BLAU_EM, n = 2, default = NA),
    BLAU_EM_Lag3 = dplyr::lag(BLAU_EM, n = 3, default = NA),
    BLAU_EM_Lag4 = dplyr::lag(BLAU_EM, n = 4, default = NA),
    
    
    PWOM_EM_Lag1 = dplyr::lag(PWOM_EM, n = 1, default = NA),
    PWOM_EM_Lag2 = dplyr::lag(PWOM_EM, n = 2, default = NA),
    PWOM_EM_Lag3 = dplyr::lag(PWOM_EM, n = 3, default = NA),
    PWOM_EM_Lag4 = dplyr::lag(PWOM_EM, n = 4, default = NA),
    
    ROE_NW_Lag1 = dplyr::lag(ROE_NW, n = 1, default = NA),
    ROE_NW_Lag2 = dplyr::lag(ROE_NW, n = 2, default = NA),
    ROE_NW_Lag3 = dplyr::lag(ROE_NW, n = 3, default = NA),
    ROE_NW_Lag4 = dplyr::lag(ROE_NW, n = 4, default = NA),
    
    ROA_NW_Lag1 = dplyr::lag(ROA_NW, n = 1, default = NA),
    ROA_NW_Lag2 = dplyr::lag(ROA_NW, n = 2, default = NA),
    ROA_NW_Lag3 = dplyr::lag(ROA_NW, n = 3, default = NA),
    ROA_NW_Lag4 = dplyr::lag(ROA_NW, n = 4, default = NA),
    
    LLPRatio_NW_Lag1 = dplyr::lag(LLPRatio_NW, n = 1, default = NA),
    LLPRatio_NW_Lag2 = dplyr::lag(LLPRatio_NW, n = 2, default = NA),
    LLPRatio_NW_Lag3 = dplyr::lag(LLPRatio_NW, n = 3, default = NA),
    LLPRatio_NW_Lag4 = dplyr::lag(LLPRatio_NW, n = 4, default = NA),
    
    NIM_NW_Lag1 = dplyr::lag(NIM_NW, n = 1, default = NA),
    NIM_NW_Lag2 = dplyr::lag(NIM_NW, n = 2, default = NA),
    NIM_NW_Lag3 = dplyr::lag(NIM_NW, n = 3, default = NA),
    NIM_NW_Lag4 = dplyr::lag(NIM_NW, n = 4, default = NA),
    
    CostToIncome_NW_Lag1 = dplyr::lag(CostToIncome_NW, n = 1, default = NA),
    CostToIncome_NW_Lag2 = dplyr::lag(CostToIncome_NW, n = 2, default = NA),
    CostToIncome_NW_Lag3 = dplyr::lag(CostToIncome_NW, n = 3, default = NA),
    CostToIncome_NW_Lag4 = dplyr::lag(CostToIncome_NW, n = 4, default = NA),
    
    PER_NW_Lag1 = dplyr::lag(PER_NW, n = 1, default = NA),
    PER_NW_Lag2 = dplyr::lag(PER_NW, n = 2, default = NA),
    PER_NW_Lag3 = dplyr::lag(PER_NW, n = 3, default = NA),
    PER_NW_Lag4 = dplyr::lag(PER_NW, n = 4, default = NA),
    )

# Criar um novo DataFrame chamado Data_Tenure_BOD, removendo os NA das colunas especificadas
Data_Tenure_EM <- Data_Tenure_EM %>%
  plotly::filter(!is.na(FemaleEM) & !is.na(BoardSizeEM) & !is.na(TenureEM) & !is.na(CCSize))

# Calcular médias por Região e Ano
medias_regionais <- Data_Tenure_EM %>%
  group_by(RegiaoDeAtuacao, Year) %>%
  dplyr::summarise(  # <- FORÇA O USO DO SUMMARISE DO DPLYR
    media_regional_pwon_bod = mean(PWOM_BOD, na.rm = TRUE),
    media_regional_pwon_em = mean(PWOM_EM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()


# Calcular médias por Região e Ano
medias_regionais_BLAU <- Data_Tenure_EM %>%
  group_by(RegiaoDeAtuacao, Year) %>%
  dplyr::summarise(  # <- FORÇA O USO DO SUMMARISE DO DPLYR
    media_regional_blau_bod = mean(BLAU_BOD, na.rm = TRUE),
    media_regional_blau_em = mean(BLAU_EM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# Juntar com o dataset original
Data_Tenure_EM <- Data_Tenure_EM %>%
  left_join(medias_regionais, by = c("RegiaoDeAtuacao", "Year"))

Data_Tenure_EM <- Data_Tenure_EM %>%
  left_join(medias_regionais_BLAU, by = c("RegiaoDeAtuacao", "Year"))


#view(medias_regionais)
#view(Data_Tenure_EM)


library(dplyr)


nrow(Data_Tenure_EM)

# Conta os valores distintos na coluna 'Codigo'
quantidade_distintos <- n_distinct(Data_Tenure_EM$Codigo)

# Exibe o resultado
print(quantidade_distintos)

summary(Data_Tenure_EM)


# Contar quantos PWOM_EM = 1 e PWOM_EM = 0 no data frame Data_Tenure_EM
count_PWOM_EM_1 <- sum(Data_Tenure_EM$PWOM_EM == 1, na.rm = TRUE)
count_PWOM_EM_0 <- sum(Data_Tenure_EM$PWOM_EM == 0, na.rm = TRUE)

# Contar quantos WON1_EM = 1 no data frame Data_Tenure_EM
count_WON1_EM_1 <- sum(Data_Tenure_EM$WON1_EM == 1, na.rm = TRUE)

# Imprimir os resultados
cat("Count of PWOM_EM = 1:", count_PWOM_EM_1, "\n")
cat("Count of PWOM_EM = 0:", count_PWOM_EM_0, "\n")


cat("Count of WON1_EM = 1 (at least one woman):", count_WON1_EM_1, "\n")


# Função para calcular estatísticas descritivas, incluindo o número de observações
summary_stats <- function(df) {
  # Lista das variáveis de interesse
  vars <- c("ROA_NW", "ROE_NW", #"NIM_NW", "LLPRatio_NW",# "PWOM_BOD", 
            "BLAU_EM",
            "PWOM_EM", 
            #"ln_TenureBOD", 
            "ln_TenureEM", 
           # "ln_BoardSizeBOD", 
            "BoardSizeEM", 
            "CCSize", "CCLeverage")
  
  # Criando um data frame para armazenar as estatísticas
  stats <- data.frame(
    Variable = vars,
    Observations = NA, # Adiciona a coluna para número de observações
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )
  
  # Calculando as estatísticas para cada variável
  for(i in 1:nrow(stats)) {
    var <- stats$Variable[i]
    if(var %in% names(df)) {
      # Removendo os NAs
      cleaned_data <- df[[var]][!is.na(df[[var]])]
      
      # Calculando as estatísticas descritivas
      stats$Observations[i] <- length(cleaned_data) # Número de observações não nulas
      stats$Mean[i] <- mean(cleaned_data, na.rm = TRUE)
      stats$SD[i] <- sd(cleaned_data, na.rm = TRUE)
      stats$Min[i] <- min(cleaned_data, na.rm = TRUE)
      stats$Max[i] <- max(cleaned_data, na.rm = TRUE)
      stats$Skewness[i] <- moments::skewness(cleaned_data, na.rm = TRUE)
    }
  }
  
  return(stats)
}



# Aplicando a função para calcular as estatísticas descritivas
summary_stats_result <- summary_stats(Data_Tenure_EM)



# Gerando o sumário estatístico
stats <- summary_stats(Data_Tenure_EM)
print(stats)




# Salvando os resultados em um arquivo Excel
library(openxlsx)

# Definindo o caminho de destino
#output_path <- "F:/BACKUP GERAL 01.06.2020/UFSC/2024/Francisco Bravo Urquiza/summary_stats_EM.xlsx"

# Salvando os resultados
openxlsx::write.xlsx(summary_stats_result, output_path)

# Confirmando que o arquivo foi salvo
cat("Arquivo Excel salvo em:", output_path, "\n")




# Primeiro data frame: seleção inicial
df1 <- Data_Tenure %>%
  dplyr::select(
    "ROA_NW",
    "ROE_NW",
    #"NIM_NW", 
   # "LLPRatio_NW",
   "BLAU_BOD",
   "BLAU_EM",
    "PWOM_BOD",
    "PWOM_EM",
    "ln_TenureBOD",
    "ln_TenureEM",
    "BoardSizeBOD",
    "BoardSizeEM",
    "CCSize", 
    "CCLeverage",
    "DummyLivreAdmissao",
    "DummyCentralEConfederacao",
    "Covid"
  )



# Combinando os dois data frames em um só
statistics_2 <- df1 # Une pelos índices (colunas)

# Visualizando as primeiras linhas do novo data frame combinado
head(statistics_2)


Base_Correlacao <- statistics_2


corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  #mystars <- ifelse(p < .0001, "**", ifelse(p < .001, "** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  # Original
  # mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  # mystars <- ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", " "))
  mystars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  
  
  #** Correlation is significant at the 0.01 level. 
  #* Correlation is significant at the 0.05 level.
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

#### Correlation matrix ####

library(Hmisc)
library(xtable)
library(htmltools)

Matriz_Correlacao_Pearson <- corstars(Base_Correlacao,
                                      method = c("pearson"),
                                      removeTriangle = c("upper"),
                                      result = c("none", "html", "latex"))

Matriz_Correlacao_Spearman <- corstars(Base_Correlacao,
                                       method = c("spearman"),
                                       removeTriangle = c("lower"),
                                       result = c("none", "html", "latex"))

library(writexl)

tibble::view(Matriz_Correlacao_Pearson)


#write_xlsx(Matriz_Correlacao_Pearson,"F:/BACKUP GERAL 01.06.2020/UFSC/2024/DEFESA TESE - FINAL/Resultados - Artigo 2/Corr_pearson_final.xlsx")
#write_xlsx(Matriz_Correlacao_Spearman, "F:/BACKUP GERAL 01.06.2020/UFSC/2024/DEFESA TESE - FINAL/Resultados - Artigo 2/Corr_spearman.xlsx")

#BOD######
# novo ROA######

# testes OLS	full sample
# sem interação	
# com interação	
# blau sem interação
# blau com interação



#Estimar o modelo OLS com ln
ols_model_ROA <- lm(ROA_NW ~ PWOM_BOD #+ TenureBOD_NW 
                    + ln_TenureBOD 
                   #  + BoardSizeBOD_NW
                   # + TenureBOD
                    + BoardSizeBOD
                      
                 #   + ln_BoardSizeBOD
                 + CCSize + CCLeverage 
                    + DummyLivreAdmissao + DummyCentralEConfederacao #+  BigFour 
                 + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_BOD)

summary(ols_model_ROA)



# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROA_INT <- lm(ROA_NW ~ PWOM_BOD + ln_TenureBOD + BoardSizeBOD + PWOM_BOD:ln_TenureBOD + CCSize + CCLeverage  
                      + DummyLivreAdmissao 
                      + DummyCentralEConfederacao  
                      #+ BigFour 
                      + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_BOD)

summary(ols_model_ROA_INT)


#Estimar o modelo OLS com ln - BLAU
ols_model_ROA_BLAU <- lm(ROA_NW ~ BLAU_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage 
                      + DummyLivreAdmissao + DummyCentralEConfederacao 
                     # + BigFour 
                      + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_BOD)

summary(ols_model_ROA_BLAU)


#BLAU
# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROA_BLAU_INT <- lm(ROA_NW ~ BLAU_BOD + ln_TenureBOD + BoardSizeBOD + BLAU_BOD:ln_TenureBOD + CCSize + CCLeverage  
                       + DummyLivreAdmissao + DummyCentralEConfederacao 
                      # + BigFour 
                       + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_BOD)

summary(ols_model_ROA_BLAU_INT)


#2SLS
#Normal com ln

iv_model_ROA <- ivreg(ROA_NW ~ # plm::lag(NIM_NW, 1) +
                        
                        PWOM_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage 
                      + DummyLivreAdmissao + DummyCentralEConfederacao 
                      #+ BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                      #  PWOM_BOD_Lag1
                      #  + PWOM_BOD_Lag2
                       + PWOM_BOD_Lag3
                     #   + PWOM_BOD_Lag4
                      ##  + NIM_NW_Lag1
                      #  + NIM_NW_Lag2
                      #  + NIM_NW_Lag3
                      # + NIM_NW_Lag4
                      # PWOM_EM_Lag
                      #   + PWOM_SB
                     #  + media_regional_pwon_em
                   #  + media_regional_pwon_bod
                     #  +  plm::lag(PWOM_BOD, 1) 
                     #  + plm::lag(PWOM_BOD, 2) 
                      #+ plm::lag(NIM_NW, 1)
                      #+ plm::lag(NIM_NW, 2)
                      
                    #  + PWOM_EM
                    #    + BLAU_BOD
                   #  + BLAU_BOD_Lag1
                  #  + BLAU_BOD_Lag2
                   + BLAU_BOD_Lag3
                 #  + BLAU_BOD_Lag4
                      # + SHAN_BOD
                     # + WON1_BOD
                      # +  W_BOD_33
                     #  + NumberMenBOD
                     #   + NumberMenEM
                      # + plm::lag(PWOM_BOD, 2) 
                      + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                    #  + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_BOD)



summary(iv_model_ROA, diagnostics = TRUE)

#2SLS
#PWOM_BOD:TenureBOD - com interação

iv_model_ROA_INT <- ivreg(ROA_NW ~ 
                        
                        PWOM_BOD + ln_TenureBOD + BoardSizeBOD + PWOM_BOD:TenureBOD + CCSize + CCLeverage  
                      + DummyLivreAdmissao + DummyCentralEConfederacao 
                      + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                     #  PWOM_BOD_Lag1
                    #   + PWOM_BOD_Lag2
                      # + PWOM_BOD_Lag3
                      #  + PWOM_BOD_Lag4
                        + ROA_NW_Lag1
                       + ROA_NW_Lag2
                      #  + NIM_NW_Lag2
                      #  + NIM_NW_Lag3
                      # + NIM_NW_Lag4
                      # PWOM_EM_Lag
                      #   + PWOM_SB
                      
                      # +  plm::lag(PWOM_BOD, 1) 
                      # + plm::lag(PWOM_BOD, 2) 
                      #+ plm::lag(NIM_NW, 1)
                      #+ plm::lag(NIM_NW, 2)
                    # + media_regional_pwon_bod
                     # + PWOM_EM
                  #    + BLAU_BOD
                  # + BLAU_BOD_Lag1
                    # + BLAU_EM
                     #  + SHAN_BOD
                      # + WON1_BOD
                      # +  W_BOD_33
                    #   + NumberMenBOD
                      #  + NumberMenEM
                      # + plm::lag(PWOM_BOD, 2) 
                       + PWOM_BOD:TenureBOD
                      + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                      + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_BOD)



summary(iv_model_ROA_INT, diagnostics = TRUE)


#2SLS
#Normal com ln - BLAU

iv_model_ROA_BLAU <- ivreg(ROA_NW ~ # plm::lag(NIM_NW, 1) +
                        
                        BLAU_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage  
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                      #+ BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                      # BLAU_BOD_Lag1
                      #  + PWOM_BOD_Lag2
                      # + PWOM_BOD_Lag3
                      #  + PWOM_BOD_Lag4
                     #  + ROA_NW_Lag1
                      #  + ROA_NW_Lag2
                      #  + NIM_NW_Lag3
                      # + NIM_NW_Lag4
                      # PWOM_EM_Lag
                      #   + PWOM_SB
                     + PWOM_BOD
                    #  + PWOM_BOD_Lag1
                    + media_regional_pwon_bod
                    #+ media_regional_blau_bod
                      # +  plm::lag(PWOM_BOD, 1) 
                      # + plm::lag(PWOM_BOD, 2) 
                      #+ plm::lag(NIM_NW, 1)
                      #+ plm::lag(NIM_NW, 2)
                      
                     # + PWOM_BOD
                      #  + BLAU_BOD
                    #   + SHAN_BOD
                    # + SHAN_BOD_Lag1
                      # + WON1_BOD
                      # +  W_BOD_33
                     #  + NumberMenBOD
                      #  + NumberMenEM
                      # + plm::lag(PWOM_BOD, 2) 
                      + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                     # + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_BOD)



summary(iv_model_ROA_BLAU, diagnostics = TRUE)



#2SLS
#PWOM_BOD:TenureBOD - com interação - BLAU

iv_model_ROA_BLAU_INT <- ivreg(ROA_NW ~ 
                            
                            BLAU_BOD + ln_TenureBOD + BoardSizeBOD + BLAU_BOD:ln_TenureBOD + CCSize + CCLeverage  
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                         # + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                          | 
                        #    BLAU_BOD_Lag1
                          + BLAU_BOD:ln_TenureBOD
                          #  + BLAU_BOD_Lag2
                          # + PWOM_BOD_Lag3
                          #  + PWOM_BOD_Lag4
                          # + ROA_NW_Lag1
                          #  + ROA_NW_Lag2
                          #  + NIM_NW_Lag3
                          # + NIM_NW_Lag4
                          # PWOM_EM_Lag
                          #   + PWOM_SB
                        #  + PWOM_BOD
                        # + PWOM_BOD_Lag1
                          
                          # +  plm::lag(PWOM_BOD, 1) 
                          # + plm::lag(PWOM_BOD, 2) 
                          #+ plm::lag(NIM_NW, 1)
                          #+ plm::lag(NIM_NW, 2)
                        + media_regional_pwon_bod 
                         # + PWOM_EM
                          #  + BLAU_BOD
                          # + BLAU_EM
                          #  + SHAN_BOD
                          # + WON1_BOD
                          # +  W_BOD_33
                           + NumberMenBOD
                          #  + NumberMenEM
                          # + plm::lag(PWOM_BOD, 2) 
                          + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                         # + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                          ,
                          data = Data_Tenure_BOD)



summary(iv_model_ROA_BLAU_INT, diagnostics = TRUE)

# novo ROE######

# testes OLS	full sample
# sem interação	
# com interação	
# blau sem interação
# blau com interação


#Estimar o modelo OLS com ln
ols_model_ROE <- lm(ROE_NW ~ PWOM_BOD + ln_TenureBOD
                    + CCSize + CCLeverage + 
                      BoardSizeBOD 
                   # + ln_TenureBOD
                   # + ln_BoardSizeBOD
                    + DummyLivreAdmissao + DummyCentralEConfederacao 
                    #+ BigFour 
                   + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_BOD)

summary(ols_model_ROE)



# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROE_INT <- lm(ROE_NW ~ PWOM_BOD + ln_TenureBOD + BoardSizeBOD + PWOM_BOD:ln_TenureBOD + CCSize + CCLeverage 
                           + DummyLivreAdmissao #+ DummyCentralEConfederacao #+ BigFour 
                        + Covid + 
                          RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                        + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                        data = Data_Tenure_BOD)

summary(ols_model_ROE_INT)


#Estimar o modelo OLS com ln - BLAU
ols_model_ROE_BLAU <- lm(ROE_NW ~ BLAU_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage
                          + DummyLivreAdmissao + DummyCentralEConfederacao #+ BigFour 
                         + Covid + 
                           RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                         + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                         data = Data_Tenure_BOD)

summary(ols_model_ROE_BLAU)


#BLAU
# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROE_BLAU_INT <- lm(ROE_NW ~ BLAU_BOD + ln_TenureBOD + BoardSizeBOD + BLAU_BOD:ln_TenureBOD + CCSize + CCLeverage + 
                                + DummyLivreAdmissao  + DummyCentralEConfederacao
                            # + BigFour 
                             + Covid + 
                               RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                             + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                             data = Data_Tenure_BOD)

summary(ols_model_ROE_BLAU_INT)


#2SLS
#Normal com ln

iv_model_ROE <- ivreg(ROE_NW ~ # plm::lag(NIM_NW, 1) +
                        
                        PWOM_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage  
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                      #+ BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                       PWOM_BOD_Lag1
                    #  + PWOM_BOD_Lag2
                     #  + PWOM_BOD_Lag3
                    #    + PWOM_BOD_Lag4
                      #  + NIM_NW_Lag1
                      #  + NIM_NW_Lag2
                      #  + NIM_NW_Lag3
                      # + NIM_NW_Lag4
                      # PWOM_EM_Lag
                       #  + PWOM_SB
                      
                      # +  plm::lag(ROE_NW, 1) 
                      # + plm::lag(PWOM_BOD, 2) 
                      #+ plm::lag(NIM_NW, 1)
                      #+ plm::lag(NIM_NW, 2)
                      
                  #  + ROE_NW_Lag1
                  + media_regional_pwon_bod
                    #  + PWOM_EM
                        + BLAU_BOD
                   #  + BLAU_BOD_Lag1
                    # + BLAU_BOD_Lag2
                    # + BLAU_BOD_Lag3
                    # + BLAU_BOD_Lag4
                      # + SHAN_BOD
                      # + WON1_BOD
                      # +  W_BOD_33
                      # + NumberMenBOD
                      #  + NumberMenEM
                      # + plm::lag(PWOM_BOD, 2) 
                      + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                      #+ BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_BOD)



summary(iv_model_ROE, diagnostics = TRUE)



#2SLS
#PWOM_BOD:TenureBOD - com interação

iv_model_ROE_INT <- ivreg(ROE_NW ~ 
                            
                            PWOM_BOD + ln_TenureBOD + PWOM_BOD:ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD 
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                          #+ BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                          | 
                           # PWOM_BOD_Lag1
                         
                           # + PWOM_BOD_Lag2
                          # + PWOM_BOD_Lag3
                         #   + PWOM_BOD_Lag4
                          #  + NIM_NW_Lag1
                          #  + NIM_NW_Lag2
                          #  + NIM_NW_Lag3
                          # + NIM_NW_Lag4
                          # PWOM_EM_Lag
                          #    + PWOM_SB
                           
                          # +  plm::lag(PWOM_BOD, 1) 
                          # + plm::lag(PWOM_BOD, 2) 
                          #+ plm::lag(NIM_NW, 1)
                          #+ plm::lag(NIM_NW, 2)
                        # + media_regional_pwon_bod
                       #  + PWOM_EM
                         + PWOM_BOD:ln_TenureBOD
                           + BLAU_BOD
                         + BLAU_BOD_Lag1
                          # + BLAU_EM
                          #  + SHAN_BOD
                          # + WON1_BOD
                          # +  W_BOD_33
                          # + NumberMenBOD
                          #  + NumberMenEM
                          # + plm::lag(PWOM_BOD, 2) 
                          + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                         # + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                          ,
                          data = Data_Tenure_BOD)



summary(iv_model_ROE_INT, diagnostics = TRUE)


#2SLS
#Normal com ln - BLAU

iv_model_ROE_BLAU <- ivreg(ROE_NW ~ # plm::lag(NIM_NW, 1) +
                             
                             BLAU_BOD + ln_TenureBOD + BoardSizeBOD + CCSize + CCLeverage  
                           + DummyLivreAdmissao + DummyCentralEConfederacao
                          # + BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                           | 
                          #   BLAU_BOD_Lag1
                           #  + PWOM_BOD_Lag2
                           # + PWOM_BOD_Lag3
                           #  + PWOM_BOD_Lag4
                           #  + NIM_NW_Lag1
                           #  + NIM_NW_Lag2
                           #  + NIM_NW_Lag3
                           # + NIM_NW_Lag4
                           # PWOM_EM_Lag
                           #  + PWOM_SB
                           + media_regional_pwon_bod
                           + PWOM_BOD
                        #  + PWOM_BOD_Lag1
                           # +  plm::lag(PWOM_BOD, 1) 
                           # + plm::lag(PWOM_BOD, 2) 
                           #+ plm::lag(NIM_NW, 1)
                           #+ plm::lag(NIM_NW, 2)
                           
                         # + PWOM_EM
                           #  + BLAU_BOD
                           # + SHAN_BOD
                           # + WON1_BOD
                           # +  W_BOD_33
                           # + NumberMenBOD
                           #  + NumberMenEM
                           # + plm::lag(PWOM_BOD, 2) 
                           + ln_TenureBOD + CCSize + CCLeverage + BoardSizeBOD
                           + DummyLivreAdmissao + DummyCentralEConfederacao
                         #  + BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                           ,
                           data = Data_Tenure_BOD)



summary(iv_model_ROE_BLAU, diagnostics = TRUE)



#2SLS
#PWOM_BOD:TenureBOD - com interação - BLAU

iv_model_ROE_BLAU_INT <- ivreg(ROE_NW ~ 
                                 
                                 BLAU_BOD + ln_TenureBOD + BLAU_BOD:ln_TenureBOD + CCSize + CCLeverage + ln_BoardSizeBOD 
                               + DummyLivreAdmissao + DummyCentralEConfederacao
                               + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                               | 
                               #  BLAU_BOD_Lag1
                               + BLAU_BOD:ln_TenureBOD
                                # + PWOM_BOD_Lag2
                               # + PWOM_BOD_Lag3
                               #  + PWOM_BOD_Lag4
                               #  + NIM_NW_Lag1
                               #  + NIM_NW_Lag2
                               #  + NIM_NW_Lag3
                               # + NIM_NW_Lag4
                               # PWOM_EM_Lag
                               #   + PWOM_SB
                                + PWOM_BOD
                                + PWOM_BOD_Lag1
                               # +  plm::lag(PWOM_BOD, 1) 
                               # + plm::lag(PWOM_BOD, 2) 
                               #+ plm::lag(NIM_NW, 1)
                               #+ plm::lag(NIM_NW, 2)
                               
                               #+ PWOM_EM
                               #  + BLAU_BOD
                               # + BLAU_EM
                               #  + SHAN_BOD
                               # + WON1_BOD
                               # +  W_BOD_33
                               # + NumberMenBOD
                               #  + NumberMenEM
                               # + plm::lag(PWOM_BOD, 2) 
                               + ln_TenureBOD + CCSize + CCLeverage + ln_BoardSizeBOD
                               + DummyLivreAdmissao 
                               + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                               ,
                               data = Data_Tenure_BOD)



summary(iv_model_ROE_BLAU_INT, diagnostics = TRUE)

# EM######
# novo ROA######

# testes OLS	full sample
# sem interação	
# com interação	
# blau sem interação
# blau com interação



#Estimar o modelo OLS com ln
ols_model_ROA <- lm(ROA_NW ~ PWOM_EM + ln_TenureEM + BoardSizeEM
                    + CCSize + CCLeverage + 
                       + DummyLivreAdmissao + DummyCentralEConfederacao 
                    #+ BigFour 
                    + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_EM)

summary(ols_model_ROA)



# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROA_INT <- lm(ROA_NW ~ PWOM_EM + ln_TenureEM + BoardSizeEM + PWOM_EM:ln_TenureEM + CCSize + CCLeverage 
                         + DummyLivreAdmissao 
                        + DummyCentralEConfederacao 
                        #+ BigFour 
                        + Covid + 
                          RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                        + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                        data = Data_Tenure_EM)

summary(ols_model_ROA_INT)


#Estimar o modelo OLS com ln - BLAU
ols_model_ROA_BLAU <- lm(ROA_NW ~ BLAU_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage + 
                            + DummyLivreAdmissao + DummyCentralEConfederacao #+ BigFour 
                         + Covid + 
                           RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                         + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                         data = Data_Tenure_EM)

summary(ols_model_ROA_BLAU)


#BLAU
# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROA_BLAU_INT <- lm(ROA_NW ~ BLAU_EM + ln_TenureEM + BoardSizeEM + BLAU_EM:ln_TenureEM + CCSize + CCLeverage 
                                + DummyLivreAdmissao + DummyCentralEConfederacao #+ BigFour 
                             + Covid + 
                               RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                             + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                             data = Data_Tenure_EM)

summary(ols_model_ROA_BLAU_INT)


#2SLS
#Normal com ln

iv_model_ROA <- ivreg(ROA_NW ~ # plm::lag(NIM_NW, 1) +
                        
                        PWOM_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage  
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                     # + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                        # PWOM_EM_Lag1
                        #   + PWOM_EM_Lag2
                        # + PWOM_EM_Lag3
                        #  + PWOM_EM_Lag4
                        #  + NIM_NW_Lag1
                        #  + NIM_NW_Lag2
                        #  + NIM_NW_Lag3
                        # + NIM_NW_Lag4
                        # PWOM_EM_Lag
                        #   + PWOM_SB
                        
                        # +  plm::lag(PWOM_EM, 1) 
                        # + plm::lag(PWOM_EM, 2) 
                        #+ plm::lag(NIM_NW, 1)
                        #+ plm::lag(NIM_NW, 2)
                       + media_regional_pwon_bod
                        # + PWOM_EM
                        + BLAU_EM
                      + BLAU_EM_Lag1
                      # + SHAN_EM
                      # + WON1_EM
                      # +  W_EM_33
                      # + NumberMenEM
                      #  + NumberMenEM
                      # + plm::lag(PWOM_EM, 2) 
                      + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                      + DummyLivreAdmissao + DummyCentralEConfederacao
                      #+ BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_EM)



summary(iv_model_ROA, diagnostics = TRUE)



#2SLS
#PWOM_EM:TenureEM - com interação

iv_model_ROA_INT <- ivreg(ROA_NW ~ 
                            
                            PWOM_EM + ln_TenureEM + BoardSizeEM + PWOM_EM:ln_TenureEM + CCSize + CCLeverage  
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                          + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                          | 
                          #  PWOM_EM_Lag1
                          + PWOM_EM:ln_TenureEM
                          #  + PWOM_EM_Lag2
                          # + PWOM_EM_Lag3
                          #  + PWOM_EM_Lag4
                          #  + NIM_NW_Lag1
                          #  + NIM_NW_Lag2
                          #  + NIM_NW_Lag3
                          # + NIM_NW_Lag4
                          # PWOM_EM_Lag
                          #    + PWOM_SB
                          
                          # +  plm::lag(PWOM_EM, 1) 
                          # + plm::lag(PWOM_EM, 2) 
                          #+ plm::lag(NIM_NW, 1)
                          #+ plm::lag(NIM_NW, 2)
                        #  + media_regional_pwon_bod
                         # + PWOM_EM
                            + BLAU_EM
                      + BLAU_EM_Lag1
                          #  + BLAU_EM
                          # + BLAU_EM
                          #  + SHAN_EM
                          # + WON1_EM
                          # +  W_EM_33
                          # + NumberMenEM
                          #  + NumberMenEM
                          # + plm::lag(PWOM_EM, 2) 
                          + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                          + DummyLivreAdmissao + DummyCentralEConfederacao
                          + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                          ,
                          data = Data_Tenure_EM)



summary(iv_model_ROA_INT, diagnostics = TRUE)


#2SLS
#Normal com ln - BLAU

iv_model_ROA_BLAU <- ivreg(ROA_NW ~ # plm::lag(NIM_NW, 1) +
                             
                             BLAU_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage 
                           + DummyLivreAdmissao + DummyCentralEConfederacao
                          # + BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                           | 
                            #  BLAU_EM_Lag1
                           #  + BLAU_EM_Lag2
                             #  + PWOM_EM_Lag2
                             # + PWOM_EM_Lag3
                             #  + PWOM_EM_Lag4
                             #  + NIM_NW_Lag1
                             #  + NIM_NW_Lag2
                             #  + NIM_NW_Lag3
                             # + NIM_NW_Lag4
                             # PWOM_EM_Lag
                             #   + PWOM_SB
                          #   + PWOM_EM
                         #  + PWOM_EM_Lag1
                        #  + media_regional_pwon_bod
                         + media_regional_pwon_em
                           # +  plm::lag(PWOM_EM, 1) 
                           # + plm::lag(PWOM_EM, 2) 
                           #+ plm::lag(NIM_NW, 1)
                           #+ plm::lag(NIM_NW, 2)
                           
                           + PWOM_EM
                           #  + BLAU_EM
                           #   + SHAN_EM
                           # + SHAN_EM_Lag1
                           # + WON1_EM
                           # +  W_EM_33
                           # + NumberMenEM
                           #  + NumberMenEM
                           # + plm::lag(PWOM_EM, 2) 
                           + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                           + DummyLivreAdmissao + DummyCentralEConfederacao
                          # + BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                           ,
                           data = Data_Tenure_EM)



summary(iv_model_ROA_BLAU, diagnostics = TRUE)



#2SLS
#PWOM_EM:TenureEM - com interação - BLAU

iv_model_ROA_BLAU_INT <- ivreg(ROA_NW ~ 
                                 
                                 BLAU_EM + ln_TenureEM + BoardSizeEM + BLAU_EM:ln_TenureEM + CCSize + CCLeverage  
                               + DummyLivreAdmissao + DummyCentralEConfederacao
                              # + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                               | 
                                    BLAU_EM_Lag1
                                 + BLAU_EM:ln_TenureEM
                               #  + PWOM_EM_Lag2
                               # + PWOM_EM_Lag3
                               #  + PWOM_EM_Lag4
                               #  + NIM_NW_Lag1
                               #  + NIM_NW_Lag2
                               #  + NIM_NW_Lag3
                               # + NIM_NW_Lag4
                               # PWOM_EM_Lag
                               #   + PWOM_SB
                             #  + PWOM_EM
                              # + PWOM_EM_Lag1
                              + media_regional_pwon_em
                             # + media_regional_blau_em
                               # +  plm::lag(PWOM_EM, 1) 
                               # + plm::lag(PWOM_EM, 2) 
                               #+ plm::lag(NIM_NW, 1)
                               #+ plm::lag(NIM_NW, 2)
                               
                               # + PWOM_EM
                               #  + BLAU_EM
                               # + BLAU_EM
                               #  + SHAN_EM
                               # + WON1_EM
                               # +  W_EM_33
                               # + NumberMenEM
                               #  + NumberMenEM
                               # + plm::lag(PWOM_EM, 2) 
                               + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                               + DummyLivreAdmissao + DummyCentralEConfederacao
                              # + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                               ,
                               data = Data_Tenure_EM)



summary(iv_model_ROA_BLAU_INT, diagnostics = TRUE)

# novo ROE######

# testes OLS	full sample
# sem interação	
# com interação	
# blau sem interação
# blau com interação

#Estimar o modelo OLS com ln
ols_model_ROE <- lm(ROE_NW ~ PWOM_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage + 
                       + DummyLivreAdmissao + DummyCentralEConfederacao 
                    #+ BigFour 
                    + Covid + 
                      RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                    + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                    data = Data_Tenure_EM)

summary(ols_model_ROE)



# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROE_INT <- lm(ROE_NW ~ PWOM_EM + ln_TenureEM + BoardSizeEM + PWOM_EM:ln_TenureEM + CCSize + CCLeverage + 
                           + DummyLivreAdmissao + DummyCentralEConfederacao 
                        #+ BigFour 
                        + Covid + 
                          RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                        + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                        data = Data_Tenure_EM)

summary(ols_model_ROE_INT)


#Estimar o modelo OLS com ln - BLAU
ols_model_ROE_BLAU <- lm(ROE_NW ~ BLAU_EM + ln_TenureEM +  BoardSizeEM + CCSize + CCLeverage + 
                           + DummyLivreAdmissao + DummyCentralEConfederacao 
                        # + BigFour
                         + Covid + 
                           RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                         + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                         data = Data_Tenure_EM)

summary(ols_model_ROE_BLAU)


#BLAU
# = Women percentage + Tenure + Women percentage * Tenure + Other control variables (including year effect)

ols_model_ROE_BLAU_INT <- lm(ROE_NW ~ BLAU_EM + ln_TenureEM +  BoardSizeEM  + BLAU_EM:ln_TenureEM + CCSize + CCLeverage + 
                              + DummyLivreAdmissao + DummyCentralEConfederacao 
                             #+ BigFour
                             + Covid + 
                               RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                             + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021, 
                             data = Data_Tenure_EM)

summary(ols_model_ROE_BLAU_INT)


#2SLS
#Normal com ln

iv_model_ROE <- ivreg(ROE_NW ~ # plm::lag(NIM_NW, 1) +
                        
                        PWOM_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage  
                      + DummyLivreAdmissao + DummyCentralEConfederacao 
                     # + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                      | 
                        #   PWOM_EM_Lag1
                       #    + PWOM_EM_Lag2
                        #  + PWOM_EM_Lag3
                        #   + PWOM_EM_Lag4
                        #  + NIM_NW_Lag1
                        #  + NIM_NW_Lag2
                        #  + NIM_NW_Lag3
                        # + NIM_NW_Lag4
                        # PWOM_EM_Lag
                        #   + PWOM_SB
                        
                     # + ROE_NW_Lag1
                        # +  plm::lag(ROE_NW, 1) 
                        # + plm::lag(PWOM_EM, 2) 
                        #+ plm::lag(NIM_NW, 1)
                        #+ plm::lag(NIM_NW, 2)
                     + media_regional_pwon_em
                     #    + PWOM_BOD
                        + BLAU_EM
                      + BLAU_EM_Lag1
                      # + SHAN_EM
                      # + WON1_EM
                      # +  W_EM_33
                      # + NumberMenEM
                      #  + NumberMenEM
                      # + plm::lag(PWOM_EM, 2) 
                      + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                      + DummyLivreAdmissao + DummyCentralEConfederacao 
                      + BigFour 
                      + Covid 
                      + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                      + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                      ,
                      data = Data_Tenure_EM)



summary(iv_model_ROE, diagnostics = TRUE)



#2SLS
#PWOM_EM:TenureEM - com interação

iv_model_ROE_INT <- ivreg(ROE_NW ~ 
                            
                            PWOM_EM + ln_TenureEM + PWOM_EM:ln_TenureEM + CCSize + CCLeverage + ln_BoardSizeEM 
                          + DummyLivreAdmissao + DummyCentralEConfederacao 
                          + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                          | 
                               PWOM_EM_Lag1
                            + PWOM_EM:ln_TenureEM
                          #   + PWOM_EM_Lag2
                          #  + PWOM_EM_Lag3
                          #  + PWOM_EM_Lag4
                          #  + NIM_NW_Lag1
                          #  + NIM_NW_Lag2
                          #  + NIM_NW_Lag3
                          # + NIM_NW_Lag4
                          # PWOM_EM_Lag
                          #    + PWOM_SB
                          + PWOM_BOD
                          
                          # +  plm::lag(PWOM_EM, 1) 
                          # + plm::lag(PWOM_EM, 2) 
                          #+ plm::lag(NIM_NW, 1)
                          #+ plm::lag(NIM_NW, 2)
                          
                          # + PWOM_EM
                         # + BLAU_EM
                          #+ BLAU_EM_Lag1
                          # + BLAU_EM
                          #  + SHAN_EM
                          # + WON1_EM
                          # +  W_EM_33
                          # + NumberMenEM
                          #  + NumberMenEM
                          # + plm::lag(PWOM_EM, 2) 
                          + ln_TenureEM + CCSize + CCLeverage + ln_BoardSizeEM
                          + DummyLivreAdmissao + DummyCentralEConfederacao 
                          + BigFour 
                          + Covid 
                          + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                          + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                          ,
                          data = Data_Tenure_EM)



summary(iv_model_ROE_INT, diagnostics = TRUE)


#2SLS
#Normal com ln - BLAU

iv_model_ROE_BLAU <- ivreg(ROE_NW ~ # plm::lag(NIM_NW, 1) +
                             
                             BLAU_EM + ln_TenureEM + BoardSizeEM + CCSize + CCLeverage  
                           + DummyLivreAdmissao + DummyCentralEConfederacao 
                           #+ BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                           | 
                            #    BLAU_EM_Lag1
                             # + PWOM_EM_Lag1
                             # + PWOM_EM_Lag3
                             #  + PWOM_EM_Lag4
                             #  + NIM_NW_Lag1
                             #  + NIM_NW_Lag2
                             #  + NIM_NW_Lag3
                             # + NIM_NW_Lag4
                             # PWOM_EM_Lag
                             #  + PWOM_SB
                           #  + PWOM_BOD
                           + media_regional_pwon_em
                             + PWOM_EM
                           + PWOM_EM_Lag1
                           # +  plm::lag(PWOM_EM, 1) 
                           # + plm::lag(PWOM_EM, 2) 
                           #+ plm::lag(NIM_NW, 1)
                           #+ plm::lag(NIM_NW, 2)
                           
                           # + PWOM_EM
                           #  + BLAU_EM
                           # + SHAN_EM
                           # + WON1_EM
                           # +  W_EM_33
                           # + NumberMenEM
                           #  + NumberMenEM
                           # + plm::lag(PWOM_EM, 2) 
                           + ln_TenureEM + CCSize + CCLeverage + BoardSizeEM
                           + DummyLivreAdmissao + DummyCentralEConfederacao 
                          # + BigFour 
                           + Covid 
                           + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                           + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                           ,
                           data = Data_Tenure_EM)



summary(iv_model_ROE_BLAU, diagnostics = TRUE)


#2SLS
#PWOM_EM:TenureEM - com interação - BLAU

iv_model_ROE_BLAU_INT <- ivreg(ROE_NW ~ 
                                 
                                 BLAU_EM + ln_TenureEM + BLAU_EM:ln_TenureEM + CCSize + CCLeverage + ln_BoardSizeEM 
                               + DummyLivreAdmissao + DummyCentralEConfederacao 
                               + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021  
                               | 
                                   BLAU_EM_Lag1
                                 + BLAU_EM:ln_TenureEM
                               # + PWOM_EM_Lag2
                               # + PWOM_EM_Lag3
                               #  + PWOM_EM_Lag4
                               #  + NIM_NW_Lag1
                               #  + NIM_NW_Lag2
                               #  + NIM_NW_Lag3
                               # + NIM_NW_Lag4
                               # PWOM_EM_Lag
                                  + PWOM_BOD
                              # + PWOM_EM
                             #  + PWOM_EM_Lag1
                               # +  plm::lag(PWOM_EM, 1) 
                               # + plm::lag(PWOM_EM, 2) 
                               #+ plm::lag(NIM_NW, 1)
                               #+ plm::lag(NIM_NW, 2)
                               
                               #+ PWOM_EM
                               #  + BLAU_EM
                               # + BLAU_EM
                               #  + SHAN_EM
                               # + WON1_EM
                               # +  W_EM_33
                               # + NumberMenEM
                               #  + NumberMenEM
                               # + plm::lag(PWOM_EM, 2) 
                               + ln_TenureEM + CCSize + CCLeverage + ln_BoardSizeEM
                               + DummyLivreAdmissao + DummyCentralEConfederacao 
                               + BigFour 
                               + Covid 
                               + RegiaoDeAtuacao + Year_2009 + Year_2010 + Year_2011 + Year_2012 + Year_2013 + Year_2014 + Year_2015 
                               + Year_2016 + Year_2017 + Year_2018 + Year_2019 + Year_2020 + Year_2021
                               ,
                               data = Data_Tenure_EM)



summary(iv_model_ROE_BLAU_INT, diagnostics = TRUE)

#EXPORTAR BOD######

# library(car)
# # Teste Durbin-Watson
# dwtest(iv_model_ROE)
# 
# # Calculando o VIF
# car::vif(iv_model_ROE)


# Carregar o pacote necessário
library(stargazer)

# Criação do arquivo de texto com a saída de stargazer


#ROE

stargazer(ols_model_ROE, iv_model_ROE, ols_model_ROE_INT, iv_model_ROE_INT, 
          title = "Resultados das Regressões", 
          column.labels = c("OLS", "IV", "OLS interaction", "IV interaction"),
          align = TRUE, 
          type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
          digits = 3, 
          omit.stat = c("f", "ser"), 
          # covariate.labels = c("PWOM BOD", 
          #                      "Ln Tenure BOD", 
          #                      "CC Size", 
          #                      "CC Leverage", 
          #                      "Ln Board Size BOD", 
          #                      "Dummy Livre Admissao", 
          #                      "Big Four", 
          #                      "Covid",
          #                      "PWOM BOD × Ln Tenure BOD"),  # Adicione esta linha
          dep.var.labels = "ROE NW", 
          notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
          add.lines = list(
            c("Regional effect", "Yes", "Yes", "Yes", "Yes"), 
            c("Year effect", "Yes", "Yes", "Yes", "Yes")
          ), 
          no.space = TRUE, 
          single.row = FALSE)


#ROE  blau

stargazer(ols_model_ROE_BLAU, iv_model_ROE_BLAU, 
          #ols_model_ROE_BLAU_INT, iv_model_ROE_BLAU_INT, 
          title = "Resultados das Regressões", 
          column.labels = c("OLS", "IV"#, "OLS interaction", "IV interaction"
                            ),
          align = TRUE, 
          type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
          digits = 3, 
          omit.stat = c("f", "ser"), 
          # covariate.labels = c("BLAU BOD", 
          #                      "Ln Tenure BOD", 
          #                      "CC Size", 
          #                      "CC Leverage", 
          #                      "Ln Board Size BOD", 
          #                      "Dummy Livre Admissao", 
          #                      "Big Four", 
          #                      "Covid"
          #                       #"BLAU BOD × Ln Tenure BOD"
          #                      ),  # Adicione esta linha
          dep.var.labels = "ROE NW", 
          notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
          add.lines = list(
            c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
              ), 
            c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
              )
          ), 
          no.space = TRUE, 
          single.row = FALSE)



stargazer(#ols_model_ROE_BLAU, iv_model_ROE_BLAU, 
          ols_model_ROE_BLAU_INT, iv_model_ROE_BLAU_INT, 
          title = "Resultados das Regressões", 
          column.labels = c(#"OLS", "IV", 
                            "OLS interaction", "IV interaction"),
          align = TRUE, 
          type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
          digits = 3, 
          omit.stat = c("f", "ser"), 
          # covariate.labels = c("BLAU BOD", 
          #                      "Ln Tenure BOD", 
          #                      "CC Size", 
          #                      "CC Leverage", 
          #                      "Ln Board Size BOD", 
          #                      "Dummy Livre Admissao", 
          #                      "Big Four", 
          #                      "Covid"
          #                      #,
          #                     # "BLAU BOD × Ln Tenure BOD"
          #                     ),  # Adicione esta linha
          dep.var.labels = "ROE NW", 
          notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
          add.lines = list(
            c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
              ), 
            c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
              )
          ), 
          no.space = TRUE, 
          single.row = FALSE)



#ROA

stargazer(ols_model_ROA, iv_model_ROA, ols_model_ROA_INT, iv_model_ROA_INT, 
          title = "Resultados das Regressões", 
          column.labels = c("OLS", "IV", "OLS interaction", "IV interaction"),
          align = TRUE, 
          type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
          digits = 3, 
          omit.stat = c("f", "ser"), 
          # covariate.labels = c("PWOM BOD", 
          #                      "Ln Tenure BOD", 
          #                      "CC Size", 
          #                      "CC Leverage", 
          #                      "Ln Board Size BOD", 
          #                      "Dummy Livre Admissao", 
          #                      "Big Four", 
          #                      "Covid",
          #                      "PWOM BOD × Ln Tenure BOD"),  # Adicione esta linha
          dep.var.labels = "ROA NW", 
          notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
          add.lines = list(
            c("Regional effect", "Yes", "Yes", "Yes", "Yes"), 
            c("Year effect", "Yes", "Yes", "Yes", "Yes")
          ), 
          no.space = TRUE, 
          single.row = FALSE)


#ROA  blau

stargazer(ols_model_ROA_BLAU, iv_model_ROA_BLAU, 
          #ols_model_ROA_BLAU_INT, iv_model_ROA_BLAU_INT, 
          title = "Resultados das Regressões", 
          column.labels = c("OLS", "IV"#, "OLS interaction", "IV interaction"
          ),
          align = TRUE, 
          type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
          digits = 3, 
          omit.stat = c("f", "ser"), 
          # covariate.labels = c("BLAU BOD", 
          #                      "Ln Tenure BOD", 
          #                      "CC Size", 
          #                      "CC Leverage", 
          #                      "Ln Board Size BOD", 
          #                      "Dummy Livre Admissao", 
          #                      "Big Four", 
          #                      "Covid"
          #                       #"BLAU BOD × Ln Tenure BOD"
          #                      ),  # Adicione esta linha
          dep.var.labels = "ROA NW", 
          notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
          add.lines = list(
            c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
            ), 
            c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
            )
          ), 
          no.space = TRUE, 
          single.row = FALSE)



stargazer(#ols_model_ROA_BLAU, iv_model_ROA_BLAU, 
  ols_model_ROA_BLAU_INT, iv_model_ROA_BLAU_INT, 
  title = "Resultados das Regressões", 
  column.labels = c(#"OLS", "IV", 
    "OLS interaction", "IV interaction"),
  align = TRUE, 
  type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
  digits = 3, 
  omit.stat = c("f", "ser"), 
  # covariate.labels = c("BLAU BOD", 
  #                      "Ln Tenure BOD", 
  #                      "CC Size", 
  #                      "CC Leverage", 
  #                      "Ln Board Size BOD", 
  #                      "Dummy Livre Admissao", 
  #                      "Big Four", 
  #                      "Covid"
  #                      #,
  #                     # "BLAU BOD × Ln Tenure BOD"
  #                     ),  # Adicione esta linha
  dep.var.labels = "ROA NW", 
  notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
  add.lines = list(
    c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
    ), 
    c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
    )
  ), 
  no.space = TRUE, 
  single.row = FALSE)


#EXPORTAR EM######
  
  # library(car)
  # # Teste Durbin-Watson
  # dwtest(iv_model_ROE)
  # 
  # # Calculando o VIF
  # car::vif(iv_model_ROE)
  
  
  # Carregar o pacote necessário
  library(stargazer)
  
  # Criação do arquivo de texto com a saída de stargazer
  
  
  #ROE
  
  stargazer(ols_model_ROE, iv_model_ROE, ols_model_ROE_INT, iv_model_ROE_INT, 
            title = "Resultados das Regressões", 
            column.labels = c("OLS", "IV", "OLS interaction", "IV interaction"),
            align = TRUE, 
            type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
            digits = 3, 
            omit.stat = c("f", "ser"), 
            # covariate.labels = c("PWOM EM", 
            #                      "Ln Tenure EM", 
            #                      "CC Size", 
            #                      "CC Leverage", 
            #                      "Ln Board Size EM", 
            #                      "Dummy Livre Admissao", 
            #                      "Big Four", 
            #                      "Covid",
            #                      "PWOM EM × Ln Tenure EM"),  # Adicione esta linha
            dep.var.labels = "ROE NW", 
            notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
            add.lines = list(
              c("Regional effect", "Yes", "Yes", "Yes", "Yes"), 
              c("Year effect", "Yes", "Yes", "Yes", "Yes")
            ), 
            no.space = TRUE, 
            single.row = FALSE)
  
  
  #ROE  blau
  
  stargazer(ols_model_ROE_BLAU, iv_model_ROE_BLAU, 
            #ols_model_ROE_BLAU_INT, iv_model_ROE_BLAU_INT, 
            title = "Resultados das Regressões", 
            column.labels = c("OLS", "IV"#, "OLS interaction", "IV interaction"
            ),
            align = TRUE, 
            type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
            digits = 3, 
            omit.stat = c("f", "ser"), 
            # covariate.labels = c("BLAU EM", 
            #                      "Ln Tenure EM", 
            #                      "CC Size", 
            #                      "CC Leverage", 
            #                      "Ln Board Size EM", 
            #                      "Dummy Livre Admissao", 
            #                      "Big Four", 
            #                      "Covid"
            #                       #"BLAU EM × Ln Tenure EM"
            #                      ),  # Adicione esta linha
            dep.var.labels = "ROE NW", 
            notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
            add.lines = list(
              c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
              ), 
              c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
              )
            ), 
            no.space = TRUE, 
            single.row = FALSE)
  
  
  
  stargazer(#ols_model_ROE_BLAU, iv_model_ROE_BLAU, 
    ols_model_ROE_BLAU_INT, iv_model_ROE_BLAU_INT, 
    title = "Resultados das Regressões", 
    column.labels = c(#"OLS", "IV", 
      "OLS interaction", "IV interaction"),
    align = TRUE, 
    type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
    digits = 3, 
    omit.stat = c("f", "ser"), 
    # covariate.labels = c("BLAU EM", 
    #                      "Ln Tenure EM", 
    #                      "CC Size", 
    #                      "CC Leverage", 
    #                      "Ln Board Size EM", 
    #                      "Dummy Livre Admissao", 
    #                      "Big Four", 
    #                      "Covid"
    #                      #,
    #                     # "BLAU EM × Ln Tenure EM"
    #                     ),  # Adicione esta linha
    dep.var.labels = "ROE NW", 
    notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
    add.lines = list(
      c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
      ), 
      c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
      )
    ), 
    no.space = TRUE, 
    single.row = FALSE)
  
  
  
  #ROA
  
  stargazer(ols_model_ROA, iv_model_ROA, ols_model_ROA_INT, iv_model_ROA_INT, 
            title = "Resultados das Regressões", 
            column.labels = c("OLS", "IV", "OLS interaction", "IV interaction"),
            align = TRUE, 
            type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
            digits = 3, 
            omit.stat = c("f", "ser"), 
            # covariate.labels = c("PWOM EM", 
            #                      "Ln Tenure EM", 
            #                      "CC Size", 
            #                      "CC Leverage", 
            #                      "Ln Board Size EM", 
            #                      "Dummy Livre Admissao", 
            #                      "Big Four", 
            #                      "Covid",
            #                      "PWOM EM × Ln Tenure EM"),  # Adicione esta linha
            dep.var.labels = "ROA NW", 
            notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
            add.lines = list(
              c("Regional effect", "Yes", "Yes", "Yes", "Yes"), 
              c("Year effect", "Yes", "Yes", "Yes", "Yes")
            ), 
            no.space = TRUE, 
            single.row = FALSE)
  
  
  #ROA  blau
  
  stargazer(ols_model_ROA_BLAU, iv_model_ROA_BLAU, 
            #ols_model_ROA_BLAU_INT, iv_model_ROA_BLAU_INT, 
            title = "Resultados das Regressões", 
            column.labels = c("OLS", "IV"#, "OLS interaction", "IV interaction"
            ),
            align = TRUE, 
            type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
            digits = 3, 
            omit.stat = c("f", "ser"), 
            # covariate.labels = c("BLAU EM", 
            #                      "Ln Tenure EM", 
            #                      "CC Size", 
            #                      "CC Leverage", 
            #                      "Ln Board Size EM", 
            #                      "Dummy Livre Admissao", 
            #                      "Big Four", 
            #                      "Covid"
            #                       #"BLAU EM × Ln Tenure EM"
            #                      ),  # Adicione esta linha
            dep.var.labels = "ROA NW", 
            notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
            add.lines = list(
              c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
              ), 
              c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
              )
            ), 
            no.space = TRUE, 
            single.row = FALSE)
  
  
  
  stargazer(#ols_model_ROA_BLAU, iv_model_ROA_BLAU, 
    ols_model_ROA_BLAU_INT, iv_model_ROA_BLAU_INT, 
    title = "Resultados das Regressões", 
    column.labels = c(#"OLS", "IV", 
      "OLS interaction", "IV interaction"),
    align = TRUE, 
    type = "text",  # Pode ser alterado para "latex" ou "html" se necessário
    digits = 3, 
    omit.stat = c("f", "ser"), 
    # covariate.labels = c("BLAU EM", 
    #                      "Ln Tenure EM", 
    #                      "CC Size", 
    #                      "CC Leverage", 
    #                      "Ln Board Size EM", 
    #                      "Dummy Livre Admissao", 
    #                      "Big Four", 
    #                      "Covid"
    #                      #,
    #                     # "BLAU EM × Ln Tenure EM"
    #                     ),  # Adicione esta linha
    dep.var.labels = "ROA NW", 
    notes = "Standard errors in parentheses. *** p<0.01, ** p<0.05, *<0.1,", 
    add.lines = list(
      c("Regional effect", "Yes", "Yes"#, "Yes", "Yes"
      ), 
      c("Year effect", "Yes", "Yes"#, "Yes", "Yes"
      )
    ), 
    no.space = TRUE, 
    single.row = FALSE)