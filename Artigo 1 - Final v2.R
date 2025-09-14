#### INSTALLING PACKAGES ####
pacotes <- c("huxreg", "systemfit", "ivpack","robustbase", "tsibbledata","timetk", "magrittr", "base", "basictabler", "bibtex", "BiocManager", "bookdown", "caper", "car", "caret", "conflicted", "correlation", "corrplot", "cowplot", "DescTools", "distill", "dplyr", "encrptr", "factoextra", "FactoMineR", "faraway", "fastDummies", "flextable", "foreign", "gdata", "ggrepel", "ggtree", "ggplot2", "ggpubr", "graphics", "grid", "gridExtra", "gtsummary", "Hmisc", "httr2", "jsmodule", "jtools", "knitr", "kableExtra", "knitLatex", "lmtest", "lubridate", "magick", "margins", "marginaleffects", "MASS", "MatchIt", "mfx", "mgcv", "minqa", "modelr", "nnet", "nortest", "OddsPlotty", "papaja", "pandoc", "palmerpenguins", "performance", "pglm", "plm", "plotly", "pROC", "pscl", "psych", "rddtools", "readr", "regclass", "rJava", "readxl", "RefManageR", "remotes", "reshape2", "repos", "report", "ReporteRs", "rgl", "rlang", "rmarkdown", "Rmisc", "ROCR", "RSelenium", "scales", "sjlabelled", "stargazer", "stats", "stringr", "stringi", "texreg", "tidyr", "tidyverse", "tinytex", "tseries", "truncnorm", "visreg", "viridis", "xfun", "xlsx", "xtable", "wesanderson", "writexl", "vdr", "sandwich", "openintro", "OIdata", "doBy", "ivpack", "vtable", "summarytools",
             
             "htmltools", "DescTools", "sandwich", "lmtest", "car", "dplyr", "stargazer", "ggplot2", "foreign",
             
             "openintro","OIdata", "gdata", "doBy","ivpack", "psych","plm", "readxl", "vtable", "summarytools", "gtsummary", "AER","base", "basictabler", "bibtex","BiocManager", "bookdown","caper", "car","caret", "conflicted","correlation","corrplot","cowplot","DescTools","distill","dplyr","encrptr","factoextra","FactoMineR", "faraway","fastDummies","flextable","foreign","gdata", "ggrepel","ggtree", "ggplot2","ggpubr","graphics", "grid","gridExtra","gtsummary","Hmisc","httr2","jsmodule", "jtools","knitr","kableExtra","knitLatex","lmtest", "lubridate","magick","margins", "marginaleffects", "MASS", "MatchIt","mfx","mgcv","minqa","modelr" ,"mgcv","nnet","nortest","OddsPlotty","papaja", "pandoc","palmerpenguins","performance", "pglm","plm","plotly","plotly", "pROC","pscl","psych","rddtools","readr", "regclass","rJava", "readxl","RefManageR", "remotes", "reshape2", "repos", "report","ReporteRs","reshape2","rgl","rlang","rmarkdown","Rmisc","ROCR","RSelenium", "scales","sjlabelled", "stargazer","stats","stringr", "stringi","texreg","tidyr","tidyverse", "tinytex","tseries","truncnorm", "visreg","viridis", "xfun","xlsx","xtable","wesanderson", "writexl","vdr","gtsummary",
             
             "AER","base","mvoutlier", "basictabler", "bibtex","BiocManager", "bookdown","caper", "car","caret", "conflicted","correlation","corrplot","cowplot","DescTools","distill","dplyr","encrptr","factoextra","FactoMineR", "faraway","fastDummies","flextable","foreign","gdata", "ggrepel","ggtree", "ggplot2","ggpubr","graphics", "grid","gridExtra","gtsummary","Hmisc","httr2","jsmodule", "jtools","knitr","kableExtra","knitLatex","lmtest", "lubridate","magick","margins", "marginaleffects", "MASS", "MatchIt","mfx","mgcv","minqa","modelr" ,"mgcv","nnet","nortest","OddsPlotty","papaja", "pandoc","palmerpenguins","performance", "pglm","plm","plotly","plotly", "pROC","pscl","psych","rddtools","readr", "regclass","rJava", "readxl","RefManageR", "remotes", "reshape2", "repos", "report","ReporteRs","reshape2","rgl","rlang","rmarkdown","Rmisc","ROCR","RSelenium", "scales","sjlabelled", "stargazer","stats","stringr", "stringi","texreg","tidyr","tidyverse", "tinytex","tseries","truncnorm", "visreg","viridis", "xfun","xlsx","xtable","wesanderson", "writexl","vdr", "modelsummary")
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


#### IMPORTING FILE ####

library(readxl)
final_v_3 <- read_excel("F:/BACKUP GERAL 01.06.2020/UFSC/2025/Artigo 3 - Tese/FINAL/final v.4.xlsx", 
                        col_types = c("text", "numeric", "text", 
                                      "numeric", "numeric", "text", 
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "text", "numeric", "text", 
                                      "text", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "text", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

#View(final_v_3)


summary(final_v_3)


# Carregar pacotes necessários
library(dplyr)


# # Aplicar transformação logarítmica
# final_v_4 <- final_v_3 %>%
#   mutate(
#     ROE_log = log(ROE + 1),
#     ROA_log = log(ROA + 1),
#     LLPRatio_log = log(LLPRatio + 1),
#     NIM_log = log(NIM + 1),
#     CostToIncome_log = log(CostToIncome + 1),
#     PER_log = log(PER + 1)
#   )


final_v_4 <- final_v_3 %>%
  dplyr::mutate(
    ROE_log = ifelse(ROE > 0, log(ROE + 1), -log(1 + abs(ROE))),
    ROA_log = ifelse(ROA > 0, log(ROA + 1), -log(1 + abs(ROA))),
    LLPRatio_log = log1p(LLPRatio),  # log(1 + x), evita problemas com valores pequenos
    NIM_log = log1p(NIM),
    CostToIncome_log = log1p(CostToIncome),
    PER_log = ifelse(PER > 0, log(PER + 1), NA)  # Se PER pode ser negativo, evitar erro
  )



# Verificar a transformação
summary(final_v_4[, c("ROE_log", "ROA_log", "LLPRatio_log", "NIM_log", "CostToIncome_log", "PER_log")])

summary(final_v_4)


par(mfrow = c(2, 3))  # Organiza os gráficos em 2 linhas e 3 colunas
hist(final_v_4$ROE_log, main = "Distribuição de ROE_log", col = "lightblue")
hist(final_v_4$ROA_log, main = "Distribuição de ROA_log", col = "lightgreen")
hist(final_v_4$LLPRatio_log, main = "Distribuição de LLPRatio_log", col = "lightcoral")
hist(final_v_4$NIM_log, main = "Distribuição de NIM_log", col = "lightpink")
hist(final_v_4$CostToIncome_log, main = "Distribuição de CostToIncome_log", col = "lightgoldenrod")
hist(final_v_4$PER_log, main = "Distribuição de PER_log", col = "lightcyan")

par(mfrow = c(1, 1))  # Reseta o layout de gráficos
par(mar = c(4, 4, 2, 1))  # Ajusta as margens: c(bottom, left, top, right)


par(mfrow = c(2, 3))  # Organiza em 2 linhas e 3 colunas
hist(final_v_4$ROE, main = "ROE", col = "lightblue", xlab = "ROE")
hist(final_v_4$ROA, main = "ROA", col = "lightgreen", xlab = "ROA")
hist(final_v_4$LLPRatio, main = "LLPRatio", col = "lightcoral", xlab = "LLPRatio")
hist(final_v_4$NIM, main = "NIM", col = "lightpink", xlab = "NIM")
hist(final_v_4$CostToIncome, main = "CostToIncome", col = "lightgoldenrod", xlab = "CostToIncome")
hist(final_v_4$PER, main = "PER", col = "lightcyan", xlab = "PER")

par(mfrow = c(2, 3))  # Novamente 2 linhas e 3 colunas
hist(final_v_4$ROE_log, main = "ROE_log", col = "blue", xlab = "ROE_log")
hist(final_v_4$ROA_log, main = "ROA_log", col = "green", xlab = "ROA_log")
hist(final_v_4$LLPRatio_log, main = "LLPRatio_log", col = "red", xlab = "LLPRatio_log")
hist(final_v_4$NIM_log, main = "NIM_log", col = "purple", xlab = "NIM_log")
hist(final_v_4$CostToIncome_log, main = "CostToIncome_log", col = "gold", xlab = "CostToIncome_log")
hist(final_v_4$PER_log, main = "PER_log", col = "cyan", xlab = "PER_log")


# 📌 LIX Index Ranges from Appendix 2
# LIX Score	Classification (Appendix 2)
# ≤ 35	Very easy
# 36 - 45	Easy
# 46 - 55	Standard
# 56 - 65	Difficult
# ≥ 66	Very difficult

# 
# # Create a new column with LIX classification based on Appendix 2
# final_v_4 <- final_v_4 %>%
#   mutate(LIX_Classification = case_when(
#     LIX_Index <= 35 ~ "Very easy",
#     LIX_Index >= 36 & LIX_Index <= 45 ~ "Easy",
#     LIX_Index >= 46 & LIX_Index <= 55 ~ "Standard",
#     LIX_Index >= 56 & LIX_Index <= 65 ~ "Difficult",
#     LIX_Index >= 66 ~ "Very difficult"
#   ))

# Check the distribution of categories
#table(final_v_4$LIX_Classification)



colnames(final_v_4)[colnames(final_v_4) == "Flesch-Kincaid"] <- "Flesch_Kincaid"
colnames(final_v_4)[colnames(final_v_4) == "New_Dale-Chall"] <- "New_Dale_Chall"


####Verificar se há valores NA nos identificadores #####
sum(is.na(final_v_4$CNPJ))  # Deve ser 0
sum(is.na(final_v_4$Year))  # Deve ser 0

# Contar quantas observações únicas temos por painel
length(unique(final_v_4$CNPJ))  # Número de indivíduos no painel
length(unique(final_v_4$Year))  # Número de períodos

# Verificar duplicatas
table(duplicated(final_v_4[, c("CNPJ", "Year")]))  # Deve retornar apenas "FALSE"


# Verificar quais CNPJs têm mais de uma entrada por ano
duplicados <- final_v_4[duplicated(final_v_4[, c("CNPJ", "Year")]), ]
print(duplicados)

final_v_4 <- final_v_4[!duplicated(final_v_4[, c("CNPJ", "Year")]), ]



final_v_4 <- final_v_4[!duplicated(final_v_4[, c("CNPJ", "Year")]), ]
sum(is.na(final_v_4$CNPJ))  # Deve ser 0
sum(is.na(final_v_4$Year))  # Deve ser 0



summary(final_v_4)

#### Deleting Rows with NAs ####

final_v_4 <- final_v_4 %>%
  dplyr::filter(ROE != "NA", ROA != "NA")

#####renomear coluna######
names(final_v_4)[names(final_v_4) == "File_Size_(KB)"] <- "File_Size_KB"

names(final_v_4)[names(final_v_4) == "Type_Token_Ratio_(TTR)"] <- "Type_Token_Ratio_TTR"



summary(final_v_4)


library(dplyr)
library(DescTools)  # Para Winsorize()

####Criar variação do ROE#####

# Carregar pacotes necessários
library(dplyr)

final_v_4 <- final_v_4 %>%
  dplyr::group_by(CNPJ) %>%
  dplyr::mutate(
    delta_roe_t1 = dplyr::lead(ROE, 1) - ROE,
    delta_roe_t2 = dplyr::lead(ROE, 2) - ROE,
    delta_roe_t3 = dplyr::lead(ROE, 3) - ROE
  ) %>%
  dplyr::ungroup()

summary(final_v_4)




#### Winsorization ####
final_v_4 <- final_v_4 %>%
  dplyr::mutate(
    ROE_NW = Winsorize(ROE, probs = c(0.01, 0.99), na.rm = TRUE),
    ROE_NW_2.5 = Winsorize(ROE, probs = c(0.025, 0.975), na.rm = TRUE),
    ROE_NW_5 = Winsorize(ROE, probs = c(0.05, 0.95), na.rm = TRUE),
    
    ROA_NW = Winsorize(ROA, probs = c(0.01, 0.99), na.rm = TRUE),
    ROA_NW_2.5 = Winsorize(ROA, probs = c(0.025, 0.975), na.rm = TRUE),
    ROA_NW_5 = Winsorize(ROA, probs = c(0.05, 0.95), na.rm = TRUE)
  )


# Certifique-se de que o pacote DescTools está carregado
library(DescTools)

# Winsorizar delta_roe_t1 nos percentis 1% e 99%
final_v_4 <- final_v_4 %>%
  dplyr::mutate(
    delta_roe_t1_NW = Winsorize(delta_roe_t1, probs = c(0.01, 0.99), na.rm = TRUE)
  )


summary(final_v_4)

#Logs de Word_Count e File_Size_KB

final_v_4 <- final_v_4 %>%
  dplyr::mutate(
    log_WordCount = log(Word_Count),
    log_FileSize = log(File_Size_KB),
    log_PageCount = log(Page_Count)
  )


library(plm)


#criar um df para Centrais ou Confederações#########

library(dplyr)

# Filtra apenas as cooperativas ligadas a Centrais ou Confederações
final_v_6_central_confed <- final_v_4 %>%
  dplyr::filter(DummyCentralEConfederacao == 1)

nrow(final_v_6_central_confed)

# Filtra apenas as cooperativas ligadas a Centrais ou Confederações
final_v_7_LivreAdmissao <- final_v_4 %>%
  dplyr::filter(DummyLivreAdmissao == 1)


#apenas as cooperativas que não são de livre admissão,
final_v_8_NaoLivreAdmissao <- final_v_4 %>%
  dplyr::filter(DummyLivreAdmissao == 0)


nrow(final_v_4)
nrow(final_v_6_central_confed)
nrow(final_v_7_LivreAdmissao)
nrow(final_v_8_NaoLivreAdmissao)


#correlação 

# 1. Rodar o modelo OLS com todas as variáveis
ols_formula <- as.formula(
  ROE_NW ~ LIX_Index + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao +
    RegiaoDeAtuacao + factor(Year)
)

ols_model <- lm(ols_formula, data = final_v_4)

# 2. Calcular o VIF
library(car)
vif(ols_model)





library(dplyr)
library(AER)

# Garantir que Year seja numérico para ordenar corretamente
final_v_4 <- final_v_4 %>%
  mutate(Year = as.numeric(as.character(Year)))

library(dplyr)

# Remover todas as colunas de lag antigas (se existirem)
final_v_4 <- final_v_4 %>% select(-starts_with("lag_"))

# Garantir que 'Year' é numérico
final_v_4 <- final_v_4 %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  arrange(CNPJ, Year)

library(plm)

# Transformar temporariamente para painel
pdata_temp <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Criar defasagens principais diretamente no final_v_4
final_v_4$lag_FileSize_1      <- plm::lag(pdata_temp$File_Size_KB, 1)
final_v_4$lag_FileSize_2      <- plm::lag(pdata_temp$File_Size_KB, 2)

final_v_4$lag_PageCount_1     <- plm::lag(pdata_temp$Page_Count, 1)
final_v_4$lag_PageCount_2     <- plm::lag(pdata_temp$Page_Count, 2)

final_v_4$lag_WordCount_1     <- plm::lag(pdata_temp$Word_Count, 1)
final_v_4$lag_WordCount_2     <- plm::lag(pdata_temp$Word_Count, 2)

# Log-transform
final_v_4$lag_logFileSize_1   <- plm::lag(log(pdata_temp$File_Size_KB), 1)
final_v_4$lag_logFileSize_2   <- plm::lag(log(pdata_temp$File_Size_KB), 2)

final_v_4$lag_logPageCount_1  <- plm::lag(log(pdata_temp$Page_Count), 1)
final_v_4$lag_logPageCount_2  <- plm::lag(log(pdata_temp$Page_Count), 2)

final_v_4$lag_logWordCount_1  <- plm::lag(log(pdata_temp$Word_Count), 1)
final_v_4$lag_logWordCount_2  <- plm::lag(log(pdata_temp$Word_Count), 2)

# Legibilidade
final_v_4$lag_LIX_1           <- plm::lag(pdata_temp$LIX_Index, 1)
final_v_4$lag_LIX_2           <- plm::lag(pdata_temp$LIX_Index, 2)

final_v_4$lag_RIX_1           <- plm::lag(pdata_temp$RIX_Index, 1)
final_v_4$lag_RIX_2           <- plm::lag(pdata_temp$RIX_Index, 2)

final_v_4$lag_ARI_1           <- plm::lag(pdata_temp$ARI_Index, 1)
final_v_4$lag_ARI_2           <- plm::lag(pdata_temp$ARI_Index, 2)

final_v_4$lag_FOG_1           <- plm::lag(pdata_temp$Fog_Index, 1)
final_v_4$lag_FOG_2           <- plm::lag(pdata_temp$Fog_Index, 2)

final_v_4$lag_SMOG_1          <- plm::lag(pdata_temp$SMOG_Index, 1)
final_v_4$lag_SMOG_2          <- plm::lag(pdata_temp$SMOG_Index, 2)



# 🔍 Verifique com exemplo
final_v_4 %>%
  filter(CNPJ == 68987000186) %>%
  select(Year, File_Size_KB, lag_FileSize_1) %>%
  arrange(Year)



#tibble::view(final_v_4)



library(AER)
library(dplyr)

# Garantir fatores
final_v_4$RegiaoDeAtuacao <- as.factor(final_v_4$RegiaoDeAtuacao)
final_v_4$Year <- as.factor(final_v_4$Year)
final_v_4$Year <- as.numeric(as.character(final_v_4$Year))

final_v_4 <- final_v_4 %>%
  mutate(Year = as.numeric(as.character(Year)))


# Criar LAGS das explicativas############
final_v_4 <- final_v_4 %>%
  dplyr::arrange(CNPJ, Year) %>%
  group_by(CNPJ) %>%
  dplyr::mutate(
    lag_FileSize_1 = lag(File_Size_KB, 1),
    lag_FileSize_2 = lag(File_Size_KB, 2),
    lag_logFileSize_1 = lag(log_FileSize, 1),
    lag_logFileSize_2 = lag(log_FileSize, 2),
    lag_PageCount_1 = lag(Page_Count, 1),
    lag_PageCount_2 = lag(Page_Count, 2),
    lag_logPageCount_1 = lag(log_PageCount, 1),
    lag_logPageCount_2 = lag(log_PageCount, 2),
    lag_WordCount_1 = lag(Word_Count, 1),
    lag_WordCount_2 = lag(Word_Count, 2),
    lag_logWordCount_1 = lag(log_WordCount, 1),
    lag_logWordCount_2 = lag(log_WordCount, 2)
  ) %>%
  ungroup()


# Garantir que RegiaoDeAtuacao e Year são fatores
final_v_4$RegiaoDeAtuacao <- as.factor(final_v_4$RegiaoDeAtuacao)
final_v_4$Year <- as.factor(final_v_4$Year)

# Criar defasagens para índices de legibilidade
final_v_4 <- final_v_4 %>%
  dplyr::arrange(CNPJ, Year) %>%
  group_by(CNPJ) %>%
  dplyr::mutate(
    # LIX
    lag_LIX_1 = lag(LIX_Index, 1),
    lag_LIX_2 = lag(LIX_Index, 2),
    
    # RIX
    lag_RIX_1 = lag(RIX_Index, 1),
    lag_RIX_2 = lag(RIX_Index, 2),
    
    # ARI
    lag_ARI_1 = lag(ARI_Index, 1),
    lag_ARI_2 = lag(ARI_Index, 2),
    
    # Fog
    lag_FOG_1 = lag(Fog_Index, 1),
    lag_FOG_2 = lag(Fog_Index, 2),
    
    # SMOG
    lag_SMOG_1 = lag(SMOG_Index, 1),
    lag_SMOG_2 = lag(SMOG_Index, 2)
  ) %>%
  ungroup()




####✅ 1. Cálculo dos instrumentos: médias regionais e anuais#####
# Carregar pacotes
library(dplyr)
library(AER)  # para ivreg

# Criar as variáveis instrumentais: médias por região e ano
final_v_4 <- final_v_4 %>%
  dplyr::group_by(RegiaoDeAtuacao, Year) %>%
  dplyr::mutate(
    inst_FileSize = mean(File_Size_KB, na.rm = TRUE),
    inst_log_FileSize = mean(log_FileSize, na.rm = TRUE),
    inst_PageCount = mean(Page_Count, na.rm = TRUE),
    inst_log_PageCount = mean(log_PageCount, na.rm = TRUE),
    inst_WordCount = mean(Word_Count, na.rm = TRUE),
    inst_log_WordCount = mean(log_WordCount, na.rm = TRUE),
    inst_LIX = mean(LIX_Index, na.rm = TRUE),
    inst_RIX = mean(RIX_Index, na.rm = TRUE),
    inst_ARI = mean(ARI_Index, na.rm = TRUE),
    inst_FOG = mean(Fog_Index, na.rm = TRUE),
    inst_SMOG = mean(SMOG_Index, na.rm = TRUE),
  ) %>%
  ungroup()

summary(final_v_4)




#Descriptive Statistics#####

summary(final_v_4)

# Carregando pacotes necessários
library(moments)    # Para skewness
library(openxlsx)   # Para salvar em Excel
library(dplyr)      # Para n_distinct

# Resumo inicial
summary(final_v_4)

# Função para calcular estatísticas descritivas
summary_stats <- function(df) {
  # Lista das variáveis de interesse
  vars <- c("ROE_NW", "ROA_NW", "File_Size_KB", "Page_Count", "Fog_Index",
            "CCSize", "CCLeverage")
  
  # Criando data frame para armazenar as estatísticas
  stats <- data.frame(
    Variable = vars,
    Observations = NA,
    Mean = NA,
    SD = NA,
    Min = NA,
    Max = NA,
    Skewness = NA
  )
  
  # Loop pelas variáveis
  for(i in 1:nrow(stats)) {
    var <- stats$Variable[i]
    if(var %in% names(df)) {
      cleaned_data <- df[[var]][!is.na(df[[var]])]
      
      stats$Observations[i] <- length(cleaned_data)
      stats$Mean[i] <- mean(cleaned_data)
      stats$SD[i] <- sd(cleaned_data)
      stats$Min[i] <- min(cleaned_data)
      stats$Max[i] <- max(cleaned_data)
      stats$Skewness[i] <- skewness(cleaned_data)
    }
  }
  
  return(stats)
}

# Aplicando a função à base final_v_4
summary_stats_result <- summary_stats(final_v_4)

# Visualizando o resultado
print(summary_stats_result)

# Caminho para salvar o arquivo
#output_path <- "F:/BACKUP GERAL 01.06.2020/UFSC/2025/Artigo 1 - Tese/Journal of Economic and Administrative Sciences/statistics R/summary_stats.xlsx"

# Salvando no Excel
#write.xlsx(summary_stats_result, output_path)

cat("✅ Arquivo Excel salvo em:", output_path, "\n")

# Estatísticas sobre Data_Tenure
quantidade_cooperativas <- n_distinct(final_v_4$CNPJ)  # CNPJs únicos
quantidade_observacoes <- nrow(final_v_4)              # Total de linhas

# Exibindo
cat("Cooperativas distintas (CNPJ):", quantidade_cooperativas, "\n")
cat("Número total de observações:", quantidade_observacoes, "\n")



#####matriz correlação######


# Executar a função e salvar o resultado real das estatísticas
Base_Correlacao <- final_v_4[, c("ROE_NW", "ROA_NW", "File_Size_KB", 
                                 "Page_Count", "Fog_Index", "DummyLivreAdmissao", 
                                 "CCSize", "CCLeverage")]



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

#tibble::view(Matriz_Correlacao_Pearson)

#write_xlsx(Matriz_Correlacao_Pearson,"F:/BACKUP GERAL 01.06.2020/UFSC/2025/Artigo 1 - Tese/Journal of Economic and Administrative Sciences/statistics R/Corr_pearson.xlsx")
#write_xlsx(Matriz_Correlacao_Spearman, "F:/BACKUP GERAL 01.06.2020/UFSC/2025/Artigo 1 - Tese/Journal of Economic and Administrative Sciences/statistics R/Corr_spearman.xlsx")




#graficos#######

library(dplyr)

modificacoes_por_cnpj <- final_v_4 %>%
  arrange(CNPJ, Year) %>%
  group_by(CNPJ) %>%
  summarise(
    Modificacoes_Total = {
      pages <- Page_Count
      sum(rle(pages)$lengths) - 1  # total de mudanças reais
    },
    .groups = "drop"
  )

# Verifica
print(modificacoes_por_cnpj)


library(ggplot2)

ggplot(modificacoes_por_cnpj, aes(x = Modificacoes_Total)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Bylaws Modifications per Cooperative",
    x = "Total Modifications (change in number of pages)",
    y = "Number of Cooperatives"
  )+
  theme_minimal()



# Número total de estatutos (observações)
n_obs <- nrow(final_v_4)

# Número de cooperativas únicas
n_coops <- final_v_4 %>% distinct(CNPJ) %>% nrow()

# Exibir
cat("Total de observações:", n_obs, "\n")
cat("Total de cooperativas únicas:", n_coops, "\n")


# ####stargazer OLS#####

library(plm)
library(stargazer)

# Transformar em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Garanta que variáveis estão como fatores ANTES
final_v_4$Year <- as.factor(final_v_4$Year)
final_v_4$RegiaoDeAtuacao <- as.factor(final_v_4$RegiaoDeAtuacao)

# Modelos com plm e pooling (sem fator() na fórmula)
model_roe_filesize <- plm(ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
                            DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                          data = pdata, model = "pooling")

model_roe_pagecount <- plm(ROE_NW ~ Page_Count + CCSize + CCLeverage +
                             DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                           data = pdata, model = "pooling")

model_roe_fog <- plm(ROE_NW ~ Fog_Index + CCSize + CCLeverage +
                       DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = pdata, model = "pooling")

model_roa_filesize <- plm(ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
                            DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                          data = pdata, model = "pooling")

model_roa_pagecount <- plm(ROA_NW ~ Page_Count + CCSize + CCLeverage +
                             DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                           data = pdata, model = "pooling")

model_roa_fog <- plm(ROA_NW ~ Fog_Index + CCSize + CCLeverage +
                       DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = pdata, model = "pooling")


# library(texreg)
#
# screenreg(list(model_roe_filesize, model_roe_pagecount, model_roe_fog,
#                model_roa_filesize, model_roa_pagecount, model_roa_fog),
#           custom.model.names = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
#                                  "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"))

get_se <- function(model) {
  sqrt(diag(vcov(model)))
}

se_roe_filesize <- get_se(model_roe_filesize)
se_roe_pagecount <- get_se(model_roe_pagecount)
se_roe_fog <- get_se(model_roe_fog)

se_roa_filesize <- get_se(model_roa_filesize)
se_roa_pagecount <- get_se(model_roa_pagecount)
se_roa_fog <- get_se(model_roa_fog)


library(texreg)

screenreg(
  list(model_roe_filesize, model_roe_pagecount, model_roe_fog,
       model_roa_filesize, model_roa_pagecount, model_roa_fog),
  custom.model.names = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
                         "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
  digits = 5
)




library(plm)
library(lmtest)
library(sandwich)
library(stargazer)


# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Modelos ROE
model_roe_filesize <- plm(ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
                            DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                          data = pdata, model = "pooling")

model_roe_pagecount <- plm(ROE_NW ~ Page_Count + CCSize + CCLeverage +
                             DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                           data = pdata, model = "pooling")

model_roe_fog <- plm(ROE_NW ~ Fog_Index + CCSize + CCLeverage +
                       DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                     data = pdata, model = "pooling")

# Modelos ROA
model_roa_filesize <- plm(ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
                            DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                          data = pdata, model = "pooling")

model_roa_pagecount <- plm(ROA_NW ~ Page_Count + CCSize + CCLeverage +
                             DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                           data = pdata, model = "pooling")

model_roa_fog <- plm(ROA_NW ~ Fog_Index + CCSize + CCLeverage +
                       DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                     data = pdata, model = "pooling")
# Extrair erros padrão robustos
get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))
}


# Pacote necessário para coeftest
library(lmtest)

# Criar lista de modelos robustos
robust_models <- list(
  coeftest(model_roe_filesize, vcovHC(model_roe_filesize, type = "HC1", cluster = "group")),
  coeftest(model_roe_pagecount, vcovHC(model_roe_pagecount, type = "HC1", cluster = "group")),
  coeftest(model_roe_fog, vcovHC(model_roe_fog, type = "HC1", cluster = "group")),
  coeftest(model_roa_filesize, vcovHC(model_roa_filesize, type = "HC1", cluster = "group")),
  coeftest(model_roa_pagecount, vcovHC(model_roa_pagecount, type = "HC1", cluster = "group")),
  coeftest(model_roa_fog, vcovHC(model_roa_fog, type = "HC1", cluster = "group"))
)

# Usar stargazer com coeftest
stargazer(robust_models,
          type = "text",
          title = "Modelos OLS com Erros Robustos (Cluster por Empresa)",
          column.labels = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
                            "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)


# Calcular erros padrão robustos (ordem correta)
se_roe_filesize <- get_robust_se(model_roe_filesize)
se_roe_pagecount <- get_robust_se(model_roe_pagecount)
se_roe_fog <- get_robust_se(model_roe_fog)

se_roa_filesize <- get_robust_se(model_roa_filesize)
se_roa_pagecount <- get_robust_se(model_roa_pagecount)
se_roa_fog <- get_robust_se(model_roa_fog)

stargazer(model_roe_filesize, model_roe_pagecount, model_roe_fog,
          model_roa_filesize, model_roa_pagecount, model_roa_fog,
          se = list(se_roe_filesize,
                    se_roe_pagecount,
                    se_roe_fog,
                    se_roa_filesize,
                    se_roa_pagecount,
                    se_roa_fog),
          column.labels = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
                            "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
          type = "text",
          title = "Modelos OLS com Erros Robustos (Cluster por Empresa)",
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)


length(coef(model_roe_filesize)) == length(se_roe_filesize)


length(coef(model_roe_filesize))
length(se_roe_filesize)

names(coef(model_roe_filesize))
names(se_roe_filesize)


#adicionado depois da defesa da tese
# =========================
###### TESTES DE ESCOLHA DE MODELO + DIAGNÓSTICOS######
# =========================

# =========================
# LIBS
# =========================
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(texreg)
library(car)       # VIF
library(tseries)   # Jarque–Bera

# =========================
# PREPARAÇÃO DOS DADOS
# =========================
# Garanta fatores no data.frame original
final_v_4$Year <- as.factor(final_v_4$Year)
final_v_4$RegiaoDeAtuacao <- as.factor(final_v_4$RegiaoDeAtuacao)

# Converte para painel (CNPJ, Year)
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# =========================
# FORMULAS (Pooled OLS com plm)
# =========================
# Usamos Year já como fator na base (sem factor(Year) nas fórmulas)
form_roe_filesize <- ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

form_roe_pagecount <- ROE_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

form_roe_fog <- ROE_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

form_roa_filesize <- ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

form_roa_pagecount <- ROA_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

form_roa_fog <- ROA_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + Year

# =========================
# ESTIMAÇÕES (Pooling)
# =========================
model_roe_filesize <- plm(form_roe_filesize, data = pdata, model = "pooling")
model_roe_pagecount <- plm(form_roe_pagecount, data = pdata, model = "pooling")
model_roe_fog      <- plm(form_roe_fog,      data = pdata, model = "pooling")

model_roa_filesize <- plm(form_roa_filesize, data = pdata, model = "pooling")
model_roa_pagecount<- plm(form_roa_pagecount,data = pdata, model = "pooling")
model_roa_fog      <- plm(form_roa_fog,      data = pdata, model = "pooling")

# (Opcional) visualização rápida
screenreg(
  list(model_roe_filesize, model_roe_pagecount, model_roe_fog,
       model_roa_filesize, model_roa_pagecount, model_roa_fog),
  custom.model.names = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
                         "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
  digits = 5
)

# =========================
# FUNÇÕES AUXILIARES
# =========================
# Ajusta pooled, FE e RE para uma fórmula dada
fit_all <- function(formula) {
  list(
    pooled = plm(formula, data = pdata, model = "pooling"),
    fe     = plm(formula, data = pdata, model = "within", effect = "individual"),
    re     = plm(formula, data = pdata, model = "random", random.method = "swar")
  )
}

# Roda os testes de escolha de modelo e diagnósticos
run_tests <- function(fits, nome_spec) {
  cat("\n==============================\n")
  cat("Especificação:", nome_spec, "\n")
  cat("==============================\n")
  
  # ----- Escolha de modelo -----
  cat("\n[Escolha de modelo]\n")
  # Chow: FE vs Pooled
  chow <- tryCatch(pFtest(fits$fe, fits$pooled), error = function(e) e)
  print(chow)
  
  # LM Breusch-Pagan: RE vs Pooled (efeito individual e temporal)
  cat("\nLM Breusch-Pagan (RE vs Pooled) - efeito individual:\n")
  print(tryCatch(plmtest(fits$pooled, effect = "individual", type = "bp"),
                 error = function(e) e))
  
  cat("\nLM Breusch-Pagan (RE vs Pooled) - efeito temporal:\n")
  print(tryCatch(plmtest(fits$pooled, effect = "time", type = "bp"),
                 error = function(e) e))
  
  # Hausman: FE vs RE
  cat("\nHausman (FE vs RE):\n")
  haus <- tryCatch(phtest(fits$fe, fits$re), error = function(e) e)
  print(haus)
  
  # ----- Diagnósticos de pressupostos -----
  cat("\n[Diagnósticos de pressupostos]\n")
  
  # lm equivalente ao pooled (usa Year como fator já na base)
  diag_formula <- formula(fits$pooled)
  base_diag <- as.data.frame(final_v_4)
  
  lm_eq <- tryCatch(lm(diag_formula, data = base_diag), error = function(e) NULL)
  
  cat("\nHeterocedasticidade (Breusch-Pagan) no lm equivalente ao pooled:\n")
  if (!is.null(lm_eq)) {
    print(tryCatch(bptest(lm_eq), error = function(e) e))
  } else {
    cat("Não foi possível montar lm auxiliar (pulando BP no lm).\n")
  }
  
  cat("\nAutocorrelação serial (Breusch-Godfrey p/ painel) no pooled:\n")
  print(tryCatch(pbgtest(fits$pooled), error = function(e) e))
  
  cat("\nAutocorrelação serial (Wooldridge) no FE:\n")
  print(tryCatch(pwartest(fits$fe), error = function(e) e))
  
  cat("\nDependência seccional (Pesaran CD) no FE:\n")
  print(tryCatch(pcdtest(fits$fe, test = "cd"), error = function(e) e))
  
  cat("\nMulticolinearidade (VIF) no lm equivalente ao pooled:\n")
  if (!is.null(lm_eq)) {
    print(tryCatch(car::vif(lm_eq), error = function(e) e))
  } else {
    cat("Não foi possível montar lm auxiliar (pulando VIF).\n")
  }
  
  cat("\nNormalidade dos resíduos (Jarque–Bera) no lm equivalente ao pooled:\n")
  if (!is.null(lm_eq)) {
    print(tryCatch(jarque.bera.test(residuals(lm_eq)), error = function(e) e))
  } else {
    cat("Não foi possível montar lm auxiliar (pulando Jarque–Bera).\n")
  }
  
  invisible(list(chow = chow, hausman = haus))
}

# =========================
# RODAR TESTES PARA TODAS AS ESPECIFICAÇÕES
# =========================
specs <- list(
  "ROE: FileSize" = form_roe_filesize,
  "ROE: PageCount" = form_roe_pagecount,
  "ROE: FogIndex"  = form_roe_fog,
  "ROA: FileSize"  = form_roa_filesize,
  "ROA: PageCount" = form_roa_pagecount,
  "ROA: FogIndex"  = form_roa_fog
)

diagnostics_out <- list()
for (nm in names(specs)) {
  fits <- fit_all(specs[[nm]])
  diagnostics_out[[nm]] <- run_tests(fits, nm)
}

# =========================
# TABELA COM EP ROBUSTOS CLUSTERIZADOS (CNPJ)
# (via coeftest -> coef & se alinhados)
# =========================
models <- list(
  model_roe_filesize, model_roe_pagecount, model_roe_fog,
  model_roa_filesize, model_roa_pagecount, model_roa_fog
)

# coeftest com vcovHC cluster="group" e alinhamento
robust_ct <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1", cluster = "group"))
  ct[match(names(coef(m)), rownames(ct)), , drop = FALSE]
}

cts <- lapply(models, robust_ct)

coef_list <- lapply(cts, function(x) as.numeric(x[, 1]))  # estimativas
se_list   <- lapply(cts, function(x) as.numeric(x[, 2]))  # EPs robustos

# checagem defensiva
stopifnot(all(mapply(function(m, v) length(coef(m)) == length(v), models, se_list)))
stopifnot(all(mapply(function(m, v) length(coef(m)) == length(v), models, coef_list)))

stargazer(models,
          coef = coef_list,
          se   = se_list,
          column.labels = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
                            "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
          type = "text",
          title = "Pooled OLS com EP robustos (cluster por CNPJ)",
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)

# =========================
# (Opcional) EPs Driscoll–Kraay (ultra conservador) - COMENTADO
# =========================
# dk_se <- function(m) sqrt(diag(vcovSCC(m, type = "HC0", maxlag = 2)))  # precisa plm>=2.6-0
# dk_ct <- function(m) {
#   V <- vcovSCC(m, type = "HC0", maxlag = 2)
#   betas <- coef(m)
#   ses <- sqrt(diag(V))[match(names(betas), names(sqrt(diag(V))))]
#   cbind(Estimate = betas, "Std. Error" = ses)
# }
# cts_dk <- lapply(models, dk_ct)
# coef_list_dk <- lapply(cts_dk, function(x) as.numeric(x[,1]))
# se_list_dk   <- lapply(cts_dk, function(x) as.numeric(x[,2]))
# stargazer(models, coef = coef_list_dk, se = se_list_dk, type = "text",
#           column.labels = c("ROE: FileSize", "ROE: PageCount", "ROE: FogIndex",
#                             "ROA: FileSize", "ROA: PageCount", "ROA: FogIndex"),
#           title = "Pooled OLS com EP Driscoll–Kraay", model.names = FALSE,
#           keep.stat = c("n","rsq","adj.rsq"), digits = 3)



##### apendice para avaliadores #####



# =========================
# APPENDIX: SPECIFICATION TESTS & FIXED-EFFECTS RESULTS
# =========================

## 0) Libraries
suppressPackageStartupMessages({
  library(plm)
  library(lmtest)
  library(sandwich)
  library(stargazer)
})

## 1) Data prep
final_v_4$Year <- as.factor(final_v_4$Year)
final_v_4$RegiaoDeAtuacao <- as.factor(final_v_4$RegiaoDeAtuacao)
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

## 2) Formulas
formulas <- list(
  "ROE: FileSize" = ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  "ROE: PageCount" = ROE_NW ~ Page_Count + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  "ROE: FogIndex"  = ROE_NW ~ Fog_Index + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  "ROA: FileSize"  = ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  "ROA: PageCount" = ROA_NW ~ Page_Count + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  "ROA: FogIndex"  = ROA_NW ~ Fog_Index + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + Year
)

## 3) Helpers
fit_all <- function(formula) {
  list(
    pooled = plm(formula, data = pdata, model = "pooling"),
    fe     = plm(formula, data = pdata, model = "within", effect = "individual"),
    re     = plm(formula, data = pdata, model = "random", random.method = "swar")
  )
}

tidy_pf   <- function(obj) data.frame(statistic = unname(obj$statistic[1]), df = paste(obj$parameter, collapse = ", "), p = unname(obj$p.value))
tidy_lm   <- function(obj) data.frame(statistic = unname(obj$statistic),      df = obj$parameter,                     p = unname(obj$p.value))
tidy_haus <- function(obj) data.frame(statistic = unname(obj$statistic),      df = obj$parameter,                     p = unname(obj$p.value))

robust_ct <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1", cluster = "group"))
  ct[match(names(coef(m)), rownames(ct)), , drop = FALSE]
}

## 4) Run tests
test_rows <- list()
fe_models <- list()
i <- 1
for (nm in names(formulas)) {
  fm <- formulas[[nm]]
  fits <- fit_all(fm)
  
  chow   <- pFtest(fits$fe, fits$pooled)
  lm_ind <- plmtest(fits$pooled, effect = "individual", type = "bp")
  lm_tim <- plmtest(fits$pooled, effect = "time", type = "bp")
  haus   <- phtest(fits$fe, fits$re)
  
  tr_chow  <- cbind(spec = nm, test = "Chow (FE vs Pooled)", tidy_pf(chow))
  tr_lm_i  <- cbind(spec = nm, test = "Breusch-Pagan LM (RE vs Pooled) - individual", tidy_lm(lm_ind))
  tr_lm_t  <- cbind(spec = nm, test = "Breusch-Pagan LM (RE vs Pooled) - time", tidy_lm(lm_tim))
  tr_haus  <- cbind(spec = nm, test = "Hausman (FE vs RE)", tidy_haus(haus))
  
  test_rows[[i]] <- rbind(tr_chow, tr_lm_i, tr_lm_t, tr_haus); i <- i + 1
  fe_models[[nm]] <- fits$fe
}
tests_table <- do.call(rbind, test_rows)
tests_table$statistic <- round(as.numeric(tests_table$statistic), 3)
tests_table$p <- formatC(as.numeric(tests_table$p), format = "e", digits = 2)

## 5) FE results with clustered SE
fe_models_ordered <- fe_models[names(formulas)]
cts <- lapply(fe_models_ordered, robust_ct)
coef_list <- lapply(cts, function(x) as.numeric(x[,1]))
se_list   <- lapply(cts, function(x) as.numeric(x[,2]))

## 6) Save outputs in target folder
out_path <- "F:/BACKUP GERAL 01.06.2020/UFSC/2025/Artigo 1 - Tese/Journal of Economic and Administrative Sciences/Final Article/FINAL/2º Rodada"

# CSV with specification tests
write.csv(tests_table,
          file.path(out_path, "appendix_specification_tests.csv"),
          row.names = FALSE)

# LaTeX table with FE results
sink(file.path(out_path, "appendix_FE_table.tex"))
stargazer(fe_models_ordered,
          coef = coef_list,
          se   = se_list,
          type = "latex",
          title = "Fixed-Effects (Individual) with Clustered Robust SE (by CNPJ)",
          column.labels = names(formulas),
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3)
sink()

message("Files saved in: ", out_path)



#adicionado depois da defesa da tese - acaba aqui


#🔥🔥🔥🔥 Carregar pacotes necessários######

#ROE########
#🔥File_Size_KB###########      
#🔥 completo

library(AER)

#---------------------#
# 1. MODELO OLS
#---------------------#
ols_formula <- as.formula(
  ROE_NW ~ File_Size_KB + BoardSizeBOD + TenureBOD + CCSize + CCLeverage + DummyCentralEConfederacao
     + DummyLivreAdmissao + RegiaoDeAtuacao + Year
)

ols_model <- lm(ols_formula, data = final_v_4)
summary(ols_model)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ File_Size_KB #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
 # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")



#FE
# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com as mesmas variáveis de interesse
fixed_formula <- as.formula(
  ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar modelo com efeitos fixos por CNPJ
plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
summary(plm_fixed)


#🧪 Como testar se você pode usar efeitos fixos por RegiaoDeAtuacao?
#  Você precisa verificar se as cooperativas mudam de região ao longo do tempo (o que é raro), ou se há mais de uma 
#cooperativa por região por ano (o que já viabiliza efeito fixo por região/tempo):

# Número de regiões diferentes por CNPJ
library(dplyr)
final_v_4 %>%
  group_by(CNPJ) %>%
  summarise(regions = n_distinct(RegiaoDeAtuacao)) %>%
  summarise(prop_varia = mean(regions > 1))


library(plm)
library(lmtest)
library(sandwich)

pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com FE de firma (CNPJ) e ano via factor
fe_formula <- ROE ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + factor(RegiaoDeAtuacao) + factor(Year)

# Modelo FE com efeitos individuais (firma)
plm_fe <- plm(fe_formula, data = pdata, model = "within", effect = "individual")

# Erros padrão clusterizados por firma
coeftest(plm_fe, vcov = function(x) vcovHC(x, method = "arellano", cluster = "group"))


# Teste de efeito quadrático
pdata$File_Size_KB2 <- pdata$File_Size_KB^2

fe_formula_quad <- ROE ~ File_Size_KB + File_Size_KB2 + CCSize + CCLeverage +
  DummyLivreAdmissao + factor(RegiaoDeAtuacao) + factor(Year)

plm_quad <- plm(fe_formula_quad, data = pdata, model = "within", effect = "individual")
coeftest(plm_quad, vcov = function(x) vcovHC(x, cluster = "group"))


library(dplyr)

# Etapa 1 — Calcular desvio padrão intra-firma (por CNPJ)
intra_firm_sd <- final_v_4 %>%
  group_by(CNPJ) %>%
  dplyr::summarise(across(all_of(vars_to_check), ~ sd(.x, na.rm = TRUE)), .groups = "drop")

# Etapa 2 — Média dos desvios padrão entre firmas
summary_sd <- intra_firm_sd %>%
  dplyr::summarise(across(everything(), mean, na.rm = TRUE))

# Ver resultado
print(summary_sd)

# Número de regiões diferentes por CNPJ
library(dplyr)
final_v_4 %>%
  group_by(CNPJ) %>%
  summarise(regions = n_distinct(RegiaoDeAtuacao)) %>%
  summarise(prop_varia = mean(regions > 1))

#⭐Lag_FileSize#####
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_FileSize_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)#######
library(quantreg)
# Fórmula do modelo
quant_formula <- ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)

# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROE_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)


taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)
quantil_breaks <- c(0, taus, 1)  # 8 faixas (Q1 a Q8)

quantil_values <- quantile(final_v_4$ROE_NW, probs = quantil_breaks, na.rm = TRUE)

quantil_labels <- c("0–5%", "5–20%", "20–30%", "30–50%", 
                    "50–65%", "65–80%", "80–95%", "95–100%")

final_v_4$quantil_ROE <- cut(final_v_4$ROE_NW,
                             breaks = quantil_values,
                             include.lowest = TRUE,
                             right = TRUE,
                             labels = quantil_labels)

tabela_quantis <- table(final_v_4$quantil_ROE)
print(tabela_quantis)

library(quantreg)
library(car)
library(Matrix)  # Para a função bdiag()


# Passo 1: obter os coeficientes e variâncias normalmente
coef_25 <- rq_25_sum$coefficients[, 1]
coef_75 <- rq_75_sum$coefficients[, 1]

vcov_25 <- rq_25_sum$cov
vcov_75 <- rq_75_sum$cov

# Passo 2: descobrir a posição da variável desejada
var_index <- which(names(coef_25) == "File_Size_KB")

# Passo 3: calcular diferença e variância da diferença
diff_coef <- as.numeric(coef_25[var_index] - coef_75[var_index])
var_diff <- as.numeric(vcov_25[var_index, var_index] + vcov_75[var_index, var_index])

# Passo 4: calcular estatística de Wald
wald_stat <- (diff_coef^2) / var_diff
p_val <- 1 - pchisq(wald_stat, df = 1)

# Exibir resultados
cat("Diferença estimada:", round(diff_coef, 6), "\n")
cat("Estatística de Wald:", round(wald_stat, 4), "\n")
cat("p-valor:", round(p_val, 4), "\n")


# Estimar quantis separadamente com bootstrap
rq_10 <- rq(quant_formula, data = final_v_4, tau = 0.10)
rq_90 <- rq(quant_formula, data = final_v_4, tau = 0.90)

rq_10_sum <- summary(rq_10, se = "boot", R = 1000, cov = TRUE)
rq_90_sum <- summary(rq_90, se = "boot", R = 1000, cov = TRUE)

# Coeficientes e variâncias
coef_10 <- rq_10_sum$coefficients[, 1]
coef_90 <- rq_90_sum$coefficients[, 1]

vcov_10 <- rq_10_sum$cov
vcov_90 <- rq_90_sum$cov

# Índice da variável
var_index <- which(names(coef_10) == "File_Size_KB")

# Diferença, variância e teste
diff_coef_1090 <- as.numeric(coef_10[var_index] - coef_90[var_index])
var_diff_1090 <- as.numeric(vcov_10[var_index, var_index] + vcov_90[var_index, var_index])
wald_stat_1090 <- (diff_coef_1090^2) / var_diff_1090
p_val_1090 <- 1 - pchisq(wald_stat_1090, df = 1)

# Resultado
cat("🔍 Comparação Q10 vs Q90:\n")
cat("Diferença estimada:", round(diff_coef_1090, 6), "\n")
cat("Estatística de Wald:", round(wald_stat_1090, 4), "\n")
cat("p-valor:", round(p_val_1090, 4), "\n")


# Imprimir com segurança
print(diff_coef)
print(wald_stat)
print(p_val)

print(var_diff)  # Se for zero ou muito próximo, o teste fica inválido


# Certifique-se de ter esses pacotes

library(ggplot2)
library(dplyr)

# Extrair coeficientes e ICs de todos os quantis
taus <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.50, 0.65, 0.80, 0.95)

rq_models <- lapply(taus, function(tau) {
  rq(quant_formula, data = final_v_4, tau = tau)
})

rq_summaries <- lapply(rq_models, function(model) summary(model, se = "boot", R = 500))

# Extrair coef, erro e IC da variável File_Size_KB
coef_plot_data <- data.frame(
  tau = taus,
  coef = sapply(rq_summaries, function(s) s$coefficients["File_Size_KB", 1]),
  se = sapply(rq_summaries, function(s) s$coefficients["File_Size_KB", 2])
)

coef_plot_data <- coef_plot_data %>%
  mutate(
    lower = coef - 1.96 * se,
    upper = coef + 1.96 * se
  )

# Plot
ggplot(coef_plot_data, aes(x = tau, y = coef)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  theme_minimal() +
  labs(
    title = "Efeito do File_Size_KB ao longo dos quantis do ROE",
    x = "Quantil (tau)",
    y = "Coeficiente estimado (File_Size_KB)"
  )


###PSM


# 10% mais difíceis de ler
final_v_4$LowReadability <- ifelse(final_v_4$Fog_Index >= quantile(final_v_4$Fog_Index, 0.90, na.rm = TRUE), 1, 0)

# 10% maiores arquivos
final_v_4$LargeFile <- ifelse(final_v_4$File_Size_KB >= quantile(final_v_4$File_Size_KB, 0.90, na.rm = TRUE), 1, 0)

# 10% com mais páginas
final_v_4$LongReport <- ifelse(final_v_4$Page_Count >= quantile(final_v_4$Page_Count, 0.90, na.rm = TRUE), 1, 0)


# PSM
psm_fog <- matchit(LowReadability ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                   data = final_v_4,
                   method = "nearest",
                   distance = "logit")
matched_fog <- match.data(psm_fog)

# Regressão com ROE
summary(lm(ROE_NW ~ LowReadability, data = matched_fog))

# Regressão com ROA
summary(lm(ROA_NW ~ LowReadability, data = matched_fog))


psm_file <- matchit(LargeFile ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                    data = final_v_4,
                    method = "nearest",
                    distance = "logit")
matched_file <- match.data(psm_file)

# Regressões
summary(lm(ROE_NW ~ LargeFile, data = matched_file))
summary(lm(ROA_NW ~ LargeFile, data = matched_file))


psm_page <- matchit(LongReport ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                    data = final_v_4,
                    method = "nearest",
                    distance = "logit")
matched_page <- match.data(psm_page)

# Regressões
summary(lm(ROE_NW ~ LongReport, data = matched_page))
summary(lm(ROA_NW ~ LongReport, data = matched_page))



# 10% mais fáceis de ler
final_v_4$HighReadability <- ifelse(final_v_4$Fog_Index >= quantile(final_v_4$Fog_Index, 0.10, na.rm = TRUE), 1, 0)

# 10% menores arquivos
final_v_4$SmallFile <- ifelse(final_v_4$File_Size_KB >= quantile(final_v_4$File_Size_KB, 0.10, na.rm = TRUE), 1, 0)

# 10% com menos páginas
final_v_4$ShortReport <- ifelse(final_v_4$Page_Count >= quantile(final_v_4$Page_Count, 0.10, na.rm = TRUE), 1, 0)


# PSM
psm_fog <- matchit(HighReadability ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                   data = final_v_4,
                   method = "nearest",
                   distance = "logit")
matched_fog <- match.data(psm_fog)

# Regressão com ROE
summary(lm(ROE_NW ~ HighReadability, data = matched_fog))

# Regressão com ROA
summary(lm(ROA_NW ~ HighReadability, data = matched_fog))



psm_file <- matchit(SmallFile ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                    data = final_v_4,
                    method = "nearest",
                    distance = "logit")
matched_file <- match.data(psm_file)

# Regressões
summary(lm(ROE_NW ~ SmallFile, data = matched_file))
summary(lm(ROA_NW ~ SmallFile, data = matched_file))


psm_page <- matchit(ShortReport ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                    data = final_v_4,
                    method = "nearest",
                    distance = "logit")
matched_page <- match.data(psm_page)

# Regressões
summary(lm(ROE_NW ~ ShortReport, data = matched_page))
summary(lm(ROA_NW ~ ShortReport, data = matched_page))


#---------------------#
# 2. TESTE DE INSTRUMENTOS (TOP COMBINAÇÕES)
#---------------------#

test_iv_combinations <- function(data, y_var, endog_var, controls, iv_candidates, size = 2) {
  combos <- combn(iv_candidates, size, simplify = FALSE)
  results <- list()
  
  for (i in seq_along(combos)) {
    ivs <- combos[[i]]
    
    formula_iv <- as.formula(paste(
      y_var, "~", paste(c(endog_var, controls), collapse = " + "), "|",
      paste(c(ivs, controls), collapse = " + ")
    ))
    
    model <- try(ivreg(formula_iv, data = data), silent = TRUE)
    if (inherits(model, "try-error")) next
    
    diag <- summary(model, diagnostics = TRUE)$diagnostics
    
    results[[i]] <- data.frame(
      instruments = paste(ivs, collapse = " + "),
      weak_iv_f = diag["Weak instruments", "statistic"],
      weak_iv_p = diag["Weak instruments", "p-value"],
      hausman_p = diag["Wu-Hausman", "p-value"],
      sargan_p = diag["Sargan", "p-value"]
    )
  }
  
  do.call(rbind, results)
}

# Definir variáveis para teste automático
y_var <- "ROE_NW"
endog_var <- "File_Size_KB"
controls <- c("CCSize", "CCLeverage", #"DummyCentralEConfederacao", 
              #"BoardSizeBOD",
              #"TenureBOD", 
              "DummyLivreAdmissao", "RegiaoDeAtuacao", "Year")
iv_candidates <- c(
  "inst_FileSize", "inst_log_FileSize", "inst_PageCount", "inst_log_PageCount",
  "inst_WordCount", "inst_log_WordCount", "inst_LIX", "inst_RIX",
  "inst_ARI", "inst_FOG", "inst_SMOG"
)


# Executar o teste para combinações de 2 IVs
iv_results <- test_iv_combinations(final_v_4, y_var, endog_var, controls, iv_candidates, size = 2)

# Ver os melhores (ordenado por F)
print(iv_results[order(-iv_results$weak_iv_f), ])


# Para testar combinações de 3 instrumentos
iv_results <- test_iv_combinations(final_v_4, y_var, endog_var, controls, iv_candidates, size = 3)

# Ver os top resultados ordenados por F
iv_results[order(-iv_results$weak_iv_f), ]


#---------------------#
### 3. REGRESSÃO 2SLS (COM OS MELHORES INSTRUMENTOS)####
#---------------------#
library(AER)  # para ivreg

# Substitua os instrumentos abaixo pela melhor combinação da tabela anterior
iv_formula <- as.formula(
  ROE_NW ~ File_Size_KB #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage #+ DummyCentralEConfederacao
    + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_FileSize + inst_PageCount + CCSize + CCLeverage #+ DummyCentralEConfederacao +
  #  BoardSizeBOD + TenureBOD 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year
)

iv_model <- ivreg(iv_formula, data = final_v_4)
summary(iv_model, diagnostics = TRUE)


##🔥log_FileSize########

# OLS
ols_log_FileSize <- lm(ROE_NW ~ log_FileSize + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                         DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                       data = final_v_4)
summary(ols_log_FileSize)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ log_FileSize #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
 # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#⭐Lag_logFileSize#####

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_logFileSize_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


# Teste de IVs
iv_results_log_FileSize <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "log_FileSize", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "log_FileSize", controls, iv_candidates, size = 3)
)
head(iv_results_log_FileSize[order(-iv_results_log_FileSize$weak_iv_f), ])

# 2SLS - SUBSTITUA AQUI OS MELHORES IVs
iv_log_FileSize <- ivreg(
  ROE_NW ~ log_FileSize + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_log_FileSize + + inst_WordCount + inst_ARI + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_FileSize, diagnostics = TRUE)


#word_count

# Carregar pacotes (caso ainda não tenha)
library(AER)

# CONTROLES E IVs DEFINIDOS UMA VEZ
controls <- c("BoardSizeBOD", "TenureBOD", "CCSize", "CCLeverage",
              "DummyCentralEConfederacao", "DummyLivreAdmissao",
              "RegiaoDeAtuacao", "Year")

iv_candidates <- c(
  "inst_FileSize", "inst_log_FileSize", "inst_PageCount", "inst_log_PageCount",
  "inst_WordCount", "inst_log_WordCount", "inst_LIX", "inst_RIX",
  "inst_ARI", "inst_FOG", "inst_SMOG"
)

# Função para testar combinações de IVs (caso ainda não tenha rodado)
test_iv_combinations <- function(data, y_var, endog_var, controls, iv_candidates, size = 2) {
  combos <- combn(iv_candidates, size, simplify = FALSE)
  results <- list()

  for (i in seq_along(combos)) {
    ivs <- combos[[i]]
    
    formula_iv <- as.formula(paste(
      y_var, "~", paste(c(endog_var, controls), collapse = " + "), "|",
      paste(c(ivs, controls), collapse = " + ")
    ))

    model <- try(ivreg(formula_iv, data = data), silent = TRUE)
    if (inherits(model, "try-error")) next

    diag <- summary(model, diagnostics = TRUE)$diagnostics

    results[[i]] <- data.frame(
      target_var = endog_var,
      instruments = paste(ivs, collapse = " + "),
      weak_iv_f = diag["Weak instruments", "statistic"],
      weak_iv_p = diag["Weak instruments", "p-value"],
      hausman_p = diag["Wu-Hausman", "p-value"],
      sargan_p = diag["Sargan", "p-value"]
    )
  }

  do.call(rbind, results)
}

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Word_Count <- lm(ROE_NW ~ Word_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_Word_Count)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ Word_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_WordCount_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Word_Count <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "Word_Count", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "Word_Count", controls, iv_candidates, size = 3)
)

# Visualizar top IVs
iv_results_Word_Count[order(-iv_results_Word_Count$weak_iv_f), ]
# (Use os resultados acima para escolher os IVs mais fortes, válidos e necessários)

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# SUBSTITUA ABAIXO PELOS MELHORES IVs COM BASE NO RANKING ACIMA
iv_Word_Count <- ivreg(
  ROE_NW ~ Word_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_log_WordCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Word_Count, diagnostics = TRUE)


#log_WordCount

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_log_WordCount <- lm(ROE_NW ~ log_WordCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                          DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                        data = final_v_4)
summary(ols_log_WordCount)



# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ log_WordCount #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_logWordCount_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_log_WordCount <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "log_WordCount", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "log_WordCount", controls, iv_candidates, size = 3)
)

# Visualizar os melhores IVs ordenados por F
iv_results_log_WordCount[order(-iv_results_log_WordCount$weak_iv_f), ]
# (Escolha os melhores com F alto, p(Hausman) < 0.05 e p(Sargan) > 0.05)

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs com base nos resultados acima:
iv_log_WordCount <- ivreg(
  ROE_NW ~ log_WordCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_log_WordCount + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_WordCount, diagnostics = TRUE)

#🔥 Page_Count########

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Page_Count <- lm(ROE_NW ~ Page_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = final_v_4)
summary(ols_Page_Count)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ Page_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
   #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao 
  + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)



#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")


#FE
# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com as mesmas variáveis de interesse
fixed_formula <- as.formula(
  ROE_NW ~ Page_Count + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar modelo com efeitos fixos por CNPJ
plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
summary(plm_fixed)


# Teste de efeito quadrático
pdata$Page_Count2 <- pdata$Page_Count^2

fe_formula_quad <- ROE ~ Page_Count + Page_Count2 + CCSize + CCLeverage +
  DummyLivreAdmissao + factor(RegiaoDeAtuacao) + factor(Year)

plm_quad <- plm(fe_formula_quad, data = pdata, model = "within", effect = "individual")
coeftest(plm_quad, vcov = function(x) vcovHC(x, cluster = "group"))


#⭐Lag_PageCount###############

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_PageCount_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)########
library(quantreg)
# Fórmula do modelo
quant_formula <- ROE_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)

# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROE_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Page_Count <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "Page_Count", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "Page_Count", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs
iv_results_Page_Count[order(-iv_results_Page_Count$weak_iv_f), ]
# ⚠️ Use os que tiverem: F alto, p(Hausman) < 0.05, p(Sargan) > 0.05

#---------------------------#
# 3. MODELO 2SLS#######
#---------------------------#
# Substitua os IVs abaixo conforme os melhores resultados acima
iv_Page_Count <- ivreg(
  ROE_NW ~ Page_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
    # + DummyCentralEConfederacao 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_PageCount + inst_log_PageCount + #BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage #+ DummyCentralEConfederacao 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Page_Count, diagnostics = TRUE)


#log_PageCount

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_log_PageCount <- lm(ROE_NW ~ log_PageCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                          DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                        data = final_v_4)
summary(ols_log_PageCount)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ log_PageCount #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_logPageCount_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_log_PageCount <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "log_PageCount", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "log_PageCount", controls, iv_candidates, size = 3)
)

# Visualizar melhores combinações ordenadas por força dos instrumentos (F)
iv_results_log_PageCount[order(-iv_results_log_PageCount$weak_iv_f), ]
# ⚠️ Prefira combinações com F > 10, Hausman p < 0.05, Sargan p > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs abaixo pelos mais adequados conforme o ranking anterior
iv_log_PageCount <- ivreg(
  ROE_NW ~ log_PageCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_FOG + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_PageCount, diagnostics = TRUE)

#LIX_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
# ols_LIX_Index <- lm(ROE_NW ~ LIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
#                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
#                     data = final_v_4)
# summary(ols_LIX_Index)
# 

# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ LIX_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
 #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
 + RegiaoDeAtuacao 
 + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_LIX_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_LIX_Index <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "LIX_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "LIX_Index", controls, iv_candidates, size = 3)
)

# Visualizar os IVs mais fortes
iv_results_LIX_Index[order(-iv_results_LIX_Index$weak_iv_f), ]
# ⚠️ Selecione IVs com F > 10, p(Hausman) < 0.05 e p(Sargan) > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os melhores encontrados acima
iv_LIX_Index <- ivreg(
  ROE_NW ~ LIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_LIX + inst_log_FileSize + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_LIX_Index, diagnostics = TRUE)


#RIX_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_RIX_Index <- lm(ROE_NW ~ RIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_RIX_Index)



# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ RIX_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
 # + DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_RIX_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_RIX_Index <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "RIX_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "RIX_Index", controls, iv_candidates, size = 3)
)

# Visualizar melhores combinações ordenadas por F
iv_results_RIX_Index[order(-iv_results_RIX_Index$weak_iv_f), ]
# ⚠️ F > 10, Hausman p < 0.05 e Sargan p > 0.05 são desejáveis

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os melhores resultados acima
iv_RIX_Index <- ivreg(
  ROE_NW ~ RIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_RIX + inst_log_PageCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_RIX_Index, diagnostics = TRUE)


##ARI_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_ARI_Index <- lm(ROE_NW ~ ARI_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_ARI_Index)


#Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ ARI_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_ARI_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_ARI_Index <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "ARI_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "ARI_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs
iv_results_ARI_Index[order(-iv_results_ARI_Index$weak_iv_f), ]
# ⚠️ Dê preferência a F > 10, Hausman p < 0.05, Sargan p > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme o melhor ranking acima
iv_ARI_Index <- ivreg(
  ROE_NW ~ ARI_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_ARI + inst_LIX + inst_log_WordCount + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_ARI_Index, diagnostics = TRUE)


#🔥Fog_Index#######


#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Fog_Index <- lm(ROE_NW ~ Fog_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_Fog_Index)


#Carregar pacote
library(plm)
# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ Fog_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")


# 🔁 Pacotes necessários
library(plm)
library(clubSandwich)
library(lmtest)

# 🔢 Fórmula da regressão
ols_formula <- ROE_NW ~ Fog_Index + CCSize + CCLeverage + 
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# 🔧 Etapa correta de limpeza dos dados preservando as variáveis de painel
vars_needed <- all.vars(ols_formula)
vars_needed <- unique(c(vars_needed, "CNPJ", "Year"))  # Adiciona CNPJ e Year
final_clean <- na.omit(final_v_4[, vars_needed])


# 📊 Transformar dados em painel
pdata <- pdata.frame(final_clean, index = c("CNPJ", "Year"))

# 📈 Rodar modelo OLS com pooling (sem efeitos fixos)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")

# 🔗 Criar variável de cluster duplo (CNPJ x Year)
cluster_var <- interaction(pdata$CNPJ, pdata$Year)

# 📏 Estimar matriz de variância com erros robustos de cluster duplo
vcov_dc <- vcovCR(plm_ols, cluster = cluster_var, type = "CR2")

# 🧪 Testes t com erros robustos
robust_results <- coeftest(plm_ols, vcov = vcov_dc)

# 📋 Resultado
print(robust_results)

  #FE
  # Carregar pacote
  library(plm)
  
  # Transformar os dados em painel
  pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))
  
  # Fórmula com as mesmas variáveis de interesse
  fixed_formula <- as.formula(
    ROE_NW ~ Fog_Index + CCSize + CCLeverage +
      DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
  )
  
  # Rodar modelo com efeitos fixos por CNPJ
  plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
  summary(plm_fixed)

  
  # Teste de efeito quadrático
  pdata$Fog_Index2 <- pdata$Fog_Index^2
  
  fe_formula_quad <- ROE ~ Fog_Index + Fog_Index2 + CCSize + CCLeverage +
    DummyLivreAdmissao + factor(RegiaoDeAtuacao) + factor(Year)
  
  plm_quad <- plm(fe_formula_quad, data = pdata, model = "within", effect = "individual")
  coeftest(plm_quad, vcov = function(x) vcovHC(x, cluster = "group"))
  
  
  # 📦 Carregar pacotes necessários
  library(plm)
  library(lmtest)
  library(sandwich)
  
  # 🔄 Transformar os dados em painel
  pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))
  
  # 🧾 Fórmula do modelo com efeitos fixos
  fe_formula <- as.formula(
    ROE_NW ~ Fog_Index + CCSize + CCLeverage +
      DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
  )
  
  plm_timefe <- plm(fe_formula, data = pdata, model = "within", effect = "time")
  coeftest(plm_timefe, vcov = function(x) vcovHC(x, method = "arellano", cluster = "group"))
  
  
  # 📊 Resumo do modelo (coeficientes "limpos", sem intercepto geral)
  summary(plm_fe)
  
  
  plm_timefe <- plm(fe_formula, data = pdata, model = "within", effect = "time")
  coeftest(plm_timefe, vcov = function(x) vcovHC(x, method = "arellano", cluster = "group"))
  

#⭐Lagfog########

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_FOG_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")


#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)########
library(quantreg)
# Fórmula do modelo
quant_formula <- ROE_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)


# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROE_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)



#Propensity Score Matching (PSM)
# Grupo tratado = relatórios mais legíveis (25% menores valores de Fog Index)
final_v_4$HighReadability <- ifelse(final_v_4$Fog_Index <= quantile(final_v_4$Fog_Index, 0.25, na.rm = TRUE), 1, 0)


library(plm)
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

library(MatchIt)

psm_model <- matchit(HighReadability ~ CCSize + CCLeverage + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year),
                     data = final_v_4,
                     method = "nearest",
                     distance = "logit")


matched_data <- match.data(psm_model)


summary(lm(ROE_NW ~ HighReadability, data = matched_data))
summary(lm(ROA_NW ~ HighReadability, data = matched_data))



#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Fog_Index <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "Fog_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "Fog_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores instrumentos
iv_results_Fog_Index[order(-iv_results_Fog_Index$weak_iv_f), ]
# ⚠️ F > 10, p(Hausman) < 0.05 e p(Sargan) > 0.05 são critérios ideais

#---------------------------#
# 3. MODELO 2SLS#####
#---------------------------#
# Substitua os IVs conforme os melhores resultados acima
iv_Fog_Index <- ivreg(
  ROE_NW ~ Fog_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage +
    #DummyCentralEConfederacao + 
    DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_FOG + inst_PageCount #+ BoardSizeBOD + TenureBOD +
   + CCSize + CCLeverage #+ DummyCentralEConfederacao 
    + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Fog_Index, diagnostics = TRUE)


#SMOG_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_SMOG_Index <- lm(ROE_NW ~ SMOG_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = final_v_4)
summary(ols_SMOG_Index)


#Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ SMOG_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
 # + DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#lag

library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROE_NW ~ lag_SMOG_1 #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_SMOG_Index <- rbind(
  test_iv_combinations(final_v_4, "ROE_NW", "SMOG_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROE_NW", "SMOG_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs ordenados por força
iv_results_SMOG_Index[order(-iv_results_SMOG_Index$weak_iv_f), ]
# ⚠️ F > 10, Hausman p < 0.05, Sargan p > 0.05 → combinação ideal

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os mais fortes e válidos identificados
iv_SMOG_Index <- ivreg(
  ROE_NW ~ SMOG_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_SMOG + inst_log_PageCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_SMOG_Index, diagnostics = TRUE)


#ROA########
#🔥File_Size_KB###########      


library(AER)

#---------------------#
# 1. MODELO OLS
#---------------------#
ols_formula <- as.formula(
  ROA_NW ~ File_Size_KB + BoardSizeBOD + TenureBOD + CCSize + CCLeverage + DummyCentralEConfederacao
  + DummyLivreAdmissao + RegiaoDeAtuacao #+ Year
)

ols_model <- lm(ols_formula, data = final_v_4)
summary(ols_model)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ File_Size_KB #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)



#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")

#FE
# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com as mesmas variáveis de interesse
fixed_formula <- as.formula(
  ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar modelo com efeitos fixos por CNPJ
plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
summary(plm_fixed)


#---------------------#
# 2. TESTE DE INSTRUMENTOS (TOP COMBINAÇÕES)
#---------------------#

test_iv_combinations <- function(data, y_var, endog_var, controls, iv_candidates, size = 2) {
  combos <- combn(iv_candidates, size, simplify = FALSE)
  results <- list()
  
  for (i in seq_along(combos)) {
    ivs <- combos[[i]]
    
    formula_iv <- as.formula(paste(
      y_var, "~", paste(c(endog_var, controls), collapse = " + "), "|",
      paste(c(ivs, controls), collapse = " + ")
    ))
    
    model <- try(ivreg(formula_iv, data = data), silent = TRUE)
    if (inherits(model, "try-error")) next
    
    diag <- summary(model, diagnostics = TRUE)$diagnostics
    
    results[[i]] <- data.frame(
      instruments = paste(ivs, collapse = " + "),
      weak_iv_f = diag["Weak instruments", "statistic"],
      weak_iv_p = diag["Weak instruments", "p-value"],
      hausman_p = diag["Wu-Hausman", "p-value"],
      sargan_p = diag["Sargan", "p-value"]
    )
  }
  
  do.call(rbind, results)
}

# Definir variáveis para teste automático
y_var <- "ROA_NW"
endog_var <- "File_Size_KB"
controls <- c("CCSize", "CCLeverage", #"DummyCentralEConfederacao", "BoardSizeBOD",
             # "TenureBOD", 
              "DummyLivreAdmissao", "RegiaoDeAtuacao", "Year")
iv_candidates <- c(
  "inst_FileSize", "inst_log_FileSize", "inst_PageCount", "inst_log_PageCount",
  "inst_WordCount", "inst_log_WordCount", "inst_LIX", "inst_RIX",
  "inst_ARI", "inst_FOG", "inst_SMOG"
)


# Executar o teste para combinações de 2 IVs
iv_results <- test_iv_combinations(final_v_4, y_var, endog_var, controls, iv_candidates, size = 2)

# Ver os melhores (ordenado por F)
print(iv_results[order(-iv_results$weak_iv_f), ])


# Para testar combinações de 3 instrumentos
iv_results <- test_iv_combinations(final_v_4, y_var, endog_var, controls, iv_candidates, size = 3)

# Ver os top resultados ordenados por F
iv_results[order(-iv_results$weak_iv_f), ]

#---------------------#
# 3. REGRESSÃO 2SLS (COM OS MELHORES INSTRUMENTOS)#######
#---------------------#

# Substitua os instrumentos abaixo pela melhor combinação da tabela anterior
iv_formula <- as.formula(
  ROA_NW ~ File_Size_KB #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage #+ DummyCentralEConfederacao
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_FileSize + inst_PageCount + CCSize + CCLeverage #+ DummyCentralEConfederacao +
   # BoardSizeBOD + TenureBOD 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year
)

iv_model <- ivreg(iv_formula, data = final_v_4)
summary(iv_model, diagnostics = TRUE)


#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)#######
library(quantreg)
# Fórmula do modelo
quant_formula <- ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)

# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROA_NW ~ File_Size_KB + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)


taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)
quantil_breaks <- c(0, taus, 1)  # 8 faixas (Q1 a Q8)

quantil_values <- quantile(final_v_4$ROA_NW, probs = quantil_breaks, na.rm = TRUE)

quantil_labels <- c("0–5%", "5–20%", "20–30%", "30–50%", 
                    "50–65%", "65–80%", "80–95%", "95–100%")

final_v_4$quantil_ROE <- cut(final_v_4$ROA_NW,
                             breaks = quantil_values,
                             include.lowest = TRUE,
                             right = TRUE,
                             labels = quantil_labels)

tabela_quantis <- table(final_v_4$quantil_ROE)
print(tabela_quantis)

##log_FileSize########

# OLS
ols_log_FileSize <- lm(ROA_NW ~ log_FileSize + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                         DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                       data = final_v_4)
summary(ols_log_FileSize)

# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ log_FileSize #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

# Teste de IVs
iv_results_log_FileSize <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "log_FileSize", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "log_FileSize", controls, iv_candidates, size = 3)
)
head(iv_results_log_FileSize[order(-iv_results_log_FileSize$weak_iv_f), ])

# 2SLS - SUBSTITUA AQUI OS MELHORES IVs
iv_log_FileSize <- ivreg(
  ROA_NW ~ log_FileSize + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_log_FileSize + + inst_WordCount + inst_ARI + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_FileSize, diagnostics = TRUE)

#word_count

# Carregar pacotes (caso ainda não tenha)
library(AER)

# CONTROLES E IVs DEFINIDOS UMA VEZ
controls <- c("BoardSizeBOD", "TenureBOD", "CCSize", "CCLeverage",
              "DummyCentralEConfederacao", "DummyLivreAdmissao",
              "RegiaoDeAtuacao", "Year")

iv_candidates <- c(
  "inst_FileSize", "inst_log_FileSize", "inst_PageCount", "inst_log_PageCount",
  "inst_WordCount", "inst_log_WordCount", "inst_LIX", "inst_RIX",
  "inst_ARI", "inst_FOG", "inst_SMOG"
)

# Função para testar combinações de IVs (caso ainda não tenha rodado)
test_iv_combinations <- function(data, y_var, endog_var, controls, iv_candidates, size = 2) {
  combos <- combn(iv_candidates, size, simplify = FALSE)
  results <- list()
  
  for (i in seq_along(combos)) {
    ivs <- combos[[i]]
    
    formula_iv <- as.formula(paste(
      y_var, "~", paste(c(endog_var, controls), collapse = " + "), "|",
      paste(c(ivs, controls), collapse = " + ")
    ))
    
    model <- try(ivreg(formula_iv, data = data), silent = TRUE)
    if (inherits(model, "try-error")) next
    
    diag <- summary(model, diagnostics = TRUE)$diagnostics
    
    results[[i]] <- data.frame(
      target_var = endog_var,
      instruments = paste(ivs, collapse = " + "),
      weak_iv_f = diag["Weak instruments", "statistic"],
      weak_iv_p = diag["Weak instruments", "p-value"],
      hausman_p = diag["Wu-Hausman", "p-value"],
      sargan_p = diag["Sargan", "p-value"]
    )
  }
  
  do.call(rbind, results)
}

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Word_Count <- lm(ROA_NW ~ Word_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = final_v_4)
summary(ols_Word_Count)

# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ Word_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Word_Count <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "Word_Count", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "Word_Count", controls, iv_candidates, size = 3)
)

# Visualizar top IVs
iv_results_Word_Count[order(-iv_results_Word_Count$weak_iv_f), ]
# (Use os resultados acima para escolher os IVs mais fortes, válidos e necessários)

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# SUBSTITUA ABAIXO PELOS MELHORES IVs COM BASE NO RANKING ACIMA
iv_Word_Count <- ivreg(
  ROA_NW ~ Word_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_log_WordCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Word_Count, diagnostics = TRUE)


#log_WordCount

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_log_WordCount <- lm(ROA_NW ~ log_WordCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                          DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                        data = final_v_4)
summary(ols_log_WordCount)

# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ log_WordCount #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  # + BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_log_WordCount <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "log_WordCount", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "log_WordCount", controls, iv_candidates, size = 3)
)

# Visualizar os melhores IVs ordenados por F
iv_results_log_WordCount[order(-iv_results_log_WordCount$weak_iv_f), ]
# (Escolha os melhores com F alto, p(Hausman) < 0.05 e p(Sargan) > 0.05)

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs com base nos resultados acima:
iv_log_WordCount <- ivreg(
  ROA_NW ~ log_WordCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_log_WordCount + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_WordCount, diagnostics = TRUE)

#🔥 Page_Count########

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Page_Count <- lm(ROA_NW ~ Page_Count + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = final_v_4)
summary(ols_Page_Count)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ Page_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")


#FE
# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com as mesmas variáveis de interesse
fixed_formula <- as.formula(
  ROA_NW ~ Page_Count + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar modelo com efeitos fixos por CNPJ
plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
summary(plm_fixed)

#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)#######
library(quantreg)
# Fórmula do modelo
quant_formula <- ROA_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)

# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROA_NW ~ Page_Count + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Page_Count <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "Page_Count", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "Page_Count", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs
iv_results_Page_Count[order(-iv_results_Page_Count$weak_iv_f), ]
# ⚠️ Use os que tiverem: F alto, p(Hausman) < 0.05, p(Sargan) > 0.05

#---------------------------#
# 3. MODELO 2SLS #######
#---------------------------#
# Substitua os IVs abaixo conforme os melhores resultados acima
iv_Page_Count <- ivreg(
  ROA_NW ~ Page_Count #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage +
   # DummyCentralEConfederacao + 
    DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_PageCount + inst_log_PageCount + #BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage +
   # DummyCentralEConfederacao 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Page_Count, diagnostics = TRUE)


#log_PageCount

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_log_PageCount <- lm(ROA_NW ~ log_PageCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                          DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                        data = final_v_4)
summary(ols_log_PageCount)


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ log_PageCount #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  + DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_log_PageCount <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "log_PageCount", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "log_PageCount", controls, iv_candidates, size = 3)
)

# Visualizar melhores combinações ordenadas por força dos instrumentos (F)
iv_results_log_PageCount[order(-iv_results_log_PageCount$weak_iv_f), ]
# ⚠️ Prefira combinações com F > 10, Hausman p < 0.05, Sargan p > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs abaixo pelos mais adequados conforme o ranking anterior
iv_log_PageCount <- ivreg(
  ROA_NW ~ log_PageCount + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_WordCount + inst_FOG + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_log_PageCount, diagnostics = TRUE)


#LIX_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
# ols_LIX_Index <- lm(ROA_NW ~ LIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
#                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
#                     data = final_v_4)
# summary(ols_LIX_Index)
# 


# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ LIX_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao 
  + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_LIX_Index <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "LIX_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "LIX_Index", controls, iv_candidates, size = 3)
)

# Visualizar os IVs mais fortes
iv_results_LIX_Index[order(-iv_results_LIX_Index$weak_iv_f), ]
# ⚠️ Selecione IVs com F > 10, p(Hausman) < 0.05 e p(Sargan) > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os melhores encontrados acima
iv_LIX_Index <- ivreg(
  ROA_NW ~ LIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_LIX + inst_log_FileSize + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_LIX_Index, diagnostics = TRUE)

#RIX_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_RIX_Index <- lm(ROA_NW ~ RIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_RIX_Index)

# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ RIX_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
 # + DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_RIX_Index <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "RIX_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "RIX_Index", controls, iv_candidates, size = 3)
)

# Visualizar melhores combinações ordenadas por F
iv_results_RIX_Index[order(-iv_results_RIX_Index$weak_iv_f), ]
# ⚠️ F > 10, Hausman p < 0.05 e Sargan p > 0.05 são desejáveis

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os melhores resultados acima
iv_RIX_Index <- ivreg(
  ROA_NW ~ RIX_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_RIX + inst_log_PageCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_RIX_Index, diagnostics = TRUE)

##ARI_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_ARI_Index <- lm(ROA_NW ~ ARI_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_ARI_Index)

#Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ ARI_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_ARI_Index <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "ARI_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "ARI_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs
iv_results_ARI_Index[order(-iv_results_ARI_Index$weak_iv_f), ]
# ⚠️ Dê preferência a F > 10, Hausman p < 0.05, Sargan p > 0.05

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme o melhor ranking acima
iv_ARI_Index <- ivreg(
  ROA_NW ~ ARI_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_ARI + inst_LIX + inst_log_WordCount + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_ARI_Index, diagnostics = TRUE)


#🔥Fog_Index #######


#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_Fog_Index <- lm(ROA_NW ~ Fog_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                      DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                    data = final_v_4)
summary(ols_Fog_Index)


#Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ Fog_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)


#1. Cluster robust standard errors
coeftest(plm_ols, vcov = vcovHC, type = "HC1", cluster = "group")

#FE
# Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula com as mesmas variáveis de interesse
fixed_formula <- as.formula(
  ROA_NW ~ Fog_Index + CCSize + CCLeverage +
    DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)
)

# Rodar modelo com efeitos fixos por CNPJ
plm_fixed <- plm(fixed_formula, data = pdata, model = "within", effect = "individual")
summary(plm_fixed)

#✅ Regressão quantílica para múltiplos quantis (ex: 25%, 50% e 75%)#######
library(quantreg)
# Fórmula do modelo
quant_formula <- ROA_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Estimar o modelo nos quantis 0.25, 0.5 (mediana) e 0.75
rq_25 <- rq(quant_formula, data = final_v_4, tau = 0.25)
rq_50 <- rq(quant_formula, data = final_v_4, tau = 0.50)
rq_75 <- rq(quant_formula, data = final_v_4, tau = 0.75)

# Ver resultados
summary(rq_25)
summary(rq_50)
summary(rq_75)

# Carregar pacote necessário
library(quantreg)

# Fórmula do modelo
quant_formula <- ROA_NW ~ Fog_Index + CCSize + CCLeverage +
  DummyLivreAdmissao + RegiaoDeAtuacao + factor(Year)

# Definir os quantis desejados
taus <- c(0.05, 0.20, 0.30, 0.50, 0.65, 0.80, 0.95)

# Estimar um modelo para cada quantil
rq_models <- lapply(taus, function(tau) rq(quant_formula, data = final_v_4, tau = tau))
names(rq_models) <- paste0("Q", taus * 100)

# Exibir sumários de todos os modelos
lapply(rq_models, summary)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_Fog_Index <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "Fog_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "Fog_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores instrumentos
iv_results_Fog_Index[order(-iv_results_Fog_Index$weak_iv_f), ]
# ⚠️ F > 10, p(Hausman) < 0.05 e p(Sargan) > 0.05 são critérios ideais

#---------------------------#
# 3. MODELO 2SLS ######
#---------------------------#
# Substitua os IVs conforme os melhores resultados acima
iv_Fog_Index <- ivreg(
  ROA_NW ~ Fog_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
 # + DummyCentralEConfederacao 
  + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_FOG + inst_PageCount + #BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage #+ DummyCentralEConfederacao 
 + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_Fog_Index, diagnostics = TRUE)


#SMOG_Index

#---------------------------#
# 1. MODELO OLS
#---------------------------#
ols_SMOG_Index <- lm(ROA_NW ~ SMOG_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
                       DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
                     data = final_v_4)
summary(ols_SMOG_Index)


#Carregar pacote
library(plm)

# Transformar os dados em painel
pdata <- pdata.frame(final_v_4, index = c("CNPJ", "Year"))

# Fórmula OLS
ols_formula <- as.formula(
  ROA_NW ~ SMOG_Index #+ BoardSizeBOD + TenureBOD 
  + CCSize + CCLeverage 
  #+ DummyCentralEConfederacao 
  #+ BigFour
  + DummyLivreAdmissao 
  + RegiaoDeAtuacao + factor(Year)
)

# Rodar OLS com plm (modelo pooling)
plm_ols <- plm(ols_formula, data = pdata, model = "pooling")
summary(plm_ols)

#---------------------------#
# 2. TESTE DOS MELHORES INSTRUMENTOS (2 e 3 variáveis)
#---------------------------#
iv_results_SMOG_Index <- rbind(
  test_iv_combinations(final_v_4, "ROA_NW", "SMOG_Index", controls, iv_candidates, size = 2),
  test_iv_combinations(final_v_4, "ROA_NW", "SMOG_Index", controls, iv_candidates, size = 3)
)

# Ver os melhores IVs ordenados por força
iv_results_SMOG_Index[order(-iv_results_SMOG_Index$weak_iv_f), ]
# ⚠️ F > 10, Hausman p < 0.05, Sargan p > 0.05 → combinação ideal

#---------------------------#
# 3. MODELO 2SLS
#---------------------------#
# Substitua os IVs conforme os mais fortes e válidos identificados
iv_SMOG_Index <- ivreg(
  ROA_NW ~ SMOG_Index + BoardSizeBOD + TenureBOD + CCSize + CCLeverage +
    DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year |
    inst_SMOG + inst_log_PageCount + inst_ARI + BoardSizeBOD + TenureBOD +
    CCSize + CCLeverage + DummyCentralEConfederacao + DummyLivreAdmissao + RegiaoDeAtuacao + Year,
  data = final_v_4
)
summary(iv_SMOG_Index, diagnostics = TRUE)