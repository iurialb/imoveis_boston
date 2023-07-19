########################
# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'viridis',
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'neuralnet',   # Pacote para fazer redes neurais
             'shapr',
             'gamlss',
             'gamlss.add',
             'mlbench',
             'reshape'
             
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

load('EPA_19.RData')
load('HAR_test.RData')
load('HAR_train.RData')
data(BostonHousing)

descritiva2 <- function(var, resp, df) {
  # Sumariza a variável resposta por categoria da variável em análise
  tgc <- Rmisc::summarySE(df, measurevar = resp, groupvars = c(var))
  maxN <- max(tgc$N)
  
  # Gráfico de barras
  p <- ggplot(tgc) +
    geom_bar(aes(x = tgc[,var], 
                 y = max(tgc[,resp])*N/maxN, 
                 fill = as.factor(tgc[,var])), 
             position = "identity", stat = "identity", 
             alpha = 0.5) +
    scale_fill_viridis_d(direction = -1, begin = .85, end = .95)
  
  # Gráfico de linhas
  p <- p +
    geom_line(aes(x = tgc[,var], y = tgc[,resp]), colour = '1', group = '1') +
    geom_point(aes(x = tgc[,var], y = tgc[,resp] ), colour = '1', group = '1') +
    geom_errorbar(aes(x = tgc[,var], 
                      y = tgc[,resp], 
                      ymin = tgc[,resp] + qnorm(.025)*se, 
                      ymax = tgc[,resp] + qnorm(.975)*se, colour = '1'), width = .5) +
    
    #geom_point(aes(x = tgc[,var], y = tgc[,resp] - tgc[,ep]*qnorm(.975)), colour = '1', group = '1') +
    scale_color_viridis_d(direction = -1, begin = 0, end = .25)
  
  # Ajuste dos eixos
  p <- p +
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey"),
          axis.text = element_text(size = 14),  # Tamanho da fonte dos números dos eixos
          axis.title = element_text(size = 16),  # Tamanho da fonte dos títulos dos eixos
          legend.position = "none") +
    xlab(var) + ylab("Barras")
  
  p <- p +
    scale_y_continuous(sec.axis = sec_axis(~ . *maxN/max(tgc[,resp]), 
                                           name = "Frequencia", 
                                           labels = scales::number)) +
    ylab(resp) +
    # Limite do eixo vertical esquerdo
    coord_cartesian(ylim = c(0, #min(tgc[,resp]) - 0.02, 
                             max(tgc[,resp] + qnorm(.975)*tgc$se) + 0.02))
  
  return(p)
}


###Carregar o pacote abaixo


install.packages("mlbench")


# Carregar a base Housing para uma primeira vizualização

head(BostonHousing)

# Dicionário de dados:
#######################################

# CRIM: Taxa de criminalidade per capita por região.
# ZN: Proporção de terrenos residenciais divididos em lotes com mais de 25.000 pés quadrados (cerca de 2.322 metros quadrados).
# INDUS: Proporção de acres não comerciais por cidade.
# CHAS: Variável fictícia (dummy) que indica se o imóvel faz fronteira com o rio Charles (1 se faz fronteira, 0 caso contrário).
# NOX: Concentração de óxidos nítricos (partes por 10 milhões).
# RM: Média de número de quartos por habitação.
# AGE: Proporção de unidades ocupadas pelos proprietários construídas antes de 1940.
# DIS: Distância ponderada até cinco centros de emprego em Boston.
# RAD: Índice de acessibilidade a rodovias radiais.
# TAX: Taxa de imposto sobre propriedades de valor total por $10.000.
# PTRATIO: Razão aluno-professor por cidade.
# B: 1000(Bk - 0.63)^2, onde Bk é a proporção de pessoas de origem afro-americana por cidade.
# LSTAT: Porcentagem de status inferior da população.
# MEDV: Valor mediano das residências ocupadas pelos proprietários em milhares de dólares.

# Dimenções da base

dim(BostonHousing)

# Para início da análise vamos dividí-la em 'Treino', 'Separação' e 'Teste'.
# Pelo código abaixo iremos fazer um 'sorteio' de acordo com a quantidade de linhas presentes na base (506). 
# Depois o código retorna os valores com 60, 20 e 20% respectivamente de probabilidade com cada uma das divisões mencionadas.


set.seed(123)
separacao <- sample(c('Treino', 'Validação', 'Teste'),
                    size = nrow(BostonHousing),
                    replace = TRUE,
                    prob=c(0.6, 0.2, 0.2))

# Verificando tais 'sorteios' de acordo com as porcentagens, temos:
table(separacao)


# Gerando objetos para treino, validação e teste
treino    <- BostonHousing[separacao == 'Treino',]
nrow(treino)
validacao <- BostonHousing[separacao == 'Validação',]
nrow(validacao)
teste     <- BostonHousing[separacao == 'Teste',]
nrow(teste)

# Realizando a primeira etapa da análise descritiva
tmp <- BostonHousing
tmp$crim %>% hist
tmp$crim_cat <- quantcut(tmp$crim, 5)
descritiva2("crim_cat", "medv", tmp)
#Nessa primeira análise podemos observar que quanto maior o índice de criminalidade, menor será o preço do imóvel.

tmp$zn %>% hist
tmp$zn_cat <- quantcut(tmp$zn, 5)
descritiva2("zn_cat", "medv", tmp)
#Aqui, podemos ver que quando o zn é igual a zero, o valor no imóvel é mais baixo.


tmp$indus %>% hist
tmp$indus_cat <- quantcut(tmp$indus, 5)
descritiva2("indus_cat", "medv", tmp)
#Já aqui podemos ver no gráfico que conforme o índice de industrialização cai, o preço do imóvel também cai. Porém, indica que volta a subir quando o índice volta a subir mais à frente.

tmp %>% colnames

# Montando uma árvore de decisões temos
set.seed(123)
arvore0 <- rpart::rpart(medv~., 
                        data=treino,
                        control=rpart.control(maxdepth = 2, cp=0))
paleta <- scales::viridis_pal(begin=.75, end=1)(20)

# Visualização da árvore
plot <- rpart.plot::rpart.plot(arvore0,
                               box.palette = paleta)

# Nesse sentido, podemos realizar algumas considerações ao analisar a árvore de decisão
# Podemos ver, por exemplo, que quando o rm (média de número de quartos por habitação) é maior do que 6,9, temos um aumento no preço. A mesma lógica se aplica para quando se tem 7,4 quartos.

# Ainda seguindo o fluxo da árvore, podemos perceber que quando há um Istat (Média de pessoas com baixa renda) maior ou igual a 14 temos uma queda no preço do imóvel.
# Logicamente, quem possui uma média de renda inferior não vai procurar uma região onde os imóveis são caros.


#Agora iremos aplicar a função para avaliar a árvore
avalia_regressao <- function(p_var, y_var){
  n <- length(y_var)
  SQE <- sum((y_var - p_var)^2)
  QME <- SQE/n
  
  # Cálculo do SSE (Sum of Squares Total)
  SST <- sum((y_var - mean(y_var, na.rm=TRUE))**2)
  QMT <- SST/n
  
  # Cálculo do R-quadrado
  R_squared <- 1 - SQE/SST
  
  # Imprimindo os resultados
  cat("SQE: ", SQE, "QME : ", QME, "\n")
  cat("SST: ", SST, "QMT: ", QMT, "\n")
  cat("R-quadrado: ", R_squared, "\n")
  
}
avalia_regressao(predict(arvore0, treino), treino$medv)
avalia_regressao(predict(arvore0, validacao), validacao$medv)

# Desse modo podemos ver que na base de treino temos um R-quadrado de 69%, indicando que o modelo consegue explicar uma boa variabilidade com os dados.
# Ao testar a base de validação observamos que temos uma pequena queda, onde o modelo explica 55% da variabilidade com os dados.

# A fim de obtermos uma maior complexidade e detalhamento da árvore podemos pedir ao modelo que faça mais uma "dobra" (3), nos retornando mais folhas, conforme abaixo:

arvore1 <- rpart::rpart(medv~., 
                        data=treino,
                        control=rpart.control(maxdepth = 3, cp=0))

# Visualizar a árvore
plot <- rpart.plot::rpart.plot(arvore1,
                               box.palette = paleta) # Paleta de cores

avalia_regressao(predict(arvore1, treino), treino$medv)
avalia_regressao(predict(arvore1, validacao), validacao$medv)

# Podemos observar agora que temos quando tratamos da base de treino e validação, temos um aumento do R-quadrado para 78% e 60%, respectivamente. Indicando uma melhor explicação para o modelo.

