#carregamento bibliotecas
require(data.table)
require(ggplot2)

liters = 13; #quantidade de litros que gostaria de produzir
throughput = c(66,85,76,72,109); #suco produzido em ml por cada laranja da amostragem 
trials = 100000; #quantidade da simulações
squeezes = 200; #quantidade de espremidas
count = 1; #contador para o loop

dt <- data.table(NULL); # inicializando o data table e vetores
week_x <- NULL;
next_week <- NULL;

#semana 1
next_week = sample(throughput,trials,replace=T);
dt <- cbind(dt, next_week); # adicionando a simulação como nova coluna na tabela
names(dt)[count] <- paste("week",count,sep="_"); # definição do nome da coluna

#simulação
while (count < squeezes)
{
  count = count + 1;
  week_x = sample(throughput,trials,replace=T); #nova simulação
  next_week = next_week + week_x; #soma os valores
  dt <- cbind(dt, next_week); #adicionando a simulação como nova coluna na tabela
  names(dt)[count] <- paste("week",count,sep="_"); #definição do nome da coluna
}
dt <- dt/1000; #conversão para litros

dt_output <- data.table(NULL); #inicializando o data table do resultado
count = 0; #contador para o loop

#marcação dos resultados
while (count < squeezes)
{
  count = count + 1;
  column_name = paste("week",count,sep="_"); #nome da coluna
  count_next_week = sum(dt[,column_name,with=F] > liters); #conto quantas vezes a quantidade de suco produzida foi maior que a desejada
  dt_output <- cbind(dt_output, count_next_week); #adicionando o resultado como nova coluna na tabela
  names(dt_output)[count] <- paste("week",count,sep="_"); #definição do nome da coluna
}

dt_output <- dt_output/trials; #conversão do valor para porcentagem
dt_output <- as.numeric(dt_output); #conversão para exibição do gráfico

#exibição do resultado
Laranjas <- 1:200
Porcentagem <- dt_output[1:200]
df <- data.frame(Laranjas,Porcentagem)

ggplot(df, aes(x=Laranjas, y=Porcentagem)) +
  geom_line( color="#416263") +
    geom_point(shape=21, color="#416263", fill="#F44615", size=4)


