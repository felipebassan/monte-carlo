#carregamento bibliotecas
require(data.table)
require(ggplot2)

liters = 13; #quantidade de litros que gostaria de produzir
#juice = c(66,85,76,72,109); #suco produzido em ml por cada laranja da amostragem 
juice = c(66,85,76,72,109,63,77,79,84,91,102,90,121,84,70,91,50,103,88)

trials = 100000; #quantidade da simulações
squeezes = 170; #quantidade de espremidas
count = 1; #contador para o loop

dt <- data.table(NULL); # inicializando o data table e vetores
squeeze_x <- NULL;
next_squeeze <- NULL;

#semana 1
next_squeeze = sample(juice,trials,replace=T);
dt <- cbind(dt, next_squeeze); # adicionando a simulação como nova coluna na tabela
names(dt)[count] <- paste("squeeze",count,sep="_"); # definição do nome da coluna

#simulação
while (count < squeezes)
{
  count = count + 1;
  squeeze_x = sample(juice,trials,replace=T); #nova simulação
  next_squeeze = next_squeeze + squeeze_x; #soma os valores
  dt <- cbind(dt, next_squeeze); #adicionando a simulação como nova coluna na tabela
  names(dt)[count] <- paste("squeeze",count,sep="_"); #definição do nome da coluna
}
dt <- dt/1000; #conversão para litros

dt_output <- data.table(NULL); #inicializando o data table do resultado
count = 0; #contador para o loop

#marcação dos resultados
while (count < squeezes)
{
  count = count + 1;
  column_name = paste("squeeze",count,sep="_"); #nome da coluna
  count_next_squeeze = sum(dt[,column_name,with=F] > liters); #conto quantas vezes a quantidade de suco produzida foi maior que a desejada
  dt_output <- cbind(dt_output, count_next_squeeze); #adicionando o resultado como nova coluna na tabela
  names(dt_output)[count] <- paste("squeeze",count,sep="_"); #definição do nome da coluna
}

dt_output <- dt_output/trials; #conversão do valor para porcentagem
dt_output <- as.numeric(dt_output); #conversão para exibição do gráfico

#exibição do resultado
Laranjas <- 1:170
Porcentagem <- dt_output[1:170]
df <- data.frame(Laranjas,Porcentagem)

ggplot(df, aes(x=Laranjas, y=Porcentagem)) +
  geom_line( color="#416263") +
    geom_point(shape=21, color="#416263", fill="#F44615", size=4)

dt_output[155:165]