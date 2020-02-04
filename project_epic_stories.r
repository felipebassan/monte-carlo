#carregamento bibliotecas
require(data.table)
require(ggplot2)

stories = 173; #quantidade total de histórias
throughput = c(3,2,7,4,1)

trials = 100000; #quantidade da simulações
weeks = 80; #quantidade de semanas
count = 1; #contador para o loop

dt <- data.table(NULL); # inicializando o data table e vetores
week_x <- NULL;
next_week <- NULL;

#semana 1
next_week = sample(throughput,trials,replace=T);
dt <- cbind(dt, next_week); # adicionando a simulação como nova coluna na tabela
names(dt)[count] <- paste("week",count,sep="_"); # definição do nome da coluna

#simulação
while (count < weeks)
{
  count = count + 1;
  week_x = sample(throughput,trials,replace=T); #nova simulação
  next_week = next_week + week_x; #soma os valores
  dt <- cbind(dt, next_week); #adicionando a simulação como nova coluna na tabela
  names(dt)[count] <- paste("week",count,sep="_"); #definição do nome da coluna
}

dt_output <- data.table(NULL); #inicializando o data table do resultado
count = 0; #contador para o loop

#marcação dos resultados
while (count < weeks)
{
  count = count + 1;
  column_name = paste("week",count,sep="_"); #nome da coluna
  count_next_week = sum(dt[,column_name,with=F] > stories); #conto quantas vezes a quantidade de histórias entregues foi maior que a quantidade total de histórias do projeto
  dt_output <- cbind(dt_output, count_next_week); #adicionando o resultado como nova coluna na tabela
  names(dt_output)[count] <- paste("week",count,sep="_"); #definição do nome da coluna
}

dt_output <- dt_output/trials; #conversão do valor para porcentagem
dt_output <- as.numeric(dt_output); #conversão para exibição do gráfico

#exibição do resultado
Semanas <- 1:80
Porcentagem <- dt_output[1:80]
df <- data.frame(Semanas,Porcentagem)

ggplot(df, aes(x=Semanas, y=Porcentagem)) +
  geom_line( color="#416263") +
    geom_point(shape=21, color="#416263", fill="#F44615", size=4)


dt_output[51:57]