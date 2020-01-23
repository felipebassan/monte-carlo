#bibliotecas
require(data.table)

#marcação do tempo de início
begin_time <- date();

#Em uma amostragem de 19 laranjas espremidas, foram produzidas essa quantidade de suco em ml por laranja
juice = c(66,85,76,72,109,63,77,79,84,91,102,90,121,84,70,91,50,103,88)

oranges = 22000; #quantidade de laranjas
trials = 10000; #quantidade de simulações
count = 0; #contador para o loop

dt <- data.table(NULL); #inicializando o data table

#simulação para a quantidade total de laranjas
while (count < trials)
{
    count = count + 1;
    trial_x = sample(juice,oranges,replace=T); #nova simulação
    dt <- cbind(dt,trial_x); #adiciona a simulação como nova coluna na tabela
    names(dt)[count] <- paste("trial",count,sep="_") #nome da coluna
}

#soma da quantidade de suco em litros e converte para numérico
sum_juice <- dt[,lapply(.SD,sum)]/1000;
sum_juice <- as.numeric(sum_juice);

#marcação do tempo de finalização
end_time <- date();

#exibição dos resultados
summary(sum_juice);
hist(sum_juice,main="Simulação Monte Carlo",xlab="Litros", ylab="Frequência", border="#416263",col="#F44615");

#exibição do tempo de duração da simulação
begin_time;
end_time;