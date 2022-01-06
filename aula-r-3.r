# 
# O conjunto de dados a ser utilizado aqui está apresentado na Tabela 2.1
# do livro do Prof Gilberto.
# Os dados se referem a resultados de um experimento conduzido para 
# avaliar o desempenho de cinco tipos de turbinas de alta velocidade 
# para motores de avião. Foram considerados dez motores de cada tipo 
# nas análises e foi observado para cada um o tempo (em unidades
# de milhões de ciclos) até a perda da velocidade.

turbina = scan("turbina.dat", list(tipo=0, tempo=0))
turbina

turbina$tipo = factor(turbina$tipo)

boxplot(tempo~tipo, data = turbina) # perceba similiarides entre grupos?

fit1 = glm(tempo~tipo, family=Gamma(link=log), turbina)
summary(fit1)

turbina$tipo[which(turbina$tipo==3)]<-4 # G4 sendo juntado ao G3
fit1 = glm(tempo~tipo, family=Gamma(link=log), turbina)
summary(fit1)

turbina$tipo[which(turbina$tipo==4)]<-1 # G3 e G4 sendo juntados ao G1
fit1 = glm(tempo~tipo, family=Gamma(link=log), turbina)
summary(fit1)

# [G1] resíduos deviance vs indices
rd<- residuals(fit1,type="deviance")
fi<- summary(fit1)$dispersion # 
h <- hatvalues(fit1)
r <- rd*sqrt(fi/(1-h))
plot(r,pch="+") # plota os graficos com simbolo + para os pontos
abline(h=c(-2,0,2),lty=3) # inlcui linhas horizontais para melhorar visualizado 

# Cuidado, esse padrao pode nao significar nada. Os dados estao
# por algum motivo, ordenados. Melhor nao apresentar. 

# [G2] envelope simulado
library(hnp) # pacote util para envelope simulado
# note que eh preciso definir o residuo para o envelope. As opcoes sao:
# "deviance", "pearson", "response", "working", "simple", "student", or "standard"
hnp(fit1, resid.type="deviance") 
hnp(fit1, resid.type="deviance", halfnormal = F) # melhor. Nao eh half (metade)

# [G3] distacia de Cook
d <- cooks.distance(fit1) # salva distacia de cook em d
plot(d,pch="+") 

# [G4] alavancagem
h <- hatvalues(fit1) # salva medidas de alavancagem em h 
plot(h,pch="+") 

# medidas de influencia:
influence.measures(fit1)

# Graficos todos juntos
par(mfrow = c(2, 2)) # grafico em duas linhas e duas colunas
plot(r,pch="+") # residuos
abline(h=c(-2,0,2),lty=3) 
hnp(fit1, resid.type="student", halfnormal = F) # envelope
plot(d,pch="+") # cook
plot(h,pch="+") # alavanca

# Ha pouca evidência de observacoes influentes. Vamos assumir que estah ok. 

# E entao. Interpretacoes?
summary(fit1)
# 1) grupos 1, 3 e 4 (G134) nao possuem diferenca significativa 
# 2) grupo 2 possui media menor que G134. Quanto menor? exp(-0.4730) = 0.6231301
# 3) grupo 5 possui media maior que G134. Quanto maior? exp(0.41520) = 1.514674
# Note que essa interpretacao eh assim pois a ligacao eh log. 


### Atividade:
# 1) Ajuste com outras ligacoes. Verifique se o ajuste fica ok.
# 2) Ajuste assumindo distribuicao normal inversa. Ache um bom modelo. 
#    Compare com resultados da gamma. 
# 3) Explore as demais aplicacoes discutidas nas Secoes 2.4 e 2.8 do livro. 

fit <- poissonreg::poisson_reg() |>
  parsnip::set_engine("glm", family = poisson(link = 'logit')) |>
  parsnip::fit(resp ~ ., data = df)
