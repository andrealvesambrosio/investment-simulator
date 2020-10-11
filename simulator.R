# t em meses
simulator <- function(entrada, aporte, t, juros){
  t <- t*12
  M <- entrada*(1 + juros)^t + aporte*(((1 + juros)^t) - 1)/juros
  economia <- entrada + t*aporte
  dif <- M - economia
  
  cat("Montante:", M)
  cat("\nJuntou no total:", economia)
  cat("\nQuanto rendeu acima do que juntou:", dif)
  cat("\nRentabilidade de:", 100*dif/economia)
  
  #tibble(Montante = M, Economia = Economia, Diferenca = Dif)
}

simulator(entrada = 2000,
          aporte = 0,
          t = 1, # em anos
          juros = 0.005) # ao mês




# Gráfico:
# Eixo X: 


# - Tempo
# - Aporte
# - Rentabilidade
# - Montante


# Tempo vs Aporte         (Fixado Rentabilidade e Montante)
# Tempo vs Rentabilidade  (Fixado Aporte e Montante)
# Tempo vs Montante       (Fixado Aporte e Rentabilidade)

#
