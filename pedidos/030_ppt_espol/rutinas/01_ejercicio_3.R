bdd_01 <- bdd |> 
  mutate(numerador = N * sd * sd * d1_deff,
         denominador = (N * ((mer * d1 / 1.96)^2) + sd * sd * d1_deff) * (1 - 0.2),
         n0 = numerador/ denominador)
