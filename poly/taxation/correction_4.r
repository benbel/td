library(ggplot2)
library(dplyr)

data <- read.csv("fisc.csv")

impot <- function(revenu) {
  tranches <- c(14999, 29999, Inf)
  taux <- c(0, .25, .40)

  montant_impot <-   sum(diff(c(0, pmin(revenu, tranches))) * taux)

  return(montant_impot)
}


data <- (
  data
  %>% mutate(
    revenu = zh + zf,
    nombre_parts = nombre_enfants + 2,
    quotient_familial = revenu / nombre_parts,
    impot_paye = Vectorize(impot)(quotient_familial) * nombre_parts,
    revenu_disponible = revenu - impot_paye,
    taux_effectif = impot_paye / revenu_disponible
    )
  )

(
  ggplot(data)
  + aes(x = revenu, y = taux_effectif, color = factor(nombre_enfants))
  + geom_smooth(se = F, aes(group = NA))
  + geom_point()
  + theme_minimal()
  + theme(legend.position = 'bottom')
  + labs(
      x = "Revenu du ménage",
      y = "Taux d'imposition",
      fill = "Nombre d'enfants"
  )
)
