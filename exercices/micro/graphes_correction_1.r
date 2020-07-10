library(dplyr)
library(ggplot2)
library(latex2exp)

N = 150
M = 50

n = 2500

df <- (
  expand.grid(s = 0:n/n)
  %>% mutate(
    demande = M*(1-s),
    offre = N * s
    )
  )

equilibre =  df %>% filter(demande == offre)
q_star = equilibre$demande
s_star = equilibre$s

s_min = 0.1
offre_min =  N * s_min
demande_min = M * (1 - s_min)

(
  ggplot(df)
  + aes(
    y = s,
    group = interaction(M,N)
    )
  + geom_line(aes(x = demande, color = "Demande"))
  + geom_line(aes(x = offre, color = "Offre"))
  + theme_minimal()
  + labs(
    y = "Salaire",
    x = "Quantité de travail",
    color = "Courbe"
    )
  + theme(legend.position="bottom")
  + geom_segment(
      x = 0,
      xend = demande_min,
      y = s_min,
      yend = s_min,
      linetype = "dashed"
    )
  + geom_segment(
    x = demande_min,
    xend = offre_min,
    y = s_min,
    yend = s_min,
    color = 'black'
  )
  + geom_point(
      x = demande_min,
      y = s_min
      )
  + geom_point(
      x = offre_min,
      y = s_min
      )
  + annotate(
    geom = 'text',
    x = 55, y = 0.6,
    label = "Différence entre l'offre et la demande = chômage"
    )
  + annotate(
    geom = 'text',
    x = -5, y = 0.51,
    label = TeX("$s^{min}$"),
    parse = T
    )
)


dd = expand.grid(M = 0:100/25, N = 0:100/25) %>% mutate(d = M*N+2 - M-N)
ggplot(dd) + aes(x = M, y = N, fill = d > 0) + geom_raster()
