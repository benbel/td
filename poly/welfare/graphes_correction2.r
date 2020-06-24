library(dplyr)
library(ggplot2)
library(latex2exp)

total_gamma = 5
total_delta = 10

gamma_a_initial = 4
delta_a_initial = 1
gamma_b_initial = total_gamma - gamma_a_initial
delta_b_initial = total_delta - delta_a_initial
u_a_initial = gamma_a_initial + sqrt(delta_a_initial)
u_b_initial = sqrt(gamma_b_initial) + delta_b_initial


points = 4000

df <- (
  expand.grid(
    gamma_a = (0:points) / points * total_gamma,
    delta_a = (0:points) / points * total_delta
    )
  %>% mutate(
    gamma_b = total_gamma - gamma_a,
    delta_b = total_delta - delta_a,
    u_a = gamma_a + sqrt(delta_a),
    u_b = sqrt(gamma_b) + delta_b
    )
  )

(
  ggplot(df)
  + aes(
    x = delta_a,
    y = gamma_a
    )
  + geom_line(
    data = df %>% filter(abs(u_a - u_a_initial) < 0.001),
    color = 'orange'
  )
  + geom_line(
    data = df %>% filter(abs(u_b - u_b_initial) < 0.001),
    color = 'steelblue2'
    )
  + geom_point(
    data = df %>% filter(delta_a == delta_a_initial, gamma_a == gamma_a_initial),
    color = 'black',
    size = 2
    )
  + theme_minimal()
  + xlim(0, 10)
  + ylim(0, 5)
  + labs(
      x = TeX("$\\delta_a$"),
      y = TeX("$\\gamma_a$")
    )
  # + annotate(
  #     "text",
  #     x = delta_a_initial + 2.5,
  #     y = gamma_a_initial + 1 ,
  #     label = "Courbe d'indifférence de l'individu A",
  #     size = 4,
  #     colour='black'
  #   )
  # + geom_segment(
  #     data = data.frame(),
  #     aes(
  #       x = delta_a_initial + 1,
  #       y = gamma_a_initial + 0.9,
  #       xend = delta_a_initial + 1,
  #       yend = gamma_a_initial - 0.2
  #     ),
  #     arrow = arrow()
  #   )
  + labs(color = "Agent")
)

df <- (
  data.frame(
    ids = 1:10,
    revenu = c(0, 0, 5, 10, 10, 20, 30, 40, 50, 85)
  )
  %>% arrange(revenu)
  %>% mutate(revenus_cumules = cumsum(revenu))
)

(
  ggplot(df)
  + aes(
      x = ids/max(ids),
      y = revenus_cumules/max(revenus_cumules)
    )
  + geom_line()
  + theme_minimal()
  + labs(
      x = "Part de la population",
      y = "Part des revenus cumulés"
  )
)
