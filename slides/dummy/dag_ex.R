if (!requireNamespace("dagitty", quietly = TRUE)) install.packages("dagitty")
if (!requireNamespace("ggdag", quietly = TRUE)) install.packages("ggdag")

library(dagitty)
library(ggdag)

dag_confounder <- dagitty("
dag {
  TRAJ -> VIO
  ADI -> VIO
  ADI -> TRAJ
 }
")

dag_confounder <- dagitty("
dag {
  TRAJ -> VIO
  SEG -> VIO
  SEG -> TRAJ
 }
")

ggdag(dag_confounder) +
  theme_dag()

dag_confounder <- dagitty("
dag {
  TRAJ -> VIO
  ADI -> VIO
  ADI -> TRAJ
  SEG -> VIO
  SEG -> TRAJ
  ADI <-> SEG
}
")

ggdag(dag_confounder) +
  theme_dag()

dag_mediator <- dagitty("
dag {
  Lending_Trajectory -> ADI -> Firearm_Violence
  Lending_Trajectory -> Segregation -> Firearm_Violence
  ADI <-> Segregation
}
")

ggdag(dag_mediator) +
  theme_dag()
