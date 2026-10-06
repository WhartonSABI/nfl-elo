steps <- list.files("scripts", pattern = "^0[1-9]_.*\\.R$", full.names = TRUE)
for (file in sort(steps)) {
  message("\n", basename(file))
  source(file)
}
