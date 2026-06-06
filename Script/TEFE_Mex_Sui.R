# =============================================================================
# Tasas Específicas de Fecundidad por Edad (TEFE) — 2019
# Coahuila, México (nacional) y Suiza
# =============================================================================
# Fuentes:
#   - Coahuila:  CONAPO, Proyecciones de Población de México y de las Entidades
#                Federativas 2016-2050. TEFE quinquenales (nac./1,000 mujeres).
#   - México:    INEGI, Censo de Población y Vivienda 2020 (año de referencia
#                2019). Publicado en: Estrada-Ávalos et al., RDE-INEGI, 2022.
#                TEFE quinquenales (nac./1,000 mujeres).
#   - Suiza:     Federal Statistical Office (FSO/BFS) – BEVNAT /
#                Human Fertility Database, 2019 (nac./1,000 mujeres).
# =============================================================================

# ── 0. Paquetes ───────────────────────────────────────────────────────────────
pkgs <- c("ggplot2", "dplyr", "tidyr", "scales", "ggrepel")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, quiet = TRUE)
  library(p, character.only = TRUE)
}

# ── 1. Datos ──────────────────────────────────────────────────────────────────

grupos_edad  <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
puntos_medios <- c(17, 22, 27, 32, 37, 42, 47)

# Coahuila 2019 — CONAPO, Proyecciones 2016-2050
tefe_coahuila <- c(47.6, 103.8, 97.4, 71.2, 35.8, 8.8, 0.9)

# México nacional 2019 — INEGI Censo 2020 / Estrada-Ávalos et al. (RDE-INEGI 2022)
tefe_mexico   <- c(52.3, 101.2, 89.7, 61.5, 28.8, 6.8, 0.5)

# Suiza 2019 — FSO (BEVNAT) / Human Fertility Database
tefe_suiza    <- c(1.9, 26.0, 83.5, 102.8, 52.4, 10.8, 0.6)

# ── 2. Data frame largo ───────────────────────────────────────────────────────
df <- data.frame(
  grupo_edad  = rep(grupos_edad, 3),
  punto_medio = rep(puntos_medios, 3),
  tefe        = c(tefe_coahuila, tefe_mexico, tefe_suiza),
  poblacion   = rep(c("Coahuila, México",
                      "México (nacional)",
                      "Suiza"), each = 7)
)

df$grupo_edad <- factor(df$grupo_edad, levels = grupos_edad)
df$poblacion  <- factor(df$poblacion,
                        levels = c("México (nacional)",
                                   "Coahuila, México",
                                   "Suiza"))

# ── 3. TGF ────────────────────────────────────────────────────────────────────
tgf <- df |>
  dplyr::group_by(poblacion) |>
  dplyr::summarise(TGF = round(sum(tefe) * 5 / 1000, 2), .groups = "drop")

print(tgf)

# ── 4. Estética ───────────────────────────────────────────────────────────────
colores <- c(
  "México (nacional)"  = "#A8E",   # verde oscuro
  "Coahuila, México"   = "#CDB4DB",   # rojo
  "Suiza"              = "#81ECEC"    # azul acero
)
trazos <- c(
  "México (nacional)"  = "solid",
  "Coahuila, México"   = "dashed",
  "Suiza"              = "dotdash"
)
formas <- c(
  "México (nacional)"  = 21,
  "Coahuila, México"   = 22,
  "Suiza"              = 24
)

# Etiquetas de leyenda con TGF incluida
etiquetas <- setNames(
  paste0(tgf$poblacion, "  (TGF = ", tgf$TGF, ")"),
  tgf$poblacion
)

# ── 5. Gráfica A — curvas con área ────────────────────────────────────────────
p_curvas <- ggplot(df,
                   aes(x = grupo_edad, y = tefe,
                       color = poblacion, fill = poblacion,
                       group = poblacion, shape = poblacion,
                       linetype = poblacion)) +
  
  geom_area(alpha = 0.10, position = "identity") +
  geom_line(linewidth = 1.15) +
  geom_point(size = 4, stroke = 0.8, color = "white") +
  geom_point(size = 4, alpha = 0.95) +
  geom_text(aes(label = tefe),
            vjust = -1.05, size = 2.9,
            fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = colores, labels = etiquetas,
                     name = NULL) +
  scale_fill_manual(values  = colores, labels = etiquetas,
                    name = NULL) +
  scale_shape_manual(values = formas,  labels = etiquetas,
                     name = NULL) +
  scale_linetype_manual(values = trazos, labels = etiquetas,
                        name = NULL) +
  
  scale_y_continuous(
    limits = c(0, 130),
    breaks = seq(0, 120, by = 20),
    expand = expansion(mult = c(0, 0.10))
  ) +
  
  labs(
    title    = "Tasas Específicas de Fecundidad por Edad (TEFE)",
    subtitle = "Coahuila, México (nacional) y Suiza — 2019",
    x        = "Grupo de edad (años)",
    y        = "Nacimientos por cada 1,000 mujeres",
    caption  = paste0(
      "Fuentes: CONAPO – Proyecciones de Población 2016-2050 (Coahuila); ",
      "INEGI – Censo 2020 / Estrada-Ávalos et al., RDE-INEGI 2022 (México nacional);\n",
      "Federal Statistical Office – BEVNAT / Human Fertility Database (Suiza). ",
      "TGF = Σ(TEFE) × 5 / 1,000."
    )
  ) +
  
  theme_minimal(base_size = 7) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle      = element_text(size = 7.5, hjust = 0.5,
                                      color = "gray30"),
    plot.caption       = element_text(size = 4.8, hjust = 0,
                                      color = "gray50", lineheight = 1.3),
    axis.title         = element_text(face = "bold"),
    axis.text          = element_text(size = 7),
    legend.position    = "bottom",
    legend.text        = element_text(size = 6),
    legend.key.width   = unit(2.2, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(15, 20, 10, 15)
  ) +
  guides(color    = guide_legend(nrow = 3),
         fill     = guide_legend(nrow = 3),
         shape    = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))

# ── 6. Gráfica B — barras agrupadas ──────────────────────────────────────────
p_barras <- ggplot(df,
                   aes(x = grupo_edad, y = tefe, fill = poblacion)) +
  
  geom_col(position = position_dodge(width = 0.78),
           width = 0.72, alpha = 0.90, color = "white", linewidth = 0.3) +
  geom_text(aes(label = tefe),
            position = position_dodge(width = 0.78),
            vjust = -0.45, size = 2.65, fontface = "bold") +
  
  scale_fill_manual(values = colores, labels = etiquetas,
                    name = NULL) +
  scale_y_continuous(
    limits = c(0, 125),
    breaks = seq(0, 120, by = 20),
    expand = expansion(mult = c(0, 0.10))
  ) +
  
  labs(
    title    = "TEFE comparadas — gráfica de barras",
    subtitle = "Coahuila, México (nacional) y Suiza, 2019",
    x        = "Grupo de edad (años)",
    y        = "Nacimientos por cada 1,000 mujeres",
    caption  = paste0(
      "Fuentes: CONAPO – Proyecciones 2016-2050 (Coahuila); ",
      "INEGI – Censo 2020 (México); FSO – BEVNAT (Suiza)."
    )
  ) +
  
  theme_minimal(base_size = 7) +
  theme(
    plot.title         = element_text(face = "bold", size = 10, hjust = 0.5),
    plot.subtitle      = element_text(size = 7, hjust = 0.5,
                                      color = "gray30"),
    plot.caption       = element_text(size = 4, hjust = 0,
                                      color = "gray50"),
    axis.title         = element_text(face = "bold"),
    axis.text          = element_text(size = 7),
    legend.position    = "bottom",
    legend.text        = element_text(size = 6),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(15, 20, 10, 15)
  ) +
  guides(fill = guide_legend(nrow = 3))

# ── 7. Tabla resumen en consola ───────────────────────────────────────────────
cat("\n=== TEFE 2019 — nacimientos por 1,000 mujeres ===\n\n")
tabla <- df |>
  dplyr::select(grupo_edad, poblacion, tefe) |>
  tidyr::pivot_wider(names_from = poblacion, values_from = tefe) |>
  dplyr::rename(`Grupo edad` = grupo_edad)
print(as.data.frame(tabla), row.names = FALSE)

cat("\n=== TASA GLOBAL DE FECUNDIDAD (TGF) ===\n")
print(as.data.frame(tgf), row.names = FALSE)

# ── 8. Guardar ────────────────────────────────────────────────────────────────
ggsave("TEFE_curvas_3pob_2019.png",  plot = p_curvas,
       width = 10, height = 6.5, dpi = 180, bg = "white")
ggsave("TEFE_barras_3pob_2019.png",  plot = p_barras,
       width = 10, height = 6.5, dpi = 180, bg = "white")

print(p_curvas)
print(p_barras)

message("\nArchivos guardados: TEFE_curvas_3pob_2019.png  y  TEFE_barras_3pob_2019.png")
