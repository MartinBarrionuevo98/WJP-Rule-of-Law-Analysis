
library(readxl)
data_wjp <- read_excel("C:/Users/Hp/Desktop/Analisis Rule of Law dataset/data.xlsx")

library(ggplot2)
library(dplyr)

# Verificar datos
head(data_wjp) 
data <- na.omit(data_wjp)
summary(data_wjp)



# Grafico de barras
ggplot(data_wjp, aes(x = Region, y = `WJP Rule of Law Index: Overall Score`)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue4") + 
  theme_minimal() +
  labs(title = "WJP Index by region",
       x = "region",
       y = "values") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# Analisis de regresion multiple
modelo <- lm(`Factor 8: Criminal Justice` ~ `Factor 1: Constraints on Government Powers` + `Factor 4: Fundamental Rights` + `Factor 7: Civil Justice` + `Factor 3: Open Government` + `Factor 2: Absence of Corruption`, data = data_wjp)
summary(modelo)

# Data set en formato largo
data_long <- melt(data_wjp, id.vars = "Factor 8: Criminal Justice", 
                  measure.vars = c("Factor 1: Constraints on Government Powers", 
                                   "Factor 4: Fundamental Rights", 
                                   "Factor 7: Civil Justice", 
                                   "Factor 3: Open Government", 
                                   "Factor 2: Absence of Corruption"),
                  variable.name = "Independent_Variable", 
                  value.name = "Value")



# Grafico de regresion
ggplot(data_long, aes(x = Value, y = `Factor 8: Criminal Justice`, color = Independent_Variable)) +
  geom_point(color = "black", alpha = 0.5, show.legend = FALSE) +  
  geom_smooth(method = "lm", se = FALSE, color = "blue", show.legend = FALSE) +  
  labs(title = "Relationship between independent variables and criminal justice", x = "Variables Independientes", y = "Criminal Justice") +  
  facet_wrap(~Independent_Variable, scales = "free_x", labeller = labeller(Independent_Variable = custom_labeller)) +  
  theme_minimal() +  
  theme(strip.text = element_text(size = 9, face = "bold")) 

# Calculo de residuos y test Q-Q

residuos <- residuals(modelo)
residuos_df <- data.frame(residuos)

ggplot(residuos_df, aes(sample = residuos)) +
  stat_qq() + 
  stat_qq_line(color = "red") + 
  labs(title = "Gráfico Q-Q de los Residuos", 
       x = "Cuantiles Teóricos", 
       y = "Cuantiles de los Residuos") +
  theme_minimal() 

# Calculo de la estimacion de coeficientes

tidy_modelo <- tidy(modelo)

ggplot(tidy_modelo, aes(x = estimate, y = term)) +
  geom_point(size = 4, color = "black") +
  geom_errorbarh(aes(xmin = estimate - std.error * 1.96, xmax = estimate + std.error * 1.96), height = 0.2) +
  labs(title = "Coeficientes de la Regresión Múltiple",
       x = "Estimación de Coeficientes",
       y = "Variables Independientes") +
  theme_minimal()

# Analisis ANOVA para observar diferencias entre las regiones en base a la variable dependiente Criminal Justice

anova_result <- aov(`Factor 8: Criminal Justice` ~ Region, data = data_wjp)
summary(anova_result)

# Grafico BOXPLOT
ggplot(data_wjp, aes(x = Region, y = `Factor 8: Criminal Justice`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "ANOVA analysis",
       x = "region",
       y = "criminal justice") +
  theme_minimal()


# Analisis de residuos

residuos <- residuals(modelo)
summary(residuos)

# El valor central de los residuos en los datos es de -0.0005106, lo cual significa que las predicciones del modelo son un tanto mayores
# que los valores observados aunque tienden a 0

# Histograma

ggplot(data_wjp, aes(x = `WJP Rule of Law Index: Overall Score`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "azure2", color = "black", alpha = 0.7) +  
  geom_density(color = "red", size = 1, alpha = 0.5) + 
  labs(title = "Histogram of the WJP Index",
       x = "values",
       y = "density") +  
  theme_minimal()  


#Gráfico de densidad con líneas de media y mediana
ggplot(data_wjp, aes(x = `WJP Rule of Law Index: Overall Score`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +
  labs(title = "WJP Rule of Law Index density graph",
       x = "values",
       y = "density") +
  theme_minimal()



mean_value <- mean(data_wjp$`WJP Rule of Law Index: Overall Score`, na.rm = TRUE)


install.packages("performance")
library(performance)

# Mostrar el p-valor formateado
formatted_p_value


modelo <- lm(`Factor 8: Criminal Justice` ~ 
               `Factor 1: Constraints on Government Powers` + 
               `Factor 4: Fundamental Rights` + 
               `Factor 7: Civil Justice` + 
               `Factor 3: Open Government` + 
               `Factor 2: Absence of Corruption`, 
             data = data_wjp)


collinearity_results <- check_collinearity(modelo)
plot(collinearity_results)

heteroscedasticity_results <- check_heteroscedasticity(modelo)
print(heteroscedasticity_results)

# Grafico de residuos
plot(modelo, which = 1)  

modelo_residuos <- data.frame(
  Ajustados = modelo$fitted.values,
  Residuos = modelo$residuals
)


ggplot(modelo_residuos, aes(x = Ajustados, y = Residuos)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()


# Calcular valores p 
p_values <- c(2e-16, 2.68e-12, 2e-16, 2e-16, 9.55e-07)


p_adjusted_bonferroni <- p.adjust(p_values, method = "bonferroni")

p_adjusted_holm <- p.adjust(p_values, method = "holm")

p_adjusted_bh <- p.adjust(p_values, method = "BH")

# Resultados
data.frame(
  Original = p_values,
  Bonferroni = p_adjusted_bonferroni,
  Holm = p_adjusted_holm,
  BH = p_adjusted_bh
)

