library("dplyr")
library("rmarkdown")

usuarios <- read.csv("usuarios.csv")
recorridos <- read.csv("recorridos.csv")

# === FORMATEO DE DIAS === #
recorridos$dia <- factor(recorridos$dia, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"), ordered = TRUE)
levels(recorridos$dia) <- c("Lu", "Ma", "Mi", "Ju", "Vi", "Sa", "Do")

save.factor.as.piechart <- function(factor, filename, sort = FALSE, ...) {
    summarized <- summary(factor)

    target <-
        if (sort)
            sort(summarized, decreasing = TRUE)
        else
            summarized

    print("Barplot data summary:")
    print(summarized)

    relative.percentages <- round(summarized / sum(summarized) * 100)
    labels <- paste(names(summarized), " ", relative.percentages, "%")

    png(filename)
    pie(target, labels = labels, ...)
    invisible(dev.off())
}

save.factor.as.barplot <- function(factor, filename, sort = FALSE, ...) {
    summarized <- summary(factor)

    target <-
        if (sort)
            sort(summarized, decreasing = TRUE)
        else
            summarized

    print("Barplot data summary:")
    print(summarized)

    png(filename)
    barplot(target, ...)
    invisible(dev.off())
}

save.ordinal.as.vlines <- function(ordinal, filename, ...) {
    png(filename)
    plot(ordinal, type = "h", ...)
    invisible(dev.off())
}

save.best.barplot <- function(factor, filename, size, ...) {
    summarized <- summary(factor, maxsum = length(factor))

    target <- sort(summarized, decreasing = TRUE)

    best = head(target, size)

    print("Best:")
    print(names(best))
    print("Best percentage:")
    print(sum(best) / sum(summarized) * 100)

    png(filename, width = 960, height = 960)
    par(mar=c(20,4,4,4))
    barplot(best, ...)
    invisible(dev.off())
}

save.as.histogram <- function(values, filename, ...) {
    print("Histogram data summary:")
    print(summary(values))

    png(filename)
    hist(values, ...)
    invisible(dev.off())
}

remove.outliers <- function(values) {
    Q1 <- quantile(values, 0.25)
    Q3 <- quantile(values, 0.75)
    IQR <- IQR(values)
    return(subset(values, values > (Q1 - 3 * IQR) & values < (Q3 + 3 * IQR)))
}

save.as.boxplot <- function(categories, distribution, filename, ...) {
    png(filename)
    boxplot(distribution ~ categories, ...)
    invisible(dev.off())
}

histogram.params <- function(values, interval = FALSE, breaks = FALSE) {
    min <- min(values)
    max <- max(values)

    limits <- c(min, max)

    breaks <-
        if (interval)
            seq(min, max, interval)
        else if (breaks)
            breaks
        else
            "Sturges"

    xaxp <-
        if (interval)
            c(min, max, length(breaks) - 1)
        else if (breaks)
            c(min, max, breaks)
        else
            NULL

    return(list(limits = limits, breaks = breaks, xaxp = xaxp))
}


# === GENEROS === #
generos <- factor(usuarios$genero_usuario)
save.factor.as.piechart(generos, "generos.png", sort = TRUE,
                        main = "Frecuencias relativas porcentuales de géneros de los usuarios",
                        clockwise = FALSE)


# === EDADES === #
edades <- usuarios$edad_usuario
params <- histogram.params(edades, 4)
save.as.histogram(edades, "edades.png",
                  xlim = params$limit,
                  xaxp = params$xaxp,
                  breaks = params$breaks,
                  main = "Histograma de frecuencia, sobre las edades de los usuarios",
                  xlab = "Rango Etario",
                  ylab = "Frecuencia absoluta de uso",
                  col = "lightblue")


# === ORIGEN  === #
origenes <- factor(recorridos$direccion_estacion_origen)
save.best.barplot(origenes, "origenes.png", 10,
                  main = "10 orígenes más frecuentes",
                  xlab = "Orígenes",
                  ylab = "Frecuencia absoluta de uso",
                  col = "orange",
                  las = 2)


# === DESTINO  === #
destinos <- factor(recorridos$direccion_estacion_destino)
save.best.barplot(destinos, "destinos.png", 10,
                  main = "10 destinos más frecuentes",
                  xlab = "Destinos",
                  ylab = "Frecuencia absoluta de uso",
                  col = "orange",
                  las = 2)


# === DIAS === #
save.factor.as.barplot(recorridos$dia, "dias.png", sort = FALSE,
                       main = "Frecuencias de los viajes, por cada día de la semana",
                       xlab = "Día de la semana",
                       ylab = "Frecuencia absoluta",
                       col = "lightgreen")


# === DURACION  === #
duracion <- recorridos$duracion_recorrido
removed <- remove.outliers(duracion)

print("Percentage of outliers:")
print((1 - (length(removed) / length(duracion))) * 100)

removed <- removed / 60

params <- histogram.params(removed, breaks = 16)
save.as.histogram(removed, "duracion.png",
                  xlim = params$limit,
                  xaxp = params$xaxp,
                  breaks = params$breaks,
                  main = "Histograma de frecuencia, sobre las duraciones de los viajes",
                  xlab = "Duración [minutos]",
                  ylab = "Frecuencia absoluta",
                  col = "orange")


# === DISTANCIA  === #
distancia <- recorridos$distancia
removed <- remove.outliers(distancia)

print("Percentage of outliers:")
print((1 - (length(removed) / length(distancia))) * 100)

removed <- removed / 1000

params <- histogram.params(removed, breaks = 16)
save.as.histogram(removed, "distancia.png",
                  xlim = params$limit,
                  xaxp = params$xaxp,
                  breaks = params$breaks,
                  main = "Histograma de frecuencia, sobre las distancias recorridas",
                  xlab = "Distancia [kilómetros]",
                  ylab = "Frecuencia absoluta",
                  col = "lightblue")


# === COMBINACION DE DATOS === #
join <- merge(usuarios, recorridos)


# === CANTIDAD DE VIAJES POR PERSONA === #
counts.by.user <- join %>% group_by(id_usuario) %>% summarise(count = n())
count <- counts.by.user$count %>% remove.outliers() %>% factor(levels = 1:16) %>% summary()
save.ordinal.as.vlines(count, "viajes.png",
                       main = "Cantidad de viajes que realizan los usuarios",
                       xlab = "Cantidad de viajes",
                       ylab = "Frecuencia de aparición de las cantidades",
                       col = "black")

# === DISTANCIA POR DIA === #
save.as.boxplot(join$dia, join$distancia, "bivariado.png",
                main = "Boxplot de distancia en metros por cada día de la semana",
                ylab = "Distancia [metros]",
                xlab = "Día de la semana",
                col = "lightgreen",
                outline = FALSE)
