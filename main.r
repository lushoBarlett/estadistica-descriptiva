library("dplyr")
library("rmarkdown")

usuarios <- read.csv("usuarios.csv")
recorridos <- read.csv("recorridos.csv")

# === FORMATEO DE DIAS === #
recorridos$dia <- factor(recorridos$dia, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"), ordered = TRUE)
levels(recorridos$dia) <- c("Lu", "Ma", "Mi", "Ju", "Vi", "Sa", "Do")

save.as <- function(filename, plotter, ...) {
    png(filename, ...)
    plotter()
    invisible(dev.off())
}

factor.as.piechart <- function(factor, filename, sort = FALSE, ...) {
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

    pie(target, labels = labels, ...)
}

factor.as.barplot <- function(factor, sort = FALSE, ...) {
    summarized <- summary(factor)

    target <-
        if (sort)
            sort(summarized, decreasing = TRUE)
        else
            summarized

    print("Barplot data summary:")
    print(summarized)

    barplot(target, ...)
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
    par(mar=c(4,20,4,4))
    barplot(best, ...)
    invisible(dev.off())
}

as.histogram <- function(values, ...) {
    print("Histogram data summary:")
    print(summary(values))

    hist(values, ...)
}

remove.outliers <- function(values) {
    Q1 <- quantile(values, 0.25)
    Q3 <- quantile(values, 0.75)
    IQR <- IQR(values)
    return(subset(values, values > (Q1 - 3 * IQR) & values < (Q3 + 3 * IQR)))
}

seq.complete <- function(min, max, interval) {
    result <- seq(min, max, interval)

    if (max %in% result) {
        return(result)
    }

    return(c(result, max))
}

histogram.params <- function(values, interval = FALSE, breaks = FALSE, limits = NULL) {
    if (length(limits) == 0) {
        min <- min(values)
        max <- max(values)
    } else {
        min <- limits[1]
        max <- limits[2]
    }

    limits <- c(min, max)

    breaks <-
        if (interval)
            seq.complete(min, max, interval)
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

save.as("generos.png", function() {
    factor.as.piechart(
        generos,
        sort = TRUE,
        main = "Frecuencias relativas porcentuales de géneros de los usuarios",
        clockwise = FALSE)
})


# === EDADES === #
edades <- usuarios$edad_usuario
params <- histogram.params(edades, 5)
save.as("edades.png", function() {
    as.histogram(
        edades,
        xlim = params$limit,
        xaxp = params$xaxp,
        breaks = params$breaks,
        main = "Histograma de frecuencia, sobre las edades de los usuarios",
        xlab = "Edad [años]",
        ylab = "Frecuencia absoluta de uso",
        ylim = c(0, 50),
        col = "lightblue")
})


# === ORIGEN  === #
origenes <- factor(recorridos$direccion_estacion_origen)
save.best.barplot(origenes, "origenes.png", 10,
                  main = "10 orígenes más frecuentes",
                  ylab = "Orígenes",
                  xlab = "Frecuencia absoluta de uso",
                  col = "orange",
                  las = 1,
                  xlim = c(0, 40),
                  horiz = TRUE)


# === DESTINO  === #
destinos <- factor(recorridos$direccion_estacion_destino)
save.best.barplot(destinos, "destinos.png", 10,
                  main = "10 destinos más frecuentes",
                  ylab = "Destinos",
                  xlab = "Frecuencia absoluta de uso",
                  col = "orange",
                  las = 1,
                  xlim = c(0, 50),
                  horiz = TRUE)


# === DIAS === #
save.as("dias.png", function() {
    factor.as.barplot(
        recorridos$dia,
        sort = FALSE,
        main = "Frecuencias de los viajes, por cada día de la semana",
        xlab = "Día de la semana",
        ylab = "Frecuencia absoluta",
        col = "lightgreen",
	ylim = c(0, 200))
})


# === DURACION  === #
duracion <- recorridos$duracion_recorrido
removed <- remove.outliers(duracion)

print("Percentage of outliers:")
print((1 - (length(removed) / length(duracion))) * 100)

removed <- removed / 60

params <- histogram.params(removed, 10, limits = c(0, 80))
save.as("duracion.png", function() {
    as.histogram(
        removed,
        xlim = params$limit,
        xaxt = "n",
        breaks = params$breaks,
        main = "Histograma de frecuencia, sobre las duraciones de los viajes",
        xlab = "Duración [minutos]",
        ylab = "Frecuencia absoluta",
        col = "orange",
        ylim = c(0, 350))
    axis(1, at = params$breaks)
})


# === DISTANCIA  === #
distancia <- recorridos$distancia
removed <- remove.outliers(distancia)

print("Percentage of outliers:")
print((1 - (length(removed) / length(distancia))) * 100)

removed <- removed / 1000

params <- histogram.params(removed, 2, limits = c(0, 18))
save.as("distancia.png", function() {
    as.histogram(
        removed,
        xlim = params$limit,
        xaxt = "n",
        breaks = params$breaks,
        main = "Histograma de frecuencia, sobre las distancias recorridas",
        xlab = "Distancia [kilómetros]",
        ylab = "Frecuencia absoluta",
        col = "lightblue")
    axis(1, at = params$breaks)
})


# === COMBINACION DE DATOS === #
join <- merge(usuarios, recorridos)


# === CANTIDAD DE VIAJES POR PERSONA === #
counts.by.user <- join %>% group_by(id_usuario) %>% summarise(count = n())
count <- counts.by.user$count %>% remove.outliers() %>% factor(levels = 1:16) %>% summary()
save.as("viajes.png", function() {
    plot(
        count,
        type = "h",
        main = "Cantidad de viajes que realizan los usuarios",
        xlab = "Cantidad de viajes",
        ylab = "Cantidad de usuarios",
        col = "black")
})

# === DISTANCIA POR DIA === #
save.as("bivariado.png", function() {
    boxplot(
        join$distancia ~ join$dia,
        main = "Distribución de la distancia recorrida por día de la semana",
        ylab = "Distancia [metros]",
        xlab = "Día de la semana",
        col = "lightgreen",
        outline = FALSE)
})
