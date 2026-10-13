# Shared presentation for the survey's interactive charts. All inputs are already
# filtered: these helpers never average survey percentages or add population bases.
obscop_chart_number <- function(x, digits = 1L) {
  out <- formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
  if (digits > 0L) out <- sub(",0+$", "", out)
  out[!is.finite(x)] <- "non disponible"
  out
}

obscop_chart_text <- function(x) {
  as.character(htmltools::htmlEscape(as.character(x)))
}

obscop_chart_wrap <- function(x, width = 32L) {
  vapply(as.character(x), function(s) {
    paste(obscop_chart_text(strwrap(s, width = width)), collapse = "<br>")
  }, character(1))
}

obscop_chart_unit <- function(d) {
  if ("unit" %in% names(d) && length(unique(d$unit)) == 1L) unique(d$unit) else "%"
}

obscop_chart_tooltip <- function(d, labels = d$group_label) {
  unit <- if ("unit" %in% names(d)) ifelse(d$unit == "%", " %", "") else " %"
  txt <- paste0("<b>", obscop_chart_text(labels), "</b><br>",
                obscop_chart_number(d$value), unit)
  if ("n" %in% names(d)) {
    txt <- paste0(txt, ifelse(is.finite(d$n),
      paste0("<br>Base : ", obscop_chart_number(d$n, 0L), " personnes"), ""))
  }
  if ("base" %in% names(d)) {
    txt <- paste0(txt, ifelse(!is.na(d$base) & nzchar(d$base),
      paste0("<br>", obscop_chart_wrap(d$base, 65L)), ""))
  }
  txt
}

obscop_chart_finish <- function(p, filename = "obscop-2025") {
  p <- plotly::layout(p,
    font = list(family = "Inter, Arial, sans-serif", size = 12, color = "#43504D"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 16, r = 30, b = 42, t = 24, pad = 5),
    hoverlabel = list(bgcolor = "#173F39", bordercolor = "#173F39",
                      font = list(color = "#FFFFFF", size = 12)),
    legend = list(orientation = "h", x = 0, y = 1.12,
                  font = list(size = 12)),
    separators = ", ",
    xaxis = list(zeroline = FALSE, gridcolor = "#E8EEEA", automargin = TRUE),
    yaxis = list(zeroline = FALSE, gridcolor = "#E8EEEA", automargin = TRUE)
  )
  plotly::config(p, locale = "fr", responsive = TRUE, displaylogo = FALSE,
    showSendToCloud = FALSE, scrollZoom = FALSE,
    modeBarButtonsToRemove = c("sendDataToCloud", "lasso2d", "select2d", "hoverCompareCartesian"),
    toImageButtonOptions = list(format = "png", filename = filename,
                               width = 1400, height = 900, scale = 2))
}

obscop_chart_source <- function(p, bottom = 72L, annotations = list()) {
  source <- list(x = 1, y = 0, xref = "paper", yref = "paper", xanchor = "right",
    yanchor = "top", yshift = -(bottom - 30), showarrow = FALSE,
    text = "Obs’COP – EDF / IPSOS<br>Septembre 2025",
    font = list(size = 10, color = "#5E7167"))
  plotly::layout(p, margin = list(b = bottom), annotations = c(annotations, list(source)))
}

obscop_chart_empty <- function(message = "Aucune donnée disponible pour cette sélection.") {
  p <- plotly::plot_ly(type = "scatter", mode = "markers", x = numeric(), y = numeric())
  p <- obscop_chart_finish(p)
  p <- plotly::layout(p, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  obscop_chart_source(p, annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
      text = message, showarrow = FALSE, font = list(color = "#75847D", size = 14))))
}

obscop_bars <- function(d, label = "group_label", reference = NULL, horizontal = TRUE) {
  d <- d[is.finite(d$value), , drop = FALSE]
  if (!nrow(d)) return(obscop_chart_empty())
  d <- d[order(d$value, decreasing = !horizontal), , drop = FALSE]
  unit <- obscop_chart_unit(d)
  suffix <- if (identical(unit, "%")) " %" else ""
  ticks <- seq_len(nrow(d))
  labels <- obscop_chart_wrap(d[[label]], if (horizontal) 35L else 20L)
  colors <- rep("#168B79", nrow(d))
  if ("is_world" %in% names(d)) colors[d$is_world %in% TRUE] <- "#D29646"
  p <- plotly::plot_ly(source = "obscop-bars", type = "bar",
    x = if (horizontal) d$value else ticks,
    y = if (horizontal) ticks else d$value,
    orientation = if (horizontal) "h" else "v",
    marker = list(color = colors, line = list(width = 0)),
    text = paste0(obscop_chart_number(d$value), suffix), textposition = "outside",
    cliponaxis = FALSE, customdata = d[[label]],
    hovertext = obscop_chart_tooltip(d, d[[label]]), hoverinfo = "text",
    showlegend = FALSE)
  p <- obscop_chart_finish(p, "obscop-classement")
  extent <- if (identical(unit, "%")) c(0, 100) else c(0, max(d$value) * 1.15)
  numeric_axis <- list(title = "", range = extent, ticksuffix = suffix,
                       gridcolor = "#E8EEEA", fixedrange = FALSE)
  label_axis <- list(title = "", tickmode = "array", tickvals = ticks,
                     ticktext = labels, showgrid = FALSE,
                     range = c(0.4, nrow(d) + 0.6), fixedrange = TRUE)
  p <- plotly::layout(p, bargap = 0.30,
    xaxis = if (horizontal) numeric_axis else label_axis,
    yaxis = if (horizontal) label_axis else numeric_axis,
    margin = list(r = if (horizontal) 65 else 25, t = 32))
  if (length(reference) == 1L && is.finite(reference)) {
    shape <- if (horizontal) list(type = "line", x0 = reference, x1 = reference,
      y0 = 0, y1 = 1, yref = "paper") else list(type = "line", y0 = reference,
      y1 = reference, x0 = 0, x1 = 1, xref = "paper")
    shape$line <- list(color = "#D29646", width = 2, dash = "dot")
    p <- plotly::layout(p, shapes = list(shape))
  }
  obscop_chart_source(p, bottom = if (horizontal) 72L else 110L)
}

obscop_heatmap <- function(d) {
  d <- d[is.finite(d$value), , drop = FALSE]
  if (!nrow(d)) return(obscop_chart_empty())
  key <- paste(d$group_label, d$answer, sep = "\r")
  if (anyDuplicated(key)) stop("Une seule valeur est attendue par groupe et réponse.")
  groups <- unique(d$group_label)
  answers <- unique(d$answer)
  z <- matrix(NA_real_, nrow = length(groups), ncol = length(answers))
  tips <- matrix("Donnée non disponible", nrow = length(groups), ncol = length(answers))
  idx <- cbind(match(d$group_label, groups), match(d$answer, answers))
  z[idx] <- d$value
  tips[idx] <- paste0(obscop_chart_tooltip(d), "<br>", obscop_chart_wrap(d$answer, 45L))
  pct <- identical(obscop_chart_unit(d), "%")
  p <- plotly::plot_ly(type = "heatmap", x = seq_along(answers), y = seq_along(groups),
    z = z, text = tips, hoverinfo = "text", hoverongaps = FALSE,
    colorscale = list(c(0, "#F0F7F3"), c(0.3, "#B4DDD0"),
                      c(0.65, "#46A68D"), c(1, "#075A50")),
    zmin = 0, zmax = if (pct) 100 else max(d$value),
    colorbar = list(title = if (pct) "%" else "Valeur", thickness = 12, len = 0.8),
    xgap = 3, ygap = 3)
  p <- obscop_chart_finish(p, "obscop-matrice")
  p <- plotly::layout(p,
    xaxis = list(title = "", tickmode = "array", tickvals = seq_along(answers),
      ticktext = obscop_chart_wrap(answers, 20L), showgrid = FALSE, side = "bottom"),
    yaxis = list(title = "", tickmode = "array", tickvals = seq_along(groups),
      ticktext = obscop_chart_wrap(groups, 28L), showgrid = FALSE, autorange = "reversed"),
    margin = list(b = 95, r = 55))
  annotations <- list()
  if (length(z) <= 500L) {
    annotations <- lapply(seq_len(nrow(d)), function(i) list(
      x = idx[i, 2], y = idx[i, 1], text = obscop_chart_number(d$value[i]),
      showarrow = FALSE, font = list(size = if (length(answers) > 8L) 10 else 12,
        color = if (d$value[i] / max(if (pct) 100 else max(d$value), 1) > 0.81) "#FFFFFF" else "#000000")))
  }
  obscop_chart_source(p, bottom = 125L, annotations = annotations)
}

obscop_gap <- function(d, a_label = NULL, b_label = NULL) {
  d <- d[is.finite(d$a) & is.finite(d$b), , drop = FALSE]
  if (!nrow(d)) return(obscop_chart_empty("Aucune réponse commune aux deux territoires."))
  if (is.null(a_label)) a_label <- if ("a_label" %in% names(d)) d$a_label[1] else "Territoire A"
  if (is.null(b_label)) b_label <- if ("b_label" %in% names(d)) d$b_label[1] else "Territoire B"
  if (!"delta" %in% names(d)) d$delta <- d$a - d$b
  d <- d[order(abs(d$delta)), , drop = FALSE]
  rows <- seq_len(nrow(d))
  unit <- obscop_chart_unit(d)
  suffix <- if (identical(unit, "%")) " %" else ""
  diff_unit <- if (identical(unit, "%")) " pts" else ""
  delta_text <- paste0(ifelse(d$delta > 0, "+", ""), obscop_chart_number(d$delta), diff_unit)
  tooltip <- paste0("<b>", obscop_chart_wrap(d$label, 65L), "</b><br>",
    obscop_chart_text(a_label), " : ", obscop_chart_number(d$a), suffix, "<br>",
    obscop_chart_text(b_label), " : ", obscop_chart_number(d$b), suffix,
    "<br>Écart A − B : ", delta_text)
  p <- plotly::plot_ly(type = "scatter", mode = "lines",
    x = as.vector(rbind(d$a, d$b, NA_real_)),
    y = as.vector(rbind(rows, rows, NA_real_)),
    line = list(color = "#D6E0D9", width = 4), hoverinfo = "skip", showlegend = FALSE)
  p <- plotly::add_trace(p, x = d$a, y = rows, type = "scatter", mode = "markers",
    marker = list(color = "#128876", size = 12, line = list(color = "#FFFFFF", width = 1.5)),
    name = a_label, text = tooltip, hoverinfo = "text", showlegend = TRUE, inherit = FALSE)
  p <- plotly::add_trace(p, x = d$b, y = rows, type = "scatter", mode = "markers",
    marker = list(color = "#D99B46", size = 12, line = list(color = "#FFFFFF", width = 1.5)),
    name = b_label, text = tooltip, hoverinfo = "text", showlegend = TRUE, inherit = FALSE)
  p <- obscop_chart_finish(p, "obscop-comparaison")
  p <- plotly::layout(p,
    xaxis = list(title = "", ticksuffix = suffix,
      range = if (identical(unit, "%")) c(0, 100) else c(0, max(c(d$a, d$b)) * 1.05)),
    yaxis = list(title = "", tickmode = "array", tickvals = rows,
      ticktext = obscop_chart_wrap(d$label, 43L), showgrid = FALSE,
      range = c(0.4, nrow(d) + 0.6), fixedrange = TRUE),
    margin = list(r = 95, t = 46))
  obscop_chart_source(p, annotations = lapply(rows, function(i) list(x = 1.025, xref = "paper", y = i,
      text = delta_text[i], xanchor = "left", showarrow = FALSE,
      font = list(size = 12, color = "#5E6D66"))))
}

obscop_scatter <- function(d, x_label, y_label, show_labels = TRUE) {
  d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  if (!nrow(d)) return(obscop_chart_empty("Aucun pays ne dispose des deux indicateurs sélectionnés."))
  short_label <- function(x) {
    if (nchar(x) > 100L) x <- paste0(substr(x, 1L, 97L), "…")
    obscop_chart_wrap(x, 50L)
  }
  x_unit <- if ("x_unit" %in% names(d)) d$x_unit[1] else "%"
  y_unit <- if ("y_unit" %in% names(d)) d$y_unit[1] else "%"
  x_suffix <- if (identical(x_unit, "%")) " %" else ""
  y_suffix <- if (identical(y_unit, "%")) " %" else ""
  full_x <- if ("x_label" %in% names(d)) d$x_label else x_label
  full_y <- if ("y_label" %in% names(d)) d$y_label else y_label
  tips <- paste0("<b>", obscop_chart_text(d$group_label), "</b><br>",
    "X · ", obscop_chart_wrap(full_x, 65L), " : ", obscop_chart_number(d$x), x_suffix,
    "<br>Y · ", obscop_chart_wrap(full_y, 65L), " : ", obscop_chart_number(d$y), y_suffix)
  if ("n" %in% names(d)) tips <- paste0(tips, ifelse(is.finite(d$n),
    paste0("<br>Échantillon X : ", obscop_chart_number(d$n, 0L), " personnes"), ""))
  if ("n_y" %in% names(d)) tips <- paste0(tips, ifelse(is.finite(d$n_y),
    paste0("<br>Échantillon Y : ", obscop_chart_number(d$n_y, 0L), " personnes"), ""))
  if ("base_x" %in% names(d)) tips <- paste0(tips, "<br>Base X : ", obscop_chart_wrap(d$base_x, 65L))
  if ("base_y" %in% names(d)) tips <- paste0(tips, "<br>Base Y : ", obscop_chart_wrap(d$base_y, 65L))
  args <- list(x = d$x, y = d$y, type = "scatter",
    mode = if (isTRUE(show_labels)) "markers+text" else "markers",
    hovertext = tips, hoverinfo = "text", customdata = d$group_label,
    marker = list(size = 12, opacity = 0.85, line = list(color = "#FFFFFF", width = 1.5)),
    showlegend = FALSE, source = "obscop-scatter")
  if (isTRUE(show_labels)) {
    args$text <- obscop_chart_text(d$group_label)
    args$textposition <- "top center"
    args$textfont <- list(size = 10, color = "#526A62")
  }
  if ("region" %in% names(d) && length(unique(d$region)) > 1L) {
    args$color <- d$region
    args$colors <- c("#148773", "#D49A49", "#6B91BC", "#926E9E", "#8BA865", "#C87E6D")
    args$showlegend <- TRUE
  } else {
    args$marker$color <- "#158A78"
  }
  p <- do.call(plotly::plot_ly, args)
  p <- obscop_chart_finish(p, "obscop-croisement")
  p <- plotly::layout(p,
    xaxis = list(title = list(text = short_label(x_label), standoff = 20),
      ticksuffix = x_suffix, range = if (identical(x_unit, "%")) c(0, 103) else NULL),
    yaxis = list(title = list(text = short_label(y_label), standoff = 15),
      ticksuffix = y_suffix, range = if (identical(y_unit, "%")) c(0, 106) else NULL),
    margin = list(t = 45, b = 110, l = 95, r = 55), hovermode = "closest")
  obscop_chart_source(p, bottom = 140L)
}

# Ordinary Plotly polygons avoid the online TopoJSON dependency of geographic traces.
# Natural Earth's public-domain geometry is bundled; Equal Earth keeps areas legible.
obscop_map_geometry <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    installed <- app_sys("app", "www", "world-map.json")
    path <- c(installed, file.path("inst", "app", "www", "world-map.json"))
    path <- path[nzchar(path) & file.exists(path)][1]
    if (is.na(path)) return(NULL)
    world <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    project <- function(ring) {
      coords <- do.call(rbind, lapply(ring, unlist))
      lambda <- coords[, 1] * pi / 180
      theta <- asin(sqrt(3) / 2 * sin(coords[, 2] * pi / 180))
      t2 <- theta * theta
      denom <- 1.340264 + 3 * (-0.081106) * t2 + t2^3 * (7 * 0.000893 + 9 * 0.003796 * t2)
      cbind(x = 2 * sqrt(3) * lambda * cos(theta) / (3 * denom),
            y = theta * (1.340264 - 0.081106 * t2 + t2^3 * (0.000893 + 0.003796 * t2)))
    }
    cached <<- lapply(world$features, function(feature) {
      geom <- feature$geometry
      polygons <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
      # Ring orientation from the GeoJSON preserves holes when SVG paths are filled.
      xy <- do.call(rbind, lapply(polygons, function(polygon) {
        do.call(rbind, lapply(polygon, function(ring) rbind(project(ring), c(NA_real_, NA_real_))))
      }))
      list(iso3 = feature$properties$iso3, label = feature$properties$name, x = xy[, 1], y = xy[, 2])
    })
    cached <<- Filter(function(country) country$iso3 != "ATA", cached)
    # Natural Earth 110m tiny-countries point (SGP); its polygon is absent at this scale.
    singapore <- project(list(c(103.81482, 1.359364)))
    cached <<- c(cached, list(list(iso3 = "SGP", label = "Singapour",
      x = singapore[, 1], y = singapore[, 2], point = TRUE)))
    cached
  }
})

obscop_map <- function(d) {
  d <- d[is.finite(d$value) & !is.na(d$iso3) & nzchar(d$iso3), , drop = FALSE]
  if (!nrow(d)) return(obscop_chart_empty("Aucun pays disponible pour cette sélection."))
  if (anyDuplicated(d$iso3)) stop("Une seule valeur est attendue par pays pour la carte.")
  world <- obscop_map_geometry()
  if (is.null(world)) return(obscop_chart_empty("Le fond cartographique local est indisponible."))
  pct <- identical(obscop_chart_unit(d), "%")
  upper <- if (pct) 100 else max(d$value)
  palette <- grDevices::colorRampPalette(c("#DFF0E7", "#A6D5C1", "#46A58A", "#087B67", "#034B41"))(101)
  color_index <- pmax(1L, pmin(101L, as.integer(round(d$value / max(upper, 1) * 100)) + 1L))
  tips <- obscop_chart_tooltip(d)
  p <- plotly::plot_ly(source = "obscop-map")
  # One neutral trace keeps the full world lightweight; only surveyed countries receive tooltips.
  p <- plotly::add_trace(p, type = "scatter", mode = "lines",
    x = unlist(lapply(world, `[[`, "x")), y = unlist(lapply(world, `[[`, "y")),
    fill = "toself", fillcolor = "#EDF1EC", line = list(color = "#FFFFFF", width = 0.65),
    hoverinfo = "skip", showlegend = FALSE)
  for (i in seq_len(nrow(d))) {
    country <- Filter(function(x) identical(x$iso3, d$iso3[i]), world)
    if (!length(country)) next
    shape <- country[[1]]
    if (isTRUE(shape$point)) {
      p <- plotly::add_trace(p, x = shape$x, y = shape$y, type = "scatter", mode = "markers",
        marker = list(color = palette[color_index[i]], size = 11,
          line = list(color = "#FFFFFF", width = 1.5)),
        text = paste0(tips[i], "<br>Repère ponctuel : territoire trop petit à cette échelle."),
        customdata = d$iso3[i], hoverinfo = "text", showlegend = FALSE,
        name = d$group_label[i], inherit = FALSE)
      next
    }
    p <- plotly::add_trace(p, x = shape$x, y = shape$y, type = "scatter", mode = "lines",
      fill = "toself", fillcolor = palette[color_index[i]],
      line = list(color = "#FFFFFF", width = 0.8), text = tips[i],
      customdata = rep(d$iso3[i], length(shape$x)), hoverinfo = "text", hoveron = "fills",
      showlegend = FALSE, name = d$group_label[i], inherit = FALSE)
  }
  p <- plotly::add_trace(p, type = "scatter", mode = "markers", x = c(0, 0),
    y = c(0, 0), hoverinfo = "skip", showlegend = FALSE, inherit = FALSE,
    marker = list(color = c(0, upper), cmin = 0, cmax = max(upper, 1), size = 0,
      colorscale = list(c(0, "#DFF0E7"), c(0.25, "#A6D5C1"), c(0.5, "#46A58A"),
                        c(0.75, "#087B67"), c(1, "#034B41")), showscale = TRUE,
      colorbar = list(title = if (pct) "%" else "Valeur", thickness = 12,
        len = 0.68, x = 1.01, outlinewidth = 0, ticksuffix = if (pct) " %" else "")))
  p <- obscop_chart_finish(p, "obscop-carte")
  p <- plotly::layout(p,
    xaxis = list(visible = FALSE, showgrid = FALSE, range = c(-2.78, 2.78), constrain = "domain"),
    yaxis = list(visible = FALSE, showgrid = FALSE, range = c(-1.12, 1.34), scaleanchor = "x", scaleratio = 1),
    margin = list(l = 0, r = 55, t = 0, b = 12), dragmode = "pan", hovermode = "closest")
  obscop_chart_source(p, bottom = 38L)
}
