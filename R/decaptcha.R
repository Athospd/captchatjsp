#' @export
is_windows <- function() {
  stringr::str_detect( tolower(sessionInfo()$running), "windows")
}

#' @export
baixar <- function() {
  link <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
  tmp <- tempfile()
  f <- download.file(link, tmp, method = 'wget', quiet = TRUE,
                     extra = '--no-check-certificate')
  if(f == 0) {
    return(tmp)
  }
  stop('erro no download')
}

#' @export
ler <- function(a) {
  img <- png::readPNG(a)
  img_dim <- dim(img)
  img_df <- data.frame(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')
  return(d)
}

#' Processa uma img e transforma no texto
#'
#' @export
limpar <- function(d) {
  d <- dplyr::group_by(d, x)
  d <- dplyr::mutate(d, n = length(y))
  d <- dplyr::ungroup(d)
  d <- dplyr::filter(d, y > 20, y < 38)

  d <- dplyr::group_by(d, cor)
  d <- dplyr::mutate(d, n = length(cor))
  d <- dplyr::ungroup(d)
  d <- dplyr::filter(d, n == max(n))

  d <- dplyr::group_by(d, x)
  d <- dplyr::mutate(d, n = length(y))
  d <- dplyr::ungroup(d)
  d <- dplyr::filter(d, n > 1)

  d
}

#' @export
validar <- function(d) {
  diff(range(d$y)) < 20
}

#' @export
picotar <- function(d) {
  somas <- dplyr::summarise(dplyr::group_by(d, x), soma = length(y))
  somas <- dplyr::arrange(somas, x)
  x <- somas$x
  grupos <- integer(length(x))
  grupos[1] <- 1
  k <- 1
  for(i in 2:length(x)) {
    if(!x[i - 1] %in% (x[i] - 1:3)) {
      k <- k + 1
    }
    grupos[i] <- k
  }
  somas$grupo <- grupos
  d2 <- dplyr::inner_join(d, somas, 'x')
  d2
}

#' @export
desenhar <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))

  p <- p + ggplot2::coord_fixed()
  p <- p + ggplot2::theme_bw()
  if(!is.null(d$grupo)) {
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::facet_wrap(~grupo, scales = 'free_y', ncol = 5)
  } else {
    p <- p + ggplot2::geom_point(colour = d$cor)
  }
  p
}

#' @export
classificar <- function(d, letras, path) {
  r <- strsplit(letras, '')[[1]]
  for(i in 1:5) {
    aux <- d[d$grupo == i, ]
    nm <- sprintf('%s/%s_%07d.rds', path, r[i],
                  as.integer(round(runif(1, 1, 1e8-1))))
    saveRDS(aux, nm)
  }
}

#' @export
carregar_treino <- function(path) {
  treino <- dplyr::bind_rows(
    lapply(list.files(path, full.names = TRUE), function(x) {
      d <- readRDS(x)
      if(nrow(d) > 0) {
        d$arq <- x
        return(d)
      }
    })
  )
  if(is_windows()) { # se o sistema operacional for windows...
    letra_regex <- '.+\\/([a-z])'
  } else {
    letra_regex <- '.+//([a-z])'
  }
  treino$letra <- stringr::str_match(treino$arq, letra_regex)[, 2]
  d_treino <- treino %>%
    group_by(arq) %>%
    mutate(x = x - min(x), y = y - min(y)) %>%
    ungroup %>%
    mutate(xs = sprintf('x%02d', x), ys = sprintf('y%02d', y)) %>%
    unite(xy, xs, ys, sep = '_') %>%
    select(arq, xy, letra) %>%
    spread(xy, letra, fill = 0) %>%
    mutate_each(funs(ifelse(.=='0', 0, 1)), starts_with('x')) %>%
    mutate(letra = stringr::str_match(arq, letra_regex)[, 2])

  d_treino
}

#' @export
carregar_teste <- function(d) {
  d <- picotar(limpar(d))
  d_teste <- d %>%
    group_by(grupo) %>%
    mutate(x = x - min(x), y = y - min(y), um = 1) %>%
    ungroup %>%
    mutate(xs = sprintf('x%02d', x), ys = sprintf('y%02d', y)) %>%
    unite(xy, xs, ys, sep = '_') %>%
    select(grupo, xy, um) %>%
    spread(xy, um, fill = 0) %>%
    select(-grupo)
  d_teste
}

#' @export
prever <- function(d) {
  data(d_treino)
  data(modelo)
  teste <- carregar_teste(d)
  nm <- names(select(d_treino, -arq, -letra))
  teste[, nm[!nm %in% names(teste)]] <- 0
  predicao <- predict(modelo, teste)
  r <- paste0(apply(predicao, 1, function(x) {
    names(x[which(x == max(x))[1]])
  }), collapse = '')
  r
}

#' @export
desenhar_treino <- function(d, l) {
  d %>%
    filter(letra == l) %>%
    gather(key, val, starts_with('x')) %>%
    filter(val == 1) %>%
    separate(key, c('x', 'y'), sep = '_') %>%
    mutate(x = as.numeric(gsub('[^0-9]', '', x)),
           y = as.numeric(gsub('[^0-9]', '', y))) %>%
    mutate(grupo = arq) %>%
    desenhar
}
