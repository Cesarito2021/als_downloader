source_unit_choices <- function() c("Automatic from file metadata" = "auto", "Metres (confirmed by user)" = "m",
  "International feet (confirmed by user)" = "ft", "US survey feet (confirmed by user)" = "us-ft")

# Extract a balanced WKT2 block, without mistaking bracket characters in names.
wkt_block <- function(wkt, token) {
  start <- regexpr(paste0(token, "["), wkt, fixed = TRUE)[1]
  if (start < 0) return("")
  chars <- strsplit(substring(wkt, start), "", fixed = TRUE)[[1]]
  depth <- 0L; quoted <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == '"') quoted <- !quoted
    if (!quoted && ch == "[") depth <- depth + 1L
    if (!quoted && ch == "]") {
      depth <- depth - 1L
      if (depth == 0L) return(paste(chars[seq_len(i)], collapse = ""))
    }
  }
  ""
}

display_unit_info <- function(crs, xy_units = "auto", z_units = "auto") {
  choose <- function(x) match.arg(if (is.null(x)) "auto" else x, c("auto", "m", "ft", "us-ft"))
  xy_units <- choose(xy_units); z_units <- choose(z_units)
  factors <- c(m = 1, ft = 0.3048, `us-ft` = 1200 / 3937)
  names_to_code <- c(metre="m", meter="m", metres="m", meters="m", m="m", foot="ft", feet="ft",
    `international foot`="ft", `US survey foot`="us-ft", `US survey feet`="us-ft")
  xy <- z <- NA_character_
  geographic <- !is.na(crs) && isTRUE(sf::st_is_longlat(crs))
  if (!is.na(crs) && !geographic) {
    xy <- unname(names_to_code[crs$units_gdal])[1]
    vertical <- wkt_block(crs$wkt, "VERTCRS")
    # A horizontal CRS alone says nothing about the units of LAS Z.
    match <- regmatches(vertical, regexpr('LENGTHUNIT\\["[^"]+",[[:space:]]*[0-9.eE+-]+', vertical))
    if (length(match) && nzchar(match)) {
      factor <- suppressWarnings(as.numeric(sub('.*",[[:space:]]*', '', match)))
      hit <- which(abs(factors - factor) < 1e-12)
      if (length(hit) == 1L) z <- names(factors)[hit]
    }
  }
  if (xy_units != "auto") xy <- xy_units
  if (z_units != "auto") z <- z_units
  known <- !geographic && !is.na(xy) && !is.na(z)
  source <- function(code, choice) paste0(if (is.na(code)) "unknown" else code,
    if (choice != "auto") " (user confirmed)" else " (metadata)")
  note <- paste0("Source XY: ", source(xy, xy_units), "; source Z: ", source(z, z_units), ". ",
    if (known) "Display coordinates scaled to metres. No vertical-datum transformation or height normalization."
    else if (geographic) "Geographic coordinates: reproject externally before metric 3D viewing. Values remain unchanged."
    else "Units unresolved: source coordinates remain unchanged; metric distances and proportions are not verified. Confirm both units from provider documentation to display metres.")
  list(known = known, factors = if (known) unname(factors[c(xy, xy, z)]) else c(1,1,1),
    label = if (known) "m" else "source units (unverified)", note = note)
}

scale_display_points <- function(points, units) {
  p <- as.data.frame(points)
  p[c("X", "Y", "Z")] <- Map(function(x, factor) x * factor, p[c("X", "Y", "Z")], units$factors)
  p
}
