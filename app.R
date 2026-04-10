# app.R
# ============================================================
# Eclips'app responsive
# - Càrrega EXIF automàtica
# - Compressió de fotos grans
# - Navegació mòbil amb barra inferior
# - Imatge inicial separada del resultat final
# - Resultat visual persistent i estable a "Sol"
# ============================================================

library(shiny)
library(magick)
library(png)
library(jpeg)
library(exifr)
library(shinyjs)
library(shinycssloaders)

options(shiny.maxRequestSize = 30 * 1024^2)

# ============================================================
# FUNCIONS BÀSIQUES
# ============================================================

deg2rad <- function(x) x * pi / 180
rad2deg <- function(x) x * 180 / pi

wrap360 <- function(x) {
  x <- x %% 360
  ifelse(x < 0, x + 360, x)
}

wrap180 <- function(x) {
  y <- ((x + 180) %% 360) - 180
  ifelse(y == -180, 180, y)
}

rotate_point <- function(x, y, cx, cy, angle_deg) {
  a <- deg2rad(angle_deg)
  xr <- cos(a) * (x - cx) - sin(a) * (y - cy) + cx
  yr <- sin(a) * (x - cx) + cos(a) * (y - cy) + cy
  list(x = xr, y = yr)
}

read_image_dims <- function(path) {
  info <- magick::image_info(magick::image_read(path))
  list(width = info$width[1], height = info$height[1])
}

prepare_uploaded_image <- function(path, max_dim = 1800, quality = 85) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  
  img <- magick::image_read(path)
  info <- magick::image_info(img)
  
  w <- info$width[1]
  h <- info$height[1]
  
  if (max(w, h) > max_dim) {
    if (w >= h) {
      img <- magick::image_scale(img, paste0(max_dim))
    } else {
      img <- magick::image_scale(img, paste0("x", max_dim))
    }
  }
  
  out_path <- tempfile(fileext = ".jpg")
  
  magick::image_write(
    image = img,
    path = out_path,
    format = "jpeg",
    quality = quality
  )
  
  out_path
}

# ============================================================
# EXIF
# ============================================================

safe_first_nonempty <- function(x) {
  if (is.null(x) || !length(x)) return(NA)
  x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
  if (!length(x)) return(NA)
  x[1]
}

parse_exif_coord <- function(value, ref = NA) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA_real_)
  
  v <- value[1]
  
  if (is.numeric(v)) {
    out <- as.numeric(v)
    if (!is.na(ref)) {
      ref <- toupper(trimws(as.character(ref[1])))
      if (ref %in% c("S", "W")) out <- -abs(out)
      if (ref %in% c("N", "E")) out <- abs(out)
    }
    return(out)
  }
  
  s <- trimws(as.character(v))
  if (!nzchar(s)) return(NA_real_)
  
  num_direct <- suppressWarnings(as.numeric(s))
  if (!is.na(num_direct)) {
    out <- num_direct
    if (!is.na(ref)) {
      ref <- toupper(trimws(as.character(ref[1])))
      if (ref %in% c("S", "W")) out <- -abs(out)
      if (ref %in% c("N", "E")) out <- abs(out)
    }
    return(out)
  }
  
  nums <- suppressWarnings(as.numeric(unlist(regmatches(
    s,
    gregexpr("[0-9]+\\.?[0-9]*", s)
  ))))
  
  if (length(nums) >= 1) {
    deg <- nums[1]
    minv <- if (length(nums) >= 2) nums[2] else 0
    sec <- if (length(nums) >= 3) nums[3] else 0
    
    out <- deg + minv / 60 + sec / 3600
    
    ref_all <- paste0(
      toupper(s),
      " ",
      ifelse(is.na(ref), "", toupper(trimws(as.character(ref[1]))))
    )
    
    if (grepl("\\bS\\b|\\bW\\b", ref_all)) out <- -abs(out)
    if (grepl("\\bN\\b|\\bE\\b", ref_all)) out <- abs(out)
    
    return(out)
  }
  
  NA_real_
}

parse_exif_altitude <- function(value, ref = NA) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA_real_)
  
  v <- value[1]
  
  if (is.numeric(v)) {
    alt <- as.numeric(v)
  } else {
    s <- trimws(as.character(v))
    nums <- suppressWarnings(as.numeric(unlist(regmatches(
      s,
      gregexpr("-?[0-9]+\\.?[0-9]*", s)
    ))))
    alt <- if (length(nums)) nums[1] else NA_real_
  }
  
  if (is.na(alt)) return(NA_real_)
  
  if (!is.na(ref)) {
    r <- trimws(as.character(ref[1]))
    if (r %in% c("1", "Below Sea Level")) alt <- -abs(alt)
  }
  
  alt
}

parse_exif_datetime <- function(value) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA)
  s <- trimws(as.character(value[1]))
  if (!nzchar(s)) return(NA)
  sub("^([0-9]{4}):([0-9]{2}):([0-9]{2}) ", "\\1-\\2-\\3 ", s)
}

read_photo_exif_summary <- function(path) {
  out <- list(
    ok = FALSE,
    lat = NA_real_,
    lon = NA_real_,
    elev = NA_real_,
    datetime = NA_character_,
    device = NA_character_,
    focal = NA_character_,
    raw = NULL,
    message = "No EXIF available."
  )
  
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    out$message <- "Image file not found."
    return(out)
  }
  
  exif_df <- tryCatch(
    exifr::read_exif(path),
    error = function(e) NULL
  )
  
  if (is.null(exif_df) || !is.data.frame(exif_df) || nrow(exif_df) == 0) {
    out$message <- "No readable EXIF metadata found."
    return(out)
  }
  
  row <- exif_df[1, , drop = FALSE]
  nm <- names(row)
  
  get_col <- function(candidates) {
    hit <- intersect(candidates, nm)
    if (!length(hit)) return(NA)
    row[[hit[1]]]
  }
  
  lat_val <- get_col(c("GPSLatitude", "Composite:GPSLatitude", "EXIF:GPSLatitude"))
  lat_ref <- get_col(c("GPSLatitudeRef", "Composite:GPSLatitudeRef", "EXIF:GPSLatitudeRef"))
  
  lon_val <- get_col(c("GPSLongitude", "Composite:GPSLongitude", "EXIF:GPSLongitude"))
  lon_ref <- get_col(c("GPSLongitudeRef", "Composite:GPSLongitudeRef", "EXIF:GPSLongitudeRef"))
  
  alt_val <- get_col(c("GPSAltitude", "Composite:GPSAltitude", "EXIF:GPSAltitude"))
  alt_ref <- get_col(c("GPSAltitudeRef", "Composite:GPSAltitudeRef", "EXIF:GPSAltitudeRef"))
  
  dt_val <- get_col(c(
    "DateTimeOriginal",
    "EXIF:DateTimeOriginal",
    "CreateDate",
    "EXIF:CreateDate",
    "DateTimeDigitized",
    "EXIF:DateTimeDigitized"
  ))
  
  make_val <- safe_first_nonempty(get_col(c("Make", "EXIF:Make")))
  model_val <- safe_first_nonempty(get_col(c("Model", "EXIF:Model")))
  focal_val <- safe_first_nonempty(get_col(c("FocalLength", "EXIF:FocalLength")))
  
  lat_num <- parse_exif_coord(lat_val, lat_ref)
  lon_num <- parse_exif_coord(lon_val, lon_ref)
  alt_num <- parse_exif_altitude(alt_val, alt_ref)
  dt_txt <- parse_exif_datetime(dt_val)
  
  device_txt <- paste(na.omit(c(make_val, model_val)), collapse = " ")
  if (!nzchar(device_txt)) device_txt <- NA_character_
  
  msg_parts <- c()
  if (!is.na(lat_num) && !is.na(lon_num)) msg_parts <- c(msg_parts, sprintf("GPS found: %.6f, %.6f", lat_num, lon_num))
  if (!is.na(alt_num)) msg_parts <- c(msg_parts, sprintf("Altitude found: %.1f m", alt_num))
  if (!is.na(dt_txt)) msg_parts <- c(msg_parts, paste("Date/time found:", dt_txt))
  if (!is.na(device_txt)) msg_parts <- c(msg_parts, paste("Device:", device_txt))
  
  out$ok <- length(msg_parts) > 0
  out$lat <- lat_num
  out$lon <- lon_num
  out$elev <- alt_num
  out$datetime <- dt_txt
  out$device <- device_txt
  out$focal <- focal_val
  out$raw <- exif_df
  out$message <- if (length(msg_parts)) paste(msg_parts, collapse = " | ") else "No useful EXIF fields found."
  
  out
}

# ============================================================
# CÀLCUL ASTRONÒMIC
# ============================================================

julian_day <- function(time_utc) {
  y  <- as.integer(format(time_utc, "%Y", tz = "UTC"))
  m  <- as.integer(format(time_utc, "%m", tz = "UTC"))
  d  <- as.integer(format(time_utc, "%d", tz = "UTC"))
  hh <- as.numeric(format(time_utc, "%H", tz = "UTC"))
  mm <- as.numeric(format(time_utc, "%M", tz = "UTC"))
  ss <- as.numeric(format(time_utc, "%S", tz = "UTC"))
  
  frac_day <- (hh + mm / 60 + ss / 3600) / 24
  
  idx <- m <= 2
  y[idx] <- y[idx] - 1
  m[idx] <- m[idx] + 12
  
  A <- floor(y / 100)
  B <- 2 - A + floor(A / 4)
  
  floor(365.25 * (y + 4716)) +
    floor(30.6001 * (m + 1)) +
    d + frac_day + B - 1524.5
}

solar_position <- function(datetime_local, tz_string, lat_deg, lon_deg) {
  time_local <- as.POSIXct(datetime_local, tz = tz_string)
  time_utc   <- as.POSIXct(format(time_local, tz = "UTC", usetz = TRUE), tz = "UTC")
  
  JD <- julian_day(time_utc)
  T  <- (JD - 2451545.0) / 36525
  
  L0 <- wrap360(280.46646 + T * (36000.76983 + T * 0.0003032))
  M  <- wrap360(357.52911 + T * (35999.05029 - 0.0001537 * T))
  
  C <- sin(deg2rad(M)) * (1.914602 - T * (0.004817 + 0.000014 * T)) +
    sin(deg2rad(2 * M)) * (0.019993 - 0.000101 * T) +
    sin(deg2rad(3 * M)) * 0.000289
  
  true_long <- L0 + C
  
  omega  <- 125.04 - 1934.136 * T
  lambda <- true_long - 0.00569 - 0.00478 * sin(deg2rad(omega))
  
  epsilon0 <- 23 +
    (26 + ((21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60)) / 60
  epsilon <- epsilon0 + 0.00256 * cos(deg2rad(omega))
  
  alpha <- rad2deg(atan2(
    cos(deg2rad(epsilon)) * sin(deg2rad(lambda)),
    cos(deg2rad(lambda))
  ))
  alpha <- wrap360(alpha)
  
  delta <- rad2deg(asin(
    sin(deg2rad(epsilon)) * sin(deg2rad(lambda))
  ))
  
  GMST <- wrap360(
    280.46061837 +
      360.98564736629 * (JD - 2451545.0) +
      0.000387933 * T^2 -
      T^3 / 38710000
  )
  
  LST <- wrap360(GMST + lon_deg)
  H   <- wrap180(LST - alpha)
  
  lat_rad <- deg2rad(lat_deg)
  dec_rad <- deg2rad(delta)
  H_rad   <- deg2rad(H)
  
  alt_rad <- asin(
    sin(lat_rad) * sin(dec_rad) +
      cos(lat_rad) * cos(dec_rad) * cos(H_rad)
  )
  alt_deg <- rad2deg(alt_rad)
  
  az_rad <- atan2(
    sin(H_rad),
    cos(H_rad) * sin(lat_rad) - tan(dec_rad) * cos(lat_rad)
  )
  
  az_deg <- wrap360(rad2deg(az_rad) + 180)
  
  list(
    azimuth = az_deg,
    altitude = alt_deg,
    datetime_local = time_local,
    datetime_utc = time_utc
  )
}

# ============================================================
# MODE DEMO
# ============================================================

create_demo_image <- function(path, w = 1400, h = 900) {
  grDevices::png(filename = path, width = w, height = h, bg = "white")
  op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  
  plot.new()
  plot.window(xlim = c(0, w), ylim = c(h, 0))
  
  horizon_y <- h * 0.58
  
  rect(0, 0, w, h, col = "#88BDE6", border = NA)
  rect(0, horizon_y, w, h, col = "#B7A17A", border = NA)
  
  polygon(c(0, 180, 360), c(horizon_y, horizon_y - 180, horizon_y), col = "#6D7B5D", border = NA)
  polygon(c(250, 520, 760), c(horizon_y, horizon_y - 260, horizon_y), col = "#5F6B4F", border = NA)
  polygon(c(650, 980, 1280), c(horizon_y, horizon_y - 210, horizon_y), col = "#6D7B5D", border = NA)
  polygon(c(1120, 1260, w), c(horizon_y, horizon_y - 120, horizon_y), col = "#758463", border = NA)
  
  text(x = 25, y = 40, labels = "DEMO IMAGE", adj = c(0, 0), col = "white", cex = 1.6, font = 2)
  text(x = 25, y = 78, labels = "Use this image to test horizon clicks and sun projection", adj = c(0, 0), col = "white", cex = 1)
  
  invisible(path)
}

# ============================================================
# PROJECCIÓ
# ============================================================

sun_to_image_xy <- function(
    sun_az, sun_alt,
    photo_center_az,
    photo_pitch = 0,
    hfov = 60,
    vfov = 40,
    img_width,
    img_height,
    roll_deg = 0
) {
  dx_deg <- wrap180(sun_az - photo_center_az)
  dy_deg <- sun_alt - photo_pitch
  
  x0 <- (dx_deg / hfov + 0.5) * img_width
  y0 <- (0.5 - dy_deg / vfov) * img_height
  
  rot <- rotate_point(
    x = x0,
    y = y0,
    cx = img_width / 2,
    cy = img_height / 2,
    angle_deg = roll_deg
  )
  
  inside <- (
    rot$x >= 0 && rot$x <= img_width &&
      rot$y >= 0 && rot$y <= img_height
  )
  
  list(
    x_raw = x0,
    y_raw = y0,
    x = rot$x,
    y = rot$y,
    inside = inside,
    dx_deg = dx_deg,
    dy_deg = dy_deg
  )
}

# ============================================================
# DIBUIX D'OVERLAY
# ============================================================

draw_overlay_image <- function(
    img_path,
    out_path,
    sun_pt = NULL,
    sun_label = NULL,
    horizon_pts = NULL,
    horizon_center_pt = NULL,
    click_pt = NULL,
    manual_pt = NULL,
    show_grid = FALSE,
    show_center_cross = TRUE
) {
  dims <- read_image_dims(img_path)
  w <- dims$width
  h <- dims$height
  
  img <- magick::image_read(img_path)
  img_arr <- as.raster(img)
  
  grDevices::png(filename = out_path, width = w, height = h, bg = "white")
  op <- par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  
  plot.new()
  plot.window(xlim = c(0, w), ylim = c(h, 0))
  rasterImage(img_arr, 0, h, w, 0)
  
  if (isTRUE(show_grid)) {
    xs <- seq(0, w, length.out = 11)
    ys <- seq(0, h, length.out = 11)
    abline(v = xs, col = "yellow", lty = 2, lwd = 1.5)
    abline(h = ys, col = "yellow", lty = 2, lwd = 1.5)
  }
  
  if (isTRUE(show_center_cross)) {
    segments(w / 2 - 25, h / 2, w / 2 + 25, h / 2, col = "#FFFFFFAA", lwd = 2)
    segments(w / 2, h / 2 - 25, w / 2, h / 2 + 25, col = "#FFFFFFAA", lwd = 2)
    text(w / 2 + 30, h / 2 - 10, labels = "Centre", col = "white", pos = 4)
  }
  
  if (!is.null(horizon_pts) && nrow(horizon_pts) == 2) {
    segments(
      horizon_pts$x[1], horizon_pts$y[1],
      horizon_pts$x[2], horizon_pts$y[2],
      col = "#00FFFF", lwd = 3
    )
    points(horizon_pts$x, horizon_pts$y, pch = 21, bg = "#00FFFF", col = "black", cex = 1.5)
    text(mean(horizon_pts$x), mean(horizon_pts$y) - 15,
         labels = "Horizon line", col = "#00FFFF", font = 2)
  }
  
  if (!is.null(horizon_center_pt)) {
    points(horizon_center_pt$x, horizon_center_pt$y, pch = 21, bg = "#00FF66", col = "black", cex = 1.5)
    text(horizon_center_pt$x + 10, horizon_center_pt$y - 10,
         labels = "Central horizon ref",
         col = "#00FF66", pos = 4, font = 2)
  }
  
  if (!is.null(click_pt)) {
    points(click_pt$x, click_pt$y, pch = 4, col = "white", cex = 1.7, lwd = 2)
    text(click_pt$x + 10, click_pt$y - 10,
         labels = sprintf("Click (%.0f, %.0f)", click_pt$x, click_pt$y),
         col = "white", pos = 4)
  }
  
  if (!is.null(manual_pt)) {
    points(manual_pt$x, manual_pt$y, pch = 21, bg = "#FF66CC", col = "black", cex = 1.5)
    text(manual_pt$x + 10, manual_pt$y - 10,
         labels = "Manual marker",
         col = "#FF66CC", pos = 4, font = 2)
  }
  
  if (!is.null(sun_pt)) {
    symbols(
      x = sun_pt$x, y = sun_pt$y,
      circles = 14,
      inches = FALSE, add = TRUE,
      bg = "#FFD700AA", fg = "black", lwd = 2.5
    )
    if (!is.null(sun_label) && nzchar(sun_label)) {
      text(sun_pt$x + 14, sun_pt$y - 14,
           labels = sun_label, col = "yellow", pos = 4, font = 2, cex = 1.8)
    }
  }
  
  invisible(out_path)
}

field_box <- function(id, state = c("ok", "missing", "exif"), label, input_tag) {
  state <- match.arg(state)
  
  cls <- switch(
    state,
    ok = "field-ok",
    missing = "field-missing",
    exif = "field-exif"
  )
  
  div(
    id = paste0(id, "_box"),
    class = cls,
    div(class = "field-label", label),
    input_tag
  )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      html, body {
        max-width: 100%;
        overflow-x: hidden;
      }

      .container-fluid {
        max-width: 100%;
        overflow-x: hidden;
      }

      .app-shell {
        display: flex;
        gap: 16px;
        align-items: stretch;
      }

      .left-panel {
        flex: 0 0 24%;
      }

      .center-panel {
        flex: 1 1 auto;
        min-width: 0;
      }

      .right-panel {
        flex: 0 0 22%;
      }

      .photo-card,
      .section-card,
      .result-card {
        background: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 14px;
        margin-bottom: 16px;
        box-sizing: border-box;
      }

      .bottom-grid {
        display: flex;
        gap: 16px;
      }

      .bottom-grid .section-card {
        flex: 1 1 0;
        margin-bottom: 0;
      }

      .section-title {
        margin-top: 0;
        margin-bottom: 12px;
        font-weight: 700;
      }

      .tight-btn .btn {
        width: 100%;
        margin-bottom: 8px;
      }

      .download-btn .btn {
        width: 100%;
      }

      .result-card pre,
      .section-card pre {
        white-space: pre-wrap;
        word-break: break-word;
      }

      .field-ok .form-control,
      .field-ok .selectize-input,
      .field-ok .shiny-date-input .form-control {
        background-color: #e8f5e9 !important;
        border-color: #81c784 !important;
      }

      .field-missing .form-control,
      .field-missing .selectize-input,
      .field-missing .shiny-date-input .form-control {
        background-color: #fff3e0 !important;
        border-color: #ff9800 !important;
        box-shadow: 0 0 0 2px rgba(255, 152, 0, 0.15) !important;
      }

      .field-exif .form-control,
      .field-exif .selectize-input,
      .field-exif .shiny-date-input .form-control {
        background-color: #e3f2fd !important;
        border-color: #64b5f6 !important;
      }

      .field-label {
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 4px;
      }

      .field-box-spacer {
        margin-bottom: 10px;
      }

      .help-text-small {
        font-size: 12px;
        color: #666;
        margin-top: 6px;
        line-height: 1.35;
      }

      .section-card .row {
        margin-left: -6px;
        margin-right: -6px;
      }

      .section-card .col-sm-4,
      .section-card .col-md-4,
      .section-card .col-lg-4 {
        padding-left: 6px;
        padding-right: 6px;
      }

      .mobile-bottom-nav {
        display: none;
      }

      .mobile-nav-inner {
        display: flex;
        gap: 4px;
        justify-content: space-between;
        align-items: stretch;
      }

      .mobile-nav-btn {
        flex: 1 1 0;
        background: white;
        border: 1px solid #d0d5dd;
        border-radius: 10px;
        padding: 6px 2px 5px 2px;
        font-size: 10px;
        font-weight: 700;
        color: #344054;
        line-height: 1.1;
        min-height: 46px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 3px;
      }

      .mobile-nav-btn i {
        font-size: 13px;
        display: block;
        line-height: 1;
      }

      .mobile-nav-btn.active {
        background: #1f78b4;
        border-color: #1f78b4;
        color: white;
      }

      .mobile-step-section {
        display: block;
      }

      .section-stack {
        width: 100%;
      }

      .mobile-photo-card {
        background: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 14px;
        box-sizing: border-box;
      }

      .mobile-photo-card .section-title {
        margin-bottom: 8px;
        font-size: 16px;
      }

      .desktop-photo-only {
        display: block;
      }

      .mobile-photo-only {
        display: none;
      }

      .shiny-image-output img {
        max-width: 100% !important;
        width: 100% !important;
        height: auto !important;
        display: block !important;
        border: 1px solid #ccc;
        border-radius: 6px;
      }

      #result_visual_wrap {
        width: 100%;
        overflow: hidden;
      }

      @media (max-width: 991px) {
        body {
          padding-bottom: 74px;
        }

        .container-fluid {
          padding-left: 8px;
          padding-right: 8px;
        }

        .app-shell {
          display: block;
        }

        .left-panel,
        .center-panel,
        .right-panel {
          flex: none;
          width: 100%;
        }

        .center-panel {
          margin-bottom: 8px;
        }

        .bottom-grid {
          display: block;
        }

        .bottom-grid .section-card {
          margin-bottom: 12px;
        }

        .photo-card,
        .section-card,
        .result-card,
        .mobile-photo-card {
          padding: 10px;
          margin-bottom: 12px;
          border-radius: 8px;
        }

        .section-title {
          font-size: 16px;
          margin-bottom: 8px;
        }

        .help-text-small {
          font-size: 11px;
        }

        .mobile-step-section {
          display: none !important;
        }

        .mobile-step-section.mobile-visible {
          display: block !important;
        }

        .mobile-bottom-nav {
          display: block;
          position: fixed;
          left: 0;
          right: 0;
          bottom: 0;
          z-index: 9999;
          background: rgba(255,255,255,0.98);
          border-top: 1px solid #d0d5dd;
          box-shadow: 0 -2px 8px rgba(0,0,0,0.08);
          padding: 6px 6px calc(6px + env(safe-area-inset-bottom));
        }

        .title-panel-mobile h2,
        .title-panel-mobile h1 {
          font-size: 22px;
          margin-top: 10px;
          margin-bottom: 10px;
        }

        .shiny-input-container {
          width: 100% !important;
          margin-bottom: 8px !important;
        }

        .form-control {
          font-size: 14px;
        }

        .split-layout {
          display: block !important;
          width: 100% !important;
        }

        .split-layout > div {
          display: block !important;
          width: 100% !important;
          margin-bottom: 8px;
        }

        .desktop-photo-only {
          display: none !important;
        }

        .mobile-photo-only {
          display: block !important;
        }
      }

      @media (min-width: 992px) {
        .mobile-bottom-nav {
          display: none !important;
        }

        .mobile-step-section {
          display: block !important;
        }

        .desktop-photo-only {
          display: block !important;
        }

        .mobile-photo-only {
          display: none !important;
        }
      }
    ")),
    
    tags$script(HTML("
      window.currentMobileStep = 'image';

      function updateMobileFlag() {
        var isMobile = window.innerWidth <= 991;
        if (window.Shiny) {
          Shiny.setInputValue('is_mobile', isMobile, {priority: 'event'});
        }
      }

      function setMobileStep(step) {
        window.currentMobileStep = step;
        var isMobile = window.innerWidth <= 991;

        var sections = document.querySelectorAll('.mobile-step-section');
        var buttons = document.querySelectorAll('.mobile-nav-btn');

        buttons.forEach(function(btn) {
          btn.classList.remove('active');
        });

        var activeBtn = document.getElementById('nav_' + step);
        if (activeBtn) {
          activeBtn.classList.add('active');
        }

        if (!isMobile) {
          sections.forEach(function(el) {
            el.classList.add('mobile-visible');
          });
          return;
        }

        sections.forEach(function(el) {
          el.classList.remove('mobile-visible');
          if (el.classList.contains('step-' + step)) {
            el.classList.add('mobile-visible');
          }
        });

        window.scrollTo({top: 0, behavior: 'smooth'});
      }

      document.addEventListener('DOMContentLoaded', function() {
        updateMobileFlag();
        setTimeout(function() {
          setMobileStep(window.currentMobileStep || 'image');
        }, 50);
      });

      window.addEventListener('resize', function() {
        updateMobileFlag();
        setMobileStep(window.currentMobileStep || 'image');
      });
    "))
  ),
  
  div(class = "title-panel-mobile", titlePanel("🌘 Eclips'app")),
  
  tags$div(
    style = paste(
      "background-color:#f8f9fa;",
      "padding:14px 18px;",
      "margin-bottom:15px;",
      "border:1px solid #ddd;",
      "border-radius:8px;"
    ),
    tags$p(
      "Aplicació interactiva per orientar l’observació de l’eclipsi solar del 12 d’agost de 2026 i ajudar a preparar millor la visualització i la fotografia des de cada ubicació.",
      style = "margin:0; font-size:15px; color:#444;"
    )
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "left-panel",
      
      div(
        id = "section_image",
        class = "section-card tight-btn mobile-step-section step-image",
        h4(class = "section-title", "1. Fotografia del lloc d'observació"),
        
        tags$div(
          class = "help-text-small",
          style = "margin-bottom:0px;",
          "Puja una fotografia del lloc on vols fer la observació, orientada a ponent ..."
        ),
        
        div(
          style = "margin-bottom:0px;",
          fileInput("photo", NULL, accept = c(".jpg", ".jpeg", ".png"))
        ),
        
        tags$div(
          class = "help-text-small",
          style = "margin-top:6px;",
          "En telèfon, si la foto és molt gran, pot fallar la càrrega. Va millor una imatge comprimida o reduïda."
        ),
        
        tags$div(
          class = "help-text-small",
          style = "margin-top:0; margin-bottom:0px;",
          "... o utilitza la imatge de prova. Quan es carrega, també s'omplen unes coordenades i un azimut de prova orientat a ponent."
        ),
        
        div(
          style = "margin-bottom:0px;",
          actionButton("use_demo_btn", "Fes servir imatge de prova")
        ),
        
        div(
          style = "margin-bottom:0;",
          checkboxInput("show_grid", "Mostrar graella", value = FALSE)
        ),
        
        div(
          class = "mobile-photo-card mobile-photo-only",
          h4(class = "section-title", "Visualització"),
          shinycssloaders::withSpinner(
            imageOutput("photo_img_mobile", click = "photo_click_mobile_image"),
            type = 4
          )
        )
      ),
      
      div(
        id = "section_coords_location",
        class = "section-card mobile-step-section step-coords",
        h4(class = "section-title", "2. Localització de la imatge"),
        tags$div(
          class = "help-text-small",
          "Les dades de localització s'obtenen de les metadates de la imatge, o manualment per l'usuari si no es disposa de metadates."
        ),
        
        splitLayout(
          cellWidths = c("33%", "33%", "34%"),
          
          div(
            id = "box_lat",
            class = "field-box-spacer",
            field_box(
              id = "lat",
              state = "missing",
              label = "Lat",
              input_tag = numericInput("lat", NULL, value = NA, step = 0.000001, width = "100%")
            )
          ),
          
          div(
            id = "box_lon",
            class = "field-box-spacer",
            field_box(
              id = "lon",
              state = "missing",
              label = "Lon",
              input_tag = numericInput("lon", NULL, value = NA, step = 0.000001, width = "100%")
            )
          ),
          
          div(
            id = "box_elev",
            class = "field-box-spacer",
            field_box(
              id = "elev",
              state = "missing",
              label = "Alt (m)",
              input_tag = numericInput("elev", NULL, value = NA, step = 1, width = "100%")
            )
          )
        ),
        
        tags$hr(),
        verbatimTextOutput("exif_info")
      ),
      
      div(
        id = "section_coords_geometry",
        class = "section-card mobile-step-section step-coords",
        h4(class = "section-title", "3. Geometria de la imatge"),
        
        div(
          id = "box_photo_az",
          class = "field-box-spacer",
          field_box(
            id = "photo_az",
            state = "missing",
            label = "Azimut central de la foto (°)",
            input_tag = numericInput("photo_az", NULL, value = 280, step = 0.1)
          ),
          tags$div(
            class = "help-text-small",
            "Valor d'azimut per aproximació: 280. Pots utilitzar la brúixola del telèfon per ajustar el valor."
          )
        ),
        
        div(
          id = "box_pitch",
          class = "field-box-spacer",
          field_box(
            id = "pitch",
            state = "ok",
            label = "Pitch inicial (°)",
            input_tag = numericInput("pitch", NULL, value = 0, step = 0.1)
          ),
          tags$div(
            class = "help-text-small",
            "Inclinació vertical de la càmera respecte a l’horitzó. Valor per defecte, modificar si fos necessari."
          )
        ),
        
        div(
          id = "box_hfov",
          class = "field-box-spacer",
          field_box(
            id = "hfov",
            state = "ok",
            label = "HFOV (°)",
            input_tag = numericInput("hfov", NULL, value = 60, step = 0.1)
          ),
          tags$div(
            class = "help-text-small",
            "HFOV és el camp de visió horitzontal de la foto, és a dir, quants graus del paisatge abasta d’esquerra a dreta. Valor per defecte, modificar si fos necessari."
          )
        ),
        
        div(
          id = "box_vfov",
          class = "field-box-spacer",
          field_box(
            id = "vfov",
            state = "ok",
            label = "VFOV (°)",
            input_tag = numericInput("vfov", NULL, value = 40, step = 0.1)
          ),
          tags$div(
            class = "help-text-small",
            "VFOV és el camp de visió vertical, és a dir, quants graus cobreix de dalt a baix. Valor per defecte, modificar si fos necessari."
          )
        )
      ),
      
      div(
        id = "section_eclipse",
        class = "section-card mobile-step-section step-eclipse",
        h4(class = "section-title", "4. Dades de l'eclipsi"),
        tags$div(
          class = "help-text-small",
          "Dia de l'eclipsi i hora de màxima ocultació del Sol. Indica l'hora segons la teva posició geogràfica."
        ),
        
        div(
          id = "box_calc_date",
          class = "field-box-spacer",
          field_box(
            id = "calc_date",
            state = "ok",
            label = "Data de l'eclipsi",
            input_tag = dateInput("calc_date", NULL, value = "2026-08-12", format = "yyyy-mm-dd")
          )
        ),
        
        div(
          id = "box_calc_time",
          class = "field-box-spacer",
          field_box(
            id = "calc_time",
            state = "missing",
            label = "Hora local de màxima ocultació (HH:MM:SS)",
            input_tag = textInput("calc_time", NULL, value = "")
          ),
          tags$div(
            class = "help-text-small",
            HTML(
              paste0(
                "L'hora prevista de màxima ocultació a Catalunya està entre les 20:15:00 i les 20:30:00. ",
                "Per consultar l'hora exacta segons el municipi, ",
                "<a href='https://eclipsicatalunya.cat/punts-d-observacio/' target='_blank'>",
                "fes servir el cercador d'Eclipsi Catalunya",
                "</a>."
              )
            )
          )
        ),
        
        div(
          id = "box_calc_tz",
          class = "field-box-spacer",
          field_box(
            id = "calc_tz",
            state = "ok",
            label = "Fus horari",
            input_tag = textInput("calc_tz", NULL, value = "Europe/Madrid")
          )
        )
      )
    ),
    
    div(
      class = "center-panel",
      
      div(
        class = "photo-card desktop-photo-only",
        h4(class = "section-title", "Visualització"),
        shinycssloaders::withSpinner(
          imageOutput("photo_img_desktop", click = "photo_click_desktop"),
          type = 4
        )
      ),
      
      div(
        class = "bottom-grid",
        
        div(
          id = "section_adjust_calibration",
          class = "section-card tight-btn mobile-step-section step-adjust",
          h4(class = "section-title", "5. Calibratge (opcional per millorar posició)"),
          tags$div(
            class = "help-text-small",
            "Amb el cursor clica sobre l'horitzó un punt a l'extrem esquerre i després un altre a l'extrem dret."
          ),
          actionButton("start_horizon", "Capturar 2 punts de l'horitzó"),
          actionButton("clear_horizon", "Esborrar horitzó"),
          tags$hr(),
          tags$div(
            class = "help-text-small",
            "Un cop assignada la línia de l'horitzó, clica el punt central."
          ),
          actionButton("set_horizon_center", "Marcar punt central de l'horitzó"),
          actionButton("clear_horizon_center", "Esborrar punt central"),
          checkboxInput("auto_use_horizon_pitch", "Usar punt central per recalcular pitch", value = TRUE),
          tags$hr(),
          verbatimTextOutput("click_info"),
          verbatimTextOutput("horizon_info"),
          verbatimTextOutput("center_info")
        ),
        
        div(
          class = "section-stack",
          
          div(
            id = "section_adjust_offsets",
            class = "section-card tight-btn mobile-step-section step-adjust",
            h4(class = "section-title", "6. Ajusta la posició del sol (opcional)"),
            tags$div(
              class = "help-text-small",
              "Offset X i Offset Y són ajustos manuals en píxels per moure la posició dibuixada del Sol sobre la foto, sense canviar el càlcul astronòmic del Sol."
            ),
            tags$div(class = "help-text-small", "Desplaçament dreta/esquerra"),
            numericInput("x_offset", "Offset X (px)", value = 0, step = 1),
            tags$div(class = "help-text-small", "Desplaçament amunt/avall"),
            numericInput("y_offset", "Offset Y (px)", value = 0, step = 1)
          ),
          
          div(
            id = "section_sol_actions",
            class = "section-card tight-btn download-btn mobile-step-section step-sol",
            h4(class = "section-title", "7. Assignació de la posició del Sol"),
            tags$div(
              class = "help-text-small",
              "Calcula la posició del Sol i, si vols, descarrega la imatge marcada."
            ),
            
            actionButton("calc_sun", "Calcular posició del Sol", class = "btn-primary"),
            downloadButton("download_marked", "Descarregar imatge marcada"),
            
            tags$hr(),
            
            div(
              id = "result_visual_wrap",
              class = "mobile-photo-card",
              h4(class = "section-title", "Resultat visual"),
              uiOutput("result_visual_msg"),
              imageOutput("result_visual_img", width = "100%")
            )
          )
        )
      )
    ),
    
    div(
      class = "right-panel",
      div(
        id = "section_sol_result",
        class = "result-card mobile-step-section step-sol",
        h4(class = "section-title", "Resultat"),
        tableOutput("sun_info"),
        tags$br(),
        uiOutput("status_ui")
      )
    )
  ),
  
  div(
    class = "mobile-bottom-nav",
    div(
      class = "mobile-nav-inner",
      tags$button(
        id = "nav_image",
        type = "button",
        class = "mobile-nav-btn active",
        onclick = "setMobileStep('image')",
        icon("image"),
        tags$span("Imatge")
      ),
      tags$button(
        id = "nav_coords",
        type = "button",
        class = "mobile-nav-btn",
        onclick = "setMobileStep('coords')",
        icon("map-marker-alt"),
        tags$span("Coords")
      ),
      tags$button(
        id = "nav_eclipse",
        type = "button",
        class = "mobile-nav-btn",
        onclick = "setMobileStep('eclipse')",
        icon("moon"),
        tags$span("Eclipsi")
      ),
      tags$button(
        id = "nav_adjust",
        type = "button",
        class = "mobile-nav-btn",
        onclick = "setMobileStep('adjust')",
        icon("sliders-h"),
        tags$span("Ajust")
      ),
      tags$button(
        id = "nav_sol",
        type = "button",
        class = "mobile-nav-btn",
        onclick = "setMobileStep('sol')",
        icon("sun"),
        tags$span("Sol")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  result_path_fixed <- file.path(tempdir(), paste0("eclips_result_", session$token, ".png"))
  
  rv <- reactiveValues(
    active_img = NULL,
    dims = NULL,
    last_click = NULL,
    manual_pt = NULL,
    horizon_pts = data.frame(x = numeric(0), y = numeric(0)),
    horizon_center_pt = NULL,
    capture_horizon = FALSE,
    capture_horizon_center = FALSE,
    calc = NULL,
    exif = NULL,
    demo_mode = FALSE,
    result_img = NULL,
    field_state = list(
      lat = "missing",
      lon = "missing",
      elev = "missing",
      calc_date = "ok",
      calc_time = "missing",
      calc_tz = "ok",
      photo_az = "missing",
      pitch = "ok",
      hfov = "ok",
      vfov = "ok"
    )
  )
  
  mark_missing_box <- function(id) {
    shinyjs::addClass(selector = paste0("#", id), class = "field-missing")
  }
  
  clear_missing_box <- function(id) {
    shinyjs::removeClass(selector = paste0("#", id), class = "field-missing")
  }
  
  set_field_state <- function(id, state) {
    rv$field_state[[id]] <- state
  }
  
  toggle_state <- function(id, state) {
    shinyjs::removeClass(selector = paste0("#", id, "_box"), class = "field-ok")
    shinyjs::removeClass(selector = paste0("#", id, "_box"), class = "field-missing")
    shinyjs::removeClass(selector = paste0("#", id, "_box"), class = "field-exif")
    shinyjs::addClass(selector = paste0("#", id, "_box"), class = paste0("field-", state))
  }
  
  has_num <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  has_txt <- function(x) is.character(x) && length(x) == 1 && nzchar(trimws(x))
  has_date <- function(x) !is.null(x) && !is.na(x)
  
  observe({
    ids <- c("lat", "lon", "elev", "calc_date", "calc_time", "calc_tz", "photo_az", "pitch", "hfov", "vfov")
    for (id in ids) {
      toggle_state(id, rv$field_state[[id]])
    }
  })
  
  reset_visual_state <- function() {
    rv$last_click <- NULL
    rv$manual_pt <- NULL
    rv$horizon_pts <- data.frame(x = numeric(0), y = numeric(0))
    rv$horizon_center_pt <- NULL
    rv$capture_horizon <- FALSE
    rv$capture_horizon_center <- FALSE
    rv$calc <- NULL
    rv$result_img <- NULL
    if (file.exists(result_path_fixed)) {
      unlink(result_path_fixed)
    }
  }
  
  observeEvent(input$photo, {
    req(input$photo)
    
    file_size_mb <- suppressWarnings(as.numeric(input$photo$size) / 1024^2)
    
    if (is.na(file_size_mb)) {
      showNotification(
        "No s'ha pogut determinar la mida del fitxer pujat.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    if (file_size_mb > 30) {
      showNotification(
        paste0(
          "La foto és massa gran (",
          round(file_size_mb, 1),
          " MB). En mòbil, prova una imatge de menys de 30 MB."
        ),
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    
    rv$demo_mode <- FALSE
    
    exif_sum <- read_photo_exif_summary(input$photo$datapath)
    rv$exif <- exif_sum
    
    prepared_img <- tryCatch(
      prepare_uploaded_image(
        path = input$photo$datapath,
        max_dim = 1800,
        quality = 85
      ),
      error = function(e) NULL
    )
    
    if (is.null(prepared_img) || !file.exists(prepared_img)) {
      showNotification(
        "No s'ha pogut preparar la imatge pujada.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    reset_visual_state()
    
    rv$active_img <- prepared_img
    rv$dims <- read_image_dims(rv$active_img)
    
    if (!is.na(exif_sum$lat)) {
      updateNumericInput(session, "lat", value = round(exif_sum$lat, 6))
      set_field_state("lat", "exif")
    } else {
      updateNumericInput(session, "lat", value = NA)
      set_field_state("lat", "missing")
    }
    
    if (!is.na(exif_sum$lon)) {
      updateNumericInput(session, "lon", value = round(exif_sum$lon, 6))
      set_field_state("lon", "exif")
    } else {
      updateNumericInput(session, "lon", value = NA)
      set_field_state("lon", "missing")
    }
    
    if (!is.na(exif_sum$elev)) {
      updateNumericInput(session, "elev", value = round(exif_sum$elev, 1))
      set_field_state("elev", "exif")
    } else {
      updateNumericInput(session, "elev", value = NA)
      set_field_state("elev", "missing")
    }
    
    showNotification(
      if (!is.na(exif_sum$lat) && !is.na(exif_sum$lon)) {
        "Foto carregada. Coordenades EXIF detectades."
      } else {
        "Foto carregada. No s'han trobat coordenades EXIF útils."
      },
      type = "message",
      duration = 4
    )
    
    if (isTRUE(input$is_mobile)) {
      runjs("setMobileStep('image');")
    }
  })
  
  observeEvent(input$use_demo_btn, {
    tmp_demo <- tempfile(fileext = ".png")
    create_demo_image(tmp_demo)
    
    reset_visual_state()
    
    rv$demo_mode <- TRUE
    rv$active_img <- tmp_demo
    rv$dims <- read_image_dims(rv$active_img)
    rv$exif <- NULL
    
    updateNumericInput(session, "lat", value = 41.470000)
    updateNumericInput(session, "lon", value = 1.020000)
    updateNumericInput(session, "elev", value = 600)
    updateNumericInput(session, "photo_az", value = 270)
    
    set_field_state("lat", "ok")
    set_field_state("lon", "ok")
    set_field_state("elev", "ok")
    set_field_state("photo_az", "ok")
    
    if (isTRUE(input$is_mobile)) {
      runjs("setMobileStep('image');")
    }
  })
  
  observeEvent(input$lat, {
    if (has_num(input$lat) && rv$field_state$lat != "exif") set_field_state("lat", "ok")
    if (!has_num(input$lat)) set_field_state("lat", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$lon, {
    if (has_num(input$lon) && rv$field_state$lon != "exif") set_field_state("lon", "ok")
    if (!has_num(input$lon)) set_field_state("lon", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$elev, {
    if (has_num(input$elev) && rv$field_state$elev != "exif") set_field_state("elev", "ok")
    if (!has_num(input$elev)) set_field_state("elev", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$calc_date, {
    if (has_date(input$calc_date)) set_field_state("calc_date", "ok") else set_field_state("calc_date", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$calc_time, {
    if (has_txt(input$calc_time)) set_field_state("calc_time", "ok") else set_field_state("calc_time", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$calc_tz, {
    if (has_txt(input$calc_tz)) set_field_state("calc_tz", "ok") else set_field_state("calc_tz", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$photo_az, {
    if (has_num(input$photo_az)) set_field_state("photo_az", "ok") else set_field_state("photo_az", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$pitch, {
    if (has_num(input$pitch)) set_field_state("pitch", "ok") else set_field_state("pitch", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$hfov, {
    if (has_num(input$hfov)) set_field_state("hfov", "ok") else set_field_state("hfov", "missing")
  }, ignoreInit = TRUE)
  
  observeEvent(input$vfov, {
    if (has_num(input$vfov)) set_field_state("vfov", "ok") else set_field_state("vfov", "missing")
  }, ignoreInit = TRUE)
  
  handle_photo_click <- function(click) {
    req(rv$dims)
    if (is.null(click$x) || is.null(click$y)) return()
    
    rv$last_click <- list(x = click$x, y = click$y)
    
    if (isTRUE(rv$capture_horizon)) {
      if (nrow(rv$horizon_pts) < 2) {
        rv$horizon_pts <- rbind(rv$horizon_pts, data.frame(x = click$x, y = click$y))
      }
      if (nrow(rv$horizon_pts) >= 2) {
        rv$horizon_pts <- rv$horizon_pts[1:2, , drop = FALSE]
        rv$capture_horizon <- FALSE
      }
    }
    
    if (isTRUE(rv$capture_horizon_center)) {
      rv$horizon_center_pt <- list(x = click$x, y = click$y)
      rv$capture_horizon_center <- FALSE
    }
  }
  
  observeEvent(input$photo_click_desktop, {
    handle_photo_click(input$photo_click_desktop)
  })
  
  observeEvent(input$photo_click_mobile_image, {
    handle_photo_click(input$photo_click_mobile_image)
  })
  
  observeEvent(input$start_horizon, {
    rv$horizon_pts <- data.frame(x = numeric(0), y = numeric(0))
    rv$capture_horizon <- TRUE
  })
  
  observeEvent(input$clear_horizon, {
    rv$horizon_pts <- data.frame(x = numeric(0), y = numeric(0))
  })
  
  observeEvent(input$set_horizon_center, {
    rv$capture_horizon_center <- TRUE
  })
  
  observeEvent(input$clear_horizon_center, {
    rv$horizon_center_pt <- NULL
  })
  
  horizon_roll_deg <- reactive({
    if (nrow(rv$horizon_pts) != 2) return(0)
    
    dx <- rv$horizon_pts$x[2] - rv$horizon_pts$x[1]
    dy <- rv$horizon_pts$y[2] - rv$horizon_pts$y[1]
    angle <- rad2deg(atan2(dy, dx))
    -angle
  })
  
  effective_pitch <- reactive({
    base_pitch <- input$pitch
    
    if (!isTRUE(input$auto_use_horizon_pitch)) return(base_pitch)
    if (is.null(rv$horizon_center_pt) || is.null(rv$dims)) return(base_pitch)
    
    pt_rot <- rotate_point(
      x = rv$horizon_center_pt$x,
      y = rv$horizon_center_pt$y,
      cx = rv$dims$width / 2,
      cy = rv$dims$height / 2,
      angle_deg = horizon_roll_deg()
    )
    
    ((pt_rot$y / rv$dims$height) - 0.5) * input$vfov
  })
  
  sun_adjusted <- reactive({
    req(rv$calc, rv$dims)
    
    x_adj <- rv$calc$proj$x + input$x_offset
    y_adj <- rv$calc$proj$y + input$y_offset
    
    inside <- (
      x_adj >= 0 && x_adj <= rv$dims$width &&
        y_adj >= 0 && y_adj <= rv$dims$height
    )
    
    list(x = x_adj, y = y_adj, inside = inside)
  })
  
  generate_result_image <- function() {
    req(rv$active_img, rv$dims, rv$calc)
    
    adj <- sun_adjusted()
    sun_pt <- list(x = adj$x, y = adj$y)
    sun_label <- sprintf(
      "Sol  az=%.2f°  alt=%.2f°",
      rv$calc$sun$azimuth,
      rv$calc$sun$altitude
    )
    
    draw_overlay_image(
      img_path = rv$active_img,
      out_path = result_path_fixed,
      sun_pt = sun_pt,
      sun_label = sun_label,
      horizon_pts = if (nrow(rv$horizon_pts) > 0) rv$horizon_pts else NULL,
      horizon_center_pt = rv$horizon_center_pt,
      click_pt = rv$last_click,
      manual_pt = rv$manual_pt,
      show_grid = isTRUE(input$show_grid),
      show_center_cross = TRUE
    )
    
    rv$result_img <- result_path_fixed
  }
  
  observeEvent(input$calc_sun, {
    req(rv$active_img, rv$dims)
    
    box_ids <- c(
      "box_calc_date", "box_calc_time", "box_calc_tz",
      "box_lat", "box_lon", "box_photo_az", "box_hfov", "box_vfov"
    )
    
    lapply(box_ids, clear_missing_box)
    
    missing_fields <- c()
    
    if (is.null(input$calc_date) || !nzchar(as.character(input$calc_date))) {
      missing_fields <- c(missing_fields, "data")
      mark_missing_box("box_calc_date")
    }
    
    if (is.null(input$calc_time) || !nzchar(trimws(input$calc_time))) {
      missing_fields <- c(missing_fields, "hora")
      mark_missing_box("box_calc_time")
    }
    
    if (is.null(input$calc_tz) || !nzchar(trimws(input$calc_tz))) {
      missing_fields <- c(missing_fields, "fus horari")
      mark_missing_box("box_calc_tz")
    }
    
    if (is.null(input$lat) || !is.finite(suppressWarnings(as.numeric(input$lat)))) {
      missing_fields <- c(missing_fields, "latitud")
      mark_missing_box("box_lat")
    }
    
    if (is.null(input$lon) || !is.finite(suppressWarnings(as.numeric(input$lon)))) {
      missing_fields <- c(missing_fields, "longitud")
      mark_missing_box("box_lon")
    }
    
    if (is.null(input$photo_az) || !is.finite(suppressWarnings(as.numeric(input$photo_az)))) {
      missing_fields <- c(missing_fields, "azimut de la foto")
      mark_missing_box("box_photo_az")
    }
    
    if (is.null(input$hfov) || !is.finite(suppressWarnings(as.numeric(input$hfov)))) {
      missing_fields <- c(missing_fields, "HFOV")
      mark_missing_box("box_hfov")
    }
    
    if (is.null(input$vfov) || !is.finite(suppressWarnings(as.numeric(input$vfov)))) {
      missing_fields <- c(missing_fields, "VFOV")
      mark_missing_box("box_vfov")
    }
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste(
          "Falten dades per calcular la posició del Sol:",
          paste(missing_fields, collapse = ", ")
        ),
        type = "warning",
        duration = 6
      )
      return(NULL)
    }
    
    datetime_local_str <- paste(as.character(input$calc_date), trimws(input$calc_time))
    
    time_local <- tryCatch(
      as.POSIXct(
        datetime_local_str,
        tz = input$calc_tz,
        format = "%Y-%m-%d %H:%M:%S"
      ),
      error = function(e) NULL
    )
    
    if (is.null(time_local) || is.na(time_local)) {
      mark_missing_box("box_calc_time")
      showNotification(
        "No s'ha pogut interpretar la data/hora de càlcul. Usa el format HH:MM:SS.",
        type = "error",
        duration = 6
      )
      return(NULL)
    }
    
    sun <- solar_position(
      datetime_local = time_local,
      tz_string = input$calc_tz,
      lat_deg = input$lat,
      lon_deg = input$lon
    )
    
    proj <- sun_to_image_xy(
      sun_az = sun$azimuth,
      sun_alt = sun$altitude,
      photo_center_az = input$photo_az,
      photo_pitch = effective_pitch(),
      hfov = input$hfov,
      vfov = input$vfov,
      img_width = rv$dims$width,
      img_height = rv$dims$height,
      roll_deg = horizon_roll_deg()
    )
    
    rv$calc <- list(
      sun = sun,
      proj = proj,
      roll_deg = horizon_roll_deg(),
      pitch_deg = effective_pitch(),
      calc_date = as.character(input$calc_date),
      calc_time = input$calc_time,
      calc_tz = input$calc_tz
    )
    
    generate_result_image()
    
    if (isTRUE(input$is_mobile)) {
      runjs("setMobileStep('sol');")
    }
  })
  
  observeEvent(
    list(
      input$x_offset, input$y_offset, input$show_grid,
      rv$horizon_pts, rv$horizon_center_pt,
      input$auto_use_horizon_pitch, input$pitch,
      input$hfov, input$vfov, input$photo_az
    ),
    {
      if (!is.null(rv$calc)) {
        generate_result_image()
      }
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$calc_time, {
    if (!is.null(input$calc_time) && nzchar(trimws(input$calc_time))) {
      clear_missing_box("box_calc_time")
    }
  })
  
  observeEvent(input$lat, {
    if (is.finite(suppressWarnings(as.numeric(input$lat)))) clear_missing_box("box_lat")
  })
  
  observeEvent(input$lon, {
    if (is.finite(suppressWarnings(as.numeric(input$lon)))) clear_missing_box("box_lon")
  })
  
  observeEvent(input$photo_az, {
    if (is.finite(suppressWarnings(as.numeric(input$photo_az)))) clear_missing_box("box_photo_az")
  })
  
  observeEvent(input$hfov, {
    if (is.finite(suppressWarnings(as.numeric(input$hfov)))) clear_missing_box("box_hfov")
  })
  
  observeEvent(input$vfov, {
    if (is.finite(suppressWarnings(as.numeric(input$vfov)))) clear_missing_box("box_vfov")
  })
  
  output$photo_img_desktop <- renderImage({
    if (is.null(rv$active_img) || !file.exists(rv$active_img)) {
      return(list(
        src = "",
        alt = "Encara no s'ha carregat cap fotografia."
      ))
    }
    
    list(
      src = normalizePath(rv$active_img),
      contentType = "image/jpeg",
      alt = "Imatge inicial"
    )
  }, deleteFile = FALSE)
  
  output$photo_img_mobile <- renderImage({
    if (is.null(rv$active_img) || !file.exists(rv$active_img)) {
      return(list(
        src = "",
        alt = "Encara no s'ha carregat cap fotografia."
      ))
    }
    
    list(
      src = normalizePath(rv$active_img),
      contentType = "image/jpeg",
      alt = "Imatge inicial"
    )
  }, deleteFile = FALSE)
  
  output$result_visual_msg <- renderUI({
    if (is.null(rv$active_img)) {
      return(tags$div(style = "color:#666;", "Encara no s'ha carregat cap fotografia."))
    }
    
    if (is.null(rv$result_img) || !file.exists(rv$result_img)) {
      return(tags$div(style = "color:#666; margin-bottom:8px;", "Encara no s'ha calculat la posició del Sol."))
    }
    
    NULL
  })
  
  output$result_visual_img <- renderImage({
    req(rv$result_img, file.exists(rv$result_img))
    list(
      src = normalizePath(rv$result_img),
      contentType = "image/png",
      alt = "Resultat visual amb el Sol assignat"
    )
  }, deleteFile = FALSE)
  
  output$sun_info <- renderTable({
    req(rv$calc)
    adj <- sun_adjusted()
    
    data.frame(
      Camp = c(
        "Data de càlcul",
        "Hora local de càlcul",
        "Fus horari de càlcul",
        "Data/hora local utilitzades",
        "Data/hora UTC utilitzades",
        "Azimut del Sol (°)",
        "Altura del Sol (°)",
        "Roll aplicat (°)",
        "Pitch efectiu (°)",
        "Offset angular X (°)",
        "Offset angular Y (°)",
        "X projectat (px)",
        "Y projectat (px)",
        "X final (px)",
        "Y final (px)"
      ),
      Valor = c(
        rv$calc$calc_date,
        rv$calc$calc_time,
        rv$calc$calc_tz,
        format(rv$calc$sun$datetime_local, "%Y-%m-%d %H:%M:%S %Z"),
        format(rv$calc$sun$datetime_utc, "%Y-%m-%d %H:%M:%S UTC"),
        sprintf("%.3f", rv$calc$sun$azimuth),
        sprintf("%.3f", rv$calc$sun$altitude),
        sprintf("%.3f", rv$calc$roll_deg),
        sprintf("%.3f", rv$calc$pitch_deg),
        sprintf("%.3f", rv$calc$proj$dx_deg),
        sprintf("%.3f", rv$calc$proj$dy_deg),
        sprintf("%.1f", rv$calc$proj$x),
        sprintf("%.1f", rv$calc$proj$y),
        sprintf("%.1f", adj$x),
        sprintf("%.1f", adj$y)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$status_ui <- renderUI({
    req(rv$calc)
    adj <- sun_adjusted()
    
    tags$div(
      style = "padding:10px; border:1px solid #ccc; border-radius:6px;",
      tags$b(
        if (rv$calc$proj$inside) {
          "La projecció geomètrica del Sol cau dins de la imatge."
        } else {
          "La projecció geomètrica del Sol cau fora de la imatge."
        }
      ),
      tags$br(),
      if (adj$inside) {
        tags$span(style = "color:#2e7d32; font-weight:600;", "La posició final ajustada és dins de la imatge.")
      } else {
        tags$span(style = "color:#c62828; font-weight:600;", "La posició final ajustada és fora de la imatge.")
      }
    )
  })
  
  output$exif_info <- renderText({
    if (isTRUE(rv$demo_mode)) {
      return(
        paste(
          "Mode demo actiu:",
          "imatge de prova carregada amb coordenades de prova",
          "(lat 41.470000, lon 1.020000, alt 600 m)",
          "i azimut central orientat a ponent (270°).",
          sep = "\n"
        )
      )
    }
    
    if (is.null(rv$exif)) {
      return("EXIF foto: encara no s'ha llegit cap foto.")
    }
    
    parts <- c("EXIF foto:")
    
    if (!is.na(rv$exif$lat) && !is.na(rv$exif$lon)) {
      parts <- c(parts, sprintf("GPS = %.6f, %.6f", rv$exif$lat, rv$exif$lon))
    }
    
    if (!is.na(rv$exif$elev)) {
      parts <- c(parts, sprintf("Altitud = %.1f m", rv$exif$elev))
    }
    
    if (!is.na(rv$exif$datetime)) {
      parts <- c(parts, paste("Data/hora foto =", rv$exif$datetime))
    }
    
    if (!is.na(rv$exif$device)) {
      parts <- c(parts, paste("Dispositiu =", rv$exif$device))
    }
    
    if (length(parts) == 1) {
      return(paste("EXIF foto:", rv$exif$message))
    }
    
    paste(parts, collapse = "\n")
  })
  
  output$click_info <- renderText({
    if (is.null(rv$last_click)) return("Clic actual: cap")
    sprintf("Clic actual:\nX = %.1f px\nY = %.1f px", rv$last_click$x, rv$last_click$y)
  })
  
  output$horizon_info <- renderText({
    if (nrow(rv$horizon_pts) == 0) return("Horitzó: no definit")
    if (nrow(rv$horizon_pts) == 1) {
      return(sprintf(
        "Horitzó:\nPunt 1 = (%.1f, %.1f)\nEsperant 2n punt...",
        rv$horizon_pts$x[1], rv$horizon_pts$y[1]
      ))
    }
    
    dx <- rv$horizon_pts$x[2] - rv$horizon_pts$x[1]
    dy <- rv$horizon_pts$y[2] - rv$horizon_pts$y[1]
    angle <- rad2deg(atan2(dy, dx))
    
    sprintf(
      paste(
        "Horitzó:",
        "Punt 1 = (%.1f, %.1f)",
        "Punt 2 = (%.1f, %.1f)",
        "Angle línia = %.3f°",
        "Roll correctiu = %.3f°",
        sep = "\n"
      ),
      rv$horizon_pts$x[1], rv$horizon_pts$y[1],
      rv$horizon_pts$x[2], rv$horizon_pts$y[2],
      angle,
      horizon_roll_deg()
    )
  })
  
  output$center_info <- renderText({
    if (is.null(rv$horizon_center_pt)) {
      return("Punt central d'horitzó: no definit")
    }
    
    sprintf(
      paste(
        "Punt central d'horitzó:",
        "X = %.1f px",
        "Y = %.1f px",
        "Pitch efectiu estimat = %.3f°",
        sep = "\n"
      ),
      rv$horizon_center_pt$x,
      rv$horizon_center_pt$y,
      effective_pitch()
    )
  })
  
  output$download_marked <- downloadHandler(
    filename = function() {
      paste0("foto_sol_v3_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$result_img, file.exists(rv$result_img))
      file.copy(rv$result_img, file, overwrite = TRUE)
    }
  )
  
  outputOptions(output, "result_visual_img", suspendWhenHidden = FALSE)
  outputOptions(output, "result_visual_msg", suspendWhenHidden = FALSE)
  outputOptions(output, "photo_img_desktop", suspendWhenHidden = FALSE)
  outputOptions(output, "photo_img_mobile", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
