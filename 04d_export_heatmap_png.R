## ============================================================
## CAHPS Low-Rater Segmentation: Static PNG Export of the Cluster
## Signature Heatmap
## Step 4d of analytic plan -- takes the interactive HTML deliverable
## built by 04a_build_heatmap_data.R (04c_interactive_heatmap.html)
## and renders it to a high-resolution PNG, for dropping directly
## into a Word document or PowerPoint slide.
##
## Uses a real headless-browser render (webshot2, backed by chromote/
## Chrome) rather than a static HTML-to-image converter, because the
## heatmap grid in 04c is built by JavaScript at page-load time --
## a converter that doesn't execute JS would capture an empty page.
##
## Requires: a locally available Chrome/Chromium install. chromote
## will look for one automatically; if none is found, install Google
## Chrome or run chromote::find_chrome() to point at a specific binary.
## ============================================================

## install.packages("webshot2")   # one-time, if not already installed
library(webshot2)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
html_path  <- "04c_interactive_heatmap.html"   # output of 04a_build_heatmap_data.R
output_png <- "04c_interactive_heatmap.png"

## Selector scoped to the card itself (not the full page background),
## so the exported image is just the heatmap, title, and legend --
## matches what you'd want to paste into a document.
css_selector <- ".container"

## Zoom > 1 renders at higher effective DPI (webshot2 upscales the
## viewport before capturing), which keeps small text legible when the
## PNG is later resized to fit a slide or a document column.
zoom_factor <- 2

## Viewport sized generously relative to the heatmap's own max-width
## (800px, see 04a's html_template) so nothing is clipped or wrapped
## awkwardly before the selector crop is applied.
viewport_width  <- 1000
viewport_height <- 1400

## Small delay so the grid-building JavaScript (renderGrid(), which
## runs on page load in 04c) has finished before the screenshot fires.
render_delay_sec <- 0.75

## ------------------------------------------------------------
## 1. VALIDATE INPUT
## ------------------------------------------------------------
if (!file.exists(html_path)) {
  stop("Could not find ", html_path, " -- run 04a_build_heatmap_data.R first ",
       "(it generates 04c_interactive_heatmap.html in this directory).")
}

## ------------------------------------------------------------
## 2. RENDER
## ------------------------------------------------------------
## webshot2 accepts a local file path directly (no need to serve it).
## file.path(getwd(), ...) makes the path unambiguous regardless of how
## webshot2's internal browser resolves relative paths.
html_full_path <- normalizePath(html_path)

cat("Rendering", html_path, "to", output_png, "via headless Chrome...\n")

tryCatch({
  webshot2::webshot(
    url      = html_full_path,
    file     = output_png,
    selector = css_selector,
    vwidth   = viewport_width,
    vheight  = viewport_height,
    zoom     = zoom_factor,
    delay    = render_delay_sec
  )
}, error = function(e) {
  stop(
    "webshot2 render failed. Most common cause: no Chrome/Chromium found.\n",
    "Try: chromote::find_chrome() to check what's detected, or install\n",
    "Google Chrome and re-run. Original error:\n", conditionMessage(e)
  )
})

if (!file.exists(output_png)) {
  stop("webshot2 did not report an error but ", output_png, " was not created -- ",
       "inspect manually before trusting downstream use.")
}

cat("\nSaved", output_png, "(zoom =", zoom_factor, "x, selector =", css_selector, ")\n")
cat("Ready to insert into a Word document or PowerPoint slide.\n")

## ------------------------------------------------------------
## 3. OPTIONAL -- SAME EXPORT FOR THE CLUSTER 3 SUB-CLUSTER HEATMAP
## ------------------------------------------------------------
## If 07_profile_hier_subclusters.R has been run and produced
## 07c_cluster3_subcluster_heatmap.html, uncomment below to export
## that one too using the same settings.
##
## html_path_2  <- "07c_cluster3_subcluster_heatmap.html"
## output_png_2 <- "07c_cluster3_subcluster_heatmap.png"
## if (file.exists(html_path_2)) {
##   webshot2::webshot(
##     url = normalizePath(html_path_2), file = output_png_2,
##     selector = css_selector, vwidth = viewport_width,
##     vheight = viewport_height, zoom = zoom_factor, delay = render_delay_sec
##   )
##   cat("Saved", output_png_2, "\n")
## }
