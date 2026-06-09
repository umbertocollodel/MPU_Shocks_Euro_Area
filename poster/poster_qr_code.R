#===============================================================================
# POSTER QR CODE — arXiv paper link
#===============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(qrcode, png)

fig_dir <- "../output/figures/poster"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

url <- "https://arxiv.org/abs/2508.13635v4"

qr <- qr_code(url)

# Save as PNG at high resolution
out_path <- file.path(fig_dir, "poster_qr_code.png")
png(out_path, width = 1200, height = 1200, res = 300, bg = "white")
plot(qr, col = c("white", "#1a1a1a"))
dev.off()

cat("Saved:", out_path, "\n")
