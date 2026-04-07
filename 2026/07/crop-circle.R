library(magick)
img_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Nikola_Jokic_free_throw_%28cropped%29.jpg/500px-Nikola_Jokic_free_throw_%28cropped%29.jpg"
img <- image_read(img_url)

# Crop smaller part of the photo
img_cropped <- image_crop(img, geometry = "300x300+120+0", repage = TRUE)

# create a new image with white background and black circle
mask <- image_draw(image_blank(300, 300))
symbols(300/2, 300/2, circles=(300/2)-3, bg = "black", inches = FALSE, add = TRUE)
dev.off()

mask_grey <- image_convert(mask, colorspace = "Gray")
img_masked <- image_composite(img_cropped, mask_grey, operator = "CopyOpacity")

# Add a border
img_final <- image_draw(img_masked)
symbols(300/2, 300/2, circles = (300/2) - 3, bg = NA, fg = "#1D428A",
        lwd = 10, inches = FALSE, add = TRUE)
dev.off()

image_write(img_final, file.path("2026", "07", "jokic.png"), format = "png")
