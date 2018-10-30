pkgname <- "ijtiff"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ijtiff')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("as_EBImage")
### * as_EBImage

flush(stderr()); flush(stdout())

### Name: as_EBImage
### Title: Convert an ijtiff_img to an EBImage::Image.
### Aliases: as_EBImage

### ** Examples

if (require(EBImage)) {
  img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
  str(img)
  str(as_EBImage(img))
  img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
  str(img)
  str(as_EBImage(img))
}



cleanEx()
nameEx("count_imgs")
### * count_imgs

flush(stderr()); flush(stdout())

### Name: count_imgs
### Title: Count the number of images in a TIFF file.
### Aliases: count_imgs

### ** Examples

count_imgs(system.file("img", "Rlogo.tif", package="ijtiff"))
count_imgs(system.file("img", "2ch_ij.tif", package="ijtiff"))




cleanEx()
nameEx("display")
### * display

flush(stderr()); flush(stdout())

### Name: display
### Title: Basic image display.
### Aliases: display

### ** Examples

img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
display(img)
display(img[, , 1, 1])  # first (red) channel, first frame
display(img[, , 2, ])  # second (green) channel, first frame
display(img[, , 3, ])  # third (blue) channel, first frame
display(img, basic = TRUE)  # displays first (red) channel, first frame




cleanEx()
nameEx("ijtiff_img")
### * ijtiff_img

flush(stderr()); flush(stdout())

### Name: ijtiff_img
### Title: 'ijtiff_img' class.
### Aliases: ijtiff_img as_ijtiff_img

### ** Examples

img <- matrix(1:4, nrow = 2)  # to be a single-channel, grayscale image
ijtiff_img(img, description = "single-channel, grayscale")
img <- array(seq_len(2 ^ 3), dim = rep(2, 3))  # 1 channel, 2 frame
ijtiff_img(img, description = "blah blah blah")
img <- array(seq_len(2 ^ 3), dim = c(2, 2, 2, 1))  #  2 channel, 1 frame
ijtiff_img(img, description = "blah blah")
img <- array(seq_len(2 ^ 4), dim = rep(2, 4))  # 2 channel, 2 frame
ijtiff_img(img, software = "R")



cleanEx()
nameEx("linescan-conversion")
### * linescan-conversion

flush(stderr()); flush(stdout())

### Name: linescan-conversion
### Title: Rejig linescan images.
### Aliases: linescan-conversion linescan_to_stack stack_to_linescan

### ** Examples

linescan <- ijtiff_img(array(rep(1:4, each = 4), dim = c(4, 4, 1, 1)))
print(linescan)
stack <- linescan_to_stack(linescan)
print(stack)
linescan <- stack_to_linescan(stack)
print(linescan)




cleanEx()
nameEx("read_tags")
### * read_tags

flush(stderr()); flush(stdout())

### Name: read_tags
### Title: Read TIFF tag information without actually reading the image
###   array.
### Aliases: read_tags

### ** Examples

read_tags(system.file("img", "Rlogo.tif", package="ijtiff"))
read_tags(system.file("img", "2ch_ij.tif", package="ijtiff"))
read_tags(system.file("img", "2ch_ij.tif", package="ijtiff"), all = c(2, 4))




cleanEx()
nameEx("read_tif")
### * read_tif

flush(stderr()); flush(stdout())

### Name: read_tif
### Title: Read an image stored in the TIFF format
### Aliases: read_tif

### ** Examples

img <- read_tif(system.file("img", "Rlogo.tif", package = "ijtiff"))
img <- read_tif(system.file("img", "2ch_ij.tif", package = "ijtiff"))
str(img)  # we see that `ijtiff` correctly recognises this image's 2 channels




cleanEx()
nameEx("text-image-io")
### * text-image-io

flush(stderr()); flush(stdout())

### Name: text-image-io
### Title: Read/write an image array to/from disk as text file(s).
### Aliases: text-image-io write_txt_img read_txt_img

### ** Examples

img <- read_tif(system.file('img', 'Rlogo.tif', package = 'ijtiff'))
tmptxt <- tempfile(pattern = "img", fileext = ".txt")
write_txt_img(img, tmptxt)
tmptxt_ch1_path <- paste0(filesstrings::before_last_dot(tmptxt), "_ch1.txt")
print(tmptxt_ch1_path)
txt_img <- read_txt_img(tmptxt_ch1_path)




cleanEx()
nameEx("write_tif")
### * write_tif

flush(stderr()); flush(stdout())

### Name: write_tif
### Title: Write images in TIFF format
### Aliases: write_tif

### ** Examples


img <- read_tif(system.file("img", "Rlogo.tif", package="ijtiff"))
temp_dir <- tempdir()
write_tif(img, paste0(temp_dir, "/", "Rlogo"))
img <- matrix(1:4, nrow = 2)
write_tif(img, paste0(temp_dir, "/", "tiny2x2"))
list.files(temp_dir, pattern = "tif$")




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
