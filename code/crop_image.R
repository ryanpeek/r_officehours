# crop an image


library(magick)

(mypic <- image_read(path = "https://pbs.twimg.com/media/EcZhdJ5XgAA-T5n.jpg"))


mypic2 <- image_crop(mypic, "1150x760+25")

mypic2
