library(coronaobj)
library(rayrender)
library(gifski)
library(magick)

write_corona_obj("custom_color2.obj", color_membrane = "cadetblue1",
                color_spike = "steelblue", color_open_spike = "deepskyblue1")
#> [1] "custom_color2.obj"

t=1:30
xpos <- 10 * sin(t*12*pi/180+pi/2)
zpos <- 10 * cos(t*12*pi/180+pi/2)

png_files <- character(0)
t <- c(seq(-10, 10, by=0.25),seq(9.75,-9.75, by=0.25))
for(i in 1:length(t)) {
 print(i)
 j <- t[i]
 fname <- paste0("SARS-CoV-2-frame", i, ".png")
 png_files <- c(png_files, fname)

obj_model("custom_color2.obj", vertex_colors = TRUE) %>%
  add_object(sphere(y=j,z=10,x=10, material=light(color="orange",intensity=160))) %>%
  add_object(sphere(y=-j,z=10,x=-10, material=light(color="yellow",intensity=80))) %>%
 render_scene(parallel=TRUE, samples = 500,
              fov = 7, min_variance=0, focal_distance = 9.6,
              aperture=0.5, width=300,height=300,
              filename=fname)
 viruspng <- image_read(fname)
 transviruspng <- image_transparent(viruspng, 'black')
 image_write(transviruspng, fname)
}

gif_file <- gifski(png_files, gif_file = "SARS-CoV-2.gif", delay=0.1)
unlink(png_files)
