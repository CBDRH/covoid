# install libraries if required
# install.packages("remotes")
# remotes::install_github("tylermorganwall/coronaobj")
# remotes::install_github("tylermorganwall/rayrender")

library(coronaobj)
library(rayrender)
library(gifski)
library(magick)

fname <- "SARS-CoV-2-base-image.png"

write_corona_obj("custom_color.obj", color_membrane = "cadetblue1",
                color_spike = "steelblue", color_open_spike = "deepskyblue1")

obj_model("custom_color.obj", vertex_colors = TRUE) %>%
  add_object(sphere(y=10,z=10,x=10, material=light(color="lightblue",intensity=100))) %>%
  add_object(sphere(y=10,z=10,x=-10, material=light(color="orange",intensity=100))) %>%
  render_scene(parallel=TRUE, samples = 1000, fov = 7, min_variance=0, focal_distance = 9.6,
               width=300,height=300,
               filename=fname)


virus <- image_read(fname)
virus <- image_transparent(virus, 'black')

virus_images <- c(virus)

t <- c(seq(0, 2.3,by=0.1), seq(2.3, 0,by=-0.1))

for(i in 1:length(t)) {
 print(i)
 j <- t[i]
 imploded_virus <- image_implode(virus, j)
 virus_images <- c(virus_images, imploded_virus)
}

animation <- image_animate(virus_images, fps = 10)
# print(animation)

image_write(animation, "SARS-CoV-2.gif")
unlink(fname)
