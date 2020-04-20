library(coronaobj)
library(rayrender)
library(gifski)
library(magick)

write_corona_obj("custom_color2.obj", color_membrane = "#a1cdf0",
                color_spike = "#3432cf", color_open_spike = "#d92bc5")
#> [1] "custom_color2.obj"

obj_model("custom_color2.obj", vertex_colors = TRUE) %>%
  add_object(sphere(y=10,z=10,x=10, material=light(color="lightblue",intensity=160))) %>%
  add_object(sphere(y=10,z=10,x=-10, material=light(color="orange",intensity=160))) %>%
  add_object(sphere(y=-10,z=-5,material=light(color="purple", intensity = 160))) -> sars_cov_2_scene

t=1:30
xpos <- 10 * sin(t*12*pi/180+pi/2)
zpos <- 10 * cos(t*12*pi/180+pi/2)

png_files <- character(0)
for(i in 1:30) {
 print(i)
 fname <- paste0("SARS-CoV-2-frame", i, ".png")
 png_files <- c(png_files, fname)

 render_scene(sars_cov_2_scene, parallel=TRUE, samples = 100,
              fov = 7, min_variance=0, focal_distance = 9.6,
              aperture=0.5, width=300,height=300,
              lookfrom = c(xpos[i],1.5,zpos[i]),
              lookat = c(0,0,0),
              filename=fname)
 viruspng <- image_read(fname)
 transviruspng <- image_transparent(viruspng, 'black')
 image_write(transviruspng, fname)
}

gif_file <- gifski(png_files, gif_file = "SARS-CoV-2.gif")
unlink(png_files)
