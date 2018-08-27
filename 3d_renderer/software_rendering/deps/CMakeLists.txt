project(software_rendering)
set(TGA_SRC tgaimage.cpp ../includes/triangle.h)
add_library(tga STATIC ${TGA_SRC})
