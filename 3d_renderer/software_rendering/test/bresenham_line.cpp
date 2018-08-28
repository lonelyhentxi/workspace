#include "tgaimage.h"
#include "bresenham_line.h"

int main() {
    using namespace software_rendering;
    const TGAColor white = TGAColor{255,255,255,255};
    const TGAColor red = TGAColor{255,0,0,255};

    TGAImage image = TGAImage{100,100,TGAImage::RGB};
    // line_fixed_step(0,0,100,100,image,white
    // line_x_pixel_step(13,20,80,40,image,white);
    // line_x_pixel_step(20,13,40,80,image,red);
    // line_pixel_step(13,20,80,40,image,white);
    // line_pixel_step(20,13,40,80,image,red);
    // line_pixel_step_optimized(13,20,80,40,image,white);
    // line_pixel_step_optimized(20,13,40,80,image,red);
    line_pixel_step_optimized2(13,20,80,40,image,white);
    line_pixel_step_optimized2(20,13,40,80,image,red);
    image.flip_vertically();
    image.write_tga_file("test.tga");
    return 0;
}
