#include "triangle.h"
#include <array>
#include <iostream>

int main() {
    using namespace software_rendering;
    auto image = TGAImage{200, 200, TGAImage::RGB};

    const auto pts = std::array<Vec2i, 3>{
            Vec2i{10, 10}, Vec2i{100, 30}, Vec2i{190, 160}
    };
    /**
     * triangle_line(pts[0],pts[1],pts[2],image,white);
     * triangle_zoning(pts[0],pts[1],pts[2],image,white);
     * triangle_zoning_merge(pts[0],pts[1],pts[2],image,white);
     */
    triangle(pts, image, white);
    image.flip_vertically();
    image.write_tga_file("framebuffer.tga");
    return 0;
}