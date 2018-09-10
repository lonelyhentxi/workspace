#include "triangle.h"
#include <array>
#include <iostream>
#include "model.h"

int main() {
    using namespace software_rendering;
    auto frame = TGAImage{200, 200, TGAImage::RGB};

    const auto pts = std::array<Vec2i, 3>{
            Vec2i{10, 10}, Vec2i{100, 30}, Vec2i{190, 160}
    };
    /**
     * triangle_line(pts[0],pts[1],pts[2],image,white);
     * triangle_zoning(pts[0],pts[1],pts[2],image,white);
     * triangle_zoning_merge(pts[0],pts[1],pts[2],image,white);
     */
    triangle(pts, frame, white);
    frame.flip_vertically();
    frame.write_tga_file("framebuffer.tga");


    auto model = Model("obj/african_head.obj");
    const int width = 800;
    const int height = 800;
    TGAImage image(width, height, TGAImage::RGB);
    for (int i=0; i<model.nfaces(); i++) {
        std::vector<int> face = model.face(i);
        auto screen_coords = std::array<Vec2i, 3>{};
        for (int j=0; j<3; j++) {
            Vec3f world_coords = model.vert(face[j]);
            screen_coords[j] = Vec2i((world_coords.x+1.)*width/2., (world_coords.y+1.)*height/2.);
        }
        triangle(screen_coords, image, TGAColor(rand()%255, rand()%255, rand()%255, 255));
    }
    image.flip_vertically(); // i want to have the origin at the left bottom corner of the image
    image.write_tga_file("output.tga");
    return 0;
}