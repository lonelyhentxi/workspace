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
    auto z_buffers = std::vector<float>(width * height);
    for (auto &buffer:z_buffers) {
        buffer = -std::numeric_limits<float>::min();
    }
    TGAImage image(width, height, TGAImage::RGB);
    Vec3f light_dir(0, 0, -1);
    for (int i = 0; i < model.nfaces(); i++) {
        std::vector<int> face = model.face(i);
        auto screen_coords = std::array<Vec3i, 3>{};
        auto world_coords = std::array<Vec3f, 3>{};
        for (int j = 0; j < 3; j++) {
            Vec3f v = model.vert(face[j]);
            screen_coords[j] =
                    Vec3i(static_cast<int>((v.x + 1.f) * width / 2.f), static_cast<int>((v.y + 1.f) * height / 2.f),
                          static_cast<int>(v.z));
            world_coords[j] = v;
        }
        Vec3f n = cross(world_coords[2] - world_coords[0], world_coords[1] - world_coords[0]);
        n = n.normalize();
        float intensity = n * light_dir;
        if (intensity > 0) {
            triangle(screen_coords, z_buffers, image,
                             TGAColor(static_cast<unsigned char>(intensity * 255),
                                      static_cast<unsigned char>(intensity * 255),
                                      static_cast<unsigned char>(intensity * 255), 255));
        }
    }
    image.flip_vertically(); // i want to have the origin at the left bottom corner of the image
    image.write_tga_file("output.tga");
    return 0;
}