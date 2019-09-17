#ifndef SOFTWARE_RENDERING_BRESENHAM_LINE_H
#define SOFTWARE_RENDERING_BRESENHAM_LINE_H

#include <cmath>
#include "tgaimage.h"
#include "geometry.h"

namespace software_rendering {
    void line_fixed_step(int x0, int y0, int x1, int y1, TGAImage &image, const TGAColor &color) {
        for (double t = 0; t < 1; t += .01) {
            auto x = static_cast<int>(x0 * (1. - t) + x1 * t);
            auto y = static_cast<int>(y0 * (1. - t) + y1 * t);
            image.set(x, y, color);
        }
    }

    void line_x_pixel_step(int x0, int y0, int x1, int y1, TGAImage &image, const TGAColor &color) {
        for (int x = x0; x <= x1; x++) {
            float t = (x - x0) / static_cast<float>(x1 - x0);
            auto y = static_cast<int>(y0 * (1. - t) + y1 * t);
            image.set(x, y, color);
        }
    }

    void line_pixel_step(int x0, int y0, int x1, int y1, TGAImage &image, const TGAColor &color) {
        bool steep = false;
        if (std::abs(x0 - x1) < std::abs(y0 - y1)) {
            std::swap(x0, y0);
            std::swap(x1, y1);
            steep = true;
        }
        if (x0 > x1) {
            std::swap(x0, x1);
            std::swap(y0, y1);
        }
        for (int x = x0; x <= x1; x++) {
            float t = (x - x0) / static_cast<float>(x1 - x0);
            auto y = static_cast<int>(y0 * (1. - t) + y1 * t);
            if (steep) {
                image.set(y, x, color);
            } else {
                image.set(x, y, color);
            }
        }
    }

    void line_pixel_step_optimized(int x0, int y0, int x1, int y1, TGAImage &image, const TGAColor &color) {
        bool steep = false;
        if (std::abs(x0 - x1) < std::abs(y0 - y1)) {
            std::swap(x0, y0);
            std::swap(x1, y1);
            steep = true;
        }
        if (x0 > x1) {
            std::swap(x0, x1);
            std::swap(y0, y1);
        }
        int dx = x1 - x0;
        int dy = y1 - y0;
        float derror = std::abs(dy/ static_cast<float>(dx));
        float error = 0;
        int y = y0;
        for (int x = x0; x <= x1; x++) {
            if (steep) {
                image.set(y, x, color);
            } else {
                image.set(x, y, color);
            }
            error += derror;
            if(error>.5) {
                y += (y1>y0?1:-1);
                error -= 1.;
            }
        }
    }

    void line_pixel_step_optimized2(int x0, int y0, int x1, int y1, TGAImage &image, const TGAColor &color) {
        bool steep = false;
        if (std::abs(x0 - x1) < std::abs(y0 - y1)) {
            std::swap(x0, y0);
            std::swap(x1, y1);
            steep = true;
        }
        if (x0 > x1) {
            std::swap(x0, x1);
            std::swap(y0, y1);
        }
        int dx = x1 - x0;
        int dy = y1 - y0;
        int derror2 = std::abs(dy)*2;
        int error2 = 0;
        int y = y0;
        for (int x = x0; x <= x1; x++) {
            if (steep) {
                image.set(y, x, color);
            } else {
                image.set(x, y, color);
            }
            error2 += derror2;
            if (error2 > dx) {
                y += (y1 > y0 ? 1 : -1);
                error2 -= dx * 2;
            }
        }
    }

    void line(const Vec2i &t0,const Vec2i &t1,TGAImage &image, const TGAColor &color) {
        line_pixel_step_optimized2(t0[0],t0[1],t1[0],t1[1],image,color);
    }
};

#endif //SOFTWARE_RENDERING_BRESENHAM_LINE_H
