#ifndef SOFTWARE_RENDERING_TRIANGLE_H
#define SOFTWARE_RENDERING_TRIANGLE_H

#include "tgaimage.h"
#include "geometry.h"
#include "bresenham_line.h"

namespace software_rendering {
    void triangle_simple(Vec2i t0,Vec2i t1,Vec2i t2, TGAImage &image,const TGAColor &color) {
        line(t0,t1,image,color);
        line(t1,t2,image,color);
        line(t1,t2,image,color);
    }

    void triangle_zoning(Vec2i t0,Vec2i t1,Vec2i t2,TGAImage &image,const TGAColor &color) {
        using std::swap;
        if(t0.y>t1.y) swap(t0,t1);
        if(t0.y>t2.y) swap(t0,t2);
        if(t1.y>t2.y) swap(t1,t2);
        int total_height = t2.y - t0.y;
        for(int y=t0.y; y<=t1.y;y++) {
            int segment_height = t1.y - t0.y + 1;
            float alpha = static_cast<float>(y-t0.y)/total_height;
            float beta = static_cast<float>(y-t0.y)/segment_height;
            Vec2i A = t0 + (t2 - t0) * alpha;
        }
    }
}

#endif //SOFTWARE_RENDERING_TRIANGLE_H
