#ifndef SOFTWARE_RENDERING_TRIANGLE_H
#define SOFTWARE_RENDERING_TRIANGLE_H

#include "tgaimage.h"
#include "typedefs.h"
#include "bresenham_line.h"

void triangle_simple(Vec2i t0,Vec2i t1,Vec2i t2, TGAImage &image,const TGAColor &color) {
    line(t0,t1,image,color);
    line(t1,t2,image,color);
    line(t1,t2,image,color);
}

void triangle_zoning(Vec2i t0,Vec2i t1,Vec2i t2,TGAImage &image,const TGAColor &color) {
    using std::swap;
    if(t0[1]>t1[1]) swap(t0,t1);
    if(t0[1]>t2[1]) swap(t0,t2);
    if(t1[1]>t2[1]) swap(t1,t2);
    int total_height = t2[1] - t0[1];
    for(int y=t0[1]; y<=t1[1];y++) {
        int segment_height = t1[1] - t0[1] + 1;
        float alpha = static_cast<float>(y-t0[1])/total_height;
        float beta = static_cast<float>(y-t0[1])/segment_height;
        Vec2i A = t0 + (t2 - t0)
    }
}

#endif //SOFTWARE_RENDERING_TRIANGLE_H
