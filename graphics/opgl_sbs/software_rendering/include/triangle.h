#ifndef SOFTWARE_RENDERING_TRIANGLE_H
#define SOFTWARE_RENDERING_TRIANGLE_H

#include "tgaimage.h"
#include "geometry.h"
#include "bresenham_line.h"
#include "util.h"
#include <limits>

namespace software_rendering {
    void triangle_line(Vec2i t0,Vec2i t1,Vec2i t2, TGAImage &image,const TGAColor &color) {
        line(t0,t1,image,color);
        line(t1,t2,image,color);
        line(t0,t2,image,color);
    }

    void triangle_zoning(Vec2i t0,Vec2i t1,Vec2i t2,TGAImage &image,const TGAColor &color) {
        using std::swap;
        if(t0.y>t1.y) swap(t0,t1);
        if(t0.y>t2.y) swap(t0,t2);
        if(t1.y>t2.y) swap(t1,t2);
        int total_height = t2.y - t0.y;
        for (int y=t0.y;y<t1.y;y++) {
            int segment_height = t1.y - t0.y + 1;
            float alpha = static_cast<float>(y-t0.y)/total_height;
            float beta = static_cast<float>(y-t0.y)/segment_height;
            Vec2i A = t0 + (t2-t0)*alpha;
            Vec2i B = t0 + (t1-t0)*beta;
            if(A.x>B.x) swap(A,B);
            for(int j=A.x;j<B.x;j++) {
                image.set(j,y,color);
            }
        }
        for(int y=t1.y;y<t2.y;y++) {
            int segment_height = t2.y - t1.y+1;
            float alpha = static_cast<float>(y-t0.y)/total_height;
            float beta = static_cast<float>(y-t1.y)/segment_height;
            Vec2i A = t0 + (t2-t0)*alpha;
            Vec2i B = t1 + (t2-t1)*beta;
            if (A.x>B.x) swap(A,B);
            for(int j=A.x;j<B.x;j++){
                image.set(j,y,color);
            }
        }
    }

    void triangle_zoning_merge(Vec2i t0,Vec2i t1,Vec2i t2,TGAImage &image,const TGAColor &color) {
        using std::swap;
        if(t0.y==t1.y&&t0.y==t2.y) return;
        if(t0.y>t1.y) swap(t0,t1);
        if(t0.y>t2.y) swap(t0,t2);
        if(t1.y>t2.y) swap(t1,t2);
        int total_height = t2.y - t0.y;
        for(int i=0;i<2;i++) {
            const auto &ts = (i==0?t0:t1);
            const auto &te = (i==0?t1:t2);
            for (int y=ts.y;y<te.y;y++) {
                int segment_height = te.y - ts.y + 1;
                float alpha = static_cast<float>(y-t0.y)/total_height;
                float beta = static_cast<float>(y-ts.y)/segment_height;
                Vec2i A = t0 + (t2-t0)*alpha;
                Vec2i B = ts + (te-ts)*beta;
                if(A.x>B.x) swap(A,B);
                for(int j=A.x;j<B.x;j++) {
                    image.set(j,y,color);
                }
            }
        }
    }

    template<typename Container>
    Vec3f barycentric(const Container &pts,const Vec2i &P) {
        Vec3f u = cross(Vec3f(pts[2].x-pts[0].x,pts[1].x-pts[0].x,pts[0].x-P.x),Vec3f(pts[2].y-pts[0].y,pts[1].y-pts[0].y,pts[0].y-P.y));
        if(std::abs(u[2])<1) return Vec3f(-1,1,1);
        return Vec3f(1.f-(u.x+u.y)/u.z, u.y/u.z, u.x/u.z);
    }

    template<typename Container>
    void triangle_simplify(const Container &pts,TGAImage &image,const TGAColor &color) {
        Vec2i bbox_min(image.get_width()-1,  image.get_height()-1);
        Vec2i bbox_max(0,0);
        Vec2i clamp(image.get_width()-1,image.get_height()-1);
        for(int i=0;i<3;i++){
            for(int j=0;j<2;j++){
                bbox_min[j] = std::max(0,std::min(bbox_min[j],pts[i][j]));
                bbox_max[j] = std::min(clamp[j],std::max(bbox_max[j],pts[i][j]));
            }
        }
        Vec2i P;
        for(P.x=bbox_min.x; P.x<=bbox_max.x;P.x++) {
            for(P.y=bbox_min.y;P.y<=bbox_max.y;P.y++) {
                Vec3f bc_screen = barycentric(pts,P);
                if(bc_screen.x<0||bc_screen.y<0||bc_screen.z<0) continue;
                image.set(P.x,P.y,color);
            }
        }
    }

    template<typename VecContainer>
    Vec3f barycentric(const VecContainer& pts,const Vec3f &P) {
        auto s = std::array<Vec3f,2>{};
        for(int i=2;i--;){
            s[i][0] = pts[2][i]-pts[0][i];
            s[i][1] = pts[1][i]-pts[0][i];
            s[i][2] = pts[0][i]-P[i];
        }
        Vec3f u = cross(s[0],s[1]);
        if(std::abs(u[2])>1e-2) return Vec3f(1.f-(u.x+u.y)/u.z,u.y/u.z,u.x/u.z);
        return Vec3f(-1,1,1);
    }

    template<typename VecContainer,typename ZBufferContainer>
    void triangle_culling(const VecContainer &pts,ZBufferContainer &zbuffer,
                          TGAImage &image,const TGAColor &color) {
        int width = image.get_width();
        Vec2f bbox_min{std::numeric_limits<float>::max(),std::numeric_limits<float>::max()};
        Vec2f bbox_max{-std::numeric_limits<float>::max(),-std::numeric_limits<float>::max()};
        Vec2f clamp{static_cast<float>(image.get_width()-1), static_cast<float>(image.get_height()-1)};
        for(int i=0;i<3;i++) {
            for(int j=0;j<2;j++) {
                bbox_min[j] = std::max(0.f,std::min<float>(bbox_min[j],pts[i][j]));
                bbox_max[j] = std::min(clamp[j],std::max<float>(bbox_max[j],pts[i][j]));
            }
        }
        Vec3f P;
        for(P.x=bbox_min.x;P.x<=bbox_max.x;P.x++) {
            for(P.y=bbox_min.y;P.y<=bbox_max.y;P.y++) {
                Vec3f bc_screen = barycentric(pts,P);
                if(bc_screen.x<0||bc_screen.y<0||bc_screen.z<0) continue;
                P.z = 0;
                for(int i=0;i<3;i++) {
                    P.z += pts[i][2]*bc_screen[i];
                }
                if(zbuffer[static_cast<int>(P.x+P.y*width)]<P.z) {
                    zbuffer[static_cast<int>(P.x+P.y*width)] = P.z;
                    image.set(static_cast<int>(P.x), static_cast<int>(P.y),color);
                }
            }
        }
    };

    template<typename ... Args>
    inline constexpr auto triangle(Args &&... args) -> decltype(triangle_culling(std::forward<Args>(args)...)){
        return triangle_culling(std::forward<Args>(args)...);
    }

    template<typename ... Args>
    inline constexpr auto triangle(Args &&... args) -> decltype(triangle_simplify(std::forward<Args>(args)...)){
        return triangle_simplify(std::forward<Args>(args)...);
    }
}

#endif //SOFTWARE_RENDERING_TRIANGLE_H
