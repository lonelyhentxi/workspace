/**
Tiny Renderer, https://github.com/ssloy/tinyrenderer
Copyright Dmitry V. Sokolov

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the use of this software.
Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it freely,
subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
*/

#ifndef SOFTWARE_RENDERING_MODEL_H
#define SOFTWARE_RENDERING_MODEL_H

#include <vector>
#include "geometry.h"

namespace software_rendering {
    class Model {
    private:
        std::vector<Vec3f> verts_;
        std::vector<std::vector<int> > faces_;
    public:
        Model(const char *filename);
        ~Model();
        int nverts();
        int nfaces();
        Vec3f vert(int i);
        std::vector<int> face(int idx);
    };
}

#endif //SOFTWARE_RENDERING_MODEL_H
