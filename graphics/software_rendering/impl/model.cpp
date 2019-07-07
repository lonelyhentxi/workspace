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

#include <iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <exception>
#include "model.h"

namespace software_rendering {
    Model::Model(const char *filename) : verts_(), faces_() {
        std::ifstream in;
        in.open (filename, std::ifstream::in);
        if (in.fail()) {
            throw std::runtime_error{"fail to open file"};
        }
        std::string line;
        while (!in.eof()) {
            std::getline(in, line);
            std::istringstream iss(line.c_str());
            char trash;
            if (!line.compare(0, 2, "v ")) {
                iss >> trash;
                Vec3f v;
                for (int i=0;i<3;i++) iss >> v[i];
                verts_.push_back(v);
            } else if (!line.compare(0, 2, "f ")) {
                std::vector<int> f;
                int itrash, idx;
                iss >> trash;
                while (iss >> idx >> trash >> itrash >> trash >> itrash) {
                    idx--; // in wavefront obj all indices start at 1, not zero
                    f.push_back(idx);
                }
                faces_.push_back(f);
            }
        }
        std::cout << "# v# " << verts_.size() << " f# "  << faces_.size() << std::endl;
    }

    Model::~Model() {
    }

    int Model::nverts() {
        return (int)verts_.size();
    }

    int Model::nfaces() {
        return (int)faces_.size();
    }

    std::vector<int> Model::face(int idx) {
        return faces_[idx];
    }

    Vec3f Model::vert(int i) {
        return verts_[i];
    }
}