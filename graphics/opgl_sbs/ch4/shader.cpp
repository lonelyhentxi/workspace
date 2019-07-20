#pragma once

#include <iostream>
#include <sstream>
#include <fstream>
#include <utility>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <glm/vec3.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/string_file.hpp>

GLuint VBO;

int main(int argc, char **argv) {
    namespace fs = boost::filesystem;
    using fs::path;
    path sub_root_dir = "D:\\workspace\\impls\\opgl_sbs\\ch4";
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowSize(1024, 768);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("chapter 04");
    glutDisplayFunc([]() {
        glClear(GL_COLOR_BUFFER_BIT);
        glEnableVertexAttribArray(0);
        glBindBuffer(GL_ARRAY_BUFFER, VBO);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glDisableVertexAttribArray(0);
        glutSwapBuffers();
    });
    GLenum res = glewInit();
    if(res!=GLEW_OK)
    {
        std::cerr<<"Error: "<<glewGetErrorString(res)<<"\n";
        return -1;
    }
    std::cout<<"GL version: "<< glGetString(GL_VERSION) <<std::endl;
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    auto create_vertex_buffer = [](){
        glm::vec3 vertices[3];
        vertices[0] = glm::vec3(-1.0f,-1.0f,0.0f);
        vertices[1] = glm::vec3(1.0f,-1.0f,0.0f);
        vertices[2] = glm::vec3(0.0f,1.0f,0.0f);
        glGenBuffers(1, &VBO);
        glBindBuffer(GL_ARRAY_BUFFER,VBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(vertices),vertices,GL_STATIC_DRAW);
    };
    create_vertex_buffer();
    auto add_shader = [](const GLuint &shader_program,const std::string &shader_text,const GLenum &shader_type){
        GLuint shader_obj = glCreateShader(shader_type);
        if(shader_obj == 0)
        {
            std::cerr<<"Error creating shader type "<<shader_type<<std::endl;
            exit(1);
        }
        const GLchar* p[1];
        p[0] = shader_text.c_str();
        GLint lengths[1];
        lengths[0] = static_cast<GLint>(strlen(shader_text.c_str()));
        glShaderSource(shader_obj, 1, p, lengths);
        glCompileShader(shader_obj);
        GLint success;
        glGetShaderiv(shader_obj,GL_COMPILE_STATUS,&success);
        if(!success)
        {
            GLchar info_log[1024];
            glGetShaderInfoLog(shader_obj,1024,nullptr,info_log);
            std::cerr<<"Error compiling shader type "<<shader_type<<": "<<info_log<<std::endl;
            exit(1);
        }
        glAttachShader(shader_program, shader_obj);
    };
    auto read_file = [](const std::string &pth) -> std::string {
        std::ifstream fst;
        fst.open(pth);
        if(!fst.is_open())
        {
            throw std::exception{"Error opening file\n"};
        }
        return std::string(std::istreambuf_iterator<char>(fst),std::istreambuf_iterator<char>());
    };
    const path vs_filename = sub_root_dir.string() + "/shader.vs.glsl";
    const path ps_filename = sub_root_dir.string() + "/shader.ps.glsl";
    auto compile_shaders = [&read_file,&add_shader,&vs_filename,&ps_filename](){
        GLuint shader_program = glCreateProgram();
        if (shader_program == 0) {
            std::cerr<<"Error creating shader program"<<std::endl;
            exit(1);
        }
        std::string vs,ps;
        try {
            vs = read_file(vs_filename.string());
            ps = read_file(ps_filename.string());
        } catch(std::exception &e) {
            std::cerr<<e.what()<<std::endl;
            exit(1);
        }
        add_shader(shader_program,vs,GL_VERTEX_SHADER);
        add_shader(shader_program, ps, GL_FRAGMENT_SHADER);
        GLint success;
        GLchar error_log[1024] = {0};
        glLinkProgram(shader_program);
        glGetProgramiv(shader_program, GL_LINK_STATUS, &success);
        if(!success)
        {
            glGetProgramInfoLog(shader_program,1024,nullptr,error_log);
            std::cerr<<"Error linking shader program: "<< error_log<<std::endl;
            exit(1);
        }
        glValidateProgram(shader_program);
        glGetProgramiv(shader_program,GL_VALIDATE_STATUS,&success);
        if(!success)
        {
            glGetProgramInfoLog(shader_program,1024,nullptr,error_log);
            std::cerr<<"Invalid shader program: "<< error_log<<std::endl;
            exit(1);
        }
        glUseProgram(shader_program);
    };
    compile_shaders();
    glutMainLoop();
    return 0;
}

