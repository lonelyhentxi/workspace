#pragma once

#include <iostream>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include <glm/vec3.hpp>

GLuint VBO;

int main(int argc, char **argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
    glutInitWindowSize(480, 320);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("chapter 03");
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
    glutMainLoop();
    return 0;
}

