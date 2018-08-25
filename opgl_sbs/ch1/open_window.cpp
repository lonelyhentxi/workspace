#include <GL/freeglut.h>
#include <iostream>

int main(int argc, char **argv)
{
    glutInit(&argc,argv);
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(960,480);
    glutCreateWindow("Chapter 01");
    glutDisplayFunc([](){
       glClear(GL_COLOR_BUFFER_BIT);
       glutSwapBuffers();
    });
    glutMainLoop();
    return 0;
}