#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>

#include <X11/Xlib.h>
#include <X11/Xlib-xcb.h>
#include <xcb/xcb.h>

#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>

#define GLX_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define GLX_CONTEXT_MINOR_VERSION_ARB 0x2092

typedef GLXContext (*glXCreateContextAttribsARBProc)(Display *, GLXFBConfig, GLXContext, Bool, const int *);

// Help to check for extension string presence.
static bool is_extension_supported(const char *ext_list, const char *extension) {
    const char *start;
    const char *where, *terminator;
    /* Extension names should not have spaces. */
    where = strchr(extension, ' ');
    if (where || *extension == '\0') {
        return false;
    }

    /* It takes a bit of care to be fool-proof about parsing the
     * OpenGL extensions string. Don't be fooled by sub-strings,
     * etc.
     */
    for (start = ext_list;;) {
        where = strstr(start, extension);
        if (!where) {
            break;
        }
        terminator = where + strlen(extension);
        if (where == start || *(where - 1) == ' ') {
            if (*terminator == ' ' || *terminator == '\0') {
                return true;
            }
        }
        start = terminator;
    }
    return false;
}

static bool ctx_error_occurred = false;

static int ctxErrorHandler(Display *dpy, XErrorEvent *ev) {
    ctx_error_occurred = true;
    return 0;
}

void draw_a_quad() {
    glClearColor(1.0, 1.0, 1.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-1., 1., -1., 1., 1., 20.);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0., 0., 10., 0., 0., 0., 0., 1., 0.);
    glBegin(GL_QUADS);
    glColor3f(1., 0., 0.);
    glVertex3f(-.75, -.75, 0.);
    glColor3f(0., 1., 0.);
    glVertex3f(.75, -.75, 0.);
    glColor3f(0., 0., 1.);
    glVertex3f(.75, .75, 0.);
    glColor3f(1., 1., 0.);
    glVertex3f(-.75, .75, 0.);
    glEnd();
}

int main(void) {
    xcb_connection_t *p_conn;
    xcb_screen_t *p_screen;
    xcb_window_t window;
    xcb_gcontext_t foreground;
    xcb_gcontext_t background;
    xcb_generic_event_t *p_event;
    xcb_colormap_t colormap;
    uint32_t mask = 0;
    uint32_t values[3];
    uint8_t is_quit = 0;

    char title[] = "Hello, Engine![OpenGL]";
    char title_icon[] = "Hello, Engine! (iconified)";

    Display *display;
    int default_screen;
    GLXContext context;
    GLXFBConfig *fb_configs;
    GLXFBConfig fb_config;
    int num_fb_configs = 0;
    XVisualInfo *vi;
    GLXDrawable drawable;
    GLXWindow glx_window;
    glXCreateContextAttribsARBProc glx_create_context_attribs_arb;
    const char *glx_exts;

    static int visual_attribs[] =
            {
                    GLX_X_RENDERABLE, True,
                    GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
                    GLX_RENDER_TYPE, GLX_RGBA_BIT,
                    GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR,
                    GLX_RED_SIZE, 8,
                    GLX_GREEN_SIZE, 8,
                    GLX_BLUE_SIZE, 8,
                    GLX_ALPHA_SIZE, 8,
                    GLX_DEPTH_SIZE, 24,
                    GLX_STENCIL_SIZE, 8,
                    // GLX_SAMPLE_BUFFERS, 1,
                    // GLX_SAMPLES, 4,
                    None
            };
    int glx_major, glx_minor;
    // open xlib display
    display = XOpenDisplay(nullptr);
    if (!display) {
        std::cerr << "Can't open display" << std::endl;
        return -1;
    }

    // FBConfig were added in GLX versions 1.3
    if (!glXQueryVersion(display, &glx_major, &glx_minor) ||
        ((glx_major == 1) && (glx_minor < 3)) || (glx_major < 1)) {
        std::cerr << "Invalid GLX version" << std::endl;
        return -1;
    }

    default_screen = DefaultScreen(display);
    /* Query frame buffer configuration */
    fb_configs = glXChooseFBConfig(display, default_screen, visual_attribs, &num_fb_configs);
    if (!fb_configs || num_fb_configs == 0) {
        std::cerr << "glXGetFBConfigs failed" << std::endl;
        return -1;
    }

    /* pick the fb config/visual with the most samples per pixel */
    {
        int best_fbc = -1, worst_fbc = -1, best_num_samp = -1, worst_num_samp = 999;
        for (int i = 0; i < num_fb_configs; ++i) {
            XVisualInfo *vi = glXGetVisualFromFBConfig(display, fb_configs[i]);
            if (vi) {
                int samp_buf, samples;
                glXGetFBConfigAttrib(display, fb_configs[i], GLX_SAMPLE_BUFFERS, &samp_buf);
                glXGetFBConfigAttrib(display, fb_configs[i], GLX_SAMPLES, &samples);
                printf("Matching fbconfig %d, visual ID 0x%lx; SAMPLE_BUFFERS = %d,"
                       "SAMPLES = %d\n",
                       i, vi->visualid, samp_buf, samples);
                if (best_fbc < 0 || (samp_buf && samples > best_num_samp)) {
                    best_fbc = i, best_num_samp = samples;
                }
                if (worst_fbc < 0 || !samp_buf || samples < worst_num_samp) {
                    worst_fbc = i, worst_num_samp = samples;
                }
            }
            XFree(vi);
        }
        fb_config = fb_configs[best_fbc];
    }

    // get a visual
    vi = glXGetVisualFromFBConfig(display, fb_config);
    printf("Chosen visual ID = 0x%lx\n", vi->visualid);

    // establish connection to X server
    p_conn = XGetXCBConnection(display);
    if (!p_conn) {
        XCloseDisplay(display);
        std::cerr << "Can't get xcb connection from display" << std::endl;
        return -1;
    }

    // acquire event queue ownership
    XSetEventQueueOwner(display, XCBOwnsEventQueue);

    // find xcb screen
    xcb_screen_iterator_t screen_iterator =
            xcb_setup_roots_iterator(xcb_get_setup(p_conn));
    for (int screen_num = vi->screen;
         screen_iterator.rem && screen_num > 0; --screen_num, xcb_screen_next(&screen_iterator));
    p_screen = screen_iterator.data;
    // get the root window
    window = p_screen->root;
    /* create XID's for colormap */
    colormap = xcb_generate_id(p_conn);
    xcb_create_colormap(p_conn, XCB_COLORMAP_ALLOC_NONE, colormap, window, vi->visualid);

    window = xcb_generate_id(p_conn);
    mask = XCB_CW_EVENT_MASK | XCB_CW_COLORMAP;
    values[0] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
    values[1] = colormap;
    values[2] = 0;
    xcb_create_window(p_conn, XCB_COPY_FROM_PARENT, window, p_screen->root, 20, 20, 640, 480, 10,
                      XCB_WINDOW_CLASS_INPUT_OUTPUT, vi->visualid, mask, values);
    XFree(vi);

    /** set the title of the window **/
    xcb_change_property(p_conn, XCB_PROP_MODE_REPLACE, window, XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8, strlen(title),
                        title);

    /** set the title of the window icon **/
    xcb_change_property(p_conn, XCB_PROP_MODE_REPLACE, window, XCB_ATOM_WM_ICON_NAME, XCB_ATOM_STRING, 8,
                        strlen(title_icon), title_icon);

    /** map the window on the screen **/
    xcb_map_window(p_conn, window);

    xcb_flush(p_conn);

    // get the default screen's GLX extension list
    glx_exts = glXQueryExtensionsString(display, default_screen);
    /** it is not necessary to create or make current to a context before calling
     * glXGetProAddressARB**/
    glx_create_context_attribs_arb = (glXCreateContextAttribsARBProc)
            glXGetProcAddressARB((const GLubyte *) "glXCreateContextAttribsARB");

    /** create opengl context**/
    ctx_error_occurred = false;
    int (*oldHandler)(Display *, XErrorEvent *)=
    XSetErrorHandler(&ctxErrorHandler);
    if (!is_extension_supported(glx_exts, "GLX_ARB_create_context") ||
        !glx_create_context_attribs_arb) {
        std::cout << "glXCreateAttribsARB() not found ...using old-style GLX context" << std::endl;
        context = glXCreateNewContext(display, fb_config, GLX_RGBA_TYPE, 0, True);
        if (context) {
            std::cerr << "glXCreateNewContext failed" << std::endl;
            return -1;
        }
    } else {
        int context_attribs[] = {
                GLX_CONTEXT_MAJOR_VERSION_ARB, 4,
                GLX_CONTEXT_MINOR_VERSION_ARB, 0,
                None
        };

        std::cout << "Creating context" << std::endl;
        context = glx_create_context_attribs_arb(display, fb_config, 0,
                                                 True, context_attribs);
        XSync(display, False);
        if (!ctx_error_occurred && context) {
            std::cout << "Create GL 3.0 context" << std::endl;
        } else {
            context_attribs[1] = 1;
            context_attribs[3] = 0;
            ctx_error_occurred = false;
            std::cout << "Failed to create GL 3.0 context" <<
                      "... using old-style GLX context" << std::endl;
            context = glx_create_context_attribs_arb(display, fb_config, 0,
                                                     True, context_attribs);
        }
    }

    XSync(display, False);
    XSetErrorHandler(oldHandler);
    if (ctx_error_occurred || !context) {
        std::cerr << "Failed to create an opengl context" << std::endl;
        return -1;
    }
    if (!glXIsDirect(display, context)) {
        std::cout << "Indirect GLX rendering context obtained" << std::endl;
    } else {
        std::cout << "Direct GLX rendering context obtained" << std::endl;
    }

    /** Create GLX Window **/
    glx_window = glXCreateWindow(display,
                                 fb_config, window, 0);
    if (!window) {
        xcb_destroy_window(p_conn, window);
        glXDestroyContext(display, context);
        std::cerr << "glXDestroyContext failed" << std::endl;
        return -1;
    }
    drawable = glx_window;

    /** make opengl conetxt current **/
    if (!glXMakeContextCurrent(display, drawable, drawable, context)) {
        xcb_destroy_window(p_conn, window);
        glXDestroyContext(display, context);
        std::cerr << "glXMakeContextCurrent failed" << std::endl;
        return -1;
    }
    while (!is_quit && (p_event = xcb_wait_for_event(p_conn))) {
        switch (p_event->response_type & ~0x80) {
            case XCB_EXPOSE: {
                draw_a_quad();
                glXSwapBuffers(display, drawable);
            }
                break;
            case XCB_KEY_PRESS:
                is_quit = 1;
                break;
        }
        free(p_event);
    }
    xcb_disconnect(p_conn);
    return 0;
}