#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <xcb/xcb.h>

int main(void) {
    xcb_generic_event_t *p_event;
    uint32_t mask = 0;
    uint32_t values[2];
    uint8_t is_quit = 0;
    char title[] = "Hello, Engine!";
    char title_icon[] = "Hello, Engine! (iconified)";
    /* establish connection to X server */
    xcb_connection_t *p_conn = xcb_connect(0, 0);
    /* get the first screen */
    xcb_screen_t *p_screen = xcb_setup_roots_iterator(xcb_get_setup(p_conn)).data;
    /* get the foot window */
    xcb_window_t window = p_screen->root;
    /* create black foreground graphic context */
    xcb_gcontext_t  foreground = xcb_generate_id(p_conn);
    mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
    values[0] = p_screen -> black_pixel;
    values[1] = 0;
    xcb_create_gc(p_conn,foreground,window,mask,values);
    /* create white background graphic context */
    xcb_gcontext_t background = xcb_generate_id(p_conn);
    mask = XCB_GC_BACKGROUND | XCB_GC_GRAPHICS_EXPOSURES;
    values[0] = p_screen -> white_pixel;
    values[1] = 0;
    xcb_create_gc(p_conn,background,window,mask,values);
    /* create window */
    window = xcb_generate_id(p_conn);
    mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
    values[0] = p_screen -> white_pixel;
    values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
    xcb_create_window(p_conn // connection
            , XCB_COPY_FROM_PARENT //depth
            , window // window id
            , p_screen->root // parent
            , 20, 20 // x, y
            , 640, 480 // width, height
            , 10 // border width
            , XCB_WINDOW_CLASS_INPUT_OUTPUT // class
            , p_screen -> root_visual // visual
            , mask, values // masks
    );
    // set the title of the window
    xcb_change_property(p_conn,XCB_PROP_MODE_REPLACE,window
            ,XCB_ATOM_WM_NAME,XCB_ATOM_STRING,8
            ,strlen(title),title);
    // set the title of the window icon
    xcb_change_property(p_conn,XCB_PROP_MODE_REPLACE,window
            ,XCB_ATOM_WM_ICON_NAME,XCB_ATOM_STRING,8
            ,strlen(title_icon),title_icon);
    // map the window on the screen
    xcb_map_window(p_conn,window);
    xcb_flush(p_conn);
    while((p_event = xcb_wait_for_event(p_conn)) && !is_quit) {
        switch (p_event->response_type & ~0x80) {
            case XCB_EXPOSE:
                {
                    xcb_rectangle_t rect = {20,20,60,80};
                    xcb_poly_fill_rectangle(p_conn,window,foreground,1,&rect);
                    xcb_flush(p_conn);
                }
                break;
            case XCB_KEY_PRESS:
                is_quit = true;
                break;
        }
        free(p_event);
    }
    xcb_disconnect(p_conn);
    return 0;
}
