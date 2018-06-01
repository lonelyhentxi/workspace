// include the basic windows header file
#include <windows.h>
#include <windowsx.h>
#include <tchar.h>

// the windowProc function prototype
LRESULT CALLBACK WindowProc(HWND h_wnd, UINT message, WPARAM w_param, LPARAM l_param);

// the entry point for any windows program
int WINAPI WinMain(HINSTANCE h_instance, HINSTANCE h_prev_instance, LPTSTR lp_cmd_line, int n_cmd_show) {
    // the handle for the window, filled by a function
    HWND h_wnd;
    // the struct holds information for the window class
    WNDCLASSEX wc;
    // clear out the window class for use
    ZeroMemory(&wc, sizeof(WNDCLASSEX));
    // fill in the struct with the needed information
    wc.cbSize = sizeof(WNDCLASSEX);
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = h_instance;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH) COLOR_WINDOW;
    wc.lpszClassName = _T("WindowClass1");

    // register the window class
    RegisterClassEx(&wc);

    // create the window and use the result as the handle
    h_wnd = CreateWindowEx(0
            , _T("WindowClass1") // name of the window class
            , _T("Hello, Engine!") // title of the window
            , WS_OVERLAPPEDWINDOW // window style
            , 300 // x-position of the window
            ,300 // y-position of the window
            ,500 // width of the window
            ,400 // height of the window
            ,NULL // parent of the window
            ,NULL // its menu
            ,h_instance // application handle
            , NULL // used with multiple window
    );

    // display the window on the screen
    ShowWindow(h_wnd,n_cmd_show);

    // enter the main loop

    // this struct holds window event message
    MSG msg;

    // wait for the next message in the queue, store the result in 'msg'
    while(GetMessage(&msg,NULL,0,0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return msg.wParam;
}

// this is the main message handler for the program
LRESULT CALLBACK WindowProc(HWND h_wnd, UINT message, WPARAM w_param, LPARAM l_param)
{
    // sort through and find what code to run for the message given
    switch (message)
    {
        case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(h_wnd,&ps);
            RECT rec = {20,20,60,80};
            HBRUSH brush = (HBRUSH) GetStockObject(BLACK_BRUSH);
            FillRect(hdc,&rec,brush);
            EndPaint(h_wnd,&ps);
        }
        break;
        // this message is read when window is closed
        case WM_DESTROY:
        {
            // close the application entirely
            PostQuitMessage(0);
            return 0;
        }
        break;
    }

    // handle any message the switch statement didn't
    return DefWindowProc(h_wnd,message,w_param,l_param);
}