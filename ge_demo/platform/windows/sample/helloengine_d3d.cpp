#include <windows.h>
#include <windowsx.h>
#include <tchar.h>

#include <cstdint>

#include <d3d11.h>
#include <d3d11_1.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include <DirectXPackedVector.h>
#include <DirectXColors.h>

using namespace DirectX;
using namespace DirectX::PackedVector;

const uint32_t SCREEN_WIDTH = 960;
const uint32_t SCREEN_HEIGHT = 480;

// global declarations
// swap chain interface
IDXGISwapChain* gp_swap_chain = nullptr;
// direct3d device interface
ID3D11Device* gp_dev = nullptr;
// direct3d device context interface
ID3D11DeviceContext* gp_devcon = nullptr;
ID3D11RenderTargetView* gp_rtview = nullptr;
// input layout
ID3D11InputLayout* gp_ip_layout = nullptr;
// vertex shader
ID3D11VertexShader* gp_vs = nullptr;
// pixel shader
ID3D11PixelShader* gp_ps = nullptr;
// vertex buffer
ID3D11Buffer* gp_buffer = nullptr;

// vertex buffer structure
struct VERTEX
{
	XMFLOAT3 position;
	XMFLOAT4 color;
};

template <typename T>
inline void safe_release(T** pp_interface_release)
{
	if ((*pp_interface_release) != nullptr)
	{
		(*pp_interface_release)->Release();
		(*pp_interface_release) = nullptr;
	}
}

void create_render_target()
{
	HRESULT hr;
	ID3D11Texture2D* p_back_buffer;

	// get a pointer to back buffer
	gp_swap_chain->GetBuffer(0, __uuidof(ID3D11Texture2D), (LPVOID *)&p_back_buffer);
	// create a render-target view
	gp_dev->CreateRenderTargetView(p_back_buffer, nullptr, &gp_rtview);
	p_back_buffer->Release();
	// bind the view
	gp_devcon->OMSetRenderTargets(1, &gp_rtview, nullptr);
}

void set_viewport()
{
	D3D11_VIEWPORT viewport;
	ZeroMemory(&viewport, sizeof(D3D11_VIEWPORT));
	viewport.TopLeftX = 0;
	viewport.TopLeftY = 0;
	viewport.Width = SCREEN_WIDTH;
	viewport.Height = SCREEN_HEIGHT;
	gp_devcon->RSSetViewports(1, &viewport);
}

// this is the function that loads and prepares the shaders
void init_pipeline()
{
	// load and compile the two shaders
	ID3DBlob *vs, *ps;
	D3DReadFileToBlob(L"copy.vso", &vs);
	D3DReadFileToBlob(L"copy.pso", &ps);
	// encapsulate both shaders into shader objects
	gp_dev->CreateVertexShader(vs->GetBufferPointer(),
	                           vs->GetBufferSize(), nullptr, &gp_vs);
	gp_dev->CreatePixelShader(ps->GetBufferPointer(),
	                          ps->GetBufferSize(), nullptr, &gp_ps);
	// set the shader object
	gp_devcon->VSSetShader(gp_vs, nullptr, 0);
	gp_devcon->PSSetShader(gp_ps, nullptr, 0);
	// create the input layout object
	D3D11_INPUT_ELEMENT_DESC ied[] =
	{
		{"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0},
		{"COLOR", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0}
	};

	gp_dev->CreateInputLayout(ied, 2, vs->GetBufferPointer(),
	                          vs->GetBufferSize(), &gp_ip_layout);
	gp_devcon->IASetInputLayout(gp_ip_layout);

	vs->Release();
	ps->Release();
}

// create shape to render
void init_graphics()
{
	// create a triangle using the VERTEX struct
	VERTEX our_vertices[] =
	{
		{XMFLOAT3(0.0f, 0.5f, 0.0f), XMFLOAT4(1.0f, 0.0f, 0.0f, 1.0f)},
		{XMFLOAT3(0.45f, -0.5, 0.0f), XMFLOAT4(0.0f, 1.0f, 0.0f, 1.0f)},
		{XMFLOAT3(-0.45f, -0.5f, 0.0f), XMFLOAT4(0.0f, 0.0f, 1.0f, 1.0f)}
	};
	// create the vertex buffer
	D3D11_BUFFER_DESC bd;
	ZeroMemory(&bd, sizeof(bd));
	bd.Usage = D3D11_USAGE_DYNAMIC; // write access access by CPU and GPU
	bd.ByteWidth = sizeof(VERTEX) * 3; // size is the VERTEX struct * 3
	bd.BindFlags = D3D11_BIND_VERTEX_BUFFER; // use as a vertex buffer
	bd.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE; // allow CPU to write in buffer
	gp_dev->CreateBuffer(&bd, nullptr, &gp_buffer);
	// copy the vertices into the buffer
	D3D11_MAPPED_SUBRESOURCE ms;
	gp_devcon->Map(gp_buffer, NULL, D3D11_MAP_WRITE_DISCARD, NULL, &ms);
	memcpy(ms.pData, our_vertices, sizeof(VERTEX) * 3);
	gp_devcon->Unmap(gp_buffer, NULL);
}

// this function prepare graphics resources for use
HRESULT create_graphics_resources(HWND h_wnd)
{
	HRESULT hr = S_OK;
	if (gp_swap_chain == nullptr)
	{
		// create a struct to hold information about the swap chain
		DXGI_SWAP_CHAIN_DESC scd;
		// clear out the struct for use
		ZeroMemory(&scd, sizeof(DXGI_SWAP_CHAIN_DESC));
		// fill the swap chain description struct
		scd.BufferCount = 1; // one back buffer
		scd.BufferDesc.Width = SCREEN_WIDTH;
		scd.BufferDesc.Height = SCREEN_HEIGHT;
		scd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; // use 32-bit color
		scd.BufferDesc.RefreshRate.Numerator = 60;
		scd.BufferDesc.RefreshRate.Denominator = 1;
		scd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT; // how swap chain is to be used
		scd.OutputWindow = h_wnd; // the window to be used
		scd.SampleDesc.Count = 4; // how many multisamples
		scd.Windowed = TRUE; // windowed/full-screen mode
		scd.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH; // allow full-screen switching

		const D3D_FEATURE_LEVEL feature_levels[] = {
			D3D_FEATURE_LEVEL_11_1,
			D3D_FEATURE_LEVEL_11_0,
			D3D_FEATURE_LEVEL_10_1,
			D3D_FEATURE_LEVEL_10_0,
			D3D_FEATURE_LEVEL_9_3,
			D3D_FEATURE_LEVEL_9_2,
			D3D_FEATURE_LEVEL_9_1
		};
		D3D_FEATURE_LEVEL feature_level_supported;

		HRESULT hr = S_OK;
		//  create a device, device context and swap chain using the information in the scd struct
		hr = D3D11CreateDeviceAndSwapChain(nullptr,
		                                   D3D_DRIVER_TYPE_HARDWARE,
		                                   nullptr,
		                                   0,
		                                   feature_levels,
		                                   _countof(feature_levels),
		                                   D3D11_SDK_VERSION,
		                                   &scd,
		                                   &gp_swap_chain,
		                                   &gp_dev,
		                                   &feature_level_supported,
		                                   &gp_devcon
		);
		if (hr == E_INVALIDARG)
		{
			hr = D3D11CreateDeviceAndSwapChain(nullptr,
			                                   D3D_DRIVER_TYPE_HARDWARE,
			                                   nullptr, 0,
			                                   &feature_level_supported,
			                                   1, D3D11_SDK_VERSION, &scd,
			                                   &gp_swap_chain, &gp_dev, nullptr,
			                                   &gp_devcon);
		}
		if(hr==S_OK)
		{
			create_render_target();
			set_viewport();
			init_pipeline();
			init_graphics();
		}
	}
	return hr;
}


void discard_graphics_resources()
{
	safe_release(&gp_ip_layout);
	safe_release(&gp_vs);
	safe_release(&gp_ps);
	safe_release(&gp_swap_chain);
	safe_release(&gp_buffer);
	safe_release(&gp_rtview);
	safe_release(&gp_dev);
	safe_release(&gp_devcon);
}

// render a single frame
void render_frame()
{
	// clear the back buffer to a deep blue
	const FLOAT clear_color[] = { 0.0f, 0.2f, 0.4f, 1.0f };
	gp_devcon->ClearRenderTargetView(gp_rtview, clear_color);
	// do 3D render on the back buffer here
	{
		// select which vertex buffer to display
		UINT stride = sizeof(VERTEX);
		UINT offset = 0;
		gp_devcon->IASetVertexBuffers(0,1, &gp_buffer, &stride, &offset);
		// select which primtive type we are using
		gp_devcon->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		// draw the back buffer and the front buffer
		gp_devcon->Draw(3, 0);
	}

	// swap the back buffer and the front buffer
	gp_swap_chain->Present(0, 0);
}

// the window proc func prototype
LRESULT CALLBACK window_proc(HWND h_wnd, UINT message,
	WPARAM w_param, LPARAM l_param);

// the entry point for any window program
int WINAPI WinMain(HINSTANCE h_instance,HINSTANCE prev_instance,
	LPTSTR lp_cmd_line,int n_cmd_show)
{
	// the handle for the window, filled by a function
	HWND h_wnd;
	// the struct holds information for the window class
	WNDCLASSEX wc;
	// clear the window class for use
	ZeroMemory(&wc, sizeof(WNDCLASSEX));

	// fill in the struct with the needed information
	wc.cbSize = sizeof(WNDCLASSEX);
	wc.style = CS_HREDRAW | CS_VREDRAW;
	wc.lpfnWndProc = window_proc;
	wc.hInstance = h_instance;
	wc.hCursor = LoadCursor(nullptr, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)COLOR_WINDOW;
	wc.lpszClassName = _T("WindowClass1");
	// register the window class
	RegisterClassEx(&wc);
	// create the window and use the result as the handle
	h_wnd = CreateWindowEx(0,
		_T("WindowClass1"),
		_T("Hello, Engine![Direct 3D]"),
		WS_OVERLAPPEDWINDOW,
		100, 100,
		SCREEN_WIDTH, SCREEN_HEIGHT,
		nullptr, nullptr, h_instance, nullptr);
	// display the window on the screen
	ShowWindow(h_wnd, n_cmd_show);
	//  enter the main loop
	// hold the windows event message
	MSG msg;
	// wait for the next message in the queue, store the result in 'msg'
	while(GetMessage(&msg,nullptr,0,0))
	{
		// translate keystoke message into the right format
		TranslateMessage(&msg);
		// send the message to the WindowPro function
		DispatchMessage(&msg);
	}
	// return the part to the windowProc  function 
	return msg.wParam;
}

// main message handle for the program 
LRESULT CALLBACK window_proc(HWND h_wnd, UINT message, WPARAM w_param, LPARAM l_param)
{
	LRESULT result = 0;
	bool was_handled = false;

	// sort through and find what code to run the message given 
	switch(message)
	{
	case WM_CREATE:
		was_handled = true;
		break;
	case WM_PAINT:
		result = create_graphics_resources(h_wnd);
		render_frame();
		was_handled = true;
		break;
	case WM_SIZE:
		if(gp_swap_chain!=nullptr)
		{
			discard_graphics_resources();
		}
		was_handled = true;
		break;
	case WM_DESTROY:
		discard_graphics_resources();
		PostQuitMessage(0);
		was_handled = true;
		break;
	case WM_DISPLAYCHANGE:
		InvalidateRect(h_wnd, nullptr, false);
		was_handled = true;
		break;
	}

	// handle any message the switch statement didn't
	if(!was_handled)
	{
		result = DefWindowProc(h_wnd, message, w_param, l_param);
	}
	return result;
}