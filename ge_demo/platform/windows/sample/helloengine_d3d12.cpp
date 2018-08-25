#include <cstdio>
#include <cstdint>
#include <string>
#include <exception>

#include <windows.h>
#include <windowsx.h>
#include <tchar.h>
#include <d3d12.h>
#include <DXGI1_4.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include <DirectXPackedVector.h>
#include <DirectXColors.h>
#include <wrl/client.h>

#include "d3dx12.h"

#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

namespace enfw
{
	namespace ge_demo
	{
		// assistant class for COM exceptions
		class com_exception: public std::exception
		{
		private:
			HRESULT result_;
		public:
			explicit com_exception(const HRESULT result):result_(result) {}

			virtual const char* what() const override
			{
				static char s_str[64] = { 0 };
				sprintf_s(s_str, "Failure with HRESULT of %o8X",
					static_cast<unsigned int>(result_));
				return s_str;
			}
		};

		// assistant func convert D3D API failures into exceptions
		inline void throw_if_failed(HRESULT result)
		{
			if (FAILED(result))
			{
				throw com_exception{result};
			}
		}
	}
}

const uint32_t n_screen_width = 960;
const uint32_t n_screen_height = 480;
const uint32_t n_frame_count = 2;
const bool be_wrap_device = true;

D3D12_VIEWPORT g_viewport = { 0.0f,0.0f,static_cast<float>(n_screen_width),static_cast<float>(n_screen_height) };
D3D12_RECT g_scissor_rect = { 0, 0 , n_screen_width, n_screen_height };

using namespace enfw::ge_demo;
using namespace DirectX;
using namespace DirectX::PackedVector;
using namespace Microsoft::WRL;
using namespace std;
ComPtr<IDXGISwapChain3> gp_swap_chain = nullptr;
ComPtr<ID3D12Device>	gp_device = nullptr;
ComPtr<ID3D12Resource> gp_render_targets[n_frame_count];
ComPtr<ID3D12CommandAllocator> gp_command_allocator;
ComPtr<ID3D12CommandQueue> gp_command_queue;
ComPtr<ID3D12RootSignature> gp_root_signature;
ComPtr<ID3D12DescriptorHeap> gp_descriptor_heap;
ComPtr<ID3D12PipelineState> gp_pipeline_state;
ComPtr<ID3D12CommandList> gp_command_list;

uint32_t gn_descriptor_size;

ComPtr<ID3D12Resource> gp_vertex_buffer;
D3D12_VERTEX_BUFFER_VIEW g_vertex_buffer_view;

uint32_t gn_frame_index;
HANDLE gh_fence_event;
ComPtr<ID3D12Fence> gp_fence;
uint32_t gn_fence_value;

struct vertex
{
	XMFLOAT3 position;
	XMFLOAT4 color;
};

wstring g_assets_path;

std::wstring get_asset_full_path(LPCWSTR asset_name)
{
	return g_assets_path + asset_name;
}

void get_assets_path(WCHAR *pth,UINT pth_size)
{
	if(pth==nullptr)
	{
		throw std::exception{};
	}
	DWORD size = GetModuleFileNameW(nullptr, pth, pth_size);
	if(size==0||size==pth_size)
	{
		throw std::exception{};
	}
	WCHAR *last_slash = wcsrchr(pth, L'\\');
	if(last_slash)
	{
		*(last_slash + 1) = L'\0';
	}
}

void wait_for_previous_frame()
{
	const uint64_t fence = gn_fence_value;
	throw_if_failed(gp_command_queue->Signal(gp_fence.Get(), fence));
	gn_fence_value++;

	if(gp_fence->GetCompletedValue() < fence)
	{
		throw_if_failed(gp_fence->SetEventOnCompletion(fence, gh_fence_event));
		WaitForSingleObject(gh_fence_event, INFINITE);
	}

	gn_frame_index = gp_swap_chain->GetCurrentBackBufferIndex();
}