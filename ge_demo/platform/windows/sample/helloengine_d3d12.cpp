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
