#include "vsoutput.hlsl"

float4 main(v2p input): SV_TARGET
{
	return input.color;
}