cbuffer TextureSizeConstantBuffer : register(b0)
{
	float curScreenX;
	float curScreenY;
	float dummy1;
	float dummy2;
};

struct VertexShaderInput
{
    float4 pos : POSITION;
    float2 tex : TEXCOORD0;
};

struct VertexShaderOutput
{
	float4 pos : SV_POSITION;
    float2 tex : TEXCOORD0;
};

VertexShaderOutput main(VertexShaderInput input)
{
	VertexShaderOutput output;

	// Map output directly to screen coordinates without any transformations.
	output.pos = input.pos;

    // Store the texture coordinates for the pixel shader.
    output.tex = float2(input.tex.x * curScreenX / 1024.0, input.tex.y * curScreenY / 1024.0);

	return output;
}
