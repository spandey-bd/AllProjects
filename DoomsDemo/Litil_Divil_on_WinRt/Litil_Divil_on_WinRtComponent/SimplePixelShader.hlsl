Texture2D shaderTexture;
SamplerState SampleType;

struct PixelShaderInput
{
    float4 position : SV_POSITION;
    float2 tex : TEXCOORD0;
};

float4 main(PixelShaderInput input) : SV_TARGET
{
    // Sample the pixel color from the texture using the sampler at this texture coordinate location.
    return shaderTexture.Sample(SampleType, input.tex);
}
