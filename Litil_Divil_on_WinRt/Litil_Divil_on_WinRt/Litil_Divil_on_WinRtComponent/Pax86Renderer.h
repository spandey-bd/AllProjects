#pragma once

#include "Direct3DBase.h"
#include <d3d11.h>
#include <xaudio2.h>
#include "libretro.h"

#define TEXTURE_WIDTH 1024
#define TEXTURE_HEIGHT 512
#define RGB565(r, g, b)  (((r) << (5+6)) | ((g) << 6) | (b))

extern "C" {
extern int MaxScreenX;
extern int MaxScreenY;
extern char remyCallbackMessage[];
}

static int CurScreenX = 640;
static int CurScreenY = 480;

struct TextureSizeConstantBuffer
{
	float curScreenX;
	float curScreenY;
	// Size must be divisible by 16 bytes!
	float dummy1;
	float dummy2;
};

struct Vertex	//Overloaded Vertex Structure
{
	Vertex(){}
	Vertex(float x, float y, float z,
		float u, float v)
		: pos(x,y,z), texCoord(u, v){}

	DirectX::XMFLOAT3 pos;
	DirectX::XMFLOAT2 texCoord;
};


bool environmentCallBack(unsigned cmd, void* data);
size_t audioCallBack(const int16_t *data, size_t frames);
void write_wav(const char * filename, unsigned long num_samples, const int16_t * data, int s_rate);
void write_little_endian(unsigned int word, int num_bytes, FILE *wav_file);

// This class renders a simple spinning cube.
ref class Pax86Renderer sealed : public Direct3DBase
{
public:
	Pax86Renderer();

	// Direct3DBase methods.
	virtual void CreateDeviceResources() override;
	virtual void CreateWindowSizeDependentResources() override;
	virtual void Render() override;
	
	// Method for updating time-dependent objects.
	void Update(float timeTotal, float timeDelta);

	void CreateTexture(int  *  buffer,int with,int height);
	void MouseMoved(int x, int y);
	void MouseDown(int x, int y, bool down);

	Platform::String^ TestString();
	Platform::String^ GetCallBackMessage();
	void ButtonDown(int code);
	void ButtonUp(int code);

private:
	
	bool								m_loadingComplete;

	Microsoft::WRL::ComPtr<ID3D11InputLayout> m_inputLayout;
	Microsoft::WRL::ComPtr<ID3D11Buffer> m_vertexBuffer;
	Microsoft::WRL::ComPtr<ID3D11Buffer> m_indexBuffer;
	Microsoft::WRL::ComPtr<ID3D11VertexShader> m_vertexShader;
	Microsoft::WRL::ComPtr<ID3D11PixelShader> m_pixelShader;
	Microsoft::WRL::ComPtr<ID3D11Buffer> m_constantBuffer;

	Microsoft::WRL::ComPtr<ID3D11Texture2D>		 m_Texture;
	Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> SRV;
	Microsoft::WRL::ComPtr<ID3D11SamplerState> CubesTexSamplerState;

    IXAudio2MasteringVoice*             m_musicMasteringVoice;

	uint32 m_indexCount;
	TextureSizeConstantBuffer m_constantBufferData;

};
