#include "pch.h"
#include "Pax86Renderer.h"
#include "ck/ck.h"
#include "ck/config.h"
#include "ck/bank.h"
#include "ck/sound.h"
#include <thread>
#include <stdio.h>
#include <share.h>
#include <assert.h>

using namespace DirectX;
using namespace Microsoft::WRL;
using namespace Windows::Foundation;
using namespace Windows::UI::Core;

//===========================================================
// libretro API callbacks.
//===========================================================

static Microsoft::WRL::ComPtr<ID3D11Texture2D> retroTexture;
static Microsoft::WRL::ComPtr<ID3D11DeviceContext1> retroContext;

static Microsoft::WRL::ComPtr<IXAudio2> audio2;
static IXAudio2SourceVoice* sourceVoice;
static bool audioAvailable = false;

volatile bool running = false;
static retro_keyboard_event_t key_cb = 0;
static retro_audio_callback_t audio_cb = 0;
static const char *message = NULL;
static int16_t touchX = 0;
static int16_t touchY = 0;
static int16_t touchDown = 0;
static int16_t mouseLB = 0;
static int16_t mouseX = 0;
static int16_t mouseY = 0;
static int16_t mouseXInc = 0;
static int16_t mouseYInc = 0;
static int16_t oldMouseX = 0;
static int16_t oldMouseY = 0;
static char remyCallbackMessage[1024] = {"abcd"};
retro_audio_callback audiocallback;
int count = 0;

bool environmentCallBack(unsigned cmd, void* data){
	if(cmd == RETRO_ENVIRONMENT_SET_MESSAGE){
		struct retro_message *message;
	
		if(data != NULL){
		message = (struct retro_message*)data;
		if(message->msg != NULL)
			strcpy(remyCallbackMessage, message->msg);
		}
	}

	if(cmd == RETRO_ENVIRONMENT_SET_MESSAGE || cmd ==  RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK){
		audio_cb = ((struct retro_audio_callback *)data)->callback;
	}
	//callbackMessage = message->msg;	
	return true;
}

void retroAudioCallBack(){
	strcpy(remyCallbackMessage, "audio call back struct");
}
 
void write_little_endian(unsigned int word, int num_bytes, FILE *wav_file)
{
    unsigned buf;
    while(num_bytes>0)
    {   buf = word & 0xff;
        fwrite(&buf, 1,1, wav_file);
        num_bytes--;
    word >>= 8;
    }
}

void write_wav(const char * filename, unsigned long num_samples, const int16_t * data, int s_rate)
{
    FILE* wav_file;
    unsigned int sample_rate;
    unsigned int num_channels;
    unsigned int bytes_per_sample;
    unsigned int byte_rate;
    unsigned long i;    /* counter for samples */
 
    num_channels = 1;   /* monoaural */
    bytes_per_sample = 2;
 
    if (s_rate<=0) sample_rate = 44100;
    else sample_rate = (unsigned int) s_rate;
 
    byte_rate = sample_rate*num_channels*bytes_per_sample;
 
    wav_file = fopen(filename, "w");
    assert(wav_file);   /* make sure it opened */
 
    /* write RIFF header */
    fwrite("RIFF", 1, 4, wav_file);
    write_little_endian(36 + bytes_per_sample* num_samples*num_channels, 4, wav_file);
    fwrite("WAVE", 1, 4, wav_file);
 
    /* write fmt  subchunk */
    fwrite("fmt ", 1, 4, wav_file);
    write_little_endian(16, 4, wav_file);   /* SubChunk1Size is 16 */
    write_little_endian(1, 2, wav_file);    /* PCM is format 1 */
    write_little_endian(num_channels, 2, wav_file);
    write_little_endian(sample_rate, 4, wav_file);
    write_little_endian(byte_rate, 4, wav_file);
    write_little_endian(num_channels*bytes_per_sample, 2, wav_file);  /* block align */
    write_little_endian(8*bytes_per_sample, 2, wav_file);  /* bits/sample */
 
    /* write data subchunk */
    fwrite("data", 1, 4, wav_file);
    write_little_endian(bytes_per_sample* num_samples*num_channels, 4, wav_file);
    for (i=0; i< num_samples; i++)
    {   write_little_endian((unsigned int)(data[i]),bytes_per_sample, wav_file);
    }
 
    fclose(wav_file);
}

size_t audioCallBack(const int16_t *data, size_t frames)
{
	count++;
	strcpy(remyCallbackMessage, "audio call back");
	//FILE *file;
	auto local = Windows::Storage::ApplicationData::Current->LocalFolder;
	//auto local = Windows::Storage::KnownFolders::MusicLibrary;
    auto localFileNamePlatformString = local->Path + "\\file.wav"; //" + count + "
	std::string path(localFileNamePlatformString->Begin(), localFileNamePlatformString->End());
	
	/*file = fopen(path.c_str(), "a");
    auto res1 = fwrite(data, sizeof(data[0]), sizeof(data)/sizeof(data[0]), file);
    auto res2 = fclose(file);*/

	write_wav(path.c_str(), 1, data, 0);

	//file = NULL;

	//auto storageFile = local->GetFileAsync("file.wav");

	CkConfig config;
    CkInit(&config);
	//std::string path(localFileNamePlatformString->Begin(), localFileNamePlatformString->End());
	strcpy(remyCallbackMessage, path.c_str());
	CkSound* sound = CkSound::newStreamSound(path.c_str(), kCkPathType_FileSystem);
	if(sound != NULL)
	{
		sound->play();
		while (sound->isPlaying())
		{
			CkUpdate();
			std::this_thread::sleep_for(std::chrono::milliseconds(30));
		}    
		sound->destroy();
	}

    CkShutdown();

	return frames;
}

//------------------------------------------------------------
// retro_environment() is called by the libretro backend to
// setup some libretro environment values.
//------------------------------------------------------------
bool retro_environment(unsigned cmd, void *data)
{
	switch (cmd)
	{
		case RETRO_ENVIRONMENT_SET_PIXEL_FORMAT:
			return (RETRO_PIXEL_FORMAT_RGB565 == *((enum retro_pixel_format *)data));
		case RETRO_ENVIRONMENT_SHUTDOWN:
			//exit(0);	// Exit the program when the backend shuts down.
			return true;
		case RETRO_ENVIRONMENT_SET_KEYBOARD_CALLBACK:
			key_cb = ((struct retro_keyboard_callback *)data)->callback;
			return true;
		case RETRO_ENVIRONMENT_SET_AUDIO_CALLBACK:
			audio_cb = ((struct retro_audio_callback *)data)->callback;
			return true;
		case RETRO_ENVIRONMENT_SET_MESSAGE:
			message = ((struct retro_message *)data)->msg;
			//LOGI("retro_message: '%s'\n", message);
			return true;
	}
	return false;
}

//------------------------------------------------------------
// video_refresh() is called once per frame by the libretro
// backend to actually blit the screen image on the screen.
//------------------------------------------------------------
void video_refresh(const void *data, unsigned width, unsigned height, size_t pitch)
{
	if (CurScreenX != width || CurScreenY != height)
	{
		// Resize the texture area if/when the graphics mode has changed.
		CurScreenX = width;
		CurScreenY = height;
	}
	D3D11_BOX destRegion;
	destRegion.left = 0;
	destRegion.right = CurScreenX;
	destRegion.top = 0;
	destRegion.bottom = CurScreenY;
	destRegion.front = 0;
	destRegion.back = 1;
	retroContext->UpdateSubresource(
		retroTexture.Get(),
		0,
		&destRegion,
		data,
		pitch*2,
		pitch*2*1024
		);
}

//------------------------------------------------------------
// input_poll() is called once per frame by the libretro
// backend to make us refresh the keyboard/mouse state.
//------------------------------------------------------------
void input_poll()
{
	mouseX = mouseXInc<<2; mouseXInc = 0;
	mouseY = mouseYInc<<2; mouseYInc = 0;
}

//------------------------------------------------------------
// input_state() is called by the libretro backend to query
// input device (keybpard/mouse/joystick) status.
//------------------------------------------------------------
int16_t input_state(unsigned port, unsigned device, unsigned index, unsigned id)
{
	if (RETRO_DEVICE_MOUSE == device)
	{
		switch(id)
		{
			case RETRO_DEVICE_ID_MOUSE_LEFT:
				return mouseLB;
			case RETRO_DEVICE_ID_MOUSE_X:
				return mouseX;
			case RETRO_DEVICE_ID_MOUSE_Y:
				return mouseY;
		}
	}
	else if (RETRO_DEVICE_POINTER == device)
	{
		switch(id)
		{
			case RETRO_DEVICE_ID_POINTER_X:
				return touchX;
			case RETRO_DEVICE_ID_POINTER_Y:
				return touchY;
			case RETRO_DEVICE_ID_POINTER_PRESSED:
				return touchDown;
		}
	}
	return 0;
}

//------------------------------------------------------------
// Write one 128-sample block of silence. Helper routine for
// when the audio system is just starting up before we have a
// game running.
//------------------------------------------------------------
static short silent_buf[128] = {0};
static void silent_audio()
{
	XAUDIO2_BUFFER buffer = {0};
	memset(silent_buf, 0, sizeof(silent_buf));
	buffer.AudioBytes = 128*2;
	buffer.pAudioData = (BYTE *)silent_buf;
	sourceVoice->SubmitSourceBuffer( &buffer );
}

//------------------------------------------------------------
// audio_sample_batch() is called by the libretro backend to
// write the next batch of audio samples to the audio output.
//------------------------------------------------------------
size_t audio_sample_batch(const int16_t *data, size_t frames)
{
	XAUDIO2_BUFFER buffer = {0};
	buffer.AudioBytes = frames*2;
	buffer.pAudioData = (BYTE *)data;
	sourceVoice->SubmitSourceBuffer( &buffer );
	return frames;
}

class VoiceCallback : public IXAudio2VoiceCallback
{
public:
    void OnStreamEnd() {}
    void OnVoiceProcessingPassEnd() { }
    void OnVoiceProcessingPassStart(UINT32 SamplesRequired) { }
    void OnBufferEnd(void * pBufferContext)	{ }
    void OnBufferStart(void * pBufferContext) 
	{    
		// Let the libretro backend know that we are ready for the next audio buffer.
		if (running && audio_cb)
			audio_cb();		// Calls pax86retro.retro_audio_event(), which in turn calls audio_sample_batch()
		else
		{
			// The game is not running yet, so we need to send
			// some dummy data to the XAudio2 audio system to
			// keep it happy.
			silent_audio();
		}
	}
    void OnLoopEnd(void * pBufferContext) {    }
    void OnVoiceError(void * pBufferContext, HRESULT Error) { }
};

static VoiceCallback voiceCallback;

Pax86Renderer::Pax86Renderer() :
	m_loadingComplete(false),
	m_indexCount(0)
{
}

void Pax86Renderer::CreateTexture(int *buffer, int width, int height)
{
	if(buffer)
	{
		CD3D11_TEXTURE2D_DESC textureDesc(
			DXGI_FORMAT_B5G6R5_UNORM, //DXGI_FORMAT_B8G8R8A8_UNORM,
			static_cast<UINT>(width),
			static_cast<UINT>(height),
			1,
			1,
			D3D11_BIND_SHADER_RESOURCE
			);
		int pixelSize = sizeof(short); //sizeof(int); 
		D3D11_SUBRESOURCE_DATA data;
		data.pSysMem = buffer;
		data.SysMemPitch = pixelSize*width;
		data.SysMemSlicePitch =	pixelSize*width*height ;

		DX::ThrowIfFailed(
			m_d3dDevice->CreateTexture2D(
			&textureDesc,
			&data,
			&m_Texture
			)
			);


		m_d3dDevice->CreateShaderResourceView(m_Texture.Get(), NULL, &SRV); 

		D3D11_SAMPLER_DESC sampDesc;
		ZeroMemory( &sampDesc, sizeof(sampDesc) );
		sampDesc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
		sampDesc.AddressU = D3D11_TEXTURE_ADDRESS_WRAP;
		sampDesc.AddressV = D3D11_TEXTURE_ADDRESS_WRAP;
		sampDesc.AddressW = D3D11_TEXTURE_ADDRESS_WRAP;
		sampDesc.ComparisonFunc = D3D11_COMPARISON_NEVER;
		sampDesc.MinLOD = 0;
		sampDesc.MaxLOD = D3D11_FLOAT32_MAX;
		m_d3dDevice->CreateSamplerState( &sampDesc, &CubesTexSamplerState );
	}
}

void Pax86Renderer::CreateDeviceResources()
{
	Direct3DBase::CreateDeviceResources();

	auto loadVSTask = DX::ReadDataAsync("SimpleVertexShader.cso");
	auto loadPSTask = DX::ReadDataAsync("SimplePixelShader.cso");

	auto createVSTask = loadVSTask.then([this](Platform::Array<byte>^ fileData) {
		DX::ThrowIfFailed(
			m_d3dDevice->CreateVertexShader(
				fileData->Data,
				fileData->Length,
				nullptr,
				&m_vertexShader
				)
			);

		const D3D11_INPUT_ELEMENT_DESC vertexDesc[] = 
		{
			{ "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,  D3D11_INPUT_PER_VERTEX_DATA, 0 },
			{ "TEXCOORD",    0, DXGI_FORMAT_R32G32_FLOAT, 0, 12, D3D11_INPUT_PER_VERTEX_DATA, 0 },
		};

		DX::ThrowIfFailed(
			m_d3dDevice->CreateInputLayout(
				vertexDesc,
				ARRAYSIZE(vertexDesc),
				fileData->Data,
				fileData->Length,
				&m_inputLayout
				)
			);
	});

	auto createPSTask = loadPSTask.then([this](Platform::Array<byte>^ fileData) {
		DX::ThrowIfFailed(
			m_d3dDevice->CreatePixelShader(
				fileData->Data,
				fileData->Length,
				nullptr,
				&m_pixelShader
				)
			);

		CD3D11_BUFFER_DESC constantBufferDesc(sizeof(TextureSizeConstantBuffer), D3D11_BIND_CONSTANT_BUFFER);
		DX::ThrowIfFailed(
			m_d3dDevice->CreateBuffer(
				&constantBufferDesc,
				nullptr,
				&m_constantBuffer
				)
			);
	});

	auto createCubeTask = (createPSTask && createVSTask).then([this] () {
		Vertex cubeVertices[] =
		{
			// Front Face
			Vertex(-1.0f, -640.0f/800.0f, 0.0f, 1.0f, 1.0f),
			Vertex(-1.0f,  640.0f/800.0f, 0.0f, 0.0f, 1.0f),
			Vertex( 1.0f,  640.0f/800.0f, 0.0f, 0.0f, 0.0f),
			Vertex( 1.0f, -640.0f/800.0f, 0.0f, 1.0f, 0.0f),
		};

		D3D11_SUBRESOURCE_DATA vertexBufferData = {0};
		vertexBufferData.pSysMem = cubeVertices;
		vertexBufferData.SysMemPitch = 0;
		vertexBufferData.SysMemSlicePitch = 0;
		CD3D11_BUFFER_DESC vertexBufferDesc(sizeof(cubeVertices), D3D11_BIND_VERTEX_BUFFER);
		DX::ThrowIfFailed(
			m_d3dDevice->CreateBuffer(
				&vertexBufferDesc,
				&vertexBufferData,
				&m_vertexBuffer
				)
			);
		unsigned short cubeIndices[] = 
		{
			0,1,2,
			2,3,0,
		};
		m_indexCount = ARRAYSIZE(cubeIndices);

		D3D11_SUBRESOURCE_DATA indexBufferData = {0};
		indexBufferData.pSysMem = cubeIndices;
		indexBufferData.SysMemPitch = 0;
		indexBufferData.SysMemSlicePitch = 0;
		CD3D11_BUFFER_DESC indexBufferDesc(sizeof(cubeIndices), D3D11_BIND_INDEX_BUFFER);
		DX::ThrowIfFailed(
			m_d3dDevice->CreateBuffer(
				&indexBufferDesc,
				&indexBufferData,
				&m_indexBuffer
				)
			);
	});

	createCubeTask.then([this] () {
		m_loadingComplete = true;
	});

	//---------------------------------------
	// Create the texture we use for blitting.
	//---------------------------------------
	int *texture = (int *)calloc(1024*4, 1024);
	CreateTexture(texture, 1024, 1024);
	free(texture);

	//=======================================
	// Audio stuff
	//=======================================

    DX::ThrowIfFailed( XAudio2Create(&audio2, 0, XAUDIO2_DEFAULT_PROCESSOR) );
    HRESULT hr = audio2->CreateMasteringVoice(&m_musicMasteringVoice, 1, 32000);
    if (!FAILED(hr))
    {
		WAVEFORMATEX wfx;
		wfx.wFormatTag = WAVE_FORMAT_PCM;
		wfx.nChannels = 1;
		wfx.nSamplesPerSec = 32000;
		wfx.nAvgBytesPerSec = 32000*2;
		wfx.wBitsPerSample = 16;
		wfx.nBlockAlign = (wfx.nChannels*wfx.wBitsPerSample)/8;
		wfx.cbSize = 0;
		HRESULT hr = audio2->CreateSourceVoice( &sourceVoice, (WAVEFORMATEX*)&wfx, XAUDIO2_VOICE_NOSRC, XAUDIO2_DEFAULT_FREQ_RATIO, &voiceCallback, NULL, NULL );
		if( !FAILED(hr) )
		{
			// Send the first silent buffer to the sourceVoice 
			silent_audio();
			// Start the audio playing.
			hr = sourceVoice->Start( 0 );
			audioAvailable = !FAILED(hr);
		}
	}

	//=======================================
	// Start up the pax86retro system.
	//=======================================
	struct retro_game_info game;
	char path[512];

	//---------------------------------------
	// If we are already running, do nothing here.
	//---------------------------------------
	if (running)
		return;

	//---------------------------------------
	// Setup the game path
	//---------------------------------------

	

	//Windows::Storage::StorageFolder^ folder = Windows::Storage::ApplicationData::Current->LocalFolder;
	Windows::Storage::StorageFolder^ folder = Windows::ApplicationModel::Package::Current->InstalledLocation;
	auto wideData = folder->Path->Data();
	int bufferSize = folder->Path->Length() + 1;
	char *fpath = new char[bufferSize];
	WideCharToMultiByte(CP_UTF8, 0, wideData, -1, fpath, bufferSize, NULL, NULL);
	//sprintf_s(path, 255, "%s\\FRAGILE.COM", fpath);
	//sprintf_s(path, 255, "%s\\TREKMO.EXE", fpath);
	sprintf_s(path, 255, "%s\\Divil.bat", fpath);


	//---------------------------------------
	// Append the CD-ROM directory to the game path.
	//---------------------------------------
	strcat_s(path, 512, ";");
	strcat_s(path, 512, fpath);
	strcat_s(path, 512, "\\CD");

	game.path = path;

	//LOGI("native_start, fdir='%s', game.path='%s'\n", fpath, game.path);
	//---------------------------------------
	// Initialize the libretro stuff.
	//---------------------------------------
	
	retro_set_environment(environmentCallBack);
	//retro_set_environment(retro_environment);
	retro_init();
	retro_set_video_refresh(video_refresh);
	//retro_set_audio_sample_batch(audio_sample_batch);
	

	audiocallback.callback = retroAudioCallBack;

	retro_set_audio_sample_batch(audioCallBack);
	retro_set_input_poll(input_poll);
	retro_set_input_state(input_state);
	//---------------------------------------
	// Prepare the game for running.
	//---------------------------------------
	running = retro_load_game(&game);
}


void Pax86Renderer::CreateWindowSizeDependentResources()
{
	Direct3DBase::CreateWindowSizeDependentResources();

	float aspectRatio = m_windowBounds.Width / m_windowBounds.Height;
	float fovAngleY = 70.0f * XM_PI / 180.0f;
	if (aspectRatio < 1.0f)
	{
		fovAngleY /= aspectRatio;
	}
	m_constantBufferData.curScreenX = 640.0;
	m_constantBufferData.curScreenY = 480.0;
}

void Pax86Renderer::Update(float timeTotal, float timeDelta)
{
	(void) timeDelta; // Unused parameter.
	// Only draw the cube once it is loaded (loading is asynchronous).
	if (!SRV || !m_loadingComplete)
		return;
	// make the context and texture accessible to the libretro callback
	retroContext = m_d3dContext;
	retroTexture = m_Texture;
	if (running)
	{
		retro_run();
		// Update the texture size, in case retro_run has changed it.
		m_constantBufferData.curScreenX = (float)CurScreenX;
		m_constantBufferData.curScreenY = (float)CurScreenY;
	}
}

void Pax86Renderer::Render()
{
	const float midnightBlue[] = { 0.0f, 0.0f, 0.0f, 1.000f };
	m_d3dContext->ClearRenderTargetView(
		m_renderTargetView.Get(),
		midnightBlue
		);

	m_d3dContext->ClearDepthStencilView(
		m_depthStencilView.Get(),
		D3D11_CLEAR_DEPTH,
		1.0f,
		0
		);

	// Only draw the cube once it is loaded (loading is asynchronous).
	if (!SRV || !m_loadingComplete)
	{
		return;
	}

	m_d3dContext->OMSetRenderTargets(
		1,
		m_renderTargetView.GetAddressOf(),
		m_depthStencilView.Get()
		);


	m_d3dContext->UpdateSubresource(
		m_constantBuffer.Get(),
		0,
		NULL,
		&m_constantBufferData,
		0,
		0
		);

	UINT stride = sizeof(Vertex);
	UINT offset = 0;
	m_d3dContext->IASetVertexBuffers(
		0,
		1,
		m_vertexBuffer.GetAddressOf(),
		&stride,
		&offset
		);

	m_d3dContext->IASetIndexBuffer(
		m_indexBuffer.Get(),
		DXGI_FORMAT_R16_UINT,
		0
		);

	m_d3dContext->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);

	m_d3dContext->IASetInputLayout(m_inputLayout.Get());

	m_d3dContext->VSSetShader(
		m_vertexShader.Get(),
		nullptr,
		0
		);

	m_d3dContext->VSSetConstantBuffers(
		0,
		1,
		m_constantBuffer.GetAddressOf()
		);

	m_d3dContext->PSSetShader(
		m_pixelShader.Get(),
		nullptr,
		0
		);

	m_d3dContext->PSSetShaderResources( 0, 1, SRV.GetAddressOf());
	m_d3dContext->PSSetSamplers( 0, 1, &CubesTexSamplerState );

	m_d3dContext->DrawIndexed(
		m_indexCount,
		0,
		0
		);
}

#define TOUCH_MOUSE 0

void Pax86Renderer::MouseMoved(int x, int y)
{
#if TOUCH_MOUSE
	// We will get X coordinates between 0 and 480, and Y coordinates between 0 and 800.
	// We want to swap X and Y, and also map the coordinates between -0x7FFF and 0x7FFF.
	int tmpx = (y - 400) * 0x7FFF / 320;
	if (tmpx < -0x7FFF)
		touchX = -0x7FFF;
	else if (tmpx > 0x7FFF)
		touchX = 0x7FFF;
	else
		touchX = tmpx;
	touchY = (240 - x) * 0x7FFF / 240;
#else
	// We want to swap X and Y.
	mouseXInc += y;
	mouseYInc -= x;
#endif
}

void Pax86Renderer::MouseDown(int x, int y, bool down)
{
#if TOUCH_MOUSE
	MouseMoved(x, y);
	touchDown = down ? 1 : 0;
#else
	mouseLB = down ? 1 : 0;
#endif
}

Platform::String^ Pax86Renderer::TestString()
{
#if 0
	char str[1024];
#if 1
	sprintf_s(str, 1024, "mouseX=%d, mouseY=%d\n", mouseX, mouseY);
#else
	XAUDIO2_PERFORMANCE_DATA pdata;
	audio2->GetPerformanceData(&pdata);
	sprintf_s(str, 1024, "latency=%d, glitches=%d\n", pdata.CurrentLatencyInSamples, pdata.GlitchesSinceEngineStarted);
#endif
#else
	retro_system_info info;
	retro_get_system_info(&info);
    const char *str = info.library_name;
#endif

    DWORD dwNum = MultiByteToWideChar(CP_UTF8, 0, str, -1, NULL, 0);
    PWSTR wideText = new WCHAR[dwNum];
    MultiByteToWideChar(CP_UTF8, 0, str, -1, wideText, dwNum);
    Platform::String^ result = ref new Platform::String(wideText);
    return result;
}

Platform::String^ Pax86Renderer::GetCallBackMessage(){
	DWORD dwNum = MultiByteToWideChar(CP_UTF8, 0, remyCallbackMessage, -1, NULL, 0);
    PWSTR wideText = new WCHAR[dwNum];
	MultiByteToWideChar(CP_UTF8, 0, remyCallbackMessage, -1, wideText, dwNum);
    Platform::String^ result = ref new Platform::String(wideText);
	return result;
}

void Pax86Renderer::ButtonDown(int code)
{
	if (key_cb)
		key_cb(true, code, 0, 0);
}

void Pax86Renderer::ButtonUp(int code)
{
	if (key_cb)
		key_cb(false, code, 0, 0);
}
