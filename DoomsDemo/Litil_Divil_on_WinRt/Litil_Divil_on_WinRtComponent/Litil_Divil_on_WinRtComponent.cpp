#include "pch.h"
#include "Litil_Divil_on_WinRtComponent.h"
#include "Direct3DContentProvider.h"

using namespace Windows::Foundation;
using namespace Windows::UI::Core;
using namespace Microsoft::WRL;
using namespace Windows::Phone::Graphics::Interop;
using namespace Windows::Phone::Input::Interop;

namespace PhoneDirect3DXamlAppComponent
{

Direct3DBackground::Direct3DBackground() :
	m_timer(ref new BasicTimer())
{
}

IDrawingSurfaceBackgroundContentProvider^ Direct3DBackground::CreateContentProvider()
{
	ComPtr<Direct3DContentProvider> provider = Make<Direct3DContentProvider>(this);
	return reinterpret_cast<IDrawingSurfaceBackgroundContentProvider^>(provider.Detach());
}

// IDrawingSurfaceManipulationHandler
void Direct3DBackground::SetManipulationHost(DrawingSurfaceManipulationHost^ manipulationHost)
{
	manipulationHost->PointerPressed +=
		ref new TypedEventHandler<DrawingSurfaceManipulationHost^, PointerEventArgs^>(this, &Direct3DBackground::OnPointerPressed);

	manipulationHost->PointerMoved +=
		ref new TypedEventHandler<DrawingSurfaceManipulationHost^, PointerEventArgs^>(this, &Direct3DBackground::OnPointerMoved);

	manipulationHost->PointerReleased +=
		ref new TypedEventHandler<DrawingSurfaceManipulationHost^, PointerEventArgs^>(this, &Direct3DBackground::OnPointerReleased);
}

#define TOUCH_MOUSE 0

// Event Handlers
void Direct3DBackground::OnPointerPressed(DrawingSurfaceManipulationHost^ sender, PointerEventArgs^ args)
{
#if TOUCH_MOUSE
	m_mousePrevX = m_mouseX - args->CurrentPoint->Position.X;
	m_mousePrevY = m_mouseY - args->CurrentPoint->Position.Y;
#else
	m_mousePrevX = args->CurrentPoint->Position.X;
	m_mousePrevY = args->CurrentPoint->Position.Y;
#endif
	m_pointerDown = true;
}

void Direct3DBackground::OnPointerMoved(DrawingSurfaceManipulationHost^ sender, PointerEventArgs^ args)
{
	if (m_pointerDown)
	{
#if TOUCH_MOUSE
		m_mouseX = args->CurrentPoint->Position.X;
		m_mouseY = args->CurrentPoint->Position.Y;
#else
		m_mouseX = args->CurrentPoint->Position.X - m_mousePrevX;
		m_mouseY = args->CurrentPoint->Position.Y - m_mousePrevY;
		m_mousePrevX = args->CurrentPoint->Position.X;
		m_mousePrevY = args->CurrentPoint->Position.Y;
#endif
		if (m_renderer)
			m_renderer->MouseMoved(m_mouseX, m_mouseY);
	}
}

void Direct3DBackground::OnPointerReleased(DrawingSurfaceManipulationHost^ sender, PointerEventArgs^ args)
{
	m_pointerDown = false;
}

// Interface With Direct3DContentProvider
HRESULT Direct3DBackground::Connect(_In_ IDrawingSurfaceRuntimeHostNative* host, _In_ ID3D11Device1* device)
{
	m_renderer = ref new Pax86Renderer();
	m_renderer->Initialize(device);
	m_renderer->UpdateForWindowSizeChange(WindowBounds.Width, WindowBounds.Height);
	// Restart timer after renderer has finished initializing.
	m_timer->Reset();

	return S_OK;
}

void Direct3DBackground::Disconnect()
{
	m_renderer = nullptr;
}

HRESULT Direct3DBackground::PrepareResources(_In_ const LARGE_INTEGER* presentTargetTime, _Inout_ DrawingSurfaceSizeF* desiredRenderTargetSize)
{
	m_timer->Update();
	m_renderer->Update(m_timer->Total, m_timer->Delta);

	desiredRenderTargetSize->width = RenderResolution.Width;
	desiredRenderTargetSize->height = RenderResolution.Height;

	return S_OK;
}

HRESULT Direct3DBackground::Draw(_In_ ID3D11Device1* device, _In_ ID3D11DeviceContext1* context, _In_ ID3D11RenderTargetView* renderTargetView)
{
	m_renderer->UpdateDevice(device, context, renderTargetView);

	m_renderer->Render();

	RequestAdditionalFrame();

	return S_OK;
}

Platform::String^ Direct3DBackground::TestString()
{
	return m_renderer->TestString();
}

void Direct3DBackground::ButtonDown(int code)
{
	m_renderer->ButtonDown(code);
}
void Direct3DBackground::ButtonUp(int code)
{
	m_renderer->ButtonUp(code);
}
void Direct3DBackground::MouseButtonDown()
{
	m_renderer->MouseDown(m_mouseX, m_mouseY, true);
}
void Direct3DBackground::MouseButtonUp()
{
	m_renderer->MouseDown(m_mouseX, m_mouseY, false);
}

}