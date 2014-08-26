#include "ck/ck.h"
#include "ck/config.h"
#include "ck/bank.h"
#include "ck/sound.h"
#include <thread>

ref class AppView sealed : public Windows::ApplicationModel::Core::IFrameworkView
{
public:
	// IFrameworkView Methods.
	virtual void Initialize(Windows::ApplicationModel::Core::CoreApplicationView^ applicationView) {}
	virtual void SetWindow(Windows::UI::Core::CoreWindow^ window) {}
	virtual void Load(Platform::String^ entryPoint) {}
	virtual void Run()
    {
        CkConfig config;
        CkInit(&config);

		CkSound* sound = CkSound::newStreamSound("hellocricket.cks");

		//if(sound->isReady())
		{
			sound->play();
		}

        while (sound->isPlaying())
        {
            CkUpdate();
            std::this_thread::sleep_for(std::chrono::milliseconds(30));
        }

        sound->destroy();

        CkShutdown();
    }
	virtual void Uninitialize() {}
};


ref class AppSource sealed : public Windows::ApplicationModel::Core::IFrameworkViewSource {
public:
    virtual Windows::ApplicationModel::Core::IFrameworkView^ CreateView() 
    {
        return ref new AppView();
    }
};


[Platform::MTAThread]
int main(Platform::Array<Platform::String^>^)
{
    Windows::ApplicationModel::Core::CoreApplication::Run(ref new AppSource());
    return 0;
}

