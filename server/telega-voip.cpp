#include <tgvoip/VoIPController.h>
#include <tgvoip/VoIPServerConfig.h>

using namespace std;
using namespace tgvoip;

static VoIPController* voip = NULL;

static VoIPController::Config voip_config {
        30.0,                   /* init_timeout */
        30.0,                   /* recv_timeout */
        DATA_SAVING_NEVER,      /* data_saving */
        true,                   /* enableAEC */
        true,                   /* enableNS */
        true,                   /* enableAGC */
        false,                  /* enableCallUpgrade */
};


void
telega_voip_update_config(const char* data)
{
        ServerConfig::GetSharedInstance()->Update(std::string(data));
}

const char*
telega_voip_version(void)
{
        return VoIPController::GetVersion();
}

/*
 * Return 0 if active call has been created
 */
int
telega_voip_start(const char* setup)
{
        if (voip != NULL) {
                fprintf(stderr, "TELEGA-VOIP: Can't start new call, already have one\n");
                return -1;
        }

        voip = new VoIPController();
        voip->SetConfig(voip_config);
        // voip->SetEncryptionKey(enc_key, is_outgoing);
        // voip->SetRemoteEndpoints(endpoints, allow_p2p, max_layer);
        voip->Start();
        voip->Connect();
        return 0;
}

/*
 * Stop active call
 * Return 0 if ok
 */
int
telega_voip_stop(void)
{
        if (voip == NULL) {
                fprintf(stderr, "TELEGA-VOIP: No active call to stop\n");
                return -1;
        }

        voip->Stop();
        delete voip;
        voip = NULL;
        return 0;
}
