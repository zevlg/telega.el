#include <sys/types.h>
#include <stdlib.h>
extern int telega_b64_pton (char const *src, u_char *target, size_t targsize);

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
 * Start new call specified by SETUP (json string)
 * Return 0 if active call has been created
 */
int
telega_voip_start(const char* setup)
{
        if (voip == NULL) {
                voip = new VoIPController();
                voip->SetConfig(voip_config);
        }

        fprintf(stderr, "voip-start: %s\n", setup);
        std::string jsonString(setup);
        std::string jsonError;
        json11::Json config = json11::Json::parse(jsonString, jsonError);
        if(!jsonError.empty()) {
                fprintf(stderr, "voip-start ERR: can't parse json\n");
                return -1;
        }

        assert(config["is_outgoing"].is_bool());
        bool is_outgoing = config["is_outgoing"].bool_value();
        assert(config["encryption_key"].is_string());
        const char* enc_key = config["encryption_key"].string_value().c_str();
        unsigned char ekey[256];
        telega_b64_pton (enc_key, ekey, 256);
        voip->SetEncryptionKey((char*)ekey, is_outgoing);

        assert(config["allow_p2p"].is_bool());
        bool allow_p2p = config["allow_p2p"].bool_value();
        assert(config["max_layer"].is_number());
        int max_layer = config["max_layer"].int_value();
        assert(config["endpoints"].is_array());

        vector<Endpoint> endpoints;
        for (const auto& pnt : config["endpoints"].array_items()) {
                assert(pnt["id"].is_string());
                uint64_t id = std::stoull(pnt["id"].string_value());
                assert(pnt["port"].is_number());
                int port = pnt["port"].int_value();
                assert(pnt["ip"].is_string());
                IPv4Address ip = IPv4Address(pnt["ip"].string_value());
                assert(pnt["ipv6"].is_string());
                IPv6Address ipv6 = IPv6Address(pnt["ipv6"].string_value());
                assert(pnt["peer_tag"].is_string());
                const char* peer_tag = pnt["peer_tag"].string_value().c_str();

                unsigned char ptag[16];
                telega_b64_pton (peer_tag, ptag, 16);
                fprintf(stderr, "PTAG: %s\n", ptag);
                endpoints.push_back(
                        Endpoint(id, port, ip, ipv6,
                                 Endpoint::Type::UDP_RELAY, ptag));
        }
        voip->SetRemoteEndpoints(endpoints, allow_p2p, max_layer);
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
                fprintf(stderr, "voip-stop ERR: No active call to stop\n");
                return -1;
        }

        voip->Stop();
        // delete voip;
        // voip = NULL;
        return 0;
}
