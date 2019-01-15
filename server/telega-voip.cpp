#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>

extern void telega_output(const char* otype, const char* json);
extern int telega_b64_pton(char const *src, u_char *target, size_t targsize);

#include <tgvoip/VoIPController.h>
#include <tgvoip/VoIPServerConfig.h>

using namespace std;
using namespace tgvoip;

static VoIPController* voip = NULL;
static int telega_voip_start(json11::Json start);
static int telega_voip_stop(json11::Json stop);

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

int
telega_voip_cmd(const char* json)
{
        std::string jsonString(json);
        std::string jsonError;
        json11::Json cmdjson = json11::Json::parse(jsonString, jsonError);
        if(!jsonError.empty()) {
                telega_output("error", "\"voip can't parse json\"");
                fprintf(stderr, "[telega-server/voip] ERR: can't parse json\n");
                return -1;
        }

        assert(cmdjson["@command"].is_string());
        std::string cmd = cmdjson["@command"].string_value();
        if (cmd == "start")
                return telega_voip_start(cmdjson);
        else if (cmd == "stop")
                return telega_voip_stop(cmdjson);
        else {
                char error[128];
                snprintf(error, 128, "\"voip invalid cmd `%s'\"", cmd.c_str());
                telega_output("error", error);
                return -1;
        }

        assert(false);
        /* NOT REACHED */
        return 0;
}

/*
 * Start new call specified by START
 * Return 0 if active call has been created
 */
static int
telega_voip_start(json11::Json start)
{
        if (voip != NULL) {
                telega_output("error", "\"Can't start voip, already have one\"");
                fprintf(stderr, "[telega-server/voip] "
                        "ERR: can't start voip, already have active call\n");
                return -1;
        }

        assert(voip == NULL);
        voip = new VoIPController();
        voip->SetConfig(voip_config);

        assert(start["server_config"].is_string());
        ServerConfig::GetSharedInstance()->Update(
                start["server_config"].string_value());
        
        assert(start["is_outgoing"].is_bool());
        bool is_outgoing = start["is_outgoing"].bool_value();
        assert(start["encryption_key"].is_string());
        const char* enc_key = start["encryption_key"].string_value().c_str();
        unsigned char ekey[256];
        telega_b64_pton (enc_key, ekey, 256);
        voip->SetEncryptionKey((char*)ekey, is_outgoing);

        vector<Endpoint> endpoints;
        for (const auto& pnt : start["endpoints"].array_items()) {
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
                endpoints.push_back(
                        Endpoint(id, port, ip, ipv6,
                                 Endpoint::Type::UDP_RELAY, ptag));
        }
        assert(start["allow_p2p"].is_bool());
        bool allow_p2p = start["allow_p2p"].bool_value();
        assert(start["max_layer"].is_number());
        int max_layer = start["max_layer"].int_value();
        assert(start["endpoints"].is_array());

        voip->SetRemoteEndpoints(endpoints, allow_p2p, max_layer);
        voip->Start();
        voip->Connect();
        return 0;
}

/*
 * Stop active call, if any
 * Return 0
 */
static int
telega_voip_stop(json11::Json stop)
{
        if (voip == NULL) {
                /* NOTE: no active call, not an error */
                return -0;
        }

        voip->Stop();
        delete voip;
        voip = NULL;
        return 0;
}
