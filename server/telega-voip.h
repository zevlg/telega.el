#pragma once

void telega_voip_update_config(const char* data);
const char* telega_voip_version(void);
int telega_voip_start(const char* setup);
int telega_voip_stop(void);
