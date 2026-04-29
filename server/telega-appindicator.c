/*
 * telega-appindicator.c --- Bridge between Emacs and TDLib.
 *
 * Copyright (C) 2016-2020 by Zajcev Evgeny
 *
 * Author: Zajcev Evgeny <zevlg@yandex.ru>
 *
 * telega is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * telega is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with telega.  If not, see <http://www.gnu.org/licenses/>.
 *
 ** Commentary:
 *
 * Support for telega icon in System Tray using libappindicator.
 *
 * Adds "appindicator" command:
 *  - appindicator setup <svg-icon:str>"
 *  - appindicator status <active|passive>"
 *  - appindicator label <label:str>"
 *
 * Can send "appindicator-event" when menu buttons are clicked.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <libgen.h>

#ifdef WITH_AYATANA_APPINDICATOR
#include <libayatana-appindicator/app-indicator.h>
#else
#include <libappindicator/app-indicator.h>
#endif

extern void telega_output(const char* otype, const char* str);

static GMainLoop* loop = NULL;
static GtkWidget* menu = NULL;
static AppIndicator* appind = NULL;

static void
appindicator_click_cb(GtkWidget *widget, gpointer data)
{
        const gchar* text = (const gchar*)data;
        if (text)
                telega_output("appindicator-event", text);
}

/* Split a full icon path into a heap-allocated directory and name
 * without extension.  Caller must free both *dir_out and *name_out. */
static void
appindicator_split_icon_path(const char* icon_path,
                             char** dir_out, char** name_out)
{
        char* tmp = strdup(icon_path);
        g_assert(tmp != NULL);
        *dir_out = strdup(dirname(tmp));
        free(tmp);

        tmp = strdup(icon_path);
        g_assert(tmp != NULL);
        *name_out = strdup(basename(tmp));
        free(tmp);

        g_assert(*dir_out != NULL && *name_out != NULL);

        char* dot = strrchr(*name_out, '.');
        if (dot)
                *dot = '\0';
}

static void
appindicator_set_icon(const char* icon_path)
{
        char *dir, *name;
        appindicator_split_icon_path(icon_path, &dir, &name);
        app_indicator_set_icon_theme_path(appind, dir);
        app_indicator_set_icon_full(appind, name, "telega icon");
        free(dir);
        free(name);
}

static void
appindicator_setup(char* icon_path)
{
        if (!appind) {
                /* Resolve the theme name + path before creating the
                 * indicator so it never appears with a placeholder
                 * icon, even briefly. */
                char *dir, *name;
                appindicator_split_icon_path(icon_path, &dir, &name);
                appind = app_indicator_new("telega", name,
                                           APP_INDICATOR_CATEGORY_COMMUNICATIONS);
                g_assert(appind != NULL);
                app_indicator_set_icon_theme_path(appind, dir);
                free(dir);
                free(name);

                app_indicator_set_status(appind, APP_INDICATOR_STATUS_ACTIVE);

                menu = gtk_menu_new();
                GtkWidget* item = gtk_menu_item_new_with_label("Open telega");
                g_signal_connect(item, "activate",
                                 G_CALLBACK(appindicator_click_cb), "\"open\"");
                gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
                gtk_widget_show(item);

                item = gtk_separator_menu_item_new();
                gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
                gtk_widget_show(item);

                item = gtk_menu_item_new_with_label("Quit telega");
                g_signal_connect(item, "activate",
                                 G_CALLBACK(appindicator_click_cb), "\"quit\"");
                gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
                gtk_widget_show(item);

                app_indicator_set_menu(appind, GTK_MENU(menu));
        } else {
                app_indicator_set_status(appind, APP_INDICATOR_STATUS_ACTIVE);
                appindicator_set_icon(icon_path);
                app_indicator_set_label(appind, "", "");
        }
}

static int
appindicator_cmd(void* data)
{
        char* cmd = (char*)data;
        char* cmd_args = strchr(cmd, ' ');
        if (cmd_args)
                cmd_args += 1;

        if (cmd_args == NULL) {
                /* All commands requires args */
                fprintf(stderr, "[telega-appindicator]: Invalid cmd -> %s\n",
                        cmd);
        } else if (!strncmp(cmd, "setup ", 6)) {
                /* setup <icon-path:str> */
                appindicator_setup(cmd_args);
        } else if (!strncmp(cmd, "status ", 7)) {
                /* status <active|passive> */
                if (appind) {
                        AppIndicatorStatus status = APP_INDICATOR_STATUS_PASSIVE;
                        if (!strcmp(cmd_args, "active"))
                                status = APP_INDICATOR_STATUS_ACTIVE;
                        app_indicator_set_status(appind, status);
                }
        } else if (!strncmp(cmd, "label ", 6)) {
                /* label <label:str> */
                if (appind)
                        app_indicator_set_label(appind, cmd_args, cmd_args);
        } else if (!strncmp(cmd, "icon ", 5)) {
                if (appind)
                        appindicator_set_icon(cmd_args);
        } else {
                fprintf(stderr, "[telega-appindicator]: unknown cmd -> %s\n",
                        cmd);
        }

        free(data);
        return 0;
}

void
telega_appindicator_send(const char* json)
{
        char* json_copy = strdup(json);
        g_idle_add(appindicator_cmd, json_copy);
}

bool
telega_appindicator_init(void)
{
        return gtk_init_check(NULL, NULL);
}

void*
telega_appindicator_loop(void* data)
{
        loop = g_main_loop_new(NULL, FALSE);
        g_main_loop_run(loop);
        return NULL;
}

void
telega_appindicator_stop(void)
{
        if (loop)
                g_main_loop_quit(loop);
}
