;;; telega-i18n.el --- I18N for the telega  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Dec 11 02:03:42 2018
;; Keywords:

;; telega is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; telega is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with telega.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://translations.telegram.org

;;; Code:
(require 'telega-core)
(require 'telega-tdlib)
(require 'telega-customize)

(defcustom telega-i18n-plural-rule-functions
  (list (cons "en" 'telega-i18n-plural-rule-en)
        (cons "ru" 'telega-i18n-plural-rule-ru))
  "Alist of plural rules functions."
  :type 'alist
  :group 'telega)

(defconst telega-i18n--en-strings
  '(("lng_saved_messages"
     :value "Saved Messages")
    ("lng_profile_send_message"
     :value "Send Message")
    ("lng_profile_share_contact"
     :value "Share Contact")
    ("lng_profile_unblock_user"
     :value "Unblock User")
    ("lng_profile_block_user"
     :value "Block User")
    ("lng_profile_username"
     :value "Username:")
    ("lng_profile_mobile_number"
     :value "Phone:")
    ("lng_profile_bio"
     :value "Bio:")
    ("lng_profile_common_groups"
     :zero_value "No groups in common"
     :one_value "{count} group in common"
     :other_value "{count} groups in common")
    ("lng_scam_badge"
     :value "SCAM")
    ("lng_polls_anonymous"
     :value "Anonymous Poll")
    ("lng_polls_votes_count"
     :zero_value "No votes"
     :one_value "{count} vote"
     :other_value "{count} votes")
    ("lng_polls_stop_warning"
     :value "If you stop this poll now, nobody will be able to vote in it anymore. This action cannot be undone.")
    ("lng_deleted"
     :value "Deleted Account")
    ("lng_deleted_message"
     :value "Deleted Message")

    ("lng_settings_privacy_title"
     :value "Privacy")
    ("lng_blocked_list_title"
     :value "Blocked Users")
    ("lng_settings_section_privacy"
     :value "Privacy and Security")

    ("lng_action_created_chat"
     :value "{from} created the group «{title}»")
    ("lng_action_created_channel"
     :value "Channel created")
    ("lng_action_changed_title"
     :value "{from} renamed group to «{title}»")
    ("lng_action_changed_title_channel"
     :value "Channel renamed to «{title}»")
    ("lng_action_pinned_message"
     :value "{from} pinned «{text}»")

    ("lng_action_user_registered"
     :value "{from} joined Telegram")
    ("lng_action_user_joined"
     :value "{from} joined the group")
    ("lng_action_user_joined_by_link"
     :value "{from} joined the group via invite link")
    ("lng_action_user_left"
     :value "{from} left the group")
    ("lng_action_kick_user"
     :value "{from} removed {user}")

    ;; chatbuf modeline
    ("lng_chat_modeline_unread"
     :value "unread:{unread_count}")
    ("lng_chat_modeline_unread_help"
     :value "{mouse}: Read all messages")
    ("lng_chat_modeline_marked"
     :value "marked:{marked_count}")
    ("lng_chat_modeline_marked_help"
     :value "{mouse}: Unmark all messages")
    ("lng_chat_modeline_mention_help"
     :value "{mouse}: Goto next mention")
    ("lng_chat_modeline_pinned_msg_help"
     :value "{mouse}: Goto pinned message")
    ("lng_chat_modeline_members"
     :zero_value "{member_count} members"
     :other_value "{member_count} members, {count} online")

    ;; queries to user
    ("lng_action_cant_undone"
     :value "This action cannot be undone")
    ("lng_query_delete_chat"
     :value "Delete «{title}» chat? ")
    ("lng_query_read_chats"
     :one_value "Toggle read for {count} chat? "
     :other_value "Toggle read for {count} chats? ")
    ("lng_query_read_anyway"
     :value "No filtering applied, toggle anyway? ")
    ("lng_query_revoke_message"
     :value "Revoke the message? ")
    ("lng_query_kill_message"
     :value "Kill the message? ")
    ("lng_query_revoke_marked_messages"
     :one_value "Revoke {count} marked message? "
     :other_value "Revoke {count} marked messages? ")
    ("lng_query_kill_marked_messages"
     :one_value "Kill {count} marked message? "
     :other_value "Kill {count} marked messages? ")
    ("lng_query_dnd_photo_as_file"
     :value "Send this photo as a file? ")

    ("lng_settings_self_destruct"
     :value "Account self-destruct settings")
    ("lng_self_destruct_title"
     :value "Account self-destruction")
    ("lng_self_destruct_description"
     :value "If you don't come online at least once within this period, your account will be deleted along with all groups, messages and contacts.")
    ("lng_settings_destroy_if"
     :value "If away for...")
    ("lng_self_destruct_months"
     :one_value "{count} month"
     :other_value "{count} months")
    ("lng_self_destruct_years"
     :one_value "{count} year"
     :other_value "{count} years")

    ("lng_archived_add"
     :value "Archive chat")
    ("lng_archived_remove"
     :value "Unarchive chat")

    ("lng_status_online"
     :value "online")

    ("lng_report_spam"
     :value "Report Spam")
    ("lng_report_location"
     ;; To report chat with "UnrelatedLocation" reason 
     :value "Report Location")
    ("lng_new_contact_add"
     :value "Add contact")
    ("lng_new_contact_add_name"
     :value "Add {user} to contacts")
    ("lng_new_contact_block"
     :value "Block user")
    ("lng_new_contact_share"
     :value "Share my phone number")
    )
  "English language strings.")
(defvar telega-i18n--strings nil
  "Language strings for `telega-language'.")
(defvar telega-i18n--plural-func nil)

(defun telega-i18n-init ()
  "Initialize I18N subsystem."
  (if (equal telega-language "en")
      (setq telega-i18n--strings telega-i18n--en-strings
            telega-i18n--plural-func 'telega-i18n-plural-rule-en)

    ;; Asynchronously load `telega-language' strings
    (telega--getLanguagePackStrings telega-language nil
      (lambda (pack-strings)
        (setq telega-i18n--strings pack-strings)))

    ;; Asynchronously setup `telega-i18n--plural-func'
    (telega--getLanguagePackInfo telega-language
      (lambda (pack-info)
        (let ((plural-code (plist-get pack-info :plural_code)))
          (setq telega-i18n--plural-func
                (cdr (assoc plural-code telega-i18n-plural-rule-functions))))))
    ))

;; See https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html
(defun telega-i18n-plural-rule (n)
  "Apply plural rule corresponding N value.
Return one of: `:zero_value', `:one_value', `:two_value',
`:few_value', `:many_value' or `:other_value'."
  (or (and telega-i18n--plural-func
           (funcall telega-i18n--plural-func n))
      :other_value))

(defun telega-i18n-plural-rule-en (n)
  "Plural rules for English language."
  (cond ((and (= (% n 10) 1)
              (not (= (% n 100) 11)))
         :one_value)
        ((and (= (% n 10) 2)
              (not (= (% n 100) 12)))
         :two_value)
        ((and (= (% n 10) 3)
              (not (= (% n 100) 13)))
         :few_value)))

(defun telega-i18n-plural-rule-ru (n)
  "Plural rules for Russian language."
  (cond ((and (= (% n 10) 1)
              (not (= (% n 100) 11)))
         :one_value)
        ((and (memq (% n 10) '(2 3 4))
              (not (memq (% n 100) '(12 13 14))))
         :few_value)
        ((or (= (% n 10) 0)
             (memq (% n 10) '(5 6 7 8 9))
             (memq (% n 100) '(11 12 13 14)))
         :many_value)))

(defun telega-i18n (key &rest args)
  "Get I18N string for the KEY."
  (declare (indent 1))
  (setq key (concat "lng_" key))
  (let* ((str (or (cdr (assoc key telega-i18n--strings))
                  (cdr (assoc key telega-i18n--en-strings))))
         (val (or (telega-tl-str str :value)
                  (let ((count (plist-get args :count)))
                    (unless count
                      (error "\"%s\" is plural, `:count' is required" key))
                    (telega-tl-str str (telega-i18n-plural-rule count)))
                  (telega-tl-str str :other_value))))
    (while args
      (setq val (replace-regexp-in-string
                 (regexp-quote
                  (concat "{" (substring (symbol-name (car args)) 1) "}"))
                 (format "%s" (cadr args))
                 val nil 'literal))
      (setq args (cddr args)))
    val))

(provide 'telega-i18n)

;;; telega-i18n.el ends here
