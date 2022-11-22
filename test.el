;;; test.el --- Testing routines for telega  -*- lexical-binding:t -*-
(require 'telega)

;; Setup
(setq telega-language "en")
(telega-i18n-init)

(telega--init-vars)
(telega--info-update
 `(:@type "supergroup" :id 11110 :usernames (:@type "usernames" :active_usernames ["noname"] :editable_username "noname")
          :status (:@type "chatMemberStatusAdministrator")))
(telega--info-update
 `(:@type "user" :id 22220 :first_name "Vasya" :last_name "Pupkin"
          :usernames (:@type "usernames" :active_usernames ["vpupkin"]
                             :editable_username "vpupkin")
          :phone_number "71112233" :status (:@type "userStatusOnline" :expires 1527776835)
          :is_contact t
          :is_mutual_contact t
          :is_verified nil
          :restriction_reason ""
          :have_access t :type (:@type "userTypeRegular") :language_code ""))

(telega--info-update
 `(:@type "user" :id 22221 :first_name "Petya" :last_name "Siskin"
          :phone_number "71112234" :status (:@type "userStatusOnline" :expires 1527776836)
          :is_contact t
          :is_mutual_contact t
          :is_verified nil
          :restriction_reason ""
          :have_access t :type (:@type "userTypeRegular") :language_code ""))
(telega--info-update
 '(:@type "supergroup" :id 1263892563
          :username (:@type "usernames" :active_usernames ["PremiumSignalsForward"] :editable_username "PremiumSignalsForward") :date 1523303192 :status (:@type "chatMemberStatusLeft") :member_count 54837 :anyone_can_invite nil :sign_messages nil :is_channel t :is_verified nil :restriction_reason ""))

(setq telega--ordered-chats
      `((:@type "chat"
                :id 1111
                :type
                (:@type "chatTypeSupergroup"
                        :supergroup_id 11110 :is_channel t)
                :title "test channel1"
                :positions [(:@type "chatPosition" :list (:@type "chatListMain") :order "22" :is_pinned t)]
                :unread_count 25
                :unread_mention_count 0
                :is_pinned nil
                :notification_settings
                (:@type "notificationSettings"
                        :mute_for 623207729
                        :sound "" :show_preview nil)
                :last_message (:@type "message" :id 44444 :sender_user_id 2220 :chat_id 1111 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users t :is_channel_post nil :contains_unread_mention nil :date 1531136430 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 0 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text "Синтезатор как максимум" :entities [])))
                )
        (:@type "chat"
                :id 22222
                :type
                (:@type "chatTypePrivate"
                        :user_id 22220 :is_channel t)
                :title "test channel1"
                :positions [(:@type "chatPosition" :list (:@type "chatListMain") :order "11" :is_pinned t)]
                :unread_count 25
                :unread_mention_count 0
                :is_pinned nil
                :notification_settings
                (:@type "notificationSettings"
                        :mute_for 623207729
                        :sound "" :show_preview nil)
                :last_message (:@type "message" :id 44442 :sender_user_id 3330 :chat_id 22222 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users t :is_channel_post nil :contains_unread_mention nil :date 1531136430 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 0 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text "Синтезатор как максимум" :entities [])))
                )
        
        ;; TODO: add more chats
        (:@type "chat" :id -1001263892563 :type (:@type "chatTypeSupergroup" :supergroup_id 1263892563 :is_channel t) :title "Premium Signals Forward" :photo (:@type "chatPhoto" :small (:@type "file" :id 2213 :size 0 :expected_size 0 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AQADAQATkvwKMAAEDMzuE4el7_5oeAEAAQI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 0)) :big (:@type "file" :id 2214 :size 0 :expected_size 0 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AQADAQATkvwKMAAEandjHTisoJpqeAEAAQI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 0))) :positions []
 :is_marked_as_unread nil :is_sponsored nil :can_be_reported t :default_disable_notification nil :unread_count 0 :last_read_inbox_message_id 0 :last_read_outbox_message_id 2251799812636672 :unread_mention_count 0 :notification_settings (:@type "chatNotificationSettings" :use_default_mute_for t :mute_for 0 :use_default_sound t :sound "" :use_default_show_preview t :show_preview nil) :reply_markup_message_id 0 :client_data "(:color (\"#849b34\" \"#596823\" \"#2c3311\"))" :uaprops (:color ("#849b34" "#596823" "#2c3311")) :last_message (:@type "message" :id 599785472 :sender_user_id 0 :chat_id -1001263892563 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users nil :is_channel_post t :contains_unread_mention nil :date 1547562092 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 26731 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text "Buy GTO between 755-765 satoshi.

Breaking out resistance risk level medium. 

Targets 9%,18%,27%,40%.

Have Stoploss 690 Satoshi." :entities []))))
        ))
(dolist (chat telega--ordered-chats)
  (puthash (plist-get chat :id) chat telega--chats))

(setq telega-tdlib--chat-filters
      '((:@type "chatFilterInfo" :id 2 :title "Emacs" :icon_name "")
        (:@type "chatFilterInfo" :id 3 :title #("\ud83d\ude39\ud83d\ude39\ud83d\ude39" 0 2 (telega-display "😹" telega-emoji-p t) 2 4 (telega-display "😹" telega-emoji-p t) 4 6 (telega-display "😹" telega-emoji-p t)) :icon_name "")))


;; Tests
(ert-deftest telega-emacs-setup ()
  "Test emacs is suitable to run telega."
  (should (telega-test-env 'quiet)))

(ert-deftest telega-info ()
  "Test info related functionality."
  (let ((user1 (telega-user-get 22220))
        (user2 (telega-user-get 22221)))
    (should (string= (telega-user-title user1) "Vasya Pupkin @vpupkin"))
    (should (string= (telega-user-title user1 'full) "Vasya Pupkin @vpupkin"))
    (should (string= (telega-user-title user1 'name) "Vasya Pupkin"))
    (should (string= (telega-user-title user1 'short) "@vpupkin"))

    ;; test fallback to 'name if username is empty
    (should (string= (telega-user-title user2 'short) "Petya Siskin"))
  ))

(ert-deftest telega-filters ()
  "Test `telega-filter' functionality."
  (should (not (null telega--ordered-chats)))
  (should (null (telega-chat-match-p 10 '(not all))))
  ;; NOTE: one chat (id=-1001263892563) has "0" order
  (should (= (length (telega-filter-chats telega--ordered-chats '(main)))
             (1- (length telega--ordered-chats))))
  (should (telega-chat-match-p 10 '(not or)))
  (should (telega-chat-match-p (car telega--ordered-chats) '(type channel)))
  (should (telega-chat-match-p
           (car telega--ordered-chats) '(all (type channel) (name "chan"))))
  (should-not
   (telega-chat-match-p
    (car telega--ordered-chats) '(name "notmatching")))

  ;; Test for `telega-filter-active-tdlib-chat-list'
  (should (equal (telega-filter-active-tdlib-chat-list)
                 '(:@type "chatListMain")))
  (let* ((telega--filters '(((chat-list "😹😹😹")) (main))))
    (should (equal (telega-filter-active-tdlib-chat-list)
                   '(:@type "chatListFilter" :chat_filter_id 3))))

  ;; TODO: add more filter tests
  )

(ert-deftest telega-desurrogate ()
  "Tests for `telega--desurrogate-apply'"
  (let ((tests '((#("\uD83D\uDC7B test"
                    0 2 (telega-display "👻")) . "👻 test")
                (#("test \uD83D\uDC41\uD83D\uDDE8"
                   5 7 (telega-display "👁")
                   7 9 (telega-display "🗨")) . "test 👁🗨")
                (#("test \uD83D\uDC41mid\uD83D\uDDE8ending"
                   5 7 (telega-display "👁")
                   10 12 (telega-display "🗨")) . "test 👁mid🗨ending")
                )))
    (dolist (ts tests)
      (should (string= (telega--desurrogate-apply (car ts)) (cdr ts))))
    ))

(ert-deftest telega-server-json-parsing ()
  "Test json->plist converter."
  )

(ert-deftest telega-tme-open ()
  "Test info related functionality."
  (should (string= (telega-tme-open "https://t.me/joinchat/itshitout" t)
                   "tg:join?invite=itshitout"))
  (should (string= (telega-tme-open "https://t.me/itshit/23423" t)
                   "tg:resolve?domain=itshit&post=23423"))
  (should (string= (telega-tme-open "https://t.me/socks?server=my&port=1234" t)
                   "tg:socks?server=my&port=1234"))
  (should (string= (telega-tme-open "https://t.me/c/101110111/286125" t)
                   "tg:privatepost?channel=101110111&post=286125"))
  ;; Non telegram urls
  (should (null (telega-tme-open "www.domain.ru" t)))
  (should (null (telega-tme-open "https://www.domain.ru" t)))
  )

(ert-deftest telega-formatting ()
  "Test some formatting related functions."
  (should (string= (telega-escape-underscores "www.ru/here_url\n_lala_")
                   "www.ru/here\\_url\n_lala_"))
  (should (string= (telega-escape-underscores " _lala_ www.ru/here_url www.me/lala_url")
                   " _lala_ www.ru/here\\_url www.me/lala\\_url"))
  (should (string= (telega-escape-underscores " _lala_ https://ya.ru/here_url")
                   " _lala_ https://ya.ru/here\\_url"))
  (should (string= (telega-escape-underscores "www.org/me_here _lala_ https://ya.ru/here_url http://www.ru/menounder")
                   "www.org/me\\_here _lala_ https://ya.ru/here\\_url http://www.ru/menounder"))

  (should (string= (telega-escape-underscores "@mention_starting_line here_notescaped_")
                   "@mention\\_starting\\_line here_notescaped_"))
  (should (string= (telega-escape-underscores "Mention in_the_ @middle_of sentence")
                   "Mention in_the_ @middle\\_of sentence"))
  )

(ert-deftest telega-waveform ()
  "Test info related functionality."
  (should (equal (telega-vvnote--waveform-decode "pCiWiiGllEKIGOOYdBAipCCzlEqOKdCYAekApB5YBoxBQFkNIAQMKYAMMAYAA4ABGAAJAANAAQkABcABQAEA" 'raw)
                 '(20 16 20 9 13 2 17 1 20 22 10 4 5 2 0 24 28 14 12 7 8 4 1 2 20 16 16 11 7 5 2 10 17 24 20 29 1 6 0 1 29 4 0 10 8 7 18 24 0 26 6 4 2 16 2 25 1 20 16 0 8 3 1 9 16 0 6 3 0 1 16 0 0 14 0 0 2 6 0 0 1 4 0 0 6 16 0 1 1 4 0 0 11 16 0 1 8 0 0 16 0)))
  (should (equal (telega-vvnote--waveform-decode "AQQAgDjruQZjZM7BttxqjqV0AgghhAAAAMAc5aCcETrGGmPPJSQAAAAMIUIAwqQGn3OOhABCAAEMEQIAQoQA" 'raw)
                 '(0 4 2 0 1 0 1 24 29 14 28 16 12 24 27 4 25 27 0 27 13 23 3 10 17 26 18 23 8 0 16 8 4 6 2 0 0 0 0 0 24 0 14 14 11 8 4 28 2 4 29 12 12 6 19 3 25 28 18 18 8 0 0 0 0 0 6 2 2 16 16 0 24 10 18 0 13 7 27 19 17 26 2 0 0 16 16 0 0 4 6 1 2 0 16 0 8 10 2 0 0)))

  ;; Test encoder
  (let ((wv-b64 "7P/////fU/T//9sNMYYQY4whhBhjiFH+/x/IDz4CHcAAAAAgAAAAAKCC/8H34IIDAAAAAPAB+AAcACYAAwAA"))
    (should (string= wv-b64 (telega-vvnote--waveform-encode
                             (telega-vvnote--waveform-decode wv-b64 'raw)))))
  )

(ert-deftest telega-internationalization ()
  "Test i18n code."
  (should (equal (telega-i18n "lng_polls_votes_count" :count 102)
                 "102 votes"))
  (should (equal (telega-i18n "lng_action_user_joined" :from #("'S'Т'А'N'&'S'L'А'\\/' \"Р\"Е\"Т\"R\"О\"\\/\"" 0 35 (face bold)))
                 #("'S'Т'А'N'&'S'L'А'\\/' \"Р\"Е\"Т\"R\"О\"\\/\" joined the group" 0 35 (face bold))))
  )

(ert-deftest telega-utils ()
  "Test util code for telega."
  (should (equal (telega-puny-decode-url "https://xn----8sbis2aqlf5f.xn--p1ai/lalabum")
                 "https://ит-гранты.рф/lalabum"))
  (should (equal (telega-puny-decode-url "test.domain.ru##[title~=\\[NSP\\]]:nth-ancestor(6)")
                 "test.domain.ru##[title~=\\[NSP\\]]:nth-ancestor(6)"))
  )

(ert-deftest telega-org-formatting ()
  "Test org mode text formatting."
  (should (equal (telega-markup-org-fmt "*bold*")
                 '(:@type "formattedText" :text "bold" :entities [(:@type "textEntity" :offset 0 :length 4 :type (:@type "textEntityTypeBold"))])))
  (should (equal (telega-markup-org-fmt "prefix *bold* here ~code~ trailing")
                 '(:@type "formattedText" :text "prefix bold here code trailing" :entities [(:@type "textEntity" :offset 7 :length 4 :type (:@type "textEntityTypeBold")) (:@type "textEntity" :offset 17 :length 4 :type (:@type "textEntityTypeCode"))])))
  )

(ert-deftest telega-plist-del-test ()
  "Testing for `telega-plist-del'."
  (should (equal '(:test 1)
                 (telega-plist-del '(:hahah 2 :test 1) :hahah)))
  (should (equal '(:not-deleted 2)
                 (telega-plist-del '(:not-deleted 2) :not-deletedXX)))
  (should (equal nil
                 (telega-plist-del '(:delete-me 1) :delete-me)))
  )

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; test.el ends here
