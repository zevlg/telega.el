;;; test.el --- Testing routines for telega.
(require 'telega)

(telega--init-vars)
(telega--info-update
 `(:@type "supergroup" :id 11110 :username "noname"
          :status (:@type "chatMemberStatusAdministrator")))
(telega--info-update
 `(:@type "user" :id 22220 :first_name "Vasya" :last_name "Pupkin" :username "vpupkin"
          :phone_number "71112233" :status (:@type "userStatusOnline" :expires 1527776835)
          :outgoing_link (:@type "linkStateIsContact")
          :incoming_link (:@type "linkStateIsContact")
          :is_verified nil
          :restriction_reason ""
          :have_access t :type (:@type "userTypeRegular") :language_code ""))

(telega--info-update
 `(:@type "user" :id 22221 :first_name "Petya" :last_name "Siskin" :username ""
          :phone_number "71112234" :status (:@type "userStatusOnline" :expires 1527776836)
          :outgoing_link (:@type "linkStateIsContact")
          :incoming_link (:@type "linkStateIsContact")
          :is_verified nil
          :restriction_reason ""
          :have_access t :type (:@type "userTypeRegular") :language_code ""))

(setq telega--ordered-chats
      `((:@type "chat"
                :id 1111
                :type
                (:@type "chatTypeSupergroup"
                        :supergroup_id 11110 :is_channel t)
                :title "test channel1"
                :order "22"
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
                :order "11"
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
        ))

(ert-deftest telega-emacs-setup ()
  "Test emacs is suitable to run telega."
  ;; 62bits for numbers is required
  ;; i.e. ./configure --with-wide-int
  (should (= most-positive-fixnum 2305843009213693951))
  (should (= (string-to-number "542353335") 542353335))

  ;; at least 25.1 emacs is required
  ;; see https://t.me/emacs_telega/1592
  (should (fboundp 'cursor-intangible-mode))

  ;; imagemagick for images
  (should (image-type-available-p 'imagemagick))
  (should (image-type-available-p 'svg))
  )

(ert-deftest telega-info ()
  "Test info related functionality."
  (let ((user1 (telega-user--get 22220))
        (user2 (telega-user--get 22221)))
    (should (string= (telega-user--name user1) "Vasya Pupkin @vpupkin"))
    (should (string= (telega-user--name user1 'full) "Vasya Pupkin @vpupkin"))
    (should (string= (telega-user--name user1 'name) "Vasya Pupkin"))
    (should (string= (telega-user--name user1 'short) "@vpupkin"))

    ;; test fallback to 'name if username is empty
    (should (string= (telega-user--name user2 'short) "Petya Siskin"))
  ))

(ert-deftest telega-filters ()
  "Test `telega-filter' functionality."
  (should (not (null telega--ordered-chats)))
  (should (null (telega-filter--test 10 '(not all))))
  (should (= (length (telega-filter-chats 'all))
             (length telega--ordered-chats)))
  (should (telega-filter--test 10 '(not any)))
  (should (telega-filter--test (car telega--ordered-chats) '(type channel)))
  (should (telega-filter--test
           (car telega--ordered-chats) '(all (type channel) (name "chan"))))
  (should-not
   (telega-filter--test
    (car telega--ordered-chats) '(name "notmatching")))
  ;; TODO: add more filter tests
  )

(ert-deftest telega-desurrogate ()
  "Tests for `telega--desurrogate-apply'"
  (let ((tests '((#("\uD83D\uDC7B test"
                    0 2 (display "👻" telega-desurrogate t)) . "👻 test")
                (#("test \uD83D\uDC41\uD83D\uDDE8"
                   5 7 (display "👁" telega-desurrogate t)
                   7 9 (display "🗨" telega-desurrogate t)) . "test 👁🗨")
                (#("test \uD83D\uDC41mid\uD83D\uDDE8ending"
                   5 7 (display "👁" telega-desurrogate t)
                   10 12 (display "🗨" telega-desurrogate t)) . "test 👁mid🗨ending")
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
                   "tg:joinchat?invite=itshitout"))
  (should (string= (telega-tme-open "https://t.me/itshit/23423" t)
                   "tg:resolve?domain=itshit&post=23423"))
  (should (string= (telega-tme-open "https://t.me/socks?server=my&port=1234" t)
                   "tg:socks?server=my&port=1234"))
  )

(ert-deftest telega-waveform ()
  "Test info related functionality."
  (should (equal (telega-waveform-decode "pCiWiiGllEKIGOOYdBAipCCzlEqOKdCYAekApB5YBoxBQFkNIAQMKYAMMAYAA4ABGAAJAANAAQkABcABQAEA")
                 '(20 16 20 9 13 2 17 1 20 22 10 4 5 2 0 24 28 14 12 7 8 4 1 2 20 16 16 11 7 5 2 10 17 24 20 29 1 6 0 1 29 4 0 10 8 7 18 24 0 26 6 4 2 16 2 25 1 20 16 0 8 3 1 9 16 0 6 3 0 1 16 0 0 14 0 0 2 6 0 0 1 4 0 0 6 16 0 1 1 4 0 0 11 16 0 1 8 0 0 16)))
  (should (equal (telega-waveform-decode "AQQAgDjruQZjZM7BttxqjqV0AgghhAAAAMAc5aCcETrGGmPPJSQAAAAMIUIAwqQGn3OOhABCAAEMEQIAQoQA")
                 '(0 4 2 0 1 0 1 24 29 14 28 16 12 24 27 4 25 27 0 27 13 23 3 10 17 26 18 23 8 0 16 8 4 6 2 0 0 0 0 0 24 0 14 14 11 8 4 28 2 4 29 12 12 6 19 3 25 28 18 18 8 0 0 0 0 0 6 2 2 16 16 0 24 10 18 0 13 7 27 19 17 26 2 0 0 16 16 0 0 4 6 1 2 0 16 0 8 10 2 0)))
  )

;;; test.el ends here
