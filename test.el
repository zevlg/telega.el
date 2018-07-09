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
                :order 22
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
                :order 11
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
  (should (= most-positive-fixnum 2305843009213693951))
  (should (= (string-to-number "542353335") 542353335))
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

;;; test.el ends here
