;;; test.el --- Testing routines for telega.
(require 'telega)

;; Setup
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
(telega--info-update
 '(:@type "supergroup" :id 1263892563 :username "PremiumSignalsForward" :date 1523303192 :status (:@type "chatMemberStatusLeft") :member_count 54837 :anyone_can_invite nil :sign_messages nil :is_channel t :is_verified nil :restriction_reason ""))

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
                :last_message (:@type "message" :id 44444 :sender_user_id 2220 :chat_id 1111 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users t :is_channel_post nil :contains_unread_mention nil :date 1531136430 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 0 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text "–°–∏–Ω—Ç–µ–∑–∞—Ç–æ—Ä –∫–∞–∫ –º–∞–∫—Å–∏–º—É–º" :entities [])))
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
                :last_message (:@type "message" :id 44442 :sender_user_id 3330 :chat_id 22222 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users t :is_channel_post nil :contains_unread_mention nil :date 1531136430 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 0 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text "–°–∏–Ω—Ç–µ–∑–∞—Ç–æ—Ä –∫–∞–∫ –º–∞–∫—Å–∏–º—É–º" :entities [])))
                )
        
        ;; TODO: add more chats
        (:@type "chat" :id -1001263892563 :type (:@type "chatTypeSupergroup" :supergroup_id 1263892563 :is_channel t) :title "Premium Signals Forward" :photo (:@type "chatPhoto" :small (:@type "file" :id 2213 :size 0 :expected_size 0 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AQADAQATkvwKMAAEDMzuE4el7_5oeAEAAQI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 0)) :big (:@type "file" :id 2214 :size 0 :expected_size 0 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AQADAQATkvwKMAAEandjHTisoJpqeAEAAQI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 0))) :order "0" :is_pinned nil :is_marked_as_unread nil :is_sponsored nil :can_be_reported t :default_disable_notification nil :unread_count 0 :last_read_inbox_message_id 0 :last_read_outbox_message_id 2251799812636672 :unread_mention_count 0 :notification_settings (:@type "chatNotificationSettings" :use_default_mute_for t :mute_for 0 :use_default_sound t :sound "" :use_default_show_preview t :show_preview nil) :reply_markup_message_id 0 :client_data "(:color (\"#849b34\" \"#596823\" \"#2c3311\"))" :uaprops (:color ("#849b34" "#596823" "#2c3311")) :last_message (:@type "message" :id 599785472 :sender_user_id 0 :chat_id -1001263892563 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users nil :is_channel_post t :contains_unread_mention nil :date 1547562092 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 26731 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text #("Buy GTO between 755-765 satoshi.

Breaking out resistance risk level medium. 

Targets 9%,18%,27%,40%.

Have Stoploss 690 Satoshi. Ì†ΩÌªë 

Ì†ΩÌ∫Ä Ì†ΩÌ∫Ä" 131 133 (display "üõë" telega-desurrogate t) 136 138 (display "üöÄ" telega-desurrogate t) 139 141 (display "üöÄ" telega-desurrogate t)) :entities []))))
        ))
(cl-dolist (chat telega--ordered-chats)
  (puthash (plist-get chat :id) chat telega--chats))


;; Tests
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
  ;; NOTE: one chat (id=-1001263892563) has "0" order
  (should (= (length (telega-filter-chats 'all telega--ordered-chats))
             (1- (length telega--ordered-chats))))
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
                    0 2 (display "üëª" telega-desurrogate t)) . "üëª test")
                (#("test \uD83D\uDC41\uD83D\uDDE8"
                   5 7 (display "üëÅ" telega-desurrogate t)
                   7 9 (display "üó®" telega-desurrogate t)) . "test üëÅüó®")
                (#("test \uD83D\uDC41mid\uD83D\uDDE8ending"
                   5 7 (display "üëÅ" telega-desurrogate t)
                   10 12 (display "üó®" telega-desurrogate t)) . "test üëÅmidüó®ending")
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
  )

(ert-deftest telega-waveform ()
  "Test info related functionality."
  (should (equal (mapcar (lambda (e)
                           (round (* e 31)))
                         (telega-vvnote--waveform-decode "pCiWiiGllEKIGOOYdBAipCCzlEqOKdCYAekApB5YBoxBQFkNIAQMKYAMMAYAA4ABGAAJAANAAQkABcABQAEA"))
                 '(20 16 20 9 13 2 17 1 20 22 10 4 5 2 0 24 28 14 12 7 8 4 1 2 20 16 16 11 7 5 2 10 17 24 20 29 1 6 0 1 29 4 0 10 8 7 18 24 0 26 6 4 2 16 2 25 1 20 16 0 8 3 1 9 16 0 6 3 0 1 16 0 0 14 0 0 2 6 0 0 1 4 0 0 6 16 0 1 1 4 0 0 11 16 0 1 8 0 0 16)))
  (should (equal (mapcar (lambda (e)
                           (round (* e 31)))
                         (telega-vvnote--waveform-decode "AQQAgDjruQZjZM7BttxqjqV0AgghhAAAAMAc5aCcETrGGmPPJSQAAAAMIUIAwqQGn3OOhABCAAEMEQIAQoQA"))
                 '(0 4 2 0 1 0 1 24 29 14 28 16 12 24 27 4 25 27 0 27 13 23 3 10 17 26 18 23 8 0 16 8 4 6 2 0 0 0 0 0 24 0 14 14 11 8 4 28 2 4 29 12 12 6 19 3 25 28 18 18 8 0 0 0 0 0 6 2 2 16 16 0 24 10 18 0 13 7 27 19 17 26 2 0 0 16 16 0 0 4 6 1 2 0 16 0 8 10 2 0)))
  )

(ert-deftest telega-bad-msg-ins ()
  (let ((channel-msg1 '(:@type "message" :id 595591168 :sender_user_id 0 :chat_id -1001263892563 :is_outgoing nil :can_be_edited nil :can_be_forwarded t :can_be_deleted_only_for_self nil :can_be_deleted_for_all_users nil :is_channel_post t :contains_unread_mention nil :date 1545471497 :edit_date 0 :reply_to_message_id 0 :ttl 0 :ttl_expires_in 0.0 :via_bot_user_id 0 :author_signature "" :views 38555 :media_album_id "0" :content (:@type "messageText" :text (:@type "formattedText" :text #("‚Äã‚ÄãDo you have difficult market relations? Do you need a breath of fresh air? 

Average out Ì†ΩÌ≥à on trade-mate.io (http://trade-mate.io/)

The trade-mate.io project Ì†ΩÌ∫Ä has launched its platform for traders and crypt holders.

You will have a store of:
Ì†ΩÌ±Ä Visual and detailed statistics for all current transactions at all connected exchanges on one screen;
Ì†ΩÌ±ì Smart Trade - post take profit and stop loss on the favourite TradingView diagram;
Ì†ΩÌµ∂ Edit the task under implementation: panic sell, averaging out and trailing are in your hands;
Ì†ΩÌ±ë Autotrade: automatically follow the signals from the leading telegram channels. 

The guys launched a referral system:
Ì†ΩÌπãÌ†ºÌøª‚Äç‚ôÄÔ∏è‚ôÇ Your referrals are your income. Get 20% after the first payment of your referrals and constant % after other payments;
Ì†ºÌΩæ Withdraw the generated income to the crypto-wallets.

Don't lose your chance and register (http://trade-mate.io/) on the most promising platform for crypto-trading! 7 days for free!" 91 93 (display "üìà" telega-desurrogate t) 162 164 (display "üöÄ" telega-desurrogate t) 249 251 (display "üëÄ" telega-desurrogate t) 354 356 (display "üëì" telega-desurrogate t) 440 442 (display "üï∂" telega-desurrogate t) 537 539 (display "üëë" telega-desurrogate t) 659 661 (display "üôã" telega-desurrogate t) 661 663 (display "üèª" telega-desurrogate t) 787 789 (display "üçæ" telega-desurrogate t)) :entities [(:@type "textEntity" :offset 0 :length 2 :type (:@type "textEntityTypeTextUrl" :url "https://telegra.ph/file/d315a098bfbb00c645add.jpg")) (:@type "textEntity" :offset 97 :length 13 :type (:@type "textEntityTypeUrl")) (:@type "textEntity" :offset 112 :length 21 :type (:@type "textEntityTypeUrl")) (:@type "textEntity" :offset 140 :length 13 :type (:@type "textEntityTypeUrl")) (:@type "textEntity" :offset 881 :length 21 :type (:@type "textEntityTypeUrl"))]) :web_page (:@type "webPage" :url "https://telegra.ph/file/d315a098bfbb00c645add.jpg" :display_url "telegra.ph/file/d315a098bfbb00c645add.jpg" :type "photo" :site_name "" :title "" :description "" :photo (:@type "photo" :id "5833771091254945861" :has_stickers nil :sizes [(:@type "photoSize" :type "s" :photo (:@type "file" :id 2242 :size 2149 :expected_size 2149 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AgADBAADRagxG-a39VASP_8fRjWYOcYWHxsABN05tREP2GGXo7UAAgI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 2149)) :width 90 :height 90) (:@type "photoSize" :type "m" :photo (:@type "file" :id 2243 :size 26532 :expected_size 26532 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AgADBAADRagxG-a39VASP_8fRjWYOcYWHxsABEllOCu_gJAzpLUAAgI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 26532)) :width 320 :height 320) (:@type "photoSize" :type "x" :photo (:@type "file" :id 2244 :size 50087 :expected_size 50087 :local (:@type "localFile" :path "" :can_be_downloaded t :can_be_deleted nil :is_downloading_active nil :is_downloading_completed nil :downloaded_prefix_size 0 :downloaded_size 0) :remote (:@type "remoteFile" :id "AgADBAADRagxG-a39VASP_8fRjWYOcYWHxsABKJk8YGenclRpbUAAgI" :is_uploading_active nil :is_uploading_completed t :uploaded_size 50087)) :width 537 :height 537)]) :embed_url "" :embed_type "" :embed_width 0 :embed_height 0 :duration 0 :author "" :has_instant_view nil)) :reply_markup (:@type "replyMarkupInlineKeyboard" :rows [[(:@type "inlineKeyboardButton" :text "Join Now" :type (:@type "inlineKeyboardButtonTypeUrl" :url "https://trade-mate.io/?utm_source=telegram&utm_medium=smm&utm_campaign=ProCrypto&utm_term=21dec%5C"))]]) :@extra 15)))
    (should (eq t (with-temp-buffer
                    (telega-ins--channel-msg channel-msg1)
                    t)))
    ))

;;; test.el ends here
