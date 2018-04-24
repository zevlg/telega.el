;;; test.el --- Testing routines for telega.
(require 'telega)

(setq telega--users
      `())

(setq telega--ordered-chats
      `((:@type "chat"
                :id 1111
                :type
                (:@type "chatTypeSupergroup"
                        :supergroup_id 11110 :is_channel t)
                :title "test channel1"
                :order 11
                :unread_count 25
                :unread_mention_count 0
                :is_pinned :json-false
                :notification_settings
                (:@type "notificationSettings"
                        :mute_for 623207729
                        :sound "" :show_preview :json-false))
        (:@type "chat"
                :id 22222
                :type
                (:@type "chatTypePrivate"
                        :user_id 22220 :is_channel t)
                :title "test channel1"
                :order 11
                :unread_count 25
                :unread_mention_count 0
                :is_pinned :json-false
                :notification_settings
                (:@type "notificationSettings"
                        :mute_for 623207729
                        :sound "" :show_preview :json-false))

        ;; TODO: add more chats
        ))


(ert-deftest telega-filters ()
  "Test `telega-filter' functionality."
  (should (null (telega-filter--test 10 '(not all))))
  (should (= (length (telega-filter-chats 'all))
             (length telega--ordered-chats)))
  (should (telega-filter--test 10 '(not any)))
  (should (telega-filter--test (car telega--chats) '(type channel)))
  (should (telega-filter--test
           (car telega-chats) '(all (type channel) (name "chan"))))
    ;; TODO: add more filter tests
  )

;;; test.el ends here
