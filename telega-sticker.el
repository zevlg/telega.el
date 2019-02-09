;;; telega-sticker.el --- Stickers for the telega

;; Copyright (C) 2019 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb  8 21:08:24 2019
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

;;

;;; Code:

(defcustom telega-sticker-height 4
  "*Height of stickers in char heights."
  :type 'integer
  :group 'telega)

(defvar telega-sticker-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [remap self-insert-command] 'ignore)
    (define-key map (kbd "f") 'telega-sticker-toggle-favorite)
    (define-key map (kbd "*") 'telega-sticker-toggle-favorite)
    (define-key map (kbd "i") 'telega-sticker-help)
    (define-key map (kbd "h") 'telega-sticker-help)))

(define-button-type 'telega-sticker
  :supertype 'telega
  :inserter 'telega-ins--sticker
  'read-only t
  'keymap telega-sticker-button-map)

(defun telega-sticker-at (&optional pos)
  "Retur sticker at POS."
  (let ((button (button-at (or pos (point)))))
    (when (and button (eq (button-type button) 'telega-sticker))
      (button-get button :value))))

(defsubst telega-sticker-get (file-id)
  (telega--getFile file-id))

(defun telega-sticker-favorite-p (sticker)
  "Return non-nil if STICKER is in favorite list."
  (cl-find (telega--tl-get sticker :sticker :id)
           telega--stickers-favorite
           :key (telega--tl-prop :id)
           :test 'eq))

(defun telega-stickers--ensure-downloaded (sticker-files)
  "Ensure all stickers in STICKER-FILES list are downloaded."
  (cl-dolist (file sticker-files)
    (when (telega-media--need-download-p file)
      (telega-file--download-monitor-progress (plist-get file :id)
        (lambda (file slist)
          (let ((stail (cl-member (plist-get file :id) slist
                                  :key (telega--tl-prop :id)
                                  :test 'eq)))
            (cl-assert stail)
            (setcar stail file)))
        sticker-files)
      (telega--downloadFile (plist-get file :id)))))

(defun telega-sticker--ensure-downloaded (sticker)
  "Ensure STICKER data is downloaded."
  (let ((sthumb (plist-get sticker :thumbnail)))
    (when (telega-media--need-download-p (plist-get sthumb :photo))
      (telega-file--download-monitoring sthumb :photo))
    (when (telega-media--need-download-p (plist-get sticker :sticker))
      (telega-file--download-monitoring sticker :sticker))
    ))

(defun telega-stickerset--ensure-downloaded (sset)
  "Ensure sticker set SSET data is downloaded."
  (mapc 'telega-sticker--ensure-downloaded (plist-get sset :stickers)))

(defun telega-stickerset-get (set-id)
  (let ((sset (cdr (assoc set-id telega--stickersets))))
    (unless sset
      (setq sset (telega-server--call
                  (list :@type "getStickerSet"
                        :set_id set-id)))

      ;; Asynchronously download sticker data for this set
      (telega-stickerset--ensure-downloaded sset)

      (setq telega--stickersets
            (cons (cons set-id sset) telega--stickersets)))
    sset))

(defun telega--on-updateInstalledStickerSets (event)
  "The list of installed sticker sets was updated."
  (let ((sset-ids (plist-get event :sticker_set_ids)))
    (if (plist-get event :is_masks)
        (telega-debug "TODO: `telega--on-updateInstalledStickerSets' is_mask=True")

      (setq telega--stickersets-installed
            (mapcar 'telega-stickerset-get sset-ids))
      )))

(defun telega--on-updateTrendingStickerSets (event)
  "The list of trending sticker sets was updated or some of them were viewed."
  (let ((ssets-info (telega--tl-get event :sticker_sets :sets)))
    (setq telega--stickersets-trending
          (mapcar 'identity ssets-info))))

(defun telega--on-updateRecentStickers (event)
  "Recent stickers has been updated."
  (let ((stickers (mapcar 'telega-sticker-get
                          (plist-get event :sticker_ids))))

    ;; Asynchronously download corresponding files
    (telega-stickers--ensure-downloaded stickers)

    (if (plist-get event :is_attached)
        (setq telega--stickers-recent-attached stickers)
      (setq telega--stickers-recent stickers))))

(defun telega--on-updateFavoriteStickers (event)
  "Favorite stickers has been updated."
  (setq telega--stickers-favorite
        (mapcar 'telega-sticker-get (plist-get event :sticker_ids)))

  ;; Asynchronously download corresponding files
  (telega-stickers--ensure-downloaded telega--stickers-favorite))

(defun telega--getStickers (emoji &optional limit)
  "Returns installed stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (telega-server--call
   (list :@type "getStickers"
         :emoji emoji
         :limit (or limit 20))))

(defun telega--searchStickers (emoji &optional limit)
  "Search for the public stickers that correspond to a given EMOJI.
LIMIT defaults to 20."
  (telega-server--call
   (list :@type "searchStickers"
         :emoji emoji
         :limit (or limit 20))))

(defun telega--getInstalledStickerSets (&optional masks-p)
  "Returns a list of installed sticker sets."
  (cl-assert (not masks-p) t "installed masks not yet supported")

  (unless telega--stickersets-installed
    (let ((reply (telega-server--call
                  (list :@type "getInstalledStickerSets"
                        :is_masks (or masks-p :false)))))
      (setq telega--stickersets-installed
            (mapcar (lambda (sinfo)
                      (telega-stickerset-get (plist-get sinfo :id)))
                    (plist-get reply :sets)))))
  telega--stickersets-installed)

(defun telega--changeStickerSet (stickerset install-p &optional archive-p)
  "Install/Uninstall STICKERSET."
  (telega-server--call
   (list :@type "changeStickerSet"
         :set_id (plist-get stickerset :id)
         :is_installed (or install-p :false)
         :is_archived (or archive-p :false))))

(defun telega--getTrendingStickerSets ()
  "Returns a list of trending sticker sets."
  (telega-server--call
   (list :@type "getTrendingStickerSets")))

(defun telega--getAttachedStickerSets (file-id)
  "Return sticker sets attached to the FILE-ID.
Photo and Video files have attached sticker sets."
  (telega-server--call
   (list :@type "getAttachedStickerSets"
         :file_id file-id)))

(defun telega--searchStickerSet (name)
  "Search for sticker set by NAME."
  (telega-server--call
   (list :@type "searchStickerSet"
         :name name)))

(defun telega--searchInstalledStickerSets (query &optional masks-p limit)
  "Searches for installed sticker sets by QUERY."
  (telega-server--call
   (list :@type "searchInstalledStickerSets"
         :is_masks (or masks-p :false)
         :query query
         :limit (or limit 20))))

(defun telega--searchStickerSets (query)
  "Searches for ordinary sticker sets by looking for specified QUERY."
  (telega-server--call
   (list :@type "searchStickerSets")))

(defun telega--viewTrendingStickerSets (set-id &rest other-ids)
  (telega-server--call
   (list :@type "viewTrendingStickerSets"
         :sticker_set_ids (cl-map 'vector 'identity
                                  (nconc (list set-id) other-ids)))))

(defun telega--getRecentStickers (&optional attached-p)
  "Returns a list of recently used stickers.
Pass non-nil ATTACHED-P to return only stickers attached to photos/videos."
  (telega-server--call
   (list :@type "getRecentStickers"
         :is_attached (or attached-p :false))))

(defun telega--getFavoriteStickers ()
  "Return favorite stickers."
  (let ((reply (telega-server--call
                (list :@type "getFavoriteStickers"))))
    (mapcar (lambda (sticker)
              (telega-sticker-get (plist-get sticker :id)))
            (plist-get reply :stickers))))

(defun telega--addFavoriteSticker (sticker-input-file)
  "Add STICKER-INPUT-FILE on top of favorite stickers."
  (telega-server--call
   (list :@type "addFavoriteSticker"
         :sticker sticker-input-file)))

(defun telega--removeFavoriteSticker (sticker-input-file)
  (telega-server--call
   (list :@type "removeFavoriteSticker"
         :sticker sticker-input-file)))

(defun telega--getStickerEmojis (sticker-input-file)
  (telega-server--call
   (list :@type "getStickerEmojis"
         :sticker sticker-input-file)))

(defun telega-sticker--progress-svg (sticker)
  (let* ((progress (telega-file--downloading-progress
                    (plist-get sticker :sticker)))
         (emoji (telega--desurrogate-apply (plist-get sticker :emoji)))
         (h (* (frame-char-height) telega-sticker-height))
         (w-chars (telega-chars-in-width h))
         (w (* (telega-chars-width 1) w-chars))
         (svg (svg-create w h))
         (font-size (/ h 2))
         ;; progress clipping mask
         (angle-o (+ pi (* 2 pi (- 1.0 progress))))
         (clip-dx (* (/ w 2) (1+ (sin angle-o))))
         (clip-dy (* (/ h 2) (1+ (cos angle-o))))
         (pclip (telega-svg-clip-path svg "pclip")))

    (svg-text svg (substring emoji 0 1)
                :font-size font-size
                :font-weight "bold"
                :fill "white"
                :font-family "monospace"
                :x (/ font-size 2)
                :y (+ font-size (/ font-size 3)))

    ;; clip mask for the progress circle
    (let ((cp (format "M %d %d L %d %d L %d 0" (/ w 2) (/ h 2) (/ w 2) 0 0)))
      (when (< progress 0.75)
        (setq cp (concat cp (format " L 0 %d" h))))
      (when (< progress 0.5)
        (setq cp (concat cp (format " L %d %d" w h))))
      (when (< progress 0.25)
        (setq cp (concat cp (format " L %d 0" w))))
      (setq cp (concat cp (format " L %d %d" clip-dx clip-dy)))
      (setq cp (concat cp " Z"))
      (telega-svg-path pclip cp))
    ;; progress circle
    (svg-circle svg (/ w 2) (/ h 2) (/ h 2)
                :fill-color (face-foreground 'shadow)
                :fill-opacity "0.25"
                :clip-path "url(#pclip)")
    (svg-image svg :scale 1.0
               :ascent 'center
               ;; text of correct width
               :telega-text
               (make-string (or (plist-get sticker :telega-image-char-width)
                                w-chars)
                            ?X))
    ))

(defun telega-sticker--create-image (filename &optional props)
  "Create sticker image frame FILENAME."
  (apply 'create-image filename 'imagemagick nil
         :height (* (frame-char-height) telega-sticker-height)
         :scale 1.0
         :ascent 'center
         props))

(defun telega-sticker--image (sticker)
  "Return image for the STICKER."
  ;; Three cases:
  ;;   1) Sticker downloaded
  ;;      Just show (and cache) sticker image
  ;;
  ;;   2) Thumbnail is downloaded, while sticker still downloading
  ;;      Show thumbnail (caching temporary), waiting for sticker to
  ;;      be downloaded
  ;;
  ;;   3) Thumbnail and sticker downloading
  ;;      Fallback to `telega-sticker--progress-svg', waiting for
  ;;      thumbnail or sticker to be downloaded
  (unless (plist-get sticker :telega-image-char-width)
    ;; Calculate resulting image width in chars
    (let ((sw (plist-get sticker :width))
          (sh (plist-get sticker :height))
          (mh (* (frame-char-height) telega-sticker-height)))
      (plist-put sticker :telega-image-char-width
                 (telega-chars-in-width (* (/ sw sh) mh)))))

  (let* ((sfile (plist-get sticker :sticker))
         (sthumb (telega--tl-get sticker :thumbnail :photo))
         (filename (or (and (telega-file--downloaded-p sfile) sfile)
                       (and (telega-file--downloaded-p sthumb) sthumb))))
    (if filename
        (telega-sticker--create-image
         (telega--tl-get filename :local :path)
         (when (telega-sticker-favorite-p sticker)
           (list :relief 4)))
      ;; Fallback to svg
      (telega-sticker--progress-svg sticker))))

(defun telega-sticker--image-update (sticker)
  "Possible update STICKER's image.
Return new image."
  (let ((cached-image (plist-get sticker :telega-image))
        (simage (telega-sticker--image sticker)))
    (unless (equal cached-image simage)
      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))
      (plist-put sticker :telega-image cached-image))
    cached-image))

(defun telega-sticker--download-monitor (file sticker)
  (cl-assert (plist-get sticker :telega-image))
  (telega-sticker--image-update sticker)
  (force-window-update))
  
(defun telega-ins--sticker (sticker &optional slices-p &rest props)
  "Inserter for the STICKER.
If SLICES-P is non-nil, then insert STICKER using slices."
  (let ((cached-image (plist-get sticker :telega-image))
        (simage (telega-sticker--image sticker))
        (sfile (plist-get sticker :sticker))
        (props (nconc (list 'button (list t)
                        'keymap telega-sticker-button-map)
                  props)))
    (unless (equal cached-image simage)
      ;; Update the image
      (if cached-image
          (setcdr cached-image (cdr simage))
        (setq cached-image simage))
      (plist-put sticker :telega-image cached-image))

    ;; Possible monitor file downloading
    (when (telega-media--need-download-p (plist-get sticker :sticker))
      (telega-file--download-monitoring
       sticker :sticker nil
       'telega-sticker--download-monitor sticker))

    (if slices-p
        (apply 'telega-ins--image-slices cached-image props)
      (apply 'telega-ins--image cached-image nil props))))

(defun telega-ins--stickerset-change-button (sset)
  (telega-ins--button (if (plist-get sset :is_installed)
                          ;; I18N: XXX
                          "Uninstall"
                        "Install")
    :value sset
    'action 'telega-button--stickerset-change-action))

(defun telega-button--stickerset-change-action (button)
  (let ((sset (button-get button :value)))
    (telega--changeStickerSet sset (not (plist-get sset :is_installed)))
    ;; Update sset value, :is_installed might change
    (setq sset (telega-stickerset-get (plist-get sset :id)))
    (telega-save-cursor
      (telega-button--change button
        (telega-ins--stickerset-change-button sset)))))

(defun telega-describe-stickerset (sset &optional info-p)
  "Describe the sticker set."
  (interactive (list (telega-sticker-set-at-point)))
  (with-telega-help-win "*Telegram Sticker Set*"
    (let ((stickers (plist-get sset (if info-p :covers :stickers))))
      (telega-ins "Title: " (plist-get sset :title) " ")
      (telega-ins--stickerset-change-button sset)
      (telega-ins "\n")
      (telega-ins "Name: " (plist-get sset :name)
                  (if (plist-get sset :is_official)
                      telega-symbol-verified
                    "") "\n")
      (telega-ins "Masks: " (if (plist-get sset :is_masks) "yes" "no") "\n")
      (telega-ins-fmt "Stickers: %d\n" (length stickers))
      (seq-doseq (sticker stickers)
        (when (> (telega-current-column) (- telega-chat-fill-column 10))
          (telega-ins "\n\n"))
        (telega-ins--sticker sticker)
        (telega-ins (plist-get sticker :emoji))
        (telega-ins "  "))
  )))

(provide 'telega-sticker)

;;; telega-sticker.el ends here
