;;;; ios-native.lisp -- Lisp's side of modus's host/ios/swift/Native.swift: app-grade native components, for an app
;;;; that is an ordinary iPhone app rather than a window over glass's desk.  Built on ios-swift.lisp's plumbing
;;;; (SWIFT-CALL, the event pump, ON-EVENT); its events are listed in Native.swift.
;;;;
;;;;   (app-nav-new "Title")  -> nav, made the window's root     (app-nav-root nav) -> its first screen
;;;;   (app-push nav "Title") -> screen        (app-pop nav)     (app-set-title screen "Title")
;;;;   (app-path-title screen '("home" "ferry" "docs"))   a path, centred, ellipsis at the left when it does not fit
;;;;   (app-browser) -> screen whose bar holds still   (app-browser-push browser '("home" "docs")) -> table
;;;;                                       a page that slides in beneath the bar; back slides it away: :pop table
;;;;   (app-browser-preview browser path name)   Quick Look in the browser's pane (below; beside, in landscape)
;;;;   (app-screen "Title") -> screen, filled and then (app-show nav screen): it slides in whole
;;;;   (app-bar-button screen "plus" :right t) -> button (a :tap)
;;;;   (app-table screen) -> table           (app-table-set table rows)   rows: plists, :title :subtitle :detail
;;;;                                                                        :image :tint :chevron :section :sort
;;;;   (app-table-append table rows)   more rows after its own, a batch of a listing as it arrives
;;;;   (app-table-actions table '((:title "Delete" :destructive t)))      -> :rowaction (action row)
;;;;   (app-table-refresh table) -> :refresh   (app-table-refresh-end table)
;;;;   (app-table-empty table "No Servers" "Tap + to add one." "server.rack")   (app-table-loading table t)
;;;;   (app-alert "Title" "message" :sheet nil) -> alert   (app-alert-action alert "OK" :style :cancel)
;;;;   (app-alert-field alert "placeholder" :text "" :secure nil)   (app-alert-show alert) -> :alert (index text ...)
;;;;   (app-form "Title" :save "Save") -> form   (app-form-field form "Label" "value" :kind :text :placeholder "")
;;;;   (app-form-button form "Copy") -> button (a :tap)   (app-form-note form "...")   (app-form-show form)
;;;;                                                        -> :form (saved value ...)
;;;;   (app-status nav "Downloading x" :progress 0.4 :cancel t)  (app-status nav "")   -> :statuscancel
;;;;   (app-preview path) -> handle, Quick Look; :previewed when closed
;;;;   (app-pick-file) -> picker, :picked path ("" cancelled)   (app-share-file path) -> :share
;;;;   (app-resolve "host") -> handle, then :resolved "1.2.3.4" ("" for none)
;;;;   (app-copy "text")   (app-documents) -> the app's Documents directory   (app-utc-offset) -> seconds east of UTC

(defpackage #:kiln-native
  (:use #:cl)
  (:import-from #:kiln-swift #:swift-call #:on-event)
  (:export #:app-nav-new #:app-nav-root #:app-push #:app-screen #:app-show #:app-pop #:app-path-title #:app-browser #:app-browser-push #:app-browser-preview #:app-browser-preview-close #:app-browser-hex-total #:app-browser-hex-part #:app-browser-pop-to #:app-browser-set-path #:app-browser-subtitle
           #:app-orientation #:app-set-title #:app-bar-button
           #:app-bar-button-enable #:app-table #:app-table-set #:app-table-actions #:app-table-refresh
           #:app-table-refresh-end #:app-table-empty #:app-table-loading #:app-table-append #:app-table-placeholder #:app-alert #:app-alert-action #:app-alert-field
           #:app-alert-show #:app-form #:app-form-field #:app-form-button #:app-form-note #:app-form-show
           #:app-status #:app-pick-file #:app-share-file #:app-preview #:app-copy #:app-documents #:app-utc-offset #:app-resolve
           #:on-event))

(in-package #:kiln-native)

(defun %text-call (name text &rest more)
  "NAME with TEXT as a pointer and a length, then MORE."
  (let ((b (kiln-swift::%octets text)))
    (apply #'swift-call name (kiln-swift::%addr b) (length b) more)))

(defun %term-call (name handle term)
  (let ((b (kiln-swift::%term-octets term)))
    (swift-call name handle (kiln-swift::%addr b) (length b))))

;;; navigation

(defun app-nav-new (title &key quiet)
  "A navigation controller made the window's root.  QUIET: a bar that holds still -- no large titles, and a
   back button that is a bare chevron."
  (%text-call "app_nav_new" title (if quiet 1 0)))
(defun app-nav-root (nav) (swift-call "app_nav_root" nav))
(defun app-push (nav title)
  (let ((b (kiln-swift::%octets title))) (swift-call "app_push" nav (kiln-swift::%addr b) (length b))))
(defun app-screen (title)
  "A screen to push, not shown yet: add its table and buttons, then APP-SHOW it, and it slides in whole."
  (%text-call "app_screen" title))
(defun app-show (nav screen) (swift-call "app_show" nav screen))
(defun app-pop (nav) (swift-call "app_pop" nav))
(defun app-browser ()
  "A browser screen, not shown yet: its bar holds still while its pages -- tables -- slide beneath it.  Push its
   first page and add its bar buttons, then APP-SHOW it.  Back from a page is a :pop for that page's table;
   from the first, the browser leaves (a :pop for the browser)."
  (swift-call "app_browser"))

(defun app-browser-push (browser path &key plain)
  "A new page in BROWSER -- a table, returned -- with PATH (strings, outermost first) as the bar's title."
  (let ((b (kiln-swift::%term-octets path)))
    (swift-call "app_browser_push" browser (kiln-swift::%addr b) (length b) (if plain 1 0))))

(defun app-browser-preview (browser path name &key (kind :quick-look))
  "The local file PATH in BROWSER's pane -- KIND :text, monospaced in a view of ours; :image, in a zoomable
   viewer of ours; :hex, a hex dump of the file's first part, which PATH holds (APP-BROWSER-HEX-TOTAL first; it
   asks for other parts with a :hexmore event, (mode offset), answered with APP-BROWSER-HEX-PART, and a line's
   edit is a :hexedit event, (offset hex-text)); :quick-look, Quick Look -- under the list in portrait, a column beside it in
   landscape -- headed NAME, with share and close buttons.  Another file replaces it.  Its close button is a
   :previewed event with the browser."
  (let ((pb (kiln-swift::%octets path)) (nb (kiln-swift::%octets name)))
    (swift-call "app_browser_preview" browser (kiln-swift::%addr pb) (length pb) (kiln-swift::%addr nb) (length nb)
                (ecase kind (:quick-look 0) (:text 1) (:image 2) (:hex 3)))))

(defun app-browser-hex-total (browser size)
  "The size of the file the next :hex preview shows: where its dump ends."
  (swift-call "app_browser_hex_total" browser size))

(defun app-browser-hex-part (browser mode start &optional (target start))
  "The :hex preview's local file now holds the part of the file from START that a :hexmore asked for -- MODE
   :jump (to TARGET), :after, :before -- or bytes rewritten from START (:edited, an edit saved): show it.  A
   part of nothing: the file ends there."
  (swift-call "app_browser_hex_part" browser (ecase mode (:jump 0) (:after 1) (:before 2) (:edited 3)) start target))

(defun app-browser-preview-close (browser) (swift-call "app_browser_preview_close" browser))
(defun app-browser-subtitle (browser text)
  "A line under BROWSER's path, smaller: where the path is (a server's name).  \"\": none."
  (let ((b (kiln-swift::%octets text)))
    (swift-call "app_browser_subtitle" browser (kiln-swift::%addr b) (length b))))

(defun app-browser-set-path (browser table path)
  "The page whose table is TABLE has PATH now (strings, outermost first): a server's home, once connected."
  (let ((b (kiln-swift::%term-octets path)))
    (swift-call "app_browser_set_path" browser table (kiln-swift::%addr b) (length b))))

(defun app-browser-pop-to (browser table)
  "Back to the page whose table is TABLE: the pages after it go, a :pop for each -- a tap in an earlier column
   (landscape shows the folder and its parents side by side), before what it opens."
  (swift-call "app_browser_pop_to" browser table))

(defun app-orientation (which)
  "Ask for :portrait or :landscape, as turning the phone does -- for a test run that cannot turn it."
  (swift-call "app_orientation" (ecase which (:portrait 0) (:landscape 1))))

(defun app-path-title (screen parts)
  "SCREEN's title as a path, PARTS outermost first: centred between the bar's buttons, the last in bold, the beginning giving way to
   an ellipsis when it does not fit."
  (%term-call "app_path_title" screen parts))
(defun app-set-title (screen title)
  (let ((b (kiln-swift::%octets title))) (swift-call "app_set_title" screen (kiln-swift::%addr b) (length b))))

(defun app-bar-button (screen symbol-or-title &key right)
  "A navigation-bar button on SCREEN: an SF Symbol when one is named SYMBOL-OR-TITLE, else text.  A :tap."
  (let ((b (kiln-swift::%octets symbol-or-title)))
    (swift-call "app_bar_button" screen (kiln-swift::%addr b) (length b) (if right 1 0))))

(defun app-bar-button-enable (button on) (swift-call "app_bar_button_enable" button (if on 1 0)))

;;; tables

(defun app-table (screen &key plain) (swift-call "app_table" screen (if plain 1 0)))
(defun app-table-set (table rows) (%term-call "app_table_set" table rows))
(defun app-table-append (table rows)
  "ROWS after TABLE's own -- a listing that arrives a batch at a time; only the batch crosses.  Their indices go
   on from the rows there.  A row's :SORT key orders what is shown (as the Finder sorts names)."
  (%term-call "app_table_append" table rows))
(defun app-table-actions (table actions) (%term-call "app_table_actions" table actions))
(defun app-table-refresh (table) (swift-call "app_table_refresh" table))
(defun app-table-refresh-end (table) (swift-call "app_table_refresh_end" table))
(defun app-table-placeholder (table &key loading title message image button)
  "While TABLE has no rows: LOADING, the system's loading view (a spinner, TITLE and MESSAGE under it); else a
   message, with IMAGE and a BUTTON whose tap is a :placeholder event.  No arguments take it away."
  (%term-call "app_table_placeholder" table
              (if (or loading title message)
                  (append (and loading (list :loading t)) (and title (list :title title))
                          (and message (list :message message)) (and image (list :image image))
                          (and button (list :button button)))
                  '())))

(defun app-table-loading (table on)
  "While ON and TABLE has no rows, a spinner where its empty view would be (after 150 ms: a quick load shows
   none).  APP-TABLE-SET ends it."
  (swift-call "app_table_loading" table (if on 1 0)))
(defun app-table-empty (table title message &optional image)
  (%term-call "app_table_empty" table (list* :title title :message message (and image (list :image image)))))

;;; alerts

(defun app-alert (title message &key sheet)
  (let ((tb (kiln-swift::%octets (or title ""))) (mb (kiln-swift::%octets (or message ""))))
    (swift-call "app_alert" (kiln-swift::%addr tb) (length tb) (kiln-swift::%addr mb) (length mb) (if sheet 1 0))))

(defun app-alert-action (alert title &key (style :default))
  "A button; STYLE :default, :cancel or :destructive.  Returns its index (the :alert event's first value)."
  (let ((b (kiln-swift::%octets title)))
    (swift-call "app_alert_action" alert (kiln-swift::%addr b) (length b)
                (ecase style (:default 0) (:cancel 1) (:destructive 2)))))

(defun app-alert-field (alert placeholder &key (text "") secure)
  (let ((pb (kiln-swift::%octets placeholder)) (tb (kiln-swift::%octets text)))
    (swift-call "app_alert_field" alert (kiln-swift::%addr pb) (length pb) (kiln-swift::%addr tb) (length tb)
                (if secure 1 0))))

(defun app-alert-show (alert) (swift-call "app_alert_show" alert))

;;; forms

(defun app-form (title &key (save "Save"))
  (let ((tb (kiln-swift::%octets title)) (sb (kiln-swift::%octets save)))
    (swift-call "app_form" (kiln-swift::%addr tb) (length tb) (kiln-swift::%addr sb) (length sb))))

(defun app-form-field (form label value &key (kind :text) (placeholder ""))
  "A field; KIND :text :number :secure :url or :switch (its value \"1\" or \"0\").  Returns its index."
  (%term-call "app_form_field" form
              (list :label label :value (or value "") :placeholder placeholder
                    :kind (ecase kind (:text 0) (:number 1) (:secure 2) (:url 3) (:switch 4)))))

(defun app-form-button (form title)
  (let ((b (kiln-swift::%octets title))) (swift-call "app_form_button" form (kiln-swift::%addr b) (length b))))
(defun app-form-note (form text)
  (let ((b (kiln-swift::%octets text))) (swift-call "app_form_note" form (kiln-swift::%addr b) (length b))))
(defun app-form-show (form) (swift-call "app_form_show" form))

;;; the status bar, files, the pasteboard

(defun app-status (nav text &key progress cancel)
  "Show TEXT at the bottom of NAV, with a progress bar at PROGRESS (0 to 1) when given and Cancel when CANCEL;
   TEXT \"\" hides it."
  (let ((b (kiln-swift::%octets text)))
    (swift-call "app_status" nav (kiln-swift::%addr b) (length b)
                (if progress (max 0 (min 1000 (round (* 1000 progress)))) 1001)
                (if cancel 1 0))))

(defun app-pick-file () (swift-call "app_pick_file"))
(defun app-share-file (path) (%text-call "app_share_file" path))
(defun app-preview (path)
  "Quick Look for the local file at PATH -- images, text, PDF, Markdown -- with its own share button.  Returns
   its handle; closing it is a :previewed event."
  (%text-call "app_preview" path))
(defun app-copy (text) (%text-call "app_copy" text))

(defun app-documents ()
  "The app's Documents directory, which the Files app shows as the app's folder."
  (let* ((cap 1024)
         (buf (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))
         (n (swift-call "app_documents" (kiln-swift::%addr buf) cap)))
    (if (<= 1 n cap) (seal:utf8-decode buf :end n) "")))

(defun app-utc-offset ()
  "The local time zone's offset from UTC now, in seconds (east positive)."
  (- (swift-call "app_utc_offset") 86400))

(defun app-resolve (name)
  "Ask the system's resolver for NAME's IPv4 address, off the main thread: a :resolved event with its handle
   brings the dotted address, or \"\" when there is none."
  (%text-call "app_resolve" name))
