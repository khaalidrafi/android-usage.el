;;; android-usage.el --- Android screen-time (Usage Access) data in Org -*- lexical-binding: t; -*-

;; Author: GLM-5.3-Flash <glm-5.3-flash@z.ai>
;; Maintainer: Khalid Rafi <khaalidrafi@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, android, convenience, tools
;; URL: https://github.com/khaalidrafi/android-usage.el

;;; Commentary:

;; StayFree's numbers, in your Org file: device-wide per-app screen
;; time pulled from Android's Usage Access data, journaled as plain
;; Org tables you own.
;;
;; Honest about the bridge: Emacs (29 through 31, feature/android
;; included) cannot call UsageStatsManager itself -- there is no
;; Java interop in any stock Android build.  Android only exposes
;; that data to apps holding the "Usage access" special permission.
;; So this package pulls the data through an export file written by
;; an app that DOES hold Usage access:
;;
;;   - StayFree's own CSV export works today (Settings > Backup >
;;     Export CSV); export to Download, then M-x android-usage-import.
;;   - Any other Usage-Access app exporting CSV/JSON with recognizable
;;     columns works the same way.
;;   - Integrators can emit the canonical JSON documented in the README
;;     (or a custom `android-usage-pull-function').
;;
;; What lands in `android-usage-org-file' (default: org-directory/
;; android-usage.org) is one level-1 heading per day with a table:
;;
;;   * 2026-11-07 Sat
;;   | App           | Package        | Time | Opens |
;;   |---------------+----------------+------+-------|
;;   | WhatsApp      | org.whatsapp   | 2:15 |    31 |
;;
;; A dynamic block `#+BEGIN: android-usage :days 7 :top 15' renders
;; the StayFree-style top list over any range; `M-x
;; android-usage-report' shows it instantly.  Day tables are rewritten
;; idempotently, so re-importing the same export never duplicates.
;;
;; Together with `actlog' (in-Emacs hours per project/buffer) this
;; covers both halves of StayFree: what the phone did, and where the
;; Emacs part of it went.  Everything stays on the device.

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function org-dynamic-block-insert-dblock
                  "org" (type &optional interactive))

(defgroup android-usage nil
  "Android Usage Access screen-time data in Org."
  :prefix "android-usage-"
  :group 'org
  :link '(url-link :tag "Repository" "https://github.com/khaalidrafi/android-usage.el"))

(defcustom android-usage-org-file nil
  "Org file holding the usage journal, or nil to derive it.
When nil: \"android-usage.org\" under `org-directory' (or \"~/org\")."
  :type '(choice file (const nil))
  :group 'android-usage)

(defcustom android-usage-import-dir nil
  "Directory pullers scan for usage export files, or nil to derive it.
When nil, the first existing of: \"~/Download\", \"/sdcard/Download\",
\"~/storage/downloads\" (Termux), then `org-directory'."
  :type '(choice directory (const nil))
  :group 'android-usage)

(defcustom android-usage-import-regexp "\\.\\(?:csv\\|json\\)\\'"
  "File-name regexp for export files considered by the auto puller."
  :type 'regexp
  :group 'android-usage)

(defcustom android-usage-ignore-packages
  '("^com\\.android\\.systemui$")
  "Package regexps excluded from tables and totals.
System UI is noise; launchers are NOT ignored by default (StayFree
counts them as \"Home\")."
  :type '(repeat regexp)
  :group 'android-usage)

(defcustom android-usage-duration-unit :auto
  "Unit of unitless integer duration values in imports.
:auto assumes milliseconds when a value exceeds two days' worth of
seconds, otherwise seconds.  Exporters that always write one unit
can pin it to \\='seconds or \\='millis."
  :type '(choice (const :auto) (const seconds) (const millis))
  :group 'android-usage)

(defcustom android-usage-column-map nil
  "Optional plist forcing column indices, e.g. (:package 1 :seconds 3).
Keys: :date :package :label :seconds :millis :opens.  When nil,
columns are detected from the CSV header row."
  :type '(plist)
  :group 'android-usage)

(defcustom android-usage-refresh-interval 3600
  "Seconds between automatic pulls in `android-usage-mode'."
  :type 'integer
  :group 'android-usage)

(defcustom android-usage-pull-function #'android-usage--pull-latest-file
  "Function that pulls new usage data into the journal.
Default scans `android-usage-import-dir' for the newest usable
export.  Replace it (or advise it) to integrate any exporter --
the function must write day snapshots via `android-usage--write-day'."
  :type 'function
  :group 'android-usage)

(defcustom android-usage-lighter " AU"
  "Mode-line lighter for `android-usage-mode'."
  :type 'string
  :group 'android-usage)

;;; ------------------------------------------------------------------------
;;; Small helpers
;;; ------------------------------------------------------------------------

(defun android-usage--day-string (&optional time)
  "Return \"YYYY-MM-DD\" for TIME (default now)."
  (format-time-string "%Y-%m-%d" (or time (current-time))))

(defun android-usage-org-file-path ()
  "Return the expanded journal file."
  (expand-file-name
   (or android-usage-org-file
       (expand-file-name
        "android-usage.org"
        (or (bound-and-true-p org-directory) "~/org")))))

(defun android-usage-import-directory ()
  "Return the directory scanned for export files."
  (or android-usage-import-dir
      (seq-find #'file-directory-p
                '("~/Download" "/sdcard/Download" "~/storage/downloads"))
      (expand-file-name (or (bound-and-true-p org-directory) "~/org"))))

(defun android-usage--fmt-dur (secs)
  "Format SECS as \"H:MM\" (floor to minutes), StayFree style."
  (let ((mins (floor (max 0 secs) 60)))
    (format "%d:%02d" (/ mins 60) (% mins 60))))

(defun android-usage--parse-hms (value)
  "Parse \"H:MM:SS\" or \"H:MM\" VALUE into seconds, or nil."
  (when (and (stringp value)
             (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'" value))
    (let ((h (string-to-number (match-string 1 value)))
          (m (string-to-number (match-string 2 value)))
          (s (if (match-string 3 value)
                 (string-to-number (match-string 3 value))
               0)))
      (+ (* h 3600) (* m 60) s))))

(defun android-usage--duration-secs (value)
  "Parse duration VALUE into seconds; nil when unparseable.
Accepts \"H:MM:SS\", \"H:MM\", \"1h 23m 4s\" style suffixes, and bare
numbers (unit per `android-usage-duration-unit', :auto heuristic
included)."
  (cond
   ((stringp value)
    (cond
     ((string-match "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)
      (android-usage--unitless-secs (string-to-number value)))
     ((string-match-p "\\`[0-9]+:[0-9]+\\(?::[0-9]+\\)?\\'" value)
      (android-usage--parse-hms value))
     ((string-match
       "\\`\\s-*\\([0-9]+\\)\\s-*h\\(?:r?s?\\)?\\s-\\([0-9]+\\)\\s-*m\\(?:in\\)?\\(?:\\s-\\([0-9]+\\)\\s-*s\\)?\\s-*\\'"
       value)
      (+ (* 3600 (string-to-number (match-string 1 value)))
         (* 60 (string-to-number (match-string 2 value)))
         (if (match-string 3 value)
             (string-to-number (match-string 3 value))
           0)))
     ((string-match "\\`\\s-*\\([0-9]+\\)\\s-*m\\(?:in\\)?\\s-*\\'" value)
      (* 60 (string-to-number (match-string 1 value))))
     (t nil)))
   ((numberp value)
    (android-usage--unitless-secs value))
   (t nil)))

(defun android-usage--unitless-secs (n)
  "Interpret unitless duration N per `android-usage-duration-unit'."
  (pcase android-usage-duration-unit
    ('seconds n)
    ('millis (/ n 1000.0))
    (_ (if (> n 172800) (/ n 1000.0) n))))        ; >48h worth => millis

(defun android-usage--ignored-p (package)
  "Return non-nil when PACKAGE matches `android-usage-ignore-packages'."
  (and package
       (seq-some
        (lambda (re) (string-match-p re package))
        android-usage-ignore-packages)))

(defun android-usage--norm-header (s)
  "Normalize header cell S for column detection."
  (downcase
   (replace-regexp-in-string "[^a-z0-9]+" "" (downcase (or s "")))))

(defun android-usage--header-match (norm candidates)
  "Non-nil if normalized header NORM equals any of CANDIDATES."
  (member norm candidates))

;;; ------------------------------------------------------------------------
;;; CSV parsing
;;; ------------------------------------------------------------------------

(defun android-usage--split-csv (text)
  "Split CSV TEXT into rows of string fields (RFC 4180 quoting)."
  (let ((rows nil) (row nil) (field nil) in-quote
        (i 0) (len (length text)))
    (while (< i len)
      (let ((c (aref text i)))
        (cond
         (in-quote
          (cond
           ((= c ?\")
            (if (and (< (1+ i) len) (= (aref text (1+ i)) ?\"))
                (setq field (cons ?\" field) i (1+ i))
              (setq in-quote nil)))
           (t (setq field (cons c field)))))
         ((= c ?\") (setq in-quote t))
         ((= c ?,)
          (setq row (cons (apply #'string (reverse field)) row)
                field nil))
         ((= c ?\n)
          (setq row (cons (apply #'string (reverse field)) row)
                field nil)
          (setq rows (cons (nreverse row) rows) row nil))
         ((= c ?\r))                       ; CRLF: drop the CR
         (t (setq field (cons c field)))))
        (setq i (1+ i)))
    (when (or field row)
      (setq row (cons (apply #'string (reverse field)) row))
      (setq rows (cons (nreverse row) rows)))
    (nreverse rows)))

(defconst android-usage--header-candidates
  '((:date    ("date" "day" "usagedate"))
    (:package ("package" "pkg" "packagename" "apppackage" "applicationpackage"
               "application"))
    (:label   ("app" "appname" "applicationlabel" "applicationname" "name"
               "label" "title"))
    (:seconds ("seconds" "sec" "duration" "time" "usage" "foreground"
               "totaltime" "timeinforeground" "totaltimeinforeground"
               "durationinforeground" "durationseconds" "durationsecs"
               "screentime" "usageseconds"))
    (:millis  ("millis" "milliseconds" "ms" "durationmillis" "timems"
               "durationms" "durationinms" "durationinmillis"
               "durationinmilliseconds" "totaltimeinmillis" "timeinms"))
    (:opens   ("opens" "open" "launches" "launchcount" "opencount"
               "timesopened" "count")))
  "Column detection table: KEY . list of accepted normalized headers.")

(defun android-usage--detect-columns (header)
  "Return plist mapping data kinds to HEADER column indices."
  (let ((map nil))
    (dotimes (i (length header))
      (let* ((norm (android-usage--norm-header (nth i header)))
             (key (cl-loop for (k cands) in android-usage--header-candidates
                           when (android-usage--header-match norm cands)
                           return k)))
        ;; First matching column wins; later duplicates are ignored.
        (when (and key (not (plist-get map key)))
          (setq map (plist-put map key i)))))
    (unless (or (plist-get map :package) (plist-get map :label))
      (error "android-usage: no app/package column in header: %S" header))
    map))

(defun android-usage--sniff-duration-unit (rows col)
  "Heuristic unit for integer duration column COL across ROWS.
Returns \\='millis when values plainly exceed two days of seconds."
  (let ((max 0))
    (dolist (row rows)
      (let ((v (android-usage--nth col row)))
        (when (and (stringp v)
                   (string-match "\\`\\s-*\\([0-9]+\\)\\s-*\\'" v))
          (setq max (max max (string-to-number (match-string 1 v)))))))
    (if (> max 172800) 'millis 'seconds)))

(defun android-usage--parse-csv (text &optional file)
  "Parse CSV TEXT into a list of row plists.
FILE (when given) supplies the fallback day for rows without a
date column: a YYYY-MM-DD in the file name, else the file's day."
  (let* ((rows (cl-remove-if
                (lambda (r) (or (null r)
                                (cl-every (lambda (f) (string= f "")) r)))
                (android-usage--split-csv text)))
         (header (car rows))
         (data (cdr rows))
         (cols (or android-usage-column-map
                   (android-usage--detect-columns header)))
         (unit nil)
         (out nil))
    (when (and (plist-get cols :seconds)
               (eq android-usage-duration-unit :auto))
      ;; Only sniff when the unit is :auto and the column holds bare
      ;; integers (durations with ":" are unambiguous).
      (setq unit (android-usage--sniff-duration-unit
                  data (plist-get cols :seconds))))
    (let ((fallback-day (or (and file
                                 (string-match "20[0-9][0-9]-[01][0-9]-[0-3][0-9]"
                                               (file-name-nondirectory file))
                                 (match-string 0 (file-name-nondirectory file)))
                            (and file
                                 (file-exists-p file)
                                 (android-usage--day-string
                                  (nth 5 (file-attributes file))))
                            (android-usage--day-string))))
      (dolist (row data)
        (let* ((date-col (plist-get cols :date))
               (pkg-col (plist-get cols :package))
               (label-col (plist-get cols :label))
               (sec-col (plist-get cols :seconds))
               (ms-col (plist-get cols :millis))
               (open-col (plist-get cols :opens))
               (pkg (and pkg-col (< pkg-col (length row))
                         (string-trim (nth pkg-col row))))
               (label (and label-col (< label-col (length row))
                           (string-trim (nth label-col row))))
               (day (cond
                     (date-col
                      (let ((d (string-trim (android-usage--nth date-col row))))
                        (if (and (stringp d)
                                 (string-match
                                  "\\`20[0-9][0-9]-[01][0-9]-[0-3][0-9]" d))
                            (match-string 0 d)
                          fallback-day)))
                     (t fallback-day)))
               (secs (cond
                      (ms-col (let ((v (android-usage--nth ms-col row)))
                                (and (string-match
                                      "\\`\\s-*\\([0-9]+\\)\\s-*" v)
                                     (/ (string-to-number
                                         (match-string 1 v))
                                        1000.0))))
                      (sec-col
                       (let* ((v (string-trim
                                  (or (android-usage--nth sec-col row) "")))
                              (integerp-val
                               (and (string-match "\\`\\s-*[0-9]+\\s-*\\'" v)
                                    (string-to-number v))))
                         (cond
                          ;; A bare integer honours the sniffed/pinned
                          ;; unit -- :auto cannot be decided per value.
                          ((and integerp-val unit)
                           (if (eq unit 'millis) (/ integerp-val 1000.0)
                             integerp-val))
                          (integerp-val
                           (android-usage--unitless-secs integerp-val))
                          (t (android-usage--duration-secs v)))))
                      (t nil))))
          (when (and (or pkg label) secs)
            (push (list :day day
                        :package (or pkg label)
                        :label (or label pkg)
                        :seconds secs
                        :opens (and open-col
                                    (string-to-number
                                     (or (android-usage--nth open-col row)
                                         ""))))
                  out)))))
    (nreverse out)))

(defun android-usage--nth (n list)
  "Return Nth element of LIST, or nil when out of range."
  (when (and (>= n 0) (< n (length list)))
    (nth n list)))

;;; ------------------------------------------------------------------------
;;; JSON parsing (canonical schema, tolerant extras)
;;; ------------------------------------------------------------------------

(defun android-usage--parse-json (text)
  "Parse canonical usage JSON TEXT into a list of row plists.
Canonical shape:
  {\"days\": [{\"date\": \"YYYY-MM-DD\",
              \"apps\": [{\"package\": ..., \"label\": ...,
                         \"seconds\": N, \"millis\": N, \"opens\": N}]}]}
Flat {\"usage\": [{\"date\"... \"package\"... \"seconds\"...}]} lists
and a bare list of such rows are also accepted."
  (let* ((obj (json-parse-string text
                                 :object-type 'alist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil))
         (days (or (alist-get 'days obj)
                   (alist-get 'usage obj)
                   (if (consp obj) obj nil)))
         (out nil))
    (dolist (entry days)
      (let* ((day (alist-get 'date entry))
             (apps (or (alist-get 'apps entry)
                       ;; A flat row IS its own app entry.
                       (if (alist-get 'package entry) (list entry)))))
        (dolist (app apps)
          (let* ((secs (or (alist-get 'seconds app)
                           (and (alist-get 'millis app)
                                (/ (alist-get 'millis app) 1000.0))))
                 (pkg (or (alist-get 'package app)
                          (alist-get 'label app))))
            (when (and day pkg secs)
              (push (list :day day
                          :package pkg
                          :label (or (alist-get 'label app) pkg)
                          :seconds secs
                          :opens (alist-get 'opens app))
                    out))))))
    (nreverse out)))

(defun android-usage-parse (text &optional file)
  "Parse export TEXT (CSV or JSON, FILE names it) into row plists."
  (let ((trimmed (string-trim text)))
    (if (and (> (length trimmed) 0)
             (memq (aref trimmed 0) '(?\{ ?\[)))
        (android-usage--parse-json trimmed)
      (android-usage--parse-csv trimmed file))))

;;; ------------------------------------------------------------------------
;;; Aggregation
;;; ------------------------------------------------------------------------

(defun android-usage--aggregate (rows)
  "Roll ROWS into alist DAY -> alist PACKAGE -> (SECONDS . OPENS).
Labels resolve to the longest label seen per package.  Ignored
packages (per `android-usage-ignore-packages') are dropped here."
  (let ((days nil))
    (dolist (row rows)
      (unless (android-usage--ignored-p (plist-get row :package))
        (let* ((day (plist-get row :day))
               (pkg (plist-get row :package))
               (label (or (plist-get row :label) pkg))
               (day-cell (assoc day days)))
          (unless day-cell
            (setq day-cell (cons day nil))
            (push day-cell days))
          (let ((day-map (cdr day-cell)))
            (let ((cell (assoc pkg day-map)))
              (unless cell
                (setq cell (cons pkg (cons 0 0)))
                (push cell day-map))
              (setcar (cdr cell)
                      (+ (car (cdr cell)) (or (plist-get row :seconds) 0)))
              (setcdr (cdr cell)
                      (+ (cdr (cdr cell)) (or (plist-get row :opens) 0))))
            ;; Remember the label (longest label seen wins).
            (let ((lcell (assoc (concat "label:" pkg) day-map)))
              (if lcell
                  (when (< (length (cdr lcell)) (length label))
                    (setcdr lcell label))
                (push (cons (concat "label:" pkg) label) day-map)))
            (setcdr day-cell day-map)))))
    days))

;;; ------------------------------------------------------------------------
;;; Org journal
;;; ------------------------------------------------------------------------

(defun android-usage--day-heading-regexp (day)
  "Return the regexp matching DAY's level-1 heading."
  (concat "^\\* " (regexp-quote day) "\\(?: \\|$\\)"))

(defun android-usage--day-table (rows)
  "Render aggregated day ROWS (pkg -> (secs . opens)) as an Org table.
Includes labels from the label pseudo-entries; sorted by time,
descending (StayFree order)."
  (let* ((labels nil)
         (apps (cl-loop for (pkg . cell) in rows
                        unless (string-prefix-p "label:" pkg)
                        collect (cons pkg cell)))
         (label-rows (cl-loop for (pkg . label) in rows
                              when (string-prefix-p "label:" pkg)
                              collect (cons (substring pkg 6) label))))
    (dolist (lr label-rows)
      (setq labels (cons lr labels)))
    (setq apps
          (cl-sort apps #'> :key (lambda (a) (car (cdr a)))))
    (let ((total 0) (total-opens 0) (lines nil))
      (dolist (app apps)
        (let ((secs (car (cdr app))) (opens (cdr (cdr app))))
          (setq total (+ total secs) total-opens (+ total-opens opens))
          (push (format "| %s | %s | %s | %d |"
                        (or (alist-get (car app) labels nil nil #'equal)
                            (car app))
                        (car app)
                        (android-usage--fmt-dur secs)
                        opens)
                lines)))
      (concat "| App | Package | Time | Opens |\n"
              "|-" (make-string 18 ?-) "+-" (make-string 24 ?-)
              "+-" (make-string 6 ?-) "+-" (make-string 5 ?-) "-|\n"
              (mapconcat #'identity (nreverse lines) "\n")
              "\n"
              "|-" (make-string 18 ?-) "+-" (make-string 24 ?-)
              "+-" (make-string 6 ?-) "+-" (make-string 5 ?-) "-|\n"
              (format "| Total | | %s | %d |\n"
                      (android-usage--fmt-dur total) total-opens)))))

(defun android-usage--write-day (day rows)
  "Rewrite DAY's section in the journal with aggregated ROWS.
Idempotent: an existing DAY table is replaced, never duplicated."
  (let* ((file (android-usage-org-file-path))
         (make-backup-files nil)
         (create-lockfiles nil)
         (aggregated (android-usage--aggregate rows))
         (day-map (alist-get day aggregated nil nil #'equal))
         (heading (format "* %s %s" day
                          (format-time-string
                           "%a" (date-to-time (concat day " 12:00"))))))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (if (re-search-forward
                 (android-usage--day-heading-regexp day) nil t)
                (let ((sec-beg (progn (forward-line 1) (point))))
                  ;; Find the start of the next level-1 heading (or EOB).
                  (let ((sec-end
                         (save-excursion
                           (if (re-search-forward "^\\* " nil t)
                               (match-beginning 0)
                             (point-max)))))
                    (delete-region sec-beg sec-end)
                    (goto-char sec-beg)
                    (insert "\n"
                            (if day-map
                                (android-usage--day-table day-map)
                              "No usage data for this day.\n"))))
              (goto-char (point-max))
              (unless (eq (point-min) (point-max)) (insert "\n"))
              (insert heading "\n\n"
                      (if day-map
                          (android-usage--day-table day-map)
                        "No usage data for this day.\n")))
            (save-buffer))
        (kill-buffer)))
    day))

;;; ------------------------------------------------------------------------
;;; Reading the journal back
;;; ------------------------------------------------------------------------

(defun android-usage--read-day-tables ()
  "Return alist DAY -> list of (LABEL PACKAGE SECONDS OPENS).
Parsed from the journal file's per-day tables."
  (let ((file (android-usage-org-file-path))
        (days nil) (day nil))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "^\\* \\(20[0-9][0-9]-[01][0-9]-[0-3][0-9]\\)")
            (setq day (match-string 1)))
           ((and day (looking-at
                      "^| \\(.*?\\) | \\(.*?\\) | \\([0-9]+:[0-9][0-9]\\) | \\([0-9]+\\) |$"))
            (let* ((label (match-string 1))
                   (pkg (match-string 2))
                   (time-str (match-string 3))
                   (opens (string-to-number (match-string 4)))
                   (colon (string-match ":" time-str))
                   (secs (+ (* 3600 (string-to-number
                                     (substring time-str 0 colon)))
                            (* 60 (string-to-number
                                   (substring time-str (1+ colon)))))))
              (unless (equal label "Total")
                (push (list label pkg secs opens)
                      (alist-get day days nil nil #'equal))))))
          (forward-line 1))))
    (nreverse days)))

(defun android-usage--range-table (days top)
  "Render a top-TOP table over the last DAYS journal days."
  (let* ((day-list (android-usage--read-day-tables))
         (cutoff (time-subtract
                  (current-time) (days-to-time (1- days))))
         (cutoff-day (android-usage--day-string (float-time cutoff)))
         (totals nil))
    (dolist (entry day-list)
      (when (not (string< (car entry) cutoff-day))
        (dolist (row (cdr entry))
          (let* ((pkg (nth 1 row))
                 (cell (or (assoc pkg totals) (cons pkg (list 0 0 (nth 0 row))))))
            (setcar (cdr cell) (+ (cadr cell) (nth 2 row)))
            (setcar (cddr cell) (+ (car (cddr cell)) (nth 3 row)))
            (unless (assoc pkg totals)
              (push cell totals))))))
    (setq totals (cl-sort totals #'> :key (lambda (c) (cadr c))))
    (let ((lines nil) (total 0) (total-opens 0) (count 0))
      (dolist (cell totals)
        (when (< count top)
          (setq total (+ total (cadr cell))
                total-opens (+ total-opens (car (cddr cell)))
                count (1+ count))
          (push (format "| %s | %s | %s | %d |"
                        (car cell) (caddr cell)
                        (android-usage--fmt-dur (cadr cell))
                        (car (cddr cell)))
                lines)))
      (concat (format "#+name: android-usage-range\n") 
              "| App | Package | Time | Opens |\n"
              "|-" (make-string 18 ?-) "+-" (make-string 24 ?-)
              "+-" (make-string 6 ?-) "+-" (make-string 5 ?-) "-|\n"
              (mapconcat #'identity (nreverse lines) "\n")
              "\n"
              "|-" (make-string 18 ?-) "+-" (make-string 24 ?-)
              "+-" (make-string 6 ?-) "+-" (make-string 5 ?-) "-|\n"
              (format "| Total (top %d of last %d days) | | %s | %d |\n"
                      top days
                      (android-usage--fmt-dur total) total-opens)))))

;;; ------------------------------------------------------------------------
;;; Dynamic block
;;; ------------------------------------------------------------------------

;;;###autoload
(defun org-dblock-write:android-usage (params)
  "Writer for `#+BEGIN: android-usage' dynamic blocks.
PARAMS: :days (default 7) :top (default 15)."
  (let ((days (or (plist-get params :days) 7))
        (top (or (plist-get params :top) 15)))
    (insert (android-usage--range-table days top))))

;;;###autoload
(defun android-usage-insert-block (&optional days top)
  "Insert an `android-usage' dynamic block for DAYS/TOP at point."
  (interactive (list (read-number "Days: " 7) (read-number "Top apps: " 15)))
  (insert (format "#+BEGIN: android-usage :days %d :top %d\n" days top)
          "#+END:\n")
  (when (derived-mode-p 'org-mode)
    (org-update-dblock)))

;;; ------------------------------------------------------------------------
;;; Pull and import
;;; ------------------------------------------------------------------------

(defun android-usage--sniff-usage-file (file)
  "Non-nil when FILE looks like a usage export (quick content sniff)."
  (condition-case _
      (let ((head (with-temp-buffer
                    (insert-file-contents file nil 0 2048 t)
                    (buffer-string))))
        (if (string-match-p "\\`[[ \t\r\n]*{" head)
            (string-match-p "package\\|apps\\|usage" head)
          (string-match-p "package\\|app\\|duration\\|seconds\\|foreground"
                          (android-usage--norm-header head))))
    (error nil)))

(defun android-usage--import-file-rows (file)
  "Parse FILE into row plists; nil when it holds no usable rows."
  (condition-case _
      (let ((text (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
        (android-usage-parse text file))
    (error nil)))

(defun android-usage-import-file (file)
  "Import the usage export FILE into the journal.
Returns the list of day strings written."
  (interactive
   (list (read-file-name "Usage export file: "
                         (android-usage-import-directory) nil nil nil
                         android-usage-import-regexp)))
  (let ((rows (android-usage--import-file-rows file)))
    (unless rows
      (user-error "android-usage: no usage rows found in %s" file))
    (let ((days (mapcar (lambda (d) (car d)) (android-usage--aggregate rows))))
      (dolist (day days)
        (let ((day-rows (cl-remove-if-not
                         (lambda (r) (equal (plist-get r :day) day))
                         rows)))
          (android-usage--write-day day day-rows)))
      (message "android-usage: imported %s (%d rows, days: %s)"
               (file-name-nondirectory file) (length rows)
               (mapconcat #'identity (sort days #'string<) ", "))
      (sort days #'string<))))

(defun android-usage--pull-latest-file ()
  "Default puller: import the newest usable export in the import dir."
  (let* ((dir (android-usage-import-directory))
         (candidates
          (cl-sort
           (cl-remove-if
            #'file-directory-p
            (mapcar (lambda (f) (expand-file-name f dir))
                    (directory-files dir nil android-usage-import-regexp)))
           #'file-newer-than-file-p))
         (imported-days nil))
    (while (and candidates (null imported-days))
      (let ((file (pop candidates)))
        (when (android-usage--sniff-usage-file file)
          (condition-case err
              (setq imported-days
                    (android-usage-import-file file))
            (error (message "android-usage: skipping %s: %s"
                            (file-name-nondirectory file)
                            (error-message-string err)))))))
    (or imported-days
        (and (featurep 'android)
             (message
              "android-usage: no export found in %s -- export from your usage app (or grant Emacs \"All files access\")" dir)
             nil))))

;;;###autoload
(defun android-usage-refresh ()
  "Pull new usage data via `android-usage-pull-function'."
  (interactive)
  (let ((days (funcall android-usage-pull-function)))
    (if days
        (message "android-usage: journal updated (%d day(s))"
                 (length days))
      (message "android-usage: nothing new pulled")))
  (when (file-exists-p (android-usage-org-file-path))
    (android-usage-update-blocks)))

(defun android-usage-update-blocks ()
  "Refresh every android-usage dynamic block in the journal."
  (let* ((file (android-usage-org-file-path))
         (buffer (find-buffer-visiting file)))
    (when (and buffer
               (eq 'org-mode (buffer-local-value 'major-mode buffer)))
      (with-current-buffer buffer
        (org-update-all-dblocks)))))

;;; ------------------------------------------------------------------------
;;; Mode and report
;;; ------------------------------------------------------------------------

(defvar android-usage--timer nil)

(defun android-usage--tick ()
  "One scheduled pull (errors never break the timer)."
  (condition-case err
      (android-usage-refresh)
    (error (message "android-usage: pull failed: %s"
                    (error-message-string err)))))

;;;###autoload
(define-minor-mode android-usage-mode
  "Periodically pull Android usage data into the Org journal."
  :global t
  :lighter android-usage-lighter
  :group 'android-usage
  (if android-usage-mode
      (setq android-usage--timer
            (run-at-time 0 android-usage-refresh-interval
                         #'android-usage--tick))
    (when android-usage--timer
      (cancel-timer android-usage--timer))
    (setq android-usage--timer nil)))

;;;###autoload
(defun android-usage-report (&optional days)
  "Show a StayFree-style top-apps report for the last DAYS (7)."
  (interactive (list (read-number "Days: " 7)))
  (require 'org nil t)
  (let* ((days (or days 7))
         (table (android-usage--range-table days 15))
         (buffer (get-buffer-create "*Android Usage*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (fboundp 'org-mode) (org-mode) (fundamental-mode))
        (insert (format "#+title: Android usage, last %d days\n\n" days)
                table)
        (goto-char (point-min)))
      (display-buffer buffer))
    buffer))

;;;###autoload
(defun android-usage-open-journal ()
  "Open the usage journal Org file."
  (interactive)
  (find-file (android-usage-org-file-path)))

;; org-dynamic-block-insert-dblock completes over this alist (org 9.x).
(with-eval-after-load 'org
  (when (boundp 'org-dynamic-block-alist)
    (add-to-list 'org-dynamic-block-alist
                 '("android-usage" . android-usage-insert-block))))

(provide 'android-usage)
;;; android-usage.el ends here
