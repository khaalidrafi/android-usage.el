;;; test.el --- ERT tests for android-usage.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'android-usage)

(defun android-usage-test--occurrences (needle haystack)
  "Count non-overlapping NEEDLE occurrences in HAYSTACK."
  (let ((n 0) (start 0))
    (while (string-match needle haystack start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defmacro android-usage-test--with-journal (&rest body)
  "Run BODY with `android-usage-org-file' at a fresh temp file."
  `(let ((android-usage-org-file
          (make-temp-file "android-usage-test" nil ".org")))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file android-usage-org-file)))))

;; Fixture days are computed at load time (2 and 3 days ago) so the
;; "last N days" window filters can never rot as real time passes;
;; the first version of this suite hardcoded 2026-11-05/06 and went
;; red nine days later for exactly that reason.
(defconst android-usage-test--day-a
  (format-time-string "%Y-%m-%d" (time-subtract nil (* 2 86400)))
  "Fixture day A: two days ago (inside every range window used).")

(defconst android-usage-test--day-b
  (format-time-string "%Y-%m-%d" (time-subtract nil (* 3 86400)))
  "Fixture day B: three days ago.")

(defconst android-usage-test--csv-date-col
  (concat "Date,Application label,Package,Duration (seconds),Opens\n"
          android-usage-test--day-a ",WhatsApp,org.whatsapp,3600,12\n"
          android-usage-test--day-a ",GNU Emacs,org.gnu.emacs,7200,40\n"
          android-usage-test--day-b ",WhatsApp,org.whatsapp,1800,5\n"))

(defconst android-usage-test--csv-stayfree-like
  "App,Package,Time,Opens
WhatsApp,org.whatsapp,1:30:00,7
Maps,com.google.android.apps.maps,0:45:30,3
")

(defconst android-usage-test--csv-ms
  "Package,Duration in ms
org.whatsapp,1500
org.gnu.emacs,90000
")

(defconst android-usage-test--json
  (concat "{\"days\": [
     {\"date\": \"" android-usage-test--day-a "\",
      \"apps\": [{\"package\": \"org.whatsapp\", \"label\": \"WhatsApp\",
                  \"seconds\": 3600, \"opens\": 12}]}]}"))

(ert-deftest android-usage--split-csv-quoted ()
  (should (equal
           (android-usage--split-csv
            "a,b\n\"x, y\",\"say \"\"hi\"\"\"\r\nz,w\n")
           '(("a" "b") ("x, y" "say \"hi\"") ("z" "w")))))

(ert-deftest android-usage--split-csv-no-trailing-newline ()
  (should (equal (android-usage--split-csv "a,b\nc,d")
                 '(("a" "b") ("c" "d")))))

(ert-deftest android-usage--parse-hms ()
  (should (= (android-usage--parse-hms "1:23:45") 5025))
  (should (= (android-usage--parse-hms "1:23") 4980))
  (should (= (android-usage--parse-hms "0:05") 300))
  (should (null (android-usage--parse-hms "5"))))

(ert-deftest android-usage--duration-secs-formats ()
  (let ((android-usage-duration-unit :auto))
    (should (= (android-usage--duration-secs "45") 45))
    (should (= (android-usage--duration-secs "1h 23m") 4980))
    (should (= (android-usage--duration-secs "1h 5m 10s") 3910))
    (should (= (android-usage--duration-secs "45m") 2700))
    (should (= (android-usage--duration-secs "2:05") 7500))
    ;; :auto: only past the two-day threshold is it milliseconds
    (should (= (android-usage--duration-secs 90000) 90000))
    (should (= (android-usage--duration-secs 200000) 200))
    (should (null (android-usage--duration-secs "not a time")))))

(ert-deftest android-usage--duration-unit-pinned ()
  (let ((android-usage-duration-unit 'millis))
    (should (= (android-usage--duration-secs 500) 0.5)))
  (let ((android-usage-duration-unit 'seconds))
    (should (= (android-usage--duration-secs 90000) 90000))))

(ert-deftest android-usage--norm-header ()
  (should (equal (android-usage--norm-header "Duration (ms)") "durationms"))
  (should (equal (android-usage--norm-header " Application Label ")
                 "applicationlabel")))

(ert-deftest android-usage--detect-columns-maps ()
  (let ((cols (android-usage--detect-columns
               '("Date" "Application label" "Package"
                 "Duration (seconds)" "Opens"))))
    (should (= (plist-get cols :date) 0))
    (should (= (plist-get cols :label) 1))
    (should (= (plist-get cols :package) 2))
    (should (= (plist-get cols :seconds) 3))
    (should (= (plist-get cols :opens) 4))))

(ert-deftest android-usage--parse-csv-date-column ()
  (let ((rows (android-usage--parse-csv android-usage-test--csv-date-col)))
    (should (= (length rows) 3))
    (should (equal (plist-get (nth 0 rows) :day) android-usage-test--day-a))
    (should (equal (plist-get (nth 0 rows) :package) "org.whatsapp"))
    (should (equal (plist-get (nth 0 rows) :label) "WhatsApp"))
    (should (= (plist-get (nth 0 rows) :seconds) 3600))
    (should (= (plist-get (nth 0 rows) :opens) 12))))

(ert-deftest android-usage--parse-csv-stayfree-like ()
  (let* ((file (concat "/tmp/stayfree-" android-usage-test--day-b ".csv"))
         (rows (android-usage--parse-csv
                android-usage-test--csv-stayfree-like file)))
    (should (= (length rows) 2))
    (should (equal (plist-get (nth 0 rows) :day) android-usage-test--day-b))
    (should (= (plist-get (nth 0 rows) :seconds) 5400))
    (should (= (plist-get (nth 1 rows) :seconds) 2730))))

(ert-deftest android-usage--parse-csv-millis ()
  (let ((rows (android-usage--parse-csv android-usage-test--csv-ms)))
    (should (= (plist-get (nth 0 rows) :seconds) 1.5))
    (should (= (plist-get (nth 1 rows) :seconds) 90))))

(ert-deftest android-usage--parse-json-canonical ()
  (let ((rows (android-usage--parse-json android-usage-test--json)))
    (should (= (length rows) 1))
    (should (equal (plist-get (nth 0 rows) :day) android-usage-test--day-a))
    (should (equal (plist-get (nth 0 rows) :package) "org.whatsapp"))
    (should (= (plist-get (nth 0 rows) :seconds) 3600))
    (should (= (plist-get (nth 0 rows) :opens) 12))))

(ert-deftest android-usage--aggregate-sums-and-labels ()
  (let* ((rows (android-usage--parse-csv android-usage-test--csv-date-col))
         (days (android-usage--aggregate rows))
         (d05 (cdr (assoc android-usage-test--day-a days))))
    (should (= (length days) 2))
    (should (= (car (cdr (assoc "org.whatsapp" d05))) 3600))
    (should (= (cdr (cdr (assoc "org.whatsapp" d05))) 12))
    (should (equal (cdr (assoc "label:org.whatsapp" d05)) "WhatsApp"))
    (should (= (car (cdr (assoc "org.gnu.emacs" d05))) 7200))))

(ert-deftest android-usage--aggregate-longest-label-wins ()
  (let ((days (android-usage--aggregate
               (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                           :seconds 10 :opens 1)
                     (list :day android-usage-test--day-a :package "org.x" :label "App X"
                           :seconds 20 :opens 1)))))
    (should (equal (cdr (assoc "label:org.x" (cdr (assoc android-usage-test--day-a days))))
                   "App X"))))

(ert-deftest android-usage--aggregate-ignores ()
  (let ((android-usage-ignore-packages '("^com\\.android\\.systemui$")))
    (let ((days (android-usage--aggregate
                 (list (list :day android-usage-test--day-a
                             :package "com.android.systemui"
                             :label "System UI" :seconds 999 :opens 9)
                       (list :day android-usage-test--day-a :package "org.x" :label "X"
                             :seconds 10 :opens 1)))))
      (should (null (assoc "com.android.systemui"
                           (cdr (assoc android-usage-test--day-a days))))))))

(ert-deftest android-usage--day-table-sorted-with-total ()
  (let ((table (android-usage--day-table
                (list (cons "label:org.a" "App A")
                      (cons "org.a" (cons 60 1))
                      (cons "org.b" (cons 3600 2))))))
    (should (string-match-p "| App A | org\\.a | 0:01 | 1 |" table))
    (should (string-match-p "| org\\.b | org\\.b | 1:00 | 2 |" table))
    ;; sorted descending: org.b row must appear before org.a row
    (should (< (string-match "org\\.b" table) (string-match "org\\.a" table)))
    (should (string-match-p "| Total | | 1:01 | 3 |" table))))

(ert-deftest android-usage--write-day-idempotent ()
  (android-usage-test--with-journal
   (let ((rows (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                           :seconds 60 :opens 1))))
     (android-usage--write-day android-usage-test--day-a rows)
     (android-usage--write-day android-usage-test--day-a rows)
     (with-temp-buffer
       (insert-file-contents android-usage-org-file)
       (let ((s (buffer-string)))
         (should (= (android-usage-test--occurrences "| App | Package |" s) 1))
         (should (= (android-usage-test--occurrences "| Total |" s) 1))
         (should (string-match-p (concat "^\\* " (regexp-quote android-usage-test--day-a) " ") s)))))))

(ert-deftest android-usage--read-day-tables-roundtrip ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                :seconds 3660 :opens 3)))
   (let ((days (android-usage--read-day-tables)))
     (should (= (length days) 1))
     (should (equal (car (car days)) android-usage-test--day-a))
     (let ((row (cadr (car days))))
       (should (equal (nth 0 row) "X"))
       (should (equal (nth 1 row) "org.x"))
       (should (= (nth 2 row) 3660))
       (should (= (nth 3 row) 3))))))

(ert-deftest android-usage--import-and-range ()
  (android-usage-test--with-journal
   (with-temp-file "/tmp/au-test-export.csv"
     (insert android-usage-test--csv-date-col))
   (android-usage-import-file "/tmp/au-test-export.csv")
   (let ((table (android-usage--range-table 7 15)))
     (should (string-match-p "org\\.whatsapp" table))
     (should (string-match-p "Total (top 15 of last 7 days)" table))
     ;; 3600+7200+1800 = 12600s = 3:30
     (should (string-match-p "3:30" table)))
   (ignore-errors (delete-file "/tmp/au-test-export.csv"))))

(ert-deftest android-usage--pull-latest-imports-newest ()
  (android-usage-test--with-journal
   (let* ((dir (make-temp-file "au-pull" t))
          (android-usage-import-dir dir)
          (old (expand-file-name "usage-old.csv" dir))
          (new (expand-file-name "usage-new.csv" dir)))
     ;; old: no package column (unusable); new: valid
     (with-temp-file old (insert "foo,bar\n1,2\n"))
     (sleep-for 0 20)
     (with-temp-file new (insert android-usage-test--csv-date-col))
     ;; days come back date-ascending (day-b is older than day-a)
     (should (equal (android-usage--pull-latest-file)
                    (list android-usage-test--day-b android-usage-test--day-a)))
     (should (file-exists-p android-usage-org-file))
     (delete-directory dir t))))

(ert-deftest android-usage--pull-no-file-returns-nil ()
  (android-usage-test--with-journal
   (let ((android-usage-import-dir (make-temp-file "au-empty" t)))
     (should (null (android-usage--pull-latest-file)))
     (delete-directory android-usage-import-dir t))))

(ert-deftest android-usage--dblock-writes-table ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: android-usage :days 7 :top 5\n#+END:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function #'android-usage--range-table)
               (lambda (_days _top)
                 "| App | Package | Time | Opens |\n| X | org.x | 1:00 | 1 |\n")))
      (org-update-all-dblocks))
    (should (string-match-p "org\\.x" (buffer-string)))
    (should (string-match-p "#\\+BEGIN: android-usage" (buffer-string)))))

(ert-deftest android-usage--report-buffer ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                :seconds 60 :opens 1)))
   (let ((buf (android-usage-report 7)))
     (with-current-buffer buf
       (should (string-match-p "org\\.x" (buffer-string)))))))

;;; ---------------------------------------------------------------------
;;; Charts (v0.2)
;;; ---------------------------------------------------------------------

(ert-deftest android-usage--xml-escape ()
  (should (equal (android-usage--xml-escape "a&b<c>\"d\"'e'")
                 "a&amp;b&lt;c&gt;&quot;d&quot;&apos;e&apos;"))
  (should (equal (android-usage--xml-escape "plain") "plain")))

(ert-deftest android-usage--svg-hbars-renders-bars ()
  (let ((svg (android-usage--svg-hbars
               '(("App A" . 3600) ("App B" . 60)) "Top apps")))
    (should (string-match-p "<svg" svg))
    ;; background + one bar rect per row
    (should (= (android-usage-test--occurrences "<rect" svg) 3))
    (should (string-match-p "App A" svg))
    (should (string-match-p "App B" svg))
    (should (string-match-p "1:00" svg))
    (should (string-match-p "Top apps" svg))))

(ert-deftest android-usage--svg-hbars-empty ()
  (should (string-match-p "no usage data"
                          (android-usage--svg-hbars nil))))

(ert-deftest android-usage--svg-hbars-escapes-labels ()
  (let ((svg (android-usage--svg-hbars '(("A&B <x>" . 60)))))
    (should (string-match-p "A&amp;B &lt;x&gt;" svg))
    (should-not (string-match-p "A&B" svg))))

(ert-deftest android-usage--svg-vbars-renders-days ()
  (let ((svg (android-usage--svg-vbars
              `((,android-usage-test--day-a . 3600) (,android-usage-test--day-b . 1800)) "Trend")))
    (should (string-match-p "<svg" svg))
    (should (= (android-usage-test--occurrences "<rect" svg) 3))
    (should (string-match-p (substring android-usage-test--day-a 5) svg))
    (should (string-match-p (substring android-usage-test--day-b 5) svg))
    (should (string-match-p "1:00" svg))))

(ert-deftest android-usage--range-day-totals-sums-days ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.a" :label "A"
                :seconds 3600 :opens 1)
          (list :day android-usage-test--day-a :package "org.b" :label "B"
                :seconds 7200 :opens 2)))
   (android-usage--write-day
    android-usage-test--day-b
    (list (list :day android-usage-test--day-b :package "org.a" :label "A"
                :seconds 1800 :opens 1)))
   ;; Force both days inside a 7-day window regardless of real date.
   (cl-letf (((symbol-function #'android-usage--read-day-tables)
              (lambda ()
                (list (cons android-usage-test--day-a
                            (list (list "A" "org.a" 3600 1)
                                  (list "B" "org.b" 7200 2)))
                      (cons android-usage-test--day-b
                            (list (list "A" "org.a" 1800 1)))))))
     (should (equal (android-usage--range-day-totals 7)
                    `((,android-usage-test--day-b . 1800) (,android-usage-test--day-a . 10800)))))))

(ert-deftest android-usage--day-top-sorts-and-falls-back ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.a" :label "A"
                :seconds 60 :opens 1)
          (list :day android-usage-test--day-a :package "org.b" :label "B"
                :seconds 3600 :opens 2)))
   ;; known day
   (should (equal (android-usage--day-top android-usage-test--day-a 1)
                  '(("B" . 3600))))
   ;; unknown day falls back to last day with data
   (should (= (length (android-usage--day-top "1999-01-01")) 2))
   ;; empty journal -> nil
   (cl-letf (((symbol-function #'android-usage--read-day-tables) #'ignore))
     (should (null (android-usage--day-top nil))))))

(ert-deftest android-usage--write-chart-rel-path ()
  (android-usage-test--with-journal
   (let ((link (android-usage--write-chart "t.svg" "<svg/>")))
     (should (equal link "charts/t.svg"))
     (should (file-exists-p
              (expand-file-name "t.svg" (android-usage-chart-dir)))))))

(ert-deftest android-usage--day-table-org ()
  (let ((table (android-usage--day-table-org
                (list (list "A" "org.a" 60 1)
                      (list "B" "org.b" 3660 2)))))
    (should (string-match-p "| B | org\\.b | 1:01 | 2 |" table))
    (should (< (string-match "org\\.b" table) (string-match "org\\.a" table)))
    (should (string-match-p "| Total | | 1:02 | 3 |" table))))

(ert-deftest android-usage--dblock-with-chart-inserts-links ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                :seconds 60 :opens 1)))
   (with-temp-buffer
     (org-mode)
     (insert "#+BEGIN: android-usage :days 7 :top 5\n#+END:\n")
     (goto-char (point-min))
     (org-update-all-dblocks)
     (let ((s (buffer-string)))
       (should (string-match-p "org\\.x" s))
       (should (string-match-p "\\[\\[file:charts/usage-trend-7d\\.svg\\]\\]" s))
       (should (string-match-p "\\[\\[file:charts/usage-top-day\\.svg\\]\\]" s))))
   (ignore-errors
     (delete-directory (android-usage-chart-dir) t))))

(ert-deftest android-usage--dblock-chart-off ()
  (android-usage-test--with-journal
   (with-temp-buffer
     (org-mode)
     (insert "#+BEGIN: android-usage :days 7 :top 5 :chart nil\n#+END:\n")
     (goto-char (point-min))
     (cl-letf (((symbol-function #'android-usage--range-table)
                (lambda (_d _t) "| T |\n")))
       (org-update-all-dblocks))
     (should-not (string-match-p "file:charts" (buffer-string))))))

(ert-deftest android-usage--dblock-day-writes-table-and-chart ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                :seconds 60 :opens 1)))
   (with-temp-buffer
     (org-mode)
     (insert (format "#+BEGIN: android-usage-day :date %S\n#+END:\n" android-usage-test--day-a))
     (goto-char (point-min))
     (org-update-all-dblocks)
     (let ((s (buffer-string)))
       (should (string-match-p "org\\.x" s))
       (should (string-match-p "\\[\\[file:charts/usage-top-day\\.svg\\]\\]" s))))
   (ignore-errors
     (delete-directory (android-usage-chart-dir) t))))

(ert-deftest android-usage--day-report-buffer ()
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "X"
                :seconds 60 :opens 1)))
   (let ((buf (android-usage-day-report android-usage-test--day-a)))
     (with-current-buffer buf
       (should (string-match-p "org\\.x" (buffer-string)))
       (should (string-match-p "file:charts/usage-top-day" (buffer-string)))))))

(ert-deftest android-usage--capture-snip-empty-and-data ()
  (android-usage-test--with-journal
   (should (equal (android-usage--capture-snip) "No usage data yet today."))
   (android-usage--write-day
    (android-usage--day-string)
    (list (list :day (android-usage--day-string) :package "org.x"
                :label "X" :seconds 60 :opens 1)))
   (should (string-match-p "Total: 0:01 | X (0:01)"
                           (android-usage--capture-snip)))))

(ert-deftest android-usage--capture-template-shape ()
  (should (equal (car android-usage-capture-template) "au"))
  (should (string-match-p "android-usage--capture-snip"
                          (nth 4 android-usage-capture-template)))
  ;; setup-capture installs into org-capture-templates
  (require 'org-capture)
  (let ((org-capture-templates nil))
    (android-usage-setup-capture)
    (should (member android-usage-capture-template org-capture-templates))))

(ert-deftest android-usage--import-alias ()
  (should (fboundp 'android-usage-import)))

(ert-deftest android-usage--range-table-shows-labels ()
  ;; Regression (v0.1): the App column showed the package and Opens
  ;; was printed twice; the label column must show the real label.
  (android-usage-test--with-journal
   (android-usage--write-day
    android-usage-test--day-a
    (list (list :day android-usage-test--day-a :package "org.x" :label "GNU Emacs"
                :seconds 3660 :opens 7)))
   (cl-letf (((symbol-function #'android-usage--read-day-tables)
              (lambda ()
                (list (cons android-usage-test--day-a
                            (list (list "GNU Emacs" "org.x" 3660 7)))))))
     (let ((table (android-usage--range-table 7 15)))
       (should (string-match-p "| GNU Emacs | org\\.x | 1:01 | 7 |" table))
       ;; label must appear in column 1, package in column 2
       (should (< (string-match "GNU Emacs" table)
                  (string-match "org\\.x" table)))))))
