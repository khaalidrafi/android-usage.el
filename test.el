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

(defconst android-usage-test--csv-date-col
  "Date,Application label,Package,Duration (seconds),Opens
2026-11-05,WhatsApp,org.whatsapp,3600,12
2026-11-05,GNU Emacs,org.gnu.emacs,7200,40
2026-11-06,WhatsApp,org.whatsapp,1800,5
")

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
  "{\"days\": [
     {\"date\": \"2026-11-05\",
      \"apps\": [{\"package\": \"org.whatsapp\", \"label\": \"WhatsApp\",
                  \"seconds\": 3600, \"opens\": 12}]}]}")

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
    (should (equal (plist-get (nth 0 rows) :day) "2026-11-05"))
    (should (equal (plist-get (nth 0 rows) :package) "org.whatsapp"))
    (should (equal (plist-get (nth 0 rows) :label) "WhatsApp"))
    (should (= (plist-get (nth 0 rows) :seconds) 3600))
    (should (= (plist-get (nth 0 rows) :opens) 12))))

(ert-deftest android-usage--parse-csv-stayfree-like ()
  (let* ((file "/tmp/stayfree-2026-11-06.csv")
         (rows (android-usage--parse-csv
                android-usage-test--csv-stayfree-like file)))
    (should (= (length rows) 2))
    (should (equal (plist-get (nth 0 rows) :day) "2026-11-06"))
    (should (= (plist-get (nth 0 rows) :seconds) 5400))
    (should (= (plist-get (nth 1 rows) :seconds) 2730))))

(ert-deftest android-usage--parse-csv-millis ()
  (let ((rows (android-usage--parse-csv android-usage-test--csv-ms)))
    (should (= (plist-get (nth 0 rows) :seconds) 1.5))
    (should (= (plist-get (nth 1 rows) :seconds) 90))))

(ert-deftest android-usage--parse-json-canonical ()
  (let ((rows (android-usage--parse-json android-usage-test--json)))
    (should (= (length rows) 1))
    (should (equal (plist-get (nth 0 rows) :day) "2026-11-05"))
    (should (equal (plist-get (nth 0 rows) :package) "org.whatsapp"))
    (should (= (plist-get (nth 0 rows) :seconds) 3600))
    (should (= (plist-get (nth 0 rows) :opens) 12))))

(ert-deftest android-usage--aggregate-sums-and-labels ()
  (let* ((rows (android-usage--parse-csv android-usage-test--csv-date-col))
         (days (android-usage--aggregate rows))
         (d05 (cdr (assoc "2026-11-05" days))))
    (should (= (length days) 2))
    (should (= (car (cdr (assoc "org.whatsapp" d05))) 3600))
    (should (= (cdr (cdr (assoc "org.whatsapp" d05))) 12))
    (should (equal (cdr (assoc "label:org.whatsapp" d05)) "WhatsApp"))
    (should (= (car (cdr (assoc "org.gnu.emacs" d05))) 7200))))

(ert-deftest android-usage--aggregate-longest-label-wins ()
  (let ((days (android-usage--aggregate
               (list (list :day "2026-11-05" :package "org.x" :label "X"
                           :seconds 10 :opens 1)
                     (list :day "2026-11-05" :package "org.x" :label "App X"
                           :seconds 20 :opens 1)))))
    (should (equal (cdr (assoc "label:org.x" (cdr (assoc "2026-11-05" days))))
                   "App X"))))

(ert-deftest android-usage--aggregate-ignores ()
  (let ((android-usage-ignore-packages '("^com\\.android\\.systemui$")))
    (let ((days (android-usage--aggregate
                 (list (list :day "2026-11-05"
                             :package "com.android.systemui"
                             :label "System UI" :seconds 999 :opens 9)
                       (list :day "2026-11-05" :package "org.x" :label "X"
                             :seconds 10 :opens 1)))))
      (should (null (assoc "com.android.systemui"
                           (cdr (assoc "2026-11-05" days))))))))

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
   (let ((rows (list (list :day "2026-11-05" :package "org.x" :label "X"
                           :seconds 60 :opens 1))))
     (android-usage--write-day "2026-11-05" rows)
     (android-usage--write-day "2026-11-05" rows)
     (with-temp-buffer
       (insert-file-contents android-usage-org-file)
       (let ((s (buffer-string)))
         (should (= (android-usage-test--occurrences "| App | Package |" s) 1))
         (should (= (android-usage-test--occurrences "| Total |" s) 1))
         (should (string-match-p "^\\* 2026-11-05 " s)))))))

(ert-deftest android-usage--read-day-tables-roundtrip ()
  (android-usage-test--with-journal
   (android-usage--write-day
    "2026-11-05"
    (list (list :day "2026-11-05" :package "org.x" :label "X"
                :seconds 3660 :opens 3)))
   (let ((days (android-usage--read-day-tables)))
     (should (= (length days) 1))
     (should (equal (car (car days)) "2026-11-05"))
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
     (should (equal (android-usage--pull-latest-file)
                    '("2026-11-05" "2026-11-06")))
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
    "2026-11-05"
    (list (list :day "2026-11-05" :package "org.x" :label "X"
                :seconds 60 :opens 1)))
   (let ((buf (android-usage-report 7)))
     (with-current-buffer buf
       (should (string-match-p "org\\.x" (buffer-string)))))))
