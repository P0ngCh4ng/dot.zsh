(defun advance-todos-schedule-from-today (time-string)
  "Interactively advance the schedule time of the TODOs with names and seconds in TODO-NAMES-SECONDS from today's TIME-STRING."
  (interactive "sEnter time (HH:MM:SS): ")
  (let* ((date-string (format-time-string "%Y-%m-%d"))
         (time-string (concat date-string " " time-string))
         (time (date-to-time time-string))
         (todo-names-seconds (with-temp-buffer
                               (insert-file-contents "~/org/list.el")
                               (read (buffer-string)))))
    (save-excursion
      (dolist (todo-name-seconds todo-names-seconds)
        (let ((todo-name (car todo-name-seconds))
              (seconds (cadr todo-name-seconds)))
          (goto-char (point-min))
          (while (re-search-forward todo-name nil t)
            (when (org-at-heading-p)
              (org-schedule nil (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                    (time-add time seconds))))))))))


;; フロー
;; 時間を入力
;; 入力された時間をもとにタスクごとの時間を決定
;; 各TODOの時間をそれに応じてずらす
