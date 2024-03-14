;;; org-scryfall.el --- Utilities for interacting with Scryfall data in Org
;;; Commentary:

;;; Code:
(defun get-json-data-from-url (url)
  "Retrieve JSON data from URL."
  (with-current-buffer (url-retrieve-synchronously url t nil 10)
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (let ((parsed-json 'error))
      (ignore-errors
        ;; if there's a problem parsing the JSON
        ;; parsed-json ==> 'error
        (if (fboundp 'json-parse-buffer)
            ;; delete everything that isn't JSON (headers)
            (setq parsed-json (json-parse-buffer
                               :object-type 'alist))
          ;; Legacy :)
          (setq parsed-json (json-read))))
      (kill-buffer) ;; don't litter with API buffers
      parsed-json)))

(defun scryfall-card-data (card-name)
  "Retrieve card data for CARD-NAME from the Scryfall API."
  (let* ((url (concat "https://api.scryfall.com/cards/named?exact="
                      (url-hexify-string card-name))))
    (get-json-data-from-url url)))

(defun scryfall-card-data-to-csv (json-object)
  "Convert the specified card JSON-OBJECT to CSV and output it in an \"org-mode\" \nsrc region."
  (with-current-buffer (get-buffer-create "*Card Data*")
    (erase-buffer)
    (insert (format "| %s | %s | %s | %s | %s |\n"
                    "Name" "Mana Cost" "Type" "Rarity" "Set"))
    (insert "|---|---|---|---|---|\n")
    (insert (format "| %s | %s | %s | %s | %s |\n"
                    (cdr (assoc 'name json-object))
                    (cdr (assoc 'mana_cost json-object))
                    (cdr (assoc 'type_line json-object))
                    (cdr (assoc 'rarity json-object))
                    (cdr (assoc 'set_name json-object))))
    (org-mode)
    (forward-line -1)
    (org-ctrl-c-ctrl-c)
    (buffer-string)))

(defun scryfall-card-data-csv (card-name)
  "Retrieve card data for CARD-NAME from the Scryfall API and output it as a \nCSV in an \"org-mode\" src block, replacing the card name at the cursor."
  (let ((csv-data (scryfall-card-data-to-csv (scryfall-card-data card-name))))
    (delete-region (point) (progn (forward-word) (point)))
    (insert (concat "#+BEGIN_SRC csv\n" csv-data "#+END_SRC"))))

;;; org-scryfall.el ends here
