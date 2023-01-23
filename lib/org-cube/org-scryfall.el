;;; org-scryfall.el --- Utilities for interacting with Scryfall data in Org
;;; Commentary:

;;; Code:
(defun scryfall-card-data (card-name)
  "Retrieve card data for CARD-NAME from the Scryfall API."
  (let* ((api-key "your-api-key-here")
         (url (concat "https://api.scryfall.com/cards/named?exact="
                      (url-hexify-string card-name)))
         (response (url-retrieve-synchronously (concat url "&api_key=" api-key)))
         (json-string (with-current-buffer response (buffer-string)))
         (json-object (json-read-from-string json-string)))
    json-object))

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
    (org-ctrl-c-ctrl-c)))

(defun scryfall-card-data-csv (card-name)
  "Retrieve card data for CARD-NAME from the Scryfall API and output it as a \nCSV in an \"org-mode\" src block, replacing the card name at the cursor."
  (let ((csv-data (with-temp-buffer
                    (scryfall-card-data-to-csv (scryfall-card-data card-name))
                    (buffer-string))))
    (delete-region (point) (progn (forward-word) (point)))
    (insert (concat "#+BEGIN_SRC csv\n" csv-data "#+END_SRC"))
    (org-ctrl-c-ctrl-c)))

;;; org-scryfall.el ends here
