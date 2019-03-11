;;; anki.el --- Sync decks with Anki  -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs wrapper around the anki-connect REST plugin API. See:
;;
;;   https://github.com/FooSoft/anki-connect

;;; Code:

(require 'dash)
(require 'json)
(require 'request)
(require 's)

(defconst anki-url "http://localhost:8765")

(defun anki--parse-json-response ()
  (-let [(&alist 'result result 'error err) (json-read-from-string (buffer-string))]
    (cond
     (err
      (error "%s" err))
     ;; If we have no error and 'null' as the response, assume this was a
     ;; side-effecting operation that succeeded.
     ((null result)
      t)
     (t
      result))))

(defun anki--request (method action data)
  (let* ((body `(("action" . ,(s-lower-camel-case (symbol-name action)))
                 ("version" . 6)
                 ,@(when data
                     `((params . ,data)))))
         (response
          (request anki-url
                   :type method
                   :sync t
                   :parser #'anki--parse-json-response
                   :data (json-encode body)
                   :error (-lambda (&rest args)
                            (-let [(&plist :error-thrown (_ . reason)) args]
                              (error "%s" reason))))))
    (request-response-data response)))


;; Horrific DSL for defining API actions with parameter validation.

(eval-when-compile
  (defun anki--marshall-seq (ty)
    ;; Check that `ty' is actually marshallable.
    (anki--param-marshal ty)
    (lambda (coll)
      (cl-assert (sequencep coll))
      (seq-into (seq-map (anki--param-marshal ty) coll)
                'vector)))

  (defun anki--marshall-alist (value-type)
    ;; Check that `value-type' is actually marshallable.
    (anki--param-marshal value-type)
    (lambda (coll)
      (cl-assert (listp coll))
      (seq-map (-lambda ((k . v))
                 (cons k (funcall (anki--param-marshal value-type) v)))
               coll)))

  (defun anki--marshall-nullable (ty)
    (lambda (it)
      (if (null it)
          nil
        (funcall (anki--param-marshal ty) it))))

  (defun anki--marshall-boolean (obj)
    (cl-assert (booleanp obj))
    (if obj t :json-false))

  (defun anki--marshall-string (it)
    (cl-assert (stringp it))
    it)

  (defun anki--marshall-config (it)
    (cl-assert (listp it))
    (cl-assert (assoc 'name it))
    it)

  (defun anki--marshall-nat (it)
    (cl-assert (or (zerop it) (plusp it)))
    (cl-assert (integerp it))
    it)

  (defun anki--marshall-card-template (it)
    (cl-assert (listp it))
    it)

  (defun anki--marshall-note (it)
    (cl-assert (listp it))
    (cl-assert (stringp (alist-get 'deckName it)))
    (cl-assert (stringp (alist-get 'modelName it)))
    (cl-assert (listp (alist-get 'fields it)))
    (cl-assert (vectorp (alist-get 'tags it)))
    it)

  (defun anki--marshall-note-update (it)
    (cl-assert (listp it))
    (cl-assert (integerp (alist-get 'id it)))
    (cl-assert (listp (alist-get 'fields it)))
    it)

  (defun anki--param-marshal (ty)
    (pcase (-list ty)
      (`(seq ,ty)
       (anki--marshall-seq ty))
      (`(nullable ,ty)
       (anki--marshall-nullable ty))
      (`(config)
       #'anki--marshall-config)
      (`(note)
       #'anki--marshall-note)
      (`(note-update)
       #'anki--marshall-note-update)
      (`(card-template)
       (anki--marshall-alist 'string))
      (`(boolean)
       #'anki--marshall-boolean)
      (`(nat)
       #'anki--marshall-nat)
      (`(string)
       #'anki--marshall-string)
      (_
       (error "Cannot marshal values of type %s" ty))))

  (defun anki--parameter-spec (it)
    (pcase it
      ((pred symbolp)
       (list
        :name it
        :marshal #'identity))

      ((and `(,name : ,r . ,rs) (guard (not (equal r '=))))
       (pcase (-partition-before-item '= (cons r rs))
         (`(,ty)
          (list :name name
                :marshal (anki--param-marshal ty)))
         (`(,ty (= ,dflt))
          (list :name name
                :marshal (anki--param-marshal ty)
                :default dflt
                :default-supplied-p t))
         (`(,_ (=))
          (error "Missing default parameter in spec: %s" it))
         (bad
          (error "Malformed type parameter: %s, in spec: %s" (-flatten bad) it))))
      (_
       (error "Malformed parameter spec: %s" it))))

  (defun anki--param-spec-to-postdata (params)
    (-map (-lambda ((&keys :name :marshal :default-supplied-p :default))
            (let ((key (s-lower-camel-case (symbol-name name)))
                  (deref-name `(funcall #',marshal ,name))
                  (deref-default `(funcall #',marshal ,default)))
              (if default-supplied-p
                  `(cons ,key (if ,name ,deref-name ,deref-default))
                `(cons ,key ,deref-name))))
          params))

  (defun anki--specs-to-defun-paramlist (specs)
    (-let [(required optional)
           (--map (-map (-lambda ((&keys :name)) name) it)
                  (-split-with (-lambda ((&plist :default-supplied-p p)) (not p)) specs))]
      (if optional
          `(,@required &optional ,@optional)
        required))))

(cl-defmacro anki-defanki (action arglist docstring)
  (declare (indent defun))
  (cl-assert (symbolp action) t)
  (let ((specs (-map #'anki--parameter-spec arglist)))
    `(defun ,(intern (format "anki-%s" (s-dashed-words (symbol-name action))))
         ,(anki--specs-to-defun-paramlist specs)
       ,docstring
       (anki--request "POST" ',action
                  (list ,@(anki--param-spec-to-postdata specs))))))



;; Misc actions

(anki-defanki version ()
  "Gets the version of the API exposed by the plugin.")

(anki-defanki sync ()
  "Synchronizes the local anki collections with ankiweb.")

;; Decks

(anki-defanki deck-names ()
  "Gets the complete list of deck names for the current user.")

(anki-defanki deck-names-and-ids ()
  "Gets the complete list of deck names and their respective IDs for the current user.")

(anki-defanki get-decks ((cards : seq string))
  "Return the cards grouped by deck name.

CARDS is a list of card IDs.")

(anki-defanki create-deck ((deck : string))
  "Create a new empty deck with the given name.

Will not overwrite a deck that exists with the same name.")

(anki-defanki change-deck ((cards : seq string)
                       (deck : string))
  "Moves cards to a different deck.

CARDS is a list of card IDs.

DECK is the deck to move to. It will be created if it doesn't exist.")

(anki-defanki delete-decks ((decks : seq string)
                        (cards-too : boolean = nil))
  "Deletes decks with the given names.

DECKS is a list of deck names.

If CARDS-TOO is nil or omitted, the cards will be moved to the
default deck. Otherwise they will be deleted.")

;; Deck config

(anki-defanki get-deck-config ((deck : string))
  "Gets the configuration group object for the given deck.")

(anki-defanki save-deck-config ((config : config))
  "Saves the given CONFIG. Returns `t' on success.")

(anki-defanki set-deck-config-id ((decks : seq string)
                              (config-id : nat))
  "Changes the configuration group for the given decks to the one with the given ID.

Returns `t' on success.")

(anki-defanki clone-deck-config-id ((name : string)
                                (clone-from : nullable nat = nil))
  "Creates a new configuration group with name.

NAME is the name of the config group to create.

CLONE-FROM is the ID of the config group to clone. If it is nil or unspecified, the default config group is used.

Returns the ID of the new configuration group.")

(anki-defanki remove-deck-config-id ((config-id : nat))
  "Remove the configuration group with the given ID.

Returns `t' on success.")

;; Models
;;
;; Models describe how to use notes to generate one or more cards.

(anki-defanki model-names-and-ids ()
  "Gets the model names and their corresponding IDs for the current user.")

(anki-defanki model-field-names-and-ids ((model-name : string))
  "Gets the complete list of field names for the provided model name.")

(anki-defanki model-fields-on-templates ((model-name : string))
  "Returns an object indicating the fields on the question and answer side of each card template for the given model.

The question side is given first in each array.")

(anki-defanki create-new-model ((model-name : string)
                            (in-order-fields : seq string)
                            (card-templates : seq card-template)
                            (css : nullable string = nil))
  "Gets the complete list of field names for the provided model name.")

;; Notes
;;
;; Notes represent the data used to populate cards.

(anki-defanki add-note ((note : note))
  "Creates a note using the associated deck and model.

The note value describes the field values and tags.

Returns the identifier of the created note.")

(anki-defanki update-note-fields ((note : note-update))
  "Modify the fields of an existing note.")

(anki-defanki add-notes ((notes : seq note))
  "Create many notes at once.")

(anki-defanki find-notes ((query : string = "deck:current"))
  "Returns an array of note IDs for a given query.")

(anki-defanki notes-info ((notes : seq nat))
  "Returns info objects for each given ID.

Each object contains the note fields, tags, type and the cards
belonging to the note.")

(anki-defanki delete-notes ((notes : seq nat))
  "Deletes notes with the given ids.

If a note has several cards associated with it, all associated
cards will be deleted.")

(provide 'anki)

;;; anki.el ends here
