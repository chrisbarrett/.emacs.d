;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Pre-declare packages this config depends on.

;; Any package with :ensure will be automatically installed via Nix using the
;; Emacs overlay.

(use-package all-the-icons :ensure t)
(use-package auctex :ensure t)
(use-package browse-at-remote :ensure t)
(use-package bufler :ensure t)
(use-package cape :ensure t)
(use-package chatgpt-shell :ensure t)
(use-package cider :ensure t)
(use-package citar :ensure t)
(use-package citar-org-roam :ensure t)
(use-package clojure-mode :ensure t)
(use-package consult :ensure t)
(use-package corfu :ensure t)
(use-package csv-mode :ensure t)
(use-package dall-e-shell :ensure t)
(use-package deadgrep :ensure t)
(use-package default-text-scale :ensure t)
(use-package delight :ensure t :demand t)
(use-package diredfl :ensure t)
(use-package dirvish :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package dogears :ensure t)
(use-package doom-themes :ensure t)
(use-package dumb-jump :ensure t)
(use-package edit-indirect :ensure t)
(use-package editorconfig :ensure t)
(use-package eglot :ensure t)
(use-package eglot-x :ensure t)
(use-package elisp-slime-nav :ensure t)
(use-package embark :ensure t)
(use-package embark-consult :ensure t)
(use-package emojify :ensure t)
(use-package envrc :ensure t)
(use-package evil :ensure t)
(use-package evil-args :ensure t)
(use-package evil-collection :ensure t)
(use-package evil-iedit-state :ensure t)
(use-package evil-matchit :ensure t)
(use-package evil-nerd-commenter :ensure t)
(use-package evil-numbers :ensure t)
(use-package evil-org :ensure t)
(use-package evil-surround :ensure t)
(use-package flx :ensure t)
(use-package forge :ensure t)
(use-package format-all :ensure t)
(use-package general :ensure t :demand t)
(use-package git-auto-commit-mode :ensure t)
(use-package git-gutter :ensure t)
(use-package git-gutter-fringe :ensure t)
(use-package gnuplot :ensure t)
(use-package graphql-mode :ensure t)
(use-package groovy-mode :ensure t)
(use-package hcl-mode :ensure t)
(use-package helpful :ensure t)
(use-package hide-comnt :ensure t)
(use-package hide-mode-line :ensure t)
(use-package highlight-indent-guides :ensure t)
(use-package highlight-thing :ensure t)
(use-package historian :ensure t)
(use-package hl-todo :ensure t)
(use-package htmlize :ensure t)
(use-package info-plus :ensure t)
(use-package iscroll :ensure t)
(use-package json-mode :ensure t)
(use-package kind-icon :ensure t)
(use-package latex-preview-pane :ensure t)
(use-package ligature :ensure t)
(use-package link-hint :ensure t)
(use-package magit :ensure t)
(use-package magit-popup :ensure t)
(use-package magit-todos :ensure t)
(use-package marginalia :ensure t)
(use-package markdown-mode :ensure t)
(use-package messages-are-flowing :ensure t)
(use-package mini-frame :ensure t)
(use-package minions :ensure t)
(use-package nix-mode :ensure t)
(use-package no-littering :ensure t)
(use-package ob-chatgpt-shell :ensure t)
(use-package ob-http :ensure t)
(use-package orderless :ensure t)
(use-package org :ensure t)
(use-package org-appear :ensure t)
(use-package org-cliplink :ensure t)
(use-package org-contrib :ensure t)
(use-package org-download :ensure t)
(use-package org-drill :ensure t)
(use-package org-fragtog :ensure t)
(use-package org-roam :ensure t)
(use-package org-roam-ui :ensure t)
(use-package org-super-agenda :ensure t)
(use-package org-superstar :ensure t)
(use-package orgtbl-aggregate :ensure t)
(use-package origami :ensure t)
(use-package ox-gfm :ensure t)
(use-package page-break-lines :ensure t)
(use-package paren-face :ensure t)
(use-package pcmpl-args :ensure t)
(use-package pcre2el :ensure t)
(use-package pdf-tools :ensure t)
(use-package plantuml-mode :ensure t)
(use-package poporg :ensure t)
(use-package prettier :ensure t)
(use-package proof-general :ensure t)
(use-package rainbow-mode :ensure t)
(use-package request :ensure t)
(use-package rotate :ensure t)
(use-package rust-mode :ensure t)
(use-package rustic :ensure t)
(use-package simple-httpd :ensure t)
(use-package smartparens :ensure t)
(use-package smex :ensure t)
(use-package string-inflection :ensure t)
(use-package swift-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package undo-tree :ensure t)
(use-package unfill :ensure t)
(use-package vertico :ensure t)
(use-package volatile-highlights :ensure t)
(use-package vscode-icon :ensure t)
(use-package websocket :ensure t)
(use-package wgrep :ensure t)
(use-package which-key :ensure t)
(use-package ws-butler :ensure t)
(use-package yasnippet :ensure t)

;;; Configure load-path

(dolist (load-dir (list
                   "/run/current-system/sw/share/emacs/site-lisp"
                   "~/.nix-profile/share/emacs/site-lisp"
                   (expand-file-name "lisp/" user-emacs-directory)
                   (expand-file-name "lisp/nursery/lisp/" user-emacs-directory)
                   ))
  (when (file-directory-p load-dir)
    (add-to-list 'load-path load-dir)))

;;; Define a helper for loading features in ./config.

(require 'cl-lib)

(defmacro use-config (feature &rest use-package-args)
  "Load FEATURE from ./config with some default `use-package' args.

USE-PACKAGE-ARGS are optional additional arguments forwarded to
`use-package'."
  (declare (indent 1))
  (let ((file (expand-file-name (format "./config/%s.el" feature)
                                user-emacs-directory)))
    (cl-assert (file-exists-p file) t))
  `(use-package ,feature
     :load-path "./config/" :demand t ,@use-package-args))

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(rx "("
                                (group "use-config") symbol-end (* (any space))
                                (group (+ (or (syntax word) (syntax symbol))))
                                (? ")"))
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))

(use-package server
  :if (not noninteractive)
  :demand t
  :config
  (server-start))

;;; Load features

(use-config cb-parameters)
(use-config cb-startup-profiling-and-debugging)
(use-config cb-gc-tuning)

(use-config cb-autoloads
  :config
  (with-no-warnings
    (cb-autoloads-build-and-load))
  (use-package autoloads :demand t))

(use-config cb-theme)
(use-config cb-completion)
(use-config cb-window-management)
(use-config cb-input)
(use-config cb-general-editing)
(use-config cb-help-systems)
(use-config cb-search)
(use-config cb-snippets)
(use-config cb-media)
(use-config cb-evil)
(use-config cb-leader)
(use-config cb-ide)
(use-config cb-eshell)
(use-config cb-smartparens)
(use-config cb-dired)
(use-config cb-git)
(use-config cb-langs)
(use-config cb-lang-clojure)
(use-config cb-lang-elisp)
(use-config cb-lang-latex)
(use-config cb-lang-markdown)
(use-config cb-lang-rust)
(use-config cb-lang-typescript)
(use-config cb-org)

(use-config cb-org-roam
  :autoload cb-org-roam-initial-buffers
  :custom
  (initial-buffer-choice #'cb-org-roam-initial-buffers))

(load (expand-file-name "lisp/init.el" org-directory) t t)
(load custom-file t t)

;;; init.el ends here
