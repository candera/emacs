;; '(default ((t (:stipple
;;             nil :background "grey10" :foreground "white" :inverse-video
;;             nil :box nil :strike-through nil :overline nil :underline
;;             nil :slant normal :weight normal :height 140 :width
;;             normal :foundry "outline" :family "Courier New"))))

 ;; '(default ((t (:stipple nil :background "grey10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 50 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))

;; Oddly, setting the background to black under a terminal turns it
;; grey. But the default in a windowed system is black on white. Sigh.
(when window-system
  (set-face-background 'default "black")
  (set-face-foreground 'default "white"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(default ((t ,(cond
                 ((or (eq system-type 'darwin) (eq system-type 'windows-nt))
                  '(:foundry "outline"
                    :family "Courier New"
                    :weight normal
                    :height 160
                    :width normal))
                 ((or (eq system-type 'gnu/linux) (eq system-type 'cygwin))
                  `(:foundry "unknown"
                    :family "Courier 10 Pitch"
                    :weight normal
                    ;; We want to use a different font if we're doing remote
                    ;; display back to Windows, so we look to see if DISPLAY
                    ;; is something like :0.0. But on Cygwin, we stick with 140
                    :height ,(if (eq system-type 'cygwin) 140
                               (if (string= (substring (or (getenv "DISPLAY") ":") 0 1) ":") 120 140))
                    :width normal))))))
 ;; '(mode-line ((((class color) (min-colors 88)) (:background "#8888ff" :foreground "black" :box (:line-width -1 :style released-button)))))
 ;; '(cursor ((t (:background "red"))))
 ;; '(highline-face ((t (:background "grey20"))))
 ;; '(hl-line ((t (:inherit highlight :background "grey20"))))
 ;; '(magit-diff-add ((((class color) (background dark)) (:foreground "lightgreen"))))
 ;; '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 ;; '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 ;; '(magit-item-highlight ((((class color) (background dark)) (:background "grey20"))))
 ;; '(nxml-attribute-colon-face ((t (:inherit nxml-name-face :foreground "blue"))))
 ;; '(nxml-attribute-local-name-face ((t (:inherit nxml-name-face :foreground "blue"))))
 ;; '(nxml-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "blue"))))
 ;; '(nxml-namespace-attribute-colon-face ((t (:inherit nxml-name-face :foreground "red"))))
 ;; '(nxml-namespace-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "red"))))
 ;; '(nxml-namespace-attribute-xmlns-face ((t (:inherit nxml-name-face :foreground "red"))))


 '(bold ((t (:bold t :weight bold))))
 '(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
 '(border ((t (nil))))
 '(buffer-menu-buffer ((t (:bold t :weight bold))))
 '(button ((t (:underline t))))
 '(change-log-acknowledgement ((t (:foreground "chocolate1"))))
 '(change-log-conditionals ((t (:foreground "LightGoldenrod"))))
 '(change-log-date ((t (:foreground "LightSalmon"))))
 '(change-log-email ((t (:foreground "LightGoldenrod"))))
 '(change-log-file ((t (:foreground "LightSkyBlue"))))
 '(change-log-function ((t (:foreground "LightGoldenrod"))))
 '(change-log-list ((t (:foreground "Cyan1"))))
 '(change-log-name ((t (:foreground "Aquamarine"))))
 '(clojure-test-error-face ((t (:background "orange4"))))
 '(clojure-test-failure-face ((t (:background "firebrick"))))
 '(clojure-test-success-face ((t (:background "green" :foreground "black"))))
 '(comint-highlight-input ((t (:bold t :weight bold))))
 '(comint-highlight-prompt ((t (:foreground "cyan1"))))
 '(completions-annotations ((t (:italic t :slant italic))))
 '(completions-first-difference ((t (:bold t :weight bold))))
 '(cursor ((t (:background "red"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-changed ((t (nil))))
 '(diff-context ((t (:foreground "grey70"))))
 '(diff-file-header ((t (:bold t :background "grey60" :weight bold))))
 '(diff-function ((t (:background "grey45"))))
 '(diff-header ((t (:background "grey45"))))
 '(diff-hunk-header ((t (:background "grey45"))))
 '(diff-index ((t (:bold t :weight bold :background "grey60"))))
 '(diff-indicator-added ((t (:foreground "green"))))
 '(diff-indicator-changed ((t (nil))))
 '(diff-indicator-removed ((t (:foreground "red"))))
 '(diff-nonexistent ((t (:bold t :weight bold :background "grey60"))))
 '(diff-refine-change ((t (:background "grey60"))))
 '(diff-removed ((t (:foreground "red"))))
 '(ediff-current-diff-A ((t (:background "pale green" :foreground "firebrick"))))
 '(ediff-current-diff-Ancestor ((t (:background "VioletRed" :foreground "Black"))))
 '(ediff-current-diff-B ((t (:background "Yellow" :foreground "DarkOrchid"))))
 '(ediff-current-diff-C ((t (:background "Pink" :foreground "Navy"))))
 '(ediff-even-diff-A ((t (:background "light grey" :foreground "Black"))))
 '(ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "White"))))
 '(ediff-even-diff-B ((t (:background "Grey" :foreground "White"))))
 '(ediff-even-diff-C ((t (:background "light grey" :foreground "Black"))))
 '(ediff-fine-diff-A ((t (:background "sky blue" :foreground "Navy"))))
 '(ediff-fine-diff-Ancestor ((t (:background "Green" :foreground "Black"))))
 '(ediff-fine-diff-B ((t (:background "cyan" :foreground "Black"))))
 '(ediff-fine-diff-C ((t (:background "Turquoise" :foreground "Black"))))
 '(ediff-odd-diff-A ((t (:background "Grey" :foreground "White"))))
 '(ediff-odd-diff-Ancestor ((t (:background "gray40" :foreground "cyan3"))))
 '(ediff-odd-diff-B ((t (:background "light grey" :foreground "Black"))))
 '(ediff-odd-diff-C ((t (:background "Grey" :foreground "White"))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(file-name-shadow ((t (:foreground "grey70"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(flexwiki-bold-face ((t (:weight normal))))
 '(flexwiki-done-face ((t (:foreground "dim grey"))))
 '(flexwiki-heading-1-face ((t (:height 1.5))))
 '(flexwiki-heading-2-face ((t (:height 1.3))))
 '(flexwiki-heading-3-face ((t (:height 1.1))))
 '(flexwiki-italic-face ((t (:italic t :slant italic))))
 '(flexwiki-property-face ((t (:foreground "green3"))))
 '(flexwiki-question-face ((t (:foreground "red2"))))
 '(flexwiki-strikethrough-face ((t (:strike-through t))))
 '(flexwiki-todo-face ((t (:foreground "dark violet"))))
 '(flexwiki-underline-face ((t (:underline t))))
 '(flexwiki-wikiword-face ((t (:foreground "blue"))))
 '(flyspell-duplicate ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "chocolate1"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(font-lock-negation-char-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink" :weight bold))))
 '(fringe ((t (:background "grey10"))))
 '(header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
 '(help-argument-name ((t (:italic t :slant italic))))
 '(hi-black-b ((t (:bold t :weight bold))))
 '(hi-black-hb ((t (:bold t :family "Sans Serif" :weight bold :height 1.67))))
 '(hi-blue ((t (:background "light blue" :foreground "black"))))
 '(hi-blue-b ((t (:bold t :foreground "blue1" :weight bold))))
 '(hi-green ((t (:background "green1" :foreground "black"))))
 '(hi-green-b ((t (:bold t :foreground "green1" :weight bold))))
 '(hi-pink ((t (:background "pink" :foreground "black"))))
 '(hi-red-b ((t (:bold t :foreground "red1" :weight bold))))
 '(hi-yellow ((t (:background "yellow1" :foreground "black"))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(highlight-symbol-face ((t (:background "gray30"))))
 '(hl-line ((t (:background "grey20"))))
 '(ido-first-match ((t (:bold t :weight bold))))
 '(ido-incomplete-regexp ((t (:bold t :weight bold :foreground "Pink"))))
 '(ido-indicator ((t (:background "red1" :foreground "yellow1" :width condensed))))
 '(ido-only-match ((t (:foreground "ForestGreen"))))
 '(ido-subdir ((t (:foreground "red1"))))
 '(info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "white"))))
 '(info-header-xref ((t (:foreground "cyan1" :underline t))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :weight bold))))
 '(info-menu-star ((t (:foreground "red1"))))
 '(info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
 '(info-title-1 ((t (:bold t :weight bold :family "Sans Serif" :height 1.728))))
 '(info-title-2 ((t (:bold t :family "Sans Serif" :weight bold :height 1.44))))
 '(info-title-3 ((t (:bold t :weight bold :family "Sans Serif" :height 1.2))))
 '(info-title-4 ((t (:bold t :family "Sans Serif" :weight bold))))
 '(info-xref ((t (:underline t :foreground "cyan1"))))
 '(info-xref-visited ((t (:foreground "violet" :underline t))))
 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(iswitchb-current-match ((t (:foreground "LightSkyBlue"))))
 '(iswitchb-invalid-regexp ((t (:bold t :weight bold :foreground "Pink"))))
 '(iswitchb-single-match ((t (:foreground "chocolate1"))))
 '(iswitchb-virtual-matches ((t (:foreground "LightSteelBlue"))))
 '(italic ((t (:italic t :slant italic))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))
 '(linum ((t (:foreground "grey70"))))
 '(magit-branch ((t (:bold t :weight bold))))
 '(magit-diff-add ((t (:foreground "lightgreen"))))
 '(magit-diff-del ((t (:foreground "OrangeRed"))))
 '(magit-diff-file-header ((t (nil))))
 '(magit-diff-hunk-header ((t (:italic t :slant italic))))
 '(magit-diff-none ((t (nil))))
 '(magit-header ((t (nil))))
 '(magit-item-highlight ((t (:background "grey20"))))
 '(magit-item-mark ((t (:foreground "orange"))))
 '(magit-log-graph ((t (:foreground "grey80"))))
 '(magit-log-head-label-bisect-bad ((t (:background "IndianRed1" :foreground "IndianRed4" :box 1))))
 '(magit-log-head-label-bisect-good ((t (:background "light green" :foreground "dark olive green" :box 1))))
 '(magit-log-head-label-default ((t (:background "Grey50" :box 1))))
 '(magit-log-head-label-local ((t (:background "Grey13" :foreground "LightSkyBlue1" :box 1))))
 '(magit-log-head-label-patches ((t (:background "IndianRed1" :foreground "IndianRed4" :box 1))))
 '(magit-log-head-label-remote ((t (:background "Grey11" :foreground "DarkSeaGreen2" :box 1))))
 '(magit-log-head-label-tags ((t (:background "LemonChiffon1" :foreground "goldenrod4" :box 1))))
 '(magit-log-message ((t (nil))))
 '(magit-log-sha1 ((t (:foreground "tomato"))))
 '(magit-log-tag-label ((t (:background "DarkGoldenRod"))))
 '(magit-menu-selected-option ((t (:foreground "orange"))))
 '(magit-section-title ((t (:bold t :weight bold))))
 '(match ((t (:background "RoyalBlue3"))))
 '(menu ((t (nil))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line ((t (:background "#8888ff" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:bold t :weight bold))))
 '(mode-line-emphasis ((t (:bold t :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
 '(mouse ((t (nil))))
 '(next-error ((t (:background "blue3"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(region ((t (:background "blue3"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "SkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(show-paren-match ((t (:background "steelblue3"))))
 '(show-paren-mismatch ((t (:background "purple" :foreground "white"))))
 '(sldb-catch-tag-face ((t (nil))))
 '(sldb-condition-face ((t (nil))))
 '(sldb-detailed-frame-line-face ((t (nil))))
 '(sldb-frame-label-face ((t (nil))))
 '(sldb-frame-line-face ((t (nil))))
 '(sldb-local-name-face ((t (nil))))
 '(sldb-local-value-face ((t (nil))))
 '(sldb-non-restartable-frame-line-face ((t (nil))))
 '(sldb-restart-face ((t (nil))))
 '(sldb-restart-number-face ((t (:bold t :weight bold))))
 '(sldb-restart-type-face ((t (:foreground "Cyan1"))))
 '(sldb-restartable-frame-line-face ((t (:foreground "lime green"))))
 '(sldb-section-face ((t (nil))))
 '(sldb-topline-face ((t (nil))))
 '(slime-error-face ((t (:underline "red"))))
 '(slime-highlight-face ((t (:background "darkolivegreen" :underline nil))))
 '(slime-inspector-action-face ((t (:bold t :weight bold :foreground "Pink"))))
 '(slime-inspector-label-face ((t (:foreground "Aquamarine"))))
 '(slime-inspector-topline-face ((t (nil))))
 '(slime-inspector-type-face ((t (:foreground "PaleGreen"))))
 '(slime-inspector-value-face ((t (:foreground "LightSteelBlue"))))
 '(slime-note-face ((t (:underline "light goldenrod"))))
 '(slime-repl-input-face ((t (:bold t :weight bold))))
 '(slime-repl-inputed-output-face ((t (:foreground "Red"))))
 '(slime-repl-output-face ((t (:foreground "LightSalmon"))))
 '(slime-repl-output-mouseover-face ((t (:foreground "Red" :box (:line-width 1 :color "black" :style released-button)))))
 '(slime-repl-prompt-face ((t (:foreground "Cyan1"))))
 '(slime-repl-result-face ((t (nil))))
 '(slime-style-warning-face ((t (:underline "gold"))))
 '(slime-warning-face ((t (:underline "coral"))))
 '(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:family "Sans Serif" :background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(vertical-border ((t (nil))))
 '(which-func ((t (:foreground "Blue1"))))
 '(widget-button ((t (:bold t :weight bold))))
 '(widget-button-pressed ((t (:foreground "red1"))))
 '(widget-documentation ((t (:foreground "lime green"))))
 '(widget-field ((t (:background "dim gray"))))
 '(widget-inactive ((t (:foreground "grey70"))))
 '(widget-single-line-field ((t (:background "dim gray")))))

;; (unless window-system
;;  (custom-set-faces
;;   '(isearch-face ((t (:foreground "black" :background "yellow"))))))


