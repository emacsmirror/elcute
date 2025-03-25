# slurpbarf-elcute

Slurpbarf and Elcute are Emacs minor modes together creating a Paredit-like experience in Lisp, nXML and SML modes.

Slurpbarf binds `C-(` and `C-)` to slurping commands, and `C-{` and `C-}` to barfing commands.

Elcute remaps `kill-line` to a killing command respecting expression structure.

Both Slurpbarf and Elcute are sensitive to Electric Indent mode.  I recommend use of both Electric Indent and Electric Pair modes alongside Slurpbarf and Elcute.

Slurpbarf works reasonably well everywhere, so a global mode is provided.  I personally have customized `global-slurpbarf-mode` to `t` and have added the following lines to my Emacs init file:

```
(add-hook 'lisp-data-mode-hook #'elcute-mode)
(add-hook 'inferior-lisp-mode-hook #'elcute-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'elcute-mode)
(add-hook 'nxml-mode-hook #'elcute-mode)
(add-hook 'sml-mode-hook #'elcute-mode)
(add-hook 'inferior-sml-mode-hook #'elcute-mode)
```

Enjoy!
