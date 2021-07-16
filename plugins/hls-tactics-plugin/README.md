<p align="center">
<img src="https://haskellwingman.dev/wingman.png" height="256" alt="Wingman for Haskell" title="Wingman for Haskell">
</p>

<p>&nbsp;</p>

# Wingman for Haskell

[![Hackage](https://img.shields.io/hackage/v/hls-tactics-plugin.svg?logo=haskell&label=hls-tactics-plugin)](https://hackage.haskell.org/package/hls-tactics-plugin)

"Focus on the important stuff; delegate the rest"


## Dedication

> There's a lot of automation that can happen that isn't a replacement of
> humans, but of mind-numbing behavior.
>
> --Stewart Butterfield


## Overview

Wingman writes the boring, auxiliary code, so you don't have to. Generate
functions from type signatures, and intelligently complete holes.


## Getting Started

Wingman for Haskell is enabled by default in all [official release of Haskell
Language Server.][hls] Just hover over a typed hole, run the "Attempt to
fill hole" code action, *et voila!*

[hls]: https://github.com/haskell/haskell-language-server/releases


## Usage

When enabled, Wingman for Haskell will remove HLS support for hole-fit code
actions. These code actions are provided by GHC and make typechecking extremely
slow in the presence of typed holes. Because Wingman relies so heavily on typed
holes, these features are in great tension.

The solution: we just remove the hole-fit actions. If you'd prefer to use these
actions, you can get them back by compiling HLS without the Wingman plugin.


## Editor Configuration

### Enabling Jump to Hole

Set the `haskell.plugin.tactics.config.hole_severity` config option to `4`, or
`hint` if your editor uses a GUI for its configuration. This has the potential
to negatively impact performance --- please holler if you notice any appreciable
slowdown by enabling this option.


### coc.nvim

The following vimscript maps Wingman code-actions to your leader key:

```viml
" use [h and ]h to navigate between holes
nnoremap <silent> [h :<C-U>call CocActionAsync('diagnosticPrevious', 'hint')<CR>
nnoremap <silent> ]h :<C-U>call <SID>JumpToNextHole()<CR>

" <leader>d to perform a pattern match, <leader>n to fill a hole
nnoremap <silent> <leader>d  :<C-u>set operatorfunc=<SID>WingmanDestruct<CR>g@l
nnoremap <silent> <leader>n  :<C-u>set operatorfunc=<SID>WingmanFillHole<CR>g@l
nnoremap <silent> <leader>r  :<C-u>set operatorfunc=<SID>WingmanRefine<CR>g@l
nnoremap <silent> <leader>c  :<C-u>set operatorfunc=<SID>WingmanUseCtor<CR>g@l
nnoremap <silent> <leader>a  :<C-u>set operatorfunc=<SID>WingmanDestructAll<CR>g@l


function! s:JumpToNextHole()
  call CocActionAsync('diagnosticNext', 'hint')
endfunction

function! s:GotoNextHole()
  " wait for the hole diagnostics to reload
  sleep 500m
  " and then jump to the next hole
  normal 0
  call <SID>JumpToNextHole()
endfunction

function! s:WingmanRefine(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.refine'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanDestruct(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.caseSplit'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanDestructAll(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.splitFuncArgs'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanFillHole(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.fillHole'])
  call <SID>GotoNextHole()
endfunction

function! s:WingmanUseCtor(type)
  call CocAction('codeAction', a:type, ['refactor.wingman.useConstructor'])
  call <SID>GotoNextHole()
endfunction
```

### Emacs

When using Emacs, wingman actions should be available out-of-the-box and
show up e.g. when using `M-x helm-lsp-code-actions RET` provided by 
[helm-lsp](https://github.com/emacs-lsp/helm-lsp) or as popups via
[lsp-ui-sideline](https://emacs-lsp.github.io/lsp-ui/#lsp-ui-sideline).

Additionally, if you want to bind wingman actions directly to specific
keybindings or use them from Emacs Lisp, you can do so like this:

``` emacs-lisp
;; will define elisp functions for the given lsp code actions, prefixing the
;; given function names with "lsp"
(lsp-make-interactive-code-action wingman-fill-hole "refactor.wingman.fillHole")
(lsp-make-interactive-code-action wingman-case-split "refactor.wingman.caseSplit")
(lsp-make-interactive-code-action wingman-refine "refactor.wingman.refine")
(lsp-make-interactive-code-action wingman-split-func-args "refactor.wingman.spltFuncArgs")
(lsp-make-interactive-code-action wingman-use-constructor "refactor.wingman.useConstructor")

;; example key bindings
(define-key haskell-mode-map (kbd "C-c d") #'lsp-wingman-case-split)
(define-key haskell-mode-map (kbd "C-c n") #'lsp-wingman-fill-hole)
(define-key haskell-mode-map (kbd "C-c r") #'lsp-wingman-refine)
(define-key haskell-mode-map (kbd "C-c c") #'lsp-wingman-use-constructor)
(define-key haskell-mode-map (kbd "C-c a") #'lsp-wingman-split-func-args)
```

### Other Editors

Please open a PR if you have a working configuration!


## Features

* [Type-directed code synthesis][auto], including pattern matching and recursion
* [Automatic case-splitting][case] --- just run the "Case split on <x>" code action
* [Smart next actions][next], for those times it can't read your mind

[auto]: https://haskellwingman.dev/foldr.gif
[case]: https://haskellwingman.dev/case-split.gif
[next]: https://haskellwingman.dev/intros.gif


## Support

Please consider [pledging on Patreon][patreon] to support the project and get
access to cutting-edge features.

[patreon]: https://www.patreon.com/wingman_for_haskell

