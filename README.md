# Spaceleader
## Description
leader-key implementation ~~copied and pasted from~~ inspired by [spacemacs](https://github.com/syl20bnr/spacemacs).

## Installation
**TODO**: add melpa support.

### Straight
You can install this package using the [straight][straight] package manager and the
following recipe.

[straight]: https://github.com/raxod502/straight.el

```elisp
(straight-use-package
 '(spaceleader :type git :host github :repo "mohkale/spaceleader"))
```

## Commands
| spaceleader                      | spacemacs                                |
|:---------------------------------|:-----------------------------------------|
| leader/set-keys                  | spacemacs/set-leader-keys                |
| leader/set-keys-for-mode         | spacemacs/set-leader-keys-for-minor-mode |
| leader/set-keys-for-mode!        | spacemacs/set-leader-keys-for-major-mode |
| leader/declare-prefix            | spacemacs/declare-prefix                 |
| leader/declare-prefix*           |                                          |
| leader/declare-prefix-for-mode   |                                          |
| leader/declare-prefix-for-mode*  |                                          |
| leader/declare-prefix-for-mode!  | spacemacs/declare-prefix-for-mode        |
| leader/declare-prefix-for-mode!* |                                          |
| leader/with-prefix               |                                          |
| leader/with-major-mode-prefix    |                                          |
| leader/without-prefix            |                                          |

**NOTE**: `leader/declare-prefix-for-mode` &amp; `leader/declare-prefix-for-mode*`
doesn't work as of yet, see [212](https://github.com/justbur/emacs-which-key/issues/212)
for why.

The commentary of *spaceleader.el* elaborates on the naming scheme:
* functions ending with `*` can take a variable amount of arguments.
* functions containing a `!` target major-modes not minor-modes.
* the `leader/set-keys` family of functions all take a variable number of arguments.

## Customisation
**WARN**: You should set any customisations before loading *spaceleader.el* or you may
end up with some bindings in the default leader & the remaining in another leader.

run `M-: customize-group leader` to see all the customisation options for *spaceleader*.
by default, *spaceleader* tries to emulate spacemacs [evil](https://github.com/emacs-evil/evil)
configuration as much as possible.

**leader-key**: the key under which all leader bindings will be assigned.

**leader-nnorm-key**: the leader-key used for non-normal emacs evil states.

**leader-major-mode-prefix**: the key under **leader-key** where major-mode leaders are bound.

**leader-major-mode-key**: the key in normal state maps used as a shortcut for **leader-key** then **leader-major-mode-prefix**.

## Deviation From Spacemacs
*spaceleader* introduces a few functions to ease the definition of leader keys.

### declare-prefix With Multiple Arguments
Firstly, variants in the `declare-prefix` family of functions have been defined to
take multiple arguments. These variants end with a `*`. Meaning if you've got some
definitions like:

```elisp
(spacemacs/declare-prefix "a" "apps" "applications")
(spacemacs/declare-prefix "b" "buffers")
(spacemacs/declare-prefix "c" "compile/comments")
```

you can convert it into a single function call and leave the interpretation of the
arguments upto *spaceleader*.

```elisp
(leader/declare-prefix*
  "a" '("apps" . "applications")
  "b" "buffers"
  "c" "compile/comments")
```

### declare-prefix For Major Modes
*spaceleader* also supports declaring prefixes for major-mode bindings. spacemacs
also supports this, however it does so indirectly; you have to manually concatenate
any of the keys you want to set with the hardcoded major-mode-prefix used by spacemacs
(I.E. `m`).

```elisp
(spacemacs/declare-prefix-for-mode 'dired-mode "mx" "command-x")
(spacemacs/declare-prefix-for-mode 'dired-mode "my" "command-y")
```

this quite quickly becomes bothersome, especially if somewhere down the line you decide you want to
change the major-mode-prefix. *spaceleader* introduced `leader/declare-prefix-for-mode!` to address
this issue, it automatically prepends your major-mode-prefix to any of the prefix keys you provide;
saving you the ordeal and ensuring the correct prefix is used.

```elisp
(leader/declare-prefix-for-mode!* 'dired-mode
  "x" "command-x"
  "y" "command-y")
```

**NOTE**: minor mode prefix decleration not yet supported by `which-key`.

### Specifying a Prefix Across Multiple set-key Calls
Have you ever had to define your leader-keys like this?

```elisp
(let ((my-special-prefix "ox"))
  (spacemacs/set-leader-keys
    (concat my-special-prefix "a") 'command-a
    (concat my-special-prefix "b") 'command-b
    (concat my-special-prefix "c") 'command-c)

  (let ((my-extra-special-prefix (concat my-special-prefix "b")))
    (spacemacs/set-leader-keys
      (concat my-extra-special-prefix "x") 'command-x
      (concat my-extra-special-prefix "y") 'command-y
      (concat my-extra-special-prefix "z") 'command-z)))
```

Well you'll never have to do so again :grinning:. *spaceleader* has introduced 3 macros to let you
specify a prefix and assume it's correctly bound in all the prefix or leader-key declarations you
make within it's body; you can even nest prefix declarations and this'll have the obvious affect.
The above can be simplified to:

```elisp
(leader/with-prefix "ox"
  (leader/set-keys
    "a" 'command-a
    "b" 'command-b
    "c" 'command-c)

  (leader/with-prefix "b"
    ;; total-prefix is "oxb"
    "x" 'command-x
    "y" 'command-y
    "z" 'command-z))
```

there's also:
- `leader/without-prefix` - which removes any and all prefixes within the macro body.
- `leader/with-major-mode-prefix` - which is an alias for
  `(leader/with-prefix leader-major-mode-prefix &rest BODY)`.

## Warnings
### Spacemacs *Emacs* Configuration
I've never actually used spacemacs's emacs configuration (& more importantly don't know how
it differs from the evil configuration). Therefore this package doesn't exactly adhere to it.
If you'd like to implement such functionality, feel free to do so and then share a pull request.

### Tonnes of Maps
By ~~consquence~~ virtue of design, this package ends up creating a lot of keymaps.
[bind-map][emacs-bind-map] creates 3 for each mode based binding. What's more any leader
bindings made within the major-mode prefix for a given minor mode requires an extra 3 maps
as well (only when the major-mode leader key isn't being simulated).

The format of such maps is:
- leader-**&lt;mode&gt;**-map-prefix
- leader-**&lt;mode&gt;**-root-map
- leader-**&lt;mode&gt;**-map

and for minor modes binding within the major mode prefix:
- leader-**&lt;mode&gt;**--major-prefix-alias-map
- leader-**&lt;mode&gt;**--major-prefix-alias-root-map
- leader-**&lt;mode&gt;**--major-prefix-alias-map-prefix

[emacs-bind-map]: https://github.com/justbur/emacs-bind-map

## Similair Packages
- [evil-leader](https://github.com/cofi/evil-leader)
