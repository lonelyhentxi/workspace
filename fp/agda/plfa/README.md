# Learn Agda with Programming Language Foundations in Agda

@Stage: Working

## Pre-request

1. Ensure you have haskell and cabal installed
2. Ensure you have cabal, haskell bin and mingw files in your `PATH`
3. Installed `emacs`
	- For Windows users, we recommend to install `emacs` with `msys2` via `pacman -S mingw-w64-x86_64-emacs` or `pacman -S mingw-w64-i686-emacs`

## Install

```shell
cabal update
cabel new-install Agda
```

The installer will not successfully create symbol-link in Windows, please add the installation path to your `PATH`.

Or you can copy the compiled `agda` and `agda-mode` binary files to cabal directory (for windows, please copy to `%AppData%\cabal\bin`).

```shell
agda-mode setup
```

This command will append the following content to your `.emacs` file.

```conf
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode.exe locate")))
```

It is also possible (but not necessary) to compile the Emacs mode’s files:

```shell
agda-mode compile
```

This can, in some cases, give a noticeable speedup.

## Config

We recommend to use `更纱黑体` to support unicode, especially Chinese, Japanese and Korean.

After installation, please use the guid options to config your font.

## The standard library

While using windows, clone the `stdlib` from [agda-stdlib](https://github.com/agda/agda-stdlib) 
and try to create a `%AppData\agda`(windows) or `~\.agda`(\*nix) folder with two files named `libraries` and `defaults`:

The content in `libraries` is:

```conf
<full-path-to-the-cloned-agda-stdlib-repo>/standard-library.agda-lib
```

The content in `defaults` is:

```conf
standard-library
```

## PLFA

Clone the [agda-plfa](https://github.com/plfa/plfa.github.io) repo.

Add the following content to your `libraries` file:

```conf
<full-path-to-the-cloned-agda-plfa-repo>/plfa.agda-lib
```

Add the following content to your `defaults` file:

```conf
plfa
```

