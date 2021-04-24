# Learn V8 with rusty_v8

A trial to learn v8 with [rusty_v8](https://github.com/denoland/rusty_v8), and the original tutorial is with c++ [here](https://github.com/danbev/learning-v8).

## Build

For win:

1. Install the latest version of rustup toolchain, for new features.
2. Enable your proxy via env.
3. Add `GYP_MSVS_OVERRIDE_PATH` to env, and the value is visual studio path, such as `C:\code\microsoft visual studio\2019\community`.
4. Add `GYP_MSVS_VERSION` to env, and the value is visual studio version, such as `2019`.
5. Install `Debugging Tools for Windows`, via install `Windows SDK` or `Windows Driver Kit`, and you should enable this feature.
6. Ensure you are using `python2.7` in current `shell/powershell/cmd`.
7. Build, using `cargo build -vv`.

For *nix:

1. Install the latest version of rustup toolchain, for new features.
2. Enable your proxy via env.
3. Install glib2.0+ devel packages. For example, if using debian series, try `sudo apt install libglib2.0-dev`.
4. `cargo build -vv`
5. Ensure you are using `python2.7` in current `shell/powershell/cmd`.
6. Build, using `cargo build -vv`.

**If build failed when use sccache**

Please remove environment variable `SCCACHE`. Instead, you can set `RUSTC_WRAPPER` as `"sccache"`.

If still failed, you should clean all caches or disable sccache.

## Status

In progress.

## License

You just DO W T F YOU WANT TO.

![WTFPL](http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl-badge-4.png")
