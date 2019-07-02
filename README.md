# stackpak

Generate a <a href="https://flatpak.org/">Flatpak</a> manifest from a Haskell stack project given a base flatpak manifest file.

It is intended to be used by open source projects which can then be released on <a href="https://flathub.org/home">Flathub</a>.
There is however, nothing stopping you from using stackpak with your own closed source Haskell projects.

An example project using stackpak is available: <a href="https://gitlab.com/rszibele/e-juice-calc#readme">E-Juice-Calc</a>

## Installing Stackpak from a Flatpak bundle

If you haven't already, you will need to install and set up Flatpak on your machine.
You can consult the following <a href="https://flatpak.org/setup/">Flatpak setup guide</a> on how to set up Flatpak on your specific Linux distribution.

Once Flatpak is set up, run the following commands to install stackpak:

```
$ wget https://gitlab.com/rszibele/stackpak/uploads/b35a7626ab21e9930713fcfe4a899dcd/stackpak-1.0.4.x86_64.flatpak
$ flatpak install stackpak-1.0.4.x86_64.flatpak
```

Finally, add an alias for stackpack to your .bashrc for convenience:

```
$ echo "alias stackpak='flatpak run com.szibele.stackpak'" >> ~/.bashrc
```

**NB:** Stackpak calls `git` and `stack`, so make sure you have them installed and in your PATH.

## Installing from Source

If you only want to use stackpak, then it is generally not advisable to install stackpak from source, as the compilation will take a long time and require gigabytes of download.

To build and install stackpak, cd into the root directory of stackpak and execute the following:

```
$ stack build
$ stack install
```

## Usage

To generate a manifest you must supply the base manifest file for your project and the path to the stack project directory.
The base manifest file is simply a flatpak manifest for building and installing _only_ your Haskell program without its Haskell dependencies.
The Haskell dependencies will automatically be added by stackpak to the generated manifest.

```
$ stackpak --output=my-new-manifest.json /path/to/base/manifest.json /path/to/stack/project/dir
```

For an example base manifest file check out the base manifest of: <a href="https://gitlab.com/rszibele/e-juice-calc/blob/master/packaging/flatpak/com.szibele.e-juice-calc.base.json">E-Juice-Calc</a>
For a more advanced options regarding the cli of stakpak run `stackpak --help`.
