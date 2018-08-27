# stackpak

Generate a <a href="https://flatpak.org/">Flatpak</a> manifest from a stack project given a base flatpak manifest file.

It is intended to be used by open source projects which can then be released on <a href="https://flathub.org/home">Flathub</a>.
There is however, nothing stopping you from using stackpak with your own closed source Haskell projects.

An example project using stackpak is available: <a href="https://gitlab.com/rszibele/e-juice-calc#readme">E-Juice-Calc</a>

## Building

To build stackpak, run `stack build` in the root directory.

## Installing

To install stackpak, run `stack install` in the root directory.

## Usage

To generate a manifest you must supply the base manifest file for your project and the path to the stack project directory.
The base manifest file is simply a flatpak manifest for building and installing _only_ your Haskell program without its dependencies.
The dependencies will automatically be added by stackpak to the generated manifest.

````
$ stackpak --output=my-new-manifest.json /path/to/base/manifest.json /path/to/stack/project/dir
````

For an example base manifest file check out the base manifest of: <a href="https://gitlab.com/rszibele/e-juice-calc/blob/master/com.szibele.EJuiceCalc.base.json">E-Juice-Calc</a>
For a more advanced options regarding the cli of stakpak run `stackpak --help`.
