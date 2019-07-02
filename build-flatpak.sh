#!/usr/bin/env sh

flatpak-builder --force-clean build-dir/ packaging/Flatpak/com.szibele.stackpak.json
flatpak-builder --repo=repo --force-clean build-dir packaging/Flatpak/com.szibele.stackpak.json
flatpak build-bundle repo/ stackpak.x86_64.flatpak com.szibele.stackpak --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo
