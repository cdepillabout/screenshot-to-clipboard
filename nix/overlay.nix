final: prev: {
  haskell = prev.haskell // {
    packageOverrides = hfinal: hprev:
      prev.haskell.packageOverrides hfinal hprev // {
        screenshot-to-clipboard =
          let
            filesToIgnore = [
              "default.nix"
              "flake.nix"
              "flake.lock"
              ".git"
              ".github"
              "nix"
              "result"
              "shell.nix"
              ".stack-work"
              "stack.yaml"
              "stack.yaml.lock"
            ];

            src = builtins.path {
              # Naming this path makes sure that people will get the same hash
              # even if they checkout this repo into a directory called
              # something else.
              name = "screenshot-to-clipboard-src";
              path = ../.;
              filter = path: type:
                with final.lib;
                ! elem (baseNameOf path) filesToIgnore &&
                ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
            };

          in
          final.lib.pipe
            (hfinal.callCabal2nix "screenshot-to-clipboard" src { })
            [ final.haskell.lib.compose.disableLibraryProfiling
              (final.haskell.lib.compose.overrideCabal (oldAttrs: {
                # TODO: Should this be using wrapGAppsHook (like Termonad) instead of just makeWrapper??
                buildTools = oldAttrs.buildTools or [] ++ [ final.buildPackages.makeWrapper ];
                postInstall = oldAttrs.postInstall or "" + ''
                  wrapProgram "$out/bin/screenshot-to-clipboard" \
                    --prefix 'PATH' ':' "${final.imagemagick}/bin"
                '';
              }))
            ];
      };
  };

  screenshot-to-clipboard =
    final.haskell.lib.justStaticExecutables final.haskellPackages.screenshot-to-clipboard;

  # This is a shell to use that provides cabal-install and ghcid.  This is
  # convenient for hacking on this package while getting Haskell dependencies
  # from Nixpkgs.
  hacking-on-screenshot-to-clipboard-shell = final.haskellPackages.shellFor {
    withHoogle = true;
    packages = hpkgs: [ hpkgs.screenshot-to-clipboard ];
    nativeBuildInputs = [
      # Tools for Haskell development.
      final.cabal-install
      final.ghcid
      final.haskellPackages.haskell-language-server

      # This is for the `image` command used by screenshot-to-clipboard.
      final.imagemagick
    ];
  };
}
