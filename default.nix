let
  haskellPackagesOverlay =
    import ./nix/override-haskell-packages.nix {
      packages = {
        "evans-ghcid" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
      };
    };


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };


  evans-ghcid = pkgs.haskellPackages.evans-ghcid;


  executable = pkgs.haskell.lib.justStaticExecutables evans-ghcid;


  shell =
    evans-ghcid.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });

in
  { inherit
      evans-ghcid
      executable
      shell
    ;
  }
