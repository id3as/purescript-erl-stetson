let
  # nixos-25.05: purescript 0.15.15, erlang 26, rebar3, erlang-ls
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      ref = "nixos-25.05";
      rev = "ac62194c3917d5f474c1a844b6fd6da2db95077d";
    };

  # Modern spago (spago.yaml/spago.lock) - nixpkgs still ships the old dhall one
  purescriptOverlay =
    builtins.fetchGit {
      name = "purescript-overlay";
      url = "https://github.com/thomashoneyman/purescript-overlay.git";
      ref = "main";
      rev = "1cf88ab9d83596db0e0c0d304a16809c410e2917";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "69ea3146f3c4f715c5dbc6e0f8ba7d0ee57bb3bd";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import "${purescriptOverlay}/overlay.nix")
        (import purerlReleases)
      ];
    };

  erlangChannel = nixpkgs.beam.packages.erlang_26;

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls

    # Purescript compiler and build tool
    purescript
    spago

    # Purerl backend for purescript
    purerl.purerl-0-0-22

  ];
}
