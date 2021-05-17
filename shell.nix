let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.18-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "0f28e40f9942c1449bfa0d400dee49a517102047";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "https://github.com/id3as/nixpkgs-purerl-support.git";
      rev = "5a4208e312f724742ecdfaf3134007d37624c413";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import purerlSupport)
      ];
    };


  erlangChannel = nixpkgs.nixerl.erlang-23-2-1.overrideScope' (self: super: {
    erlang = super.erlang.override {
      wxSupport = false;
    };
  });

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [

    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls

    purerl-support.purescript-0-14-1
    purerl-support.spago-0-20-3

    # Purerl backend for purescript
    purerl.purerl-0-0-9

  ];
}
