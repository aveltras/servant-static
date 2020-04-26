let
  
  ghcVersion = "ghc865";
  nixpkgsRev = "2d149fcaf3b794947eeb3899859a371d10d38f9f";
  # https://github.com/cachix/ghcide-nix/blob/master/nix/sources.json
  
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs-channels" nixpkgsRev) { inherit config; };
  
  ghcide = import (githubTarball "cachix" "ghcide-nix" "master") {};
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  # sessionSrc = pkgs.nix-gitignore.gitignoreSourcePure [./../sessionula/.gitignore] ./../sessionula;

  # squealSrc = githubTarball "morphismtech" "squeal" "f8a89ede7d4e3eec24f02fd949bcda16dc8b2b6f";
  
  extra-deps = with pkgs.haskell.lib; (super: {
    # sessionula = dontCheck (super.callCabal2nix "sessionula" "${sessionSrc}/sessionula" {});
    # sessionula-extra = dontCheck (super.callCabal2nix "sessionula-extra" "${sessionSrc}/sessionula-extra" {});
    # sessionula-file = dontCheck (super.callCabal2nix "sessionula-file" "${sessionSrc}/backend/sessionula-file" {});
    # sessionula-hasql = null;
    # sessionula-hedis = null;
    # sessionula-servant = dontCheck (super.callCabal2nix "sessionula-servant" "${sessionSrc}/frontend/sessionula-servant" {});
    # sessionula-servant-server = dontCheck (super.callCabal2nix "sessionula-servant-server" "${sessionSrc}/frontend/sessionula-servant-server" {});
    # sessionula-wai = dontCheck (super.callCabal2nix "sessionula-wai" "${sessionSrc}/frontend/sessionula-wai" {});

    # squeal-postgresql = dontCheck (super.callCabal2nix "squeal-postgresql" "${squealSrc}/squeal-postgresql" {});
    # with-utf8 = super.callHackageDirect { pkg = "with-utf8"; ver = "1.0.1.0"; sha256 = "129bsyawcmfig1m3cch91d4nn6wlji3g5lm26jkf08yp54l76lrq"; } {};
    # free-categories = super.callHackageDirect { pkg = "free-categories"; ver = "0.2.0.0"; sha256 = "1grlvy8r7nbb7g8sx5a5x6az03bzli510zjpp5dpliidvajncci9"; } {};
    
    # stylish-haskell
    stylish-haskell = super.callHackageDirect { pkg = "stylish-haskell"; ver = "0.11.0.0"; sha256 = "1a6jijj1lxmi20m9ddiwlnlf3x6qy3dw4si1pvfk9rpjv9azcydk"; } {};
    HsYAML = super.callHackageDirect { pkg = "HsYAML"; ver = "0.2.1.0"; sha256 = "0r2034sw633npz7d2i4brljb5q1aham7kjz6r6vfxx8qqb23dwnc"; } {};
  });
  
  packages = {
    servant-static = ./servant-static;
    # sessionula = ./sessionula;
    # sessionula-servant = ./frontend/sessionula-servant;
    # sessionula-servant-server = ./frontend/sessionula-servant-server;
    # sessionula-wai = ./frontend/sessionula-wai;
    # sessionula-file = ./backend/sessionula-file;
    # sessionula-hasql = ./backend/sessionula-hasql;
    # sessionula-hedis = ./backend/sessionula-hedis;
  };
  
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages."${ghcVersion}".override {
        overrides = self: super: (extra-deps super) // builtins.mapAttrs (name: path: super.callCabal2nix name (gitignore path) {}) packages;
      };
    };
  };

  shell = with pkgs; haskellPackages.shellFor {
    packages = p: builtins.attrValues (builtins.mapAttrs (name: path: builtins.getAttr name haskellPackages) packages); 
    buildInputs = [
      haskellPackages.cabal-install
      ghcide."ghcide-${ghcVersion}"
      stylish-haskell
      yarn
      
      (writeShellScriptBin "testLib" '' 
        ${haskellPackages.cabal-install}/bin/cabal test --enable-library-profiling --test-show-details=direct $1 ''${@:2}
      '')

      (writeShellScriptBin "watchLib" ''
        ${ghcid}/bin/ghcid -c "cabal v2-repl $1" -W ''${@:2}
      '')
      
      (writeShellScriptBin "watchExe" '' 
        ${ghcid}/bin/ghcid -c "cabal v2-repl exe:$1 " -W -T Main.main ''${@:2}
      '')

    ];
  };
  
in {
  inherit pkgs;
  inherit shell;
}
