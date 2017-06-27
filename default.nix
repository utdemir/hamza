{ pkgs ? import ../github/nixos/nixpkgs {}
, compiler ? "ghc802"
}:

let
nixfile = pkgs.runCommand "cabal2nix"
  { buildInputs = [ pkgs.cabal2nix ]; } ''
  ln -s ${./hamza.cabal} .
  cabal2nix . > "$out"
  cat "$out"
  '';
fun = import "${nixfile}";
haskellPackages = pkgs.haskell.packages.${compiler}.override {
#  overrides = se: su: {
#    haskakafka = pkgs.haskell.lib.overrideCabal su.haskakafka ({
#      preCompileBuildDriver = ''
#        PKG_CONFIG_PATH+="${pkgs.rdkafka}/lib/pkgconfig"
#      '';
#    });
#  };
};
in rec {
  hs  = fun;
  pkg = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage fun {}) (su: {
    src = ./.;
  });
  env = pkg.env.overrideAttrs (old: {
    buildInputs = with haskellPackages; [
      hlint stylish-haskell cabal-install
    ];
  });
}
