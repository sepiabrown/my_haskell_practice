{ pkgs }: with pkgs; let

  ghcid-bin = haskellPackages.ghcid.bin;

  mk-ghcid-command = { name, target}:
  runCommand name { buildInputs = [ makeWrapper ]; } ''
    makeWrapper "${ghcid-bin}/bin/ghcid" \
                $out/bin/${name} \
                --add-flags \
                "--command='cabal repl ${target}' \
                --test 'Main.main'"
  '';

  ghcid-exe = mk-ghcid-command { name = "ghcid-exe"; target = "exe:fmmdosa-golden"; };

in (haskellPackages.shellFor {
  packages = p: [ p.learn-you-haskell ];
  buildInputs =
    (with haskellPackages;
    [ haskell-language-server
      ghcid
      #threadscope
      profiteur
      ghc-prof-flamegraph
      hp2pretty
    ]) ++
    [ cabal-install
      cabal2nix
      hpack
      hlint
      ghcid-exe
      libreoffice
      numdiff
    ];
})
