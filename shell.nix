{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix {
    inherit ghc;
  };
in pkgs.prim-char.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [
    pkgs.haskell-language-server
  ];
})
