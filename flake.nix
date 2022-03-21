{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  # description = "A Hello World in Haskell with a dependency and a devShell";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        youtrack-upload = final.haskellPackages.callCabal2nix "youtrack-upload" ./. {};
      });
      packages = forAllSystems (system: {
         youtrack-upload = nixpkgsFor.${system}.youtrack-upload;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.youtrack-upload);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.youtrack-upload];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            # haskell-language-server
            ghcid
            cabal-install
            stack
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
