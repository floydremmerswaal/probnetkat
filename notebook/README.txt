install nix
install cachix (using nix)

$ cachix use jupyterwith
$ nix-shell --option sandbox false
$ jupyter lab
