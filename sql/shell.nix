let pkgs = import <nixpkgs> {}; in
pkgs.mkShell {
  buildInputs = [
    pkgs.postgresql
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.postgresql.lib}/lib
  '';
}
