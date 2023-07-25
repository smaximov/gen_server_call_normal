{
  description = "Description for the project";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          name = "genserver_call_normal";

          buildInputs = with pkgs; [ elixir erlang elixir_ls ];

          ERL_AFLAGS = "-kernel shell_history enabled";
          TERM = "xterm-256color";

          shellHook = ''
            VAR="$(git rev-parse --show-toplevel)/var"
            mkdir -p "$VAR"

            export MIX_HOME="$VAR/mix"
            export HEX_HOME="$VAR/hex"

            mix local.hex --force --if-missing
            mix local.rebar --force --if-missing
          '';
        };
      };
    };
}
