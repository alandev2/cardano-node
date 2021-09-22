{ runCommand, cardano-node, jq, mainnet-chain }:

let
  topology = { Producers = []; };
in
runCommand "membench" {
  buildInputs = [ cardano-node jq ];
  topology = builtins.toJSON topology;
  passAsFile = [ "topology" ];
} ''
  pwd
  cp -rs ${mainnet-chain} chain
  chmod +w chain chain/immutable
  rm chain/immutable/01861.*
  cp ${mainnet-chain}/immutable/01861.* chain/immutable -vi
  ls -ltrh chain
  jq '.setupScribes = [
      .setupScribes[0] * { "scFormat":"ScJson" },
      {
        scFormat:"ScJson",
        scKind:"FileSK",
        scName:"log.json",
        scRotation:{
          rpLogLimitBytes: 300000000,
          rpMaxAgeHours:   24,
          rpKeepFilesNum:  20
        }
      }
    ]
    | .defaultScribes = .defaultScribes + [ [ "FileSK", "log.json" ] ]
    ' ${./configuration/cardano/mainnet-config.json} > config.json
  cp -v ${./configuration/cardano}/*-genesis.json .
  cardano-node run --database-path chain/ --config config.json --topology $topologyPath --shutdown-on-slot-synced 2000
  pwd
  ls -ltrh
''
