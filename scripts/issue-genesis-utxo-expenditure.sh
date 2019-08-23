#!/bin/sh

RUNNER=${RUNNER:-cabal new-exec --}

genesis_file="configuration/Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData.json"
genesis_hash="$(${RUNNER} cardano-cli real-pbft print-genesis-hash --genesis-json ${genesis_file})"
## get the first nonAvvmBalance entry in the genesis file -- but unfortunately,
## we need to correlate key file and the entry:
## $(jq '.nonAvvmBalances | keys_unsorted | .[0]' ${genesis_file} | xargs echo)
from_addr="2cWKMJemoBahGYHvphuM3cmwhgWZmRzPSRX5xdx11A1aJ168wLgRpD7naamfWk4dfQ28c"
from_key="configuration/delegate-keys.000.key"
default_to_key="configuration/delegate-keys.001.key"

## rob the bank
default_lovelace="863000000000000"

case $# in
        1 ) tx="$1"
            proto_magic="$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")"
            addr="$(${RUNNER} cardano-cli real-pbft signing-key-address  \
                                --testnet-magic ${proto_magic}           \
                                --secret ${default_to_key}               \
                              | head -n1 | xargs echo)"
            lovelace=${default_lovelace};;
        3 ) tx="$1";
            addr="$2";
            lovelace="$3";;
        * ) cat >&2 <<EOF
Usage:  $(basename $0) TX-FILE TO-ADDR LOVELACE
EOF
            exit 1;; esac

args=" --genesis-file        ${genesis_file}
       --genesis-hash        ${genesis_hash}
       --tx                  ${tx}
       --wallet-key          ${from_key}
       --rich-addr-from    \"${from_addr}\"
       --txout            (\"${addr}\",${lovelace})
"
set -x
${RUNNER} cardano-cli real-pbft issue-genesis-utxo-expenditure ${args}
