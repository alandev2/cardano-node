usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    standard RUN-NAME     Standard batch of analyses: block-propagation, and
                            machine-timeline

    block-propagation RUN-NAME
                          Block propagation analysis for the entire cluster.

    machine-timeline RUN-NAME [MACH-NAME=node-1]
                          Produce a general performance timeline for MACH-NAME

    chaininfo RUN-NAME    Print basic parameters of a run, as seen by locli

    Options of 'analyse' command:

       --chain-filters F  Read chain filters to apply from the F JSON file
       --reanalyse        Skip the preparatory steps and launch 'locli' directly
       --dump-logobjects  Dump the intermediate data: lifted log objects
EOF
}

analyse() {
local time= dump_logobjects= self_args=() locli_args=() prefilter='true' prefilter_jq='false'
while test $# -gt 0
do case "$1" in
       --reanalyse | --re ) prefilter='false';       self_args+=($1);;
       --prefilter-jq )     prefilter_jq='true';     self_args+=($1);;
       --dump-logobjects )  dump_logobjects='true';  self_args+=($1);;
       --chain-filters )    locli_args+=($1 $2);     self_args+=($1 $2); shift;;
       * ) break;; esac; shift; done

local op=${1:-$(usage_analyse)}; shift

case "$op" in
    chaininfo | ci )
        local name=${1:-current}; shift
        local dir=$(run get "$name")

        locli_args+=(
            --genesis         "$dir"/genesis-shelley.json
            --run-metafile    "$dir"/meta.json
        )

        time locli 'analyse' 'chaininfo' "${locli_args[@]}"
        ;;
    standard | std )
        for r in $*
        do analyse ${self_args[*]}             block-propagation $r
           analyse ${self_args[*]} --reanalyse machine-timeline  $r
        done
        ;;
    block-propagation | bp )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; shift
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        echo "{ \"run\": \"$(jq .meta.tag "$dir"/meta.json --raw-output)\" }"

        local adir=$dir/analysis
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile="$adir"/substring-keys
        locli analyse substring-keys > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null) $(ls -d "$dir"/analysis/node-*/ 2>/dev/null))
        # "$dir"/node-*/ "$dir"/analysis/node-*/

        echo "{ \"prefilter\": $prefilter, \"prefilter_jq\": $prefilter_jq }"
        if test "$prefilter" = 'true' -o -z "$(ls "$adir"/logs-node-*.flt.json 2>/dev/null)"
        then
            local jq_args=(
                --sort-keys
                --compact-output
                $(wb backend lostream-fixup-jqargs "$dir")
                ' delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])
                '"$(wb backend lostream-fixup-jqexpr)"
            )
            for d in "${logdirs[@]}"
            do throttle_shell_job_spawns
               local logfiles="$(ls "$d"/stdout* 2>/dev/null | tac) $(ls "$d"/node-*.json 2>/dev/null)"
               if test -z "$logfiles"
               then msg "no logs in $d, skipping.."; fi
               local output="$adir"/logs-$(basename "$d").flt.json
               grep -hFf "$keyfile" $logfiles |
                   if test "$prefilter_jq" = 'true'
                   then jq "${jq_args[@]}" --arg dirHostname "$(basename "$d")"
                   else cat
                   fi > "$output" &
            done
            wait
        fi

        echo "{ \"dataSetSizeMB\": $(echo $(($(cat "$adir"/*.flt.json | wc -c) / 1000 / 1000))) }"
        locli_args+=(
            --genesis         "$dir"/genesis-shelley.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --timeline-pretty "$adir"/block-propagation.txt
            --analysis-json   "$adir"/block-propagation.json
        )
        if test -n "$dump_logobjects"; then
            locli_args+=(--logobjects-json "$adir"/logs-cluster.logobjects.json); fi

        time locli 'analyse' 'block-propagation' \
             "${locli_args[@]}" "$adir"/*.flt.json

        ## More than one run passed?
        if test $# -gt 0
        then analyse ${self_args[*]} block-propagation "$@"; fi;;

    grep-filtered-logs | grep | g )
        local usage="USAGE: wb analyse $op BLOCK [MACHSPEC=*] [RUN-NAME=current]"
        local expr=$1
        local mach=${2:-*}
        local name=${3:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$expr" "$adir"/logs-$mach.flt.json;;

    list-blocks | blocks | bs )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"
        local name=${1:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        fgrep -h "TraceForgedBlock" "$adir"/*.flt.json |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort | uniq;;

    block-propagation-block | bpb )
        local usage="USAGE: wb analyse $op BLOCK [RUN-NAME=current]"
        local block=$1
        local name=${2:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$block" "$adir"/*.flt.json |
            grep 'AddBlock\|TraceForgedBlock\|AddedToCurrentChain' |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort --stable | uniq;;

    machine-timeline | machine | mt )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        echo "{ \"run\": \"$(jq .meta.tag "$dir"/meta.json --raw-output)\" }"

        local adir=$dir/analysis
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        if test "$mach" = 'all'
        then local machs=($(run list-hosts $name))
        else local machs=($mach); fi

        for mach in ${machs[*]}
        do throttle_shell_job_spawns
           (
           ## 1. enumerate logs, filter by keyfile & consolidate
           local logs=($(ls "$dir"/$mach/stdout* 2>/dev/null | tac) $(ls "$dir"/$mach/node-*.json 2>/dev/null) $(ls "$dir"/analysis/$mach/node-*.json 2>/dev/null)) consolidated="$adir"/logs-$mach.json

           test -n "${logs[*]}" ||
               fail "no logs for $mach in run $name"

           echo "{ \"prefilter\": $prefilter }"
           if test "$prefilter" = 'true' -o -z "$(ls "$adir"/logs-$mach.json 2>/dev/null)"
           then grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"; fi

           locli_args+=(
               --genesis         "$dir"/genesis-shelley.json
               --run-metafile    "$dir"/meta.json
               ## ->
               --timeline-pretty "$adir"/logs-$mach.timeline.txt
               --stats-csv       "$adir"/logs-$mach.stats.csv
               --analysis-json   "$adir"/logs-$mach.analysis.json
               # --slotstats-json  "$adir"/logs-$mach.slotstats.json
               # --timeline-csv            "$adir"/logs-$mach.timeline.csv
               # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
               # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
               # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
           )
           if test -n "$dump_logobjects"; then
               locli_args+=(--logobjects-json "$adir"/logs-$mach.logobjects.json); fi

           time locli 'analyse' 'machine-timeline' \
                "${locli_args[@]}" "$consolidated"
           ) &
        done
        wait;;

    * ) usage_analyse;; esac
}

num_jobs="\j"
num_threads=$({ grep processor /proc/cpuinfo 2>/dev/null || echo -e '\n\n\n';
              } | wc -l)

throttle_shell_job_spawns() {
    sleep 0.5s
    while ((${num_jobs@P} >= num_threads - 4))
    do wait -n; sleep 0.$(((RANDOM % 5) + 1))s; done
}
