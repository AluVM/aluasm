#!/usr/bin/env bash

cargo build --all-targets || exit 1
ASM_FLAGS="-vv"
LINK_FLAGS="-vv"
ASM=./target/debug/aluasm
LINK=./target/debug/alink

$ASM $ASM_FLAGS examples/all.aluasm
$ASM $ASM_FLAGS examples/miner.aluasm
$ASM -vv examples/pow.aluasm

$LINK $LINK_FLAGS --org=lnpbp.org --bin all
$LINK $LINK_FLAGS --org=pandoracore.org --lib miner
$LINK $LINK_FLAGS --org=pandoracore.org --bin pow
