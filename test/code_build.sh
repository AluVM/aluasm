#!/usr/bin/env bash

#cargo build --all-targets || exit 1
ASM_FLAGS=""
LINK_FLAGS=""
ASM=./target/debug/aluasm
LINK=./target/debug/alink

$ASM $ASM_FLAGS examples/all.aluasm
$ASM $ASM_FLAGS examples/miner.aluasm
$ASM $ASM_FLAGS examples/pow.aluasm
$ASM $ASM_FLAGS examples/pedersen.aluasm
# $ASM $ASM_FLAGS examples/rgb20.aluasm

$LINK $LINK_FLAGS --org=lnpbp.org --bin all
$LINK $LINK_FLAGS --org=pandoracore.org --lib miner
$LINK $LINK_FLAGS --org=pandoracore.org --bin pow
$LINK $LINK_FLAGS --org=lnpbp.org --lib pedersen
# $LINK $LINK_FLAGS --org=lnpbp.org --bin rgb20
