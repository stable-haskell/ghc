# Separate objects share a COMDAT key to exercise duplicate selection.
.section .rdata$ll_fixture_comdat,"dr",discard,ll_fixture_comdat
.globl ll_fixture_comdat
ll_fixture_comdat:
    .long 31
