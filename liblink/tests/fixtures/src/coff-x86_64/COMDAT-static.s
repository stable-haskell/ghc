# Separate objects share a COMDAT key to exercise duplicate selection.
.section .rdata$ll_fixture_comdat_static,"dr",discard,ll_fixture_comdat_static
.def ll_fixture_comdat_static; .scl 3; .type 0; .endef
ll_fixture_comdat_static:
    .long 37
