extern __thread int fixture_external_tls __attribute__((tls_model("global-dynamic")));

int *fixture_tls_address(void)
{
    return &fixture_external_tls;
}
