_Thread_local int macho_tlv_defined = 42;
_Alignas(64) _Thread_local int macho_tlv_zero;
int macho_tlv_read_defined(void) { return macho_tlv_defined + macho_tlv_zero; }
void macho_tlv_write_defined(int value) { macho_tlv_defined = value; }
__attribute__((optnone)) int *macho_tlv_address_defined(void) { return &macho_tlv_defined; }

int macho_tlv_read_zero(void) { return macho_tlv_zero; }
void macho_tlv_write_zero(int value) { macho_tlv_zero = value; }
__attribute__((optnone)) int *macho_tlv_address_zero(void) { return &macho_tlv_zero; }
