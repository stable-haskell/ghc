extern _Thread_local int macho_tlv_value;

int macho_tlv_read(void) { return macho_tlv_value; }
void macho_tlv_write(int value) { macho_tlv_value = value; }
__attribute__((optnone)) int *macho_tlv_address(void) { return &macho_tlv_value; }
