"""Replay saved reference observations through the oracle before using a golden."""

import hashlib
from macho_metadata import require, bounded
from macho_arm64 import signed, symbol_value, verify


def replay_reference(metadata, reference, original):
    require(reference['object_sha256'] == hashlib.sha256(original).hexdigest(), 'reference object hash differs')
    layout = reference['reference_layout']
    saved = {(r['section'], r['offset'], r['type']): r for r in reference['fields']}
    require(len(saved) == len(reference['fields']), 'duplicate reference fields')
    capture = dict(format='Mach-O', host='arm64-apple-darwin', input_size=len(original),
                   object_bytes=original.hex(), section_count=len(metadata['sections']),
                   sections=[], host_symbols=layout['host_symbols'], image_base=layout['image_base'],
                   got=layout['got'], stubs=layout['stubs'])
    require(len(layout['sections']) == len(metadata['sections']), 'reference section count differs')
    for section, sample in zip(metadata['sections'], layout['sections']):
        require(sample['index'] == section['index'] and sample['size'] == section['size'],
                'reference section layout differs')
        capture['sections'].append(dict(index=section['index'], size=section['size'], name=section['name'],
            segment=section['segment'], vmaddr=hex(section['address']), mapped_start=sample['address'],
            pre_resolve_bytes=section['bytes'].hex(), pre_init_relocated_bytes=sample['relocated_bytes'],
            relocation_count=section['relocation_count'], relocations=[]))
    for section, live in zip(metadata['sections'], capture['sections']):
        pending = None
        for i, relocation in enumerate(section['relocations']):
            offset, kind, number = relocation['offset'], relocation['type'], relocation['type_number']
            sample = saved[(section['index'], offset, kind)]
            width = 0 if number == 10 else 1 << relocation['length']
            before = bounded(section['bytes'], offset, width).hex()
            addend = dict(implicit_field_width=width, implicit_field_bytes=before if width else None,
                          explicit_addend=pending)
            observed = dict(section_index=section['index'], relocation_index=i, type=kind,
                            type_number=number, P=sample['P'], A=addend, before=sample['before'],
                            after=sample['after'], S=None, symbol=None, got_slot=None,
                            raw=dict(r_address=offset, r_symbolnum=relocation['symbol_index'],
                                     r_pcrel=relocation['pcrel'], r_length=relocation['length'],
                                     r_extern=int(relocation['external']), r_type=number,
                                     record_bytes=relocation['record_bytes']))
            if number == 10:
                pending = signed(relocation['symbol_index'], 24)
                addend.update(explicit_addend=pending, paired_relocation_index=i + 1)
            else:
                if pending is not None:
                    addend['from_record_index'] = i - 1
                pending = None
                symbol = metadata['symbols'][relocation['symbol_index']]
                target, origin = symbol_value(symbol, metadata, capture)
                source = {'host-symbol': 'host-symbol-map', 'object-image-base': 'object-image-base'}.get(
                    origin['kind'], 'not-host-supplied')
                observed['S'] = dict(observed=sample['S'], input=hex(target) if source != 'not-host-supplied' else None,
                                     input_source=source)
                observed['symbol'] = sample['symbol']
                # Slots are layout. Restore the original slot associated with this observed symbol.
                slot_map = reference.get('got_slots', {})
                observed['got_slot'] = slot_map.get(symbol['name'])
            live['relocations'].append(observed)
    checked = verify(metadata, capture, original)
    for key in ('schema', 'format', 'object_sha256', 'reference_layout', 'fields', 'semantic', 'got_slots'):
        require(checked[key] == reference[key], 'reference differs from recomputed oracle: ' + key)
    return checked
