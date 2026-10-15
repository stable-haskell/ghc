//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_js_heap_views_length,_malloc,_free

function js_heap_views_length_wrapper(array, offset) {
    return h$withCStringOnHeap(array, offset, (ptr) => {
        return _js_heap_views_length(ptr);
    });
}
