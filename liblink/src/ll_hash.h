#pragma once

#include "ll_visibility.h"
#include "ll_compat.h"
#define allocHashTable ll_hash_alloc_table
#define insertHashTable ll_hash_insert_table
#define lookupHashTable ll_hash_lookup_table
#define removeHashTable ll_hash_remove_table
#define keyCountHashTable ll_hash_key_count
#define keysHashTable ll_hash_keys
#define mapHashTable ll_hash_map
#define mapHashTableKeys ll_hash_map_keys
#define iterHashTable ll_hash_iter
#define insertStrHashTable ll_hash_insert_str
#define lookupStrHashTable ll_hash_lookup_str
#define removeStrHashTable ll_hash_remove_str
#define hashBuffer ll_hash_buffer
#define hashWord ll_hash_word
#define hashStr ll_hash_str
#define insertHashTable_ ll_hash_insert_table_with
#define lookupHashTable_ ll_hash_lookup_table_with
#define removeHashTable_ ll_hash_remove_table_with
#define freeHashTable ll_hash_free_table
#define allocStrHashTable ll_hash_alloc_str_table
#define freeStrHashTable ll_hash_free_str_table
#define allocHashSet ll_hash_alloc_set
#define freeHashSet ll_hash_free_set
#define insertHashSet ll_hash_insert_set

/*-----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

#include "ll_begin_private.h"

typedef struct hashtable HashTable; /* abstract */
typedef struct strhashtable StrHashTable;
struct ll_linker;

/* Hash table access where the keys are StgWords.
 * Values are passed into the hash table and stored as `const void *` values,
 * but when the value is looked up or removed, the value is returned without the
 * `const` so that calling function can mutate what the pointer points to if it
 * needs to.
 */
HashTable * allocHashTable  ( struct ll_linker *L );
void        insertHashTable ( HashTable *table, ll_word key, const void *data );
void *      lookupHashTable ( const HashTable *table, ll_word key );
void *      removeHashTable ( HashTable *table, ll_word key, const void *data );

int keyCountHashTable (HashTable *table);

// Puts up to szKeys keys of the hash table into the given array. Returns the
// actual amount of keys that have been retrieved.
//
// If the table is modified concurrently, the function behavior is undefined.
//
int keysHashTable(HashTable *table, ll_word keys[], int szKeys);

typedef void (*MapHashFn)(void *data, ll_word key, const void *value);
typedef void (*MapHashFnKeys)(void *data, ll_word *key, const void *value);
// Return true -> continue; false -> stop
typedef bool (*IterHashFn)(void *data, ll_word key, const void *value);

void mapHashTable(HashTable *table, void *data, MapHashFn fn);
void mapHashTableKeys(HashTable *table, void *data, MapHashFnKeys fn);
LL_FRIEND_API void iterHashTable(HashTable *table, void *data, IterHashFn);

/* Hash table access where the keys are C strings (the strings are
 * assumed to be allocated by the caller, and mustn't be deallocated
 * until the corresponding hash table entry has been removed).
 */
LL_INLINE_HEADER StrHashTable * allocStrHashTable ( struct ll_linker *L )
{
    return (StrHashTable*) allocHashTable(L);
}

void        insertStrHashTable ( StrHashTable *table, const char * key,
                                 const void *data );
void *      lookupStrHashTable ( const StrHashTable *table, const char * key);
void *      removeStrHashTable ( StrHashTable *table, const char * key,
                                 const void *data );

/*
 * Hash tables for arbitrary key types.
 * Generally, these functions allow for the specification of the
 * HashFunction and CompareFunction. It's recommended that those
 * are inlinable so there's a chance the compiler can discard
 * some parameter-passing, as well as function calls, though note
 * it's not guaranteed. Either way, the functions are parameters
 * as the types should be statically known and thus
 * storing them is unnecessary.
 */
typedef int HashFunction(const HashTable *table, ll_word key);
typedef int CompareFunction(ll_word key1, ll_word key2);

// Helper for implementing hash functions
int hashBuffer(const HashTable *table, const void *buf, size_t len);

int hashWord(const HashTable *table, ll_word key);
int hashStr(const HashTable *table, ll_word w);
void        insertHashTable_ ( HashTable *table, ll_word key,
                               const void *data, HashFunction f );
void *      lookupHashTable_ ( const HashTable *table, ll_word key,
                               HashFunction f, CompareFunction cmp );
void *      removeHashTable_ ( HashTable *table, ll_word key,
                               const void *data, HashFunction f,
                               CompareFunction cmp );


/* Freeing hash tables
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );

/* Free table values through the table owner's allocator. Values must have been
 * allocated by that same owner. Keys remain borrowed.
 */
void ll_hash_free_owned_table ( HashTable *table );

LL_INLINE_HEADER void freeStrHashTable ( StrHashTable *table, void (*freeDataFun)(void *) )
{
    freeHashTable((HashTable*)table, freeDataFun);
}

/*
 * Hash set API
 *
 * A hash set is basically a hash table where values are NULL.
 */

typedef struct hashtable HashSet;

LL_INLINE_HEADER HashSet *allocHashSet ( struct ll_linker *L )
{
    return (HashSet*)allocHashTable(L);
}

LL_INLINE_HEADER void freeHashSet ( HashSet *set )
{
    freeHashTable((HashTable*)set, NULL);
}

LL_INLINE_HEADER void insertHashSet ( HashSet *set, ll_word key )
{
    insertHashTable((HashTable*)set, key, NULL);
}

#include "ll_end_private.h"

#if !defined(LL_INTERNAL_SOURCE)
#undef allocHashTable
#undef insertHashTable
#undef lookupHashTable
#undef removeHashTable
#undef keyCountHashTable
#undef keysHashTable
#undef mapHashTable
#undef mapHashTableKeys
#undef iterHashTable
#undef insertStrHashTable
#undef lookupStrHashTable
#undef removeStrHashTable
#undef hashBuffer
#undef hashWord
#undef hashStr
#undef insertHashTable_
#undef lookupHashTable_
#undef removeHashTable_
#undef freeHashTable
#undef allocStrHashTable
#undef freeStrHashTable
#undef allocHashSet
#undef freeHashSet
#undef insertHashSet
#endif
