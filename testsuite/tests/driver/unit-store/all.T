# Unit store spike: the same compiled artifacts are reached through a
# ghc-pkg database and through a unit store (-unit-store), and the results
# must agree. See docs/unit-artifact-model.md.

def unitstore(name, extra=[]):
    test(name,
         [extra_files(['unitstore.sh', 'src/', 'MainBasic.hs', 'MainMulti.hs',
                       'MainHidden.hs', 'MainReexport.hs', 'MainSafe.hs',
                       'MainBackpack.hs'])] + extra,
         makefile_test, [name])

unitstore('unitstore_basic')
unitstore('unitstore_multi')
unitstore('unitstore_hidden')
unitstore('unitstore_reexport')
unitstore('unitstore_safe')
unitstore('unitstore_dynamic', [req_dynamic_hs])
unitstore('unitstore_envfile')
unitstore('unitstore_backpack')
