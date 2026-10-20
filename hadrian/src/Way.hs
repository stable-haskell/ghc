module Way (
    WayUnit (..), Way, wayUnit, addWayUnit, removeWayUnit, wayFromUnits, allWays,

    vanilla, dynamic, threaded, debug,
    threadedDebug, threadedDynamic, threadedDebugDynamic, debugDynamic,

    wayPrefix, waySuffix, hisuf, osuf, hcsuf, obootsuf, hibootsuf, ssuf
    ) where

import Way.Type

-- | Build default _vanilla_ way.
vanilla :: Way
vanilla = wayFromUnits []

-- | Build with dynamic linking.
dynamic :: Way
dynamic = wayFromUnits [Dynamic]

-- RTS only ways below. See compiler/GHC/Driver/Session.hs.
-- | Build multithreaded RTS.
threaded :: Way
threaded = wayFromUnits [Threaded]

-- | Build RTS with debug information.
debug :: Way
debug = wayFromUnits [Debug]

-- | Various combinations of RTS only ways.
threadedDebug, threadedDynamic, threadedDebugDynamic, debugDynamic :: Way
threadedDebug            = wayFromUnits [Threaded, Debug]
threadedDynamic          = wayFromUnits [Threaded, Dynamic]
threadedDebugDynamic     = wayFromUnits [Threaded, Debug, Dynamic]
debugDynamic             = wayFromUnits [Debug, Dynamic]
-- | All ways supported by the build system.
allWays :: [Way]
allWays =
    [ vanilla, dynamic, threaded, debug
    , threadedDebug, threadedDynamic, threadedDebugDynamic, debugDynamic ]

wayPrefix :: Way -> String
wayPrefix way | way == vanilla = ""
              | otherwise      = show way ++ "_"

waySuffix :: Way -> String
waySuffix way | way == vanilla = ""
              | otherwise      = "_" ++ show way

osuf, ssuf, hisuf, hcsuf, obootsuf, hibootsuf :: Way -> String
osuf      = (++ "o"      ) . wayPrefix
ssuf      = (++ "s"      ) . wayPrefix
hisuf     = (++ "hi"     ) . wayPrefix
hcsuf     = (++ "hc"     ) . wayPrefix
obootsuf  = (++ "o-boot" ) . wayPrefix
hibootsuf = (++ "hi-boot") . wayPrefix
