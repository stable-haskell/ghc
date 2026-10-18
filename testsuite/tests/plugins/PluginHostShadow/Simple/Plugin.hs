module Simple.Plugin where

-- A target homonym must not enter the module graph merely because the host
-- plugin has this name. Following this edge would create an import cycle.
import PluginHostTH
