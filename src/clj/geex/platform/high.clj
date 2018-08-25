(ns geex.platform.high

  "High-level platform specific code that is not needed by the code generator. Therefore, this code can depend on the core module."

  (:require [geex.core :as geex]
            [geex.core.defs :as defs]
            [geex.core.datatypes :as dt]
            [geex.core.seed :as sd]
            [geex.core.stringutils :as su :refer [wrap-in-parens]]
            [bluebell.utils.core :as utils]
            [bluebell.utils.defmultiple :refer [defmultiple]]))
