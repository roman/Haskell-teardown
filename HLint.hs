-- HLint configuration file

module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- Protolude removes the String alias to avoid its usage
ignore "Use String"
