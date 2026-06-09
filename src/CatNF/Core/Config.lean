import CatNF.Core.Error

open Lean Meta

namespace CatNF

/-- Configuration options with comprehensive validation and performance optimizations -/
structure Config where
  maxSteps : Nat := 500
  timeoutMs : Nat := 1500
  monoidal : Bool := true
  trace : Bool := false
  simpSet : Option String := none
  enableCaching : Bool := true
  enableParallel : Bool := true
  enableEarlyTermination : Bool := true
  maxWorkers : Nat := 4
  cacheSize : Nat := 10000
  maxMemoryBytes : Nat := 100000000
  deriving Repr, Inhabited

/-- Validate configuration parameters with bounds checking -/
def validateConfig (config : Config) : CatNFM Unit := do
  if config.maxSteps == 0 then
    throwCatNF (CatNFError.validationError "maxSteps must be greater than 0")
  if config.maxSteps > 10000 then
    throwCatNF (CatNFError.validationError "maxSteps cannot exceed 10000 for performance reasons")

  if config.timeoutMs == 0 then
    throwCatNF (CatNFError.validationError "timeoutMs must be greater than 0")
  if config.timeoutMs > 30000 then
    throwCatNF (CatNFError.validationError "timeoutMs cannot exceed 30000ms (30 seconds)")

  if let some simpSet := config.simpSet then
    if simpSet.isEmpty then
      throwCatNF (CatNFError.validationError "simpSet cannot be empty string")
    if simpSet.length > 100 then
      throwCatNF (CatNFError.validationError "simpSet name too long (max 100 characters)")

  if config.maxWorkers == 0 then
    throwCatNF (CatNFError.validationError "maxWorkers must be greater than 0")
  if config.maxWorkers > 32 then
    throwCatNF (CatNFError.validationError "maxWorkers cannot exceed 32")
  if config.cacheSize == 0 then
    throwCatNF (CatNFError.validationError "cacheSize must be greater than 0")
  if config.cacheSize > 1000000 then
    throwCatNF (CatNFError.validationError "cacheSize cannot exceed 1000000")
  if config.maxMemoryBytes == 0 then
    throwCatNF (CatNFError.validationError "maxMemoryBytes must be greater than 0")
  if config.maxMemoryBytes > 1000000000 then
    throwCatNF (CatNFError.validationError "maxMemoryBytes cannot exceed 1GB")

/-- Create a validated configuration with error handling -/
def createConfig (maxSteps : Nat := 500) (timeoutMs : Nat := 1500)
    (monoidal : Bool := true) (trace : Bool := false)
    (simpSet : Option String := none) (enableCaching : Bool := true)
    (enableParallel : Bool := true) (enableEarlyTermination : Bool := true)
    (maxWorkers : Nat := 4) (cacheSize : Nat := 10000) (maxMemoryBytes : Nat := 100000000) : CatNFM Config := do
  let config := Config.mk maxSteps timeoutMs monoidal trace simpSet enableCaching enableParallel enableEarlyTermination maxWorkers cacheSize maxMemoryBytes
  validateConfig config
  return config

end CatNF
