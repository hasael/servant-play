module TestBase where
import Test.QuickCheck
import Test.QuickCheck.Monadic


class (Monad m) => CanPropertyTest m where
    toProperty :: m Property -> Property

monadicProp :: (CanPropertyTest m) => PropertyM m () -> Property
monadicProp = monadic toProperty 
