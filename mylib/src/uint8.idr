module uint8

import bool
import bit
import byte8
import ifthenelse

export
data UInt8 = MkUInt8 (Byte8)

export
zeroUInt8: UInt8
zeroUInt8 = MkUInt8 (byte8_new B0 B0 B0 B0 B0 B0 B0 B0)

||| Takes two UInt8 and returns the resulting UInt8 by taking out the nested
||| Byte8's within each UInt8
export
add_UInt8: UInt8 -> UInt8 -> UInt8
add_UInt8 (MkUInt8 a) (MkUInt8 b) = MkUInt8 (byte8_plus a b)

||| Takes a UInt8 and returns UInt8 + 1
export
inc_UInt8: UInt8 -> UInt8
inc_UInt8 a = add_UInt8 (a) (MkUInt8 (byte8_new B0 B0 B0 B0 B0 B0 B0 B1))

||| Returns True if and only if the given UInt8 represents zero
export
isZero_UInt8: UInt8 -> Bool
isZero_UInt8 (MkUInt8 a) =
    ifthenelse
        (byte8_eq (a) (byte8_new B0 B0 B0 B0 B0 B0 B0 B0))
        (True)
        (False)

||| Returns True if and only if two UInt8 are equal
export
isEqual_UInt8: UInt8 -> UInt8 -> Bool
isEqual_UInt8 (MkUInt8 a) (MkUInt8 b) = byte8_eq (a) (b)


{-
||| EXTRA CREDIT: this function decreases the given UInt8 by 1
export
dec_UInt8: UInt8 -> UInt8
-}
