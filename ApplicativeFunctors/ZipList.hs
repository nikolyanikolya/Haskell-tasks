{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative (ZipList(ZipList), getZipList)
infixl 4 >$<, >*<
(>$<) = (<$>)

(>*<) a b = getZipList (ZipList a <*> ZipList b)
