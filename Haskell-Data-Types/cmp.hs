{-

Реализуйте функцию cmp, сравнивающую элементы \
типа LogLevel так, чтобы имел место порядок Error > Warning > Info
-}
data LogLevel = Error | Warning | Info
    deriving Show
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Warning _ = GT
cmp _ Warning = LT