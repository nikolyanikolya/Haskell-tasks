--Для типа данных OddC
--(контейнер-последовательность, который по построению может содержать только нечетное число элементов) реализуйте функцию
--concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
--конкатенирующую три таких контейнера в один:

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) w = Bi a b w
concat3OC (Bi a b c) y z = Bi a b (concat3OC c y z) 
concat3OC (Un a) (Bi x y z) w = Bi a x (concat3OC (Un y) z w)