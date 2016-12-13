module exam2

import nat
import bool
import list
import dictionary
import pair

-----------------------------------------
-- #1: TUPLES AND PROJECTION FUNCTIONS --
-----------------------------------------

{-
Here is a data type definition for a tuple type called FunTuple,
where a value of this type represents a tuple with a first component
of type Nat and a second component of type Bool.
-}

data Tuple = MkTuple Nat Bool

-- Here's an example of a value of this type

mt1: Tuple
mt1 = MkTuple nat_one True

{-
Write two projection functions. The first, called tuple_nat, takes
a Tuple and returns the value of its Nat component. The second, called
tuple_bool, takes a Tuple and returns its Bool component. You need to
fill in the types of these functions and their implementations.
-}

tuple_nat: Tuple -> Nat
tuple_nat (MkTuple n b) = n

tuple_bool: Tuple -> Bool
tuple_bool (MkTuple n b) = b


-------------------
-- #2: SUBSCRIPTING
-------------------

{-
Refer back to our list module and in particular to the list_sub
function. Remind yourself how it works. Now, given the following
list of natural numbers, complete the expression using the list_sub
function so that the value of aNat is set to the third element in
the list (i.e., to nat_one). Remember that we start indexing at
zero. You can use the REPL to make sure you've got it right.
-}

aList: List Nat
aList = (Cons nat_three (Cons nat_two (Cons nat_one Nil)))

aNat: Nat
aNat = list_sub nat_two aList nat_zero
-- Expect nat_one; be sure to check that you get the right answer


----------------------------
-- #3: POLYMORPHIC FUNCTIONS
----------------------------

-- Consider this function definition

foo: { a: Type } -> (val: a) -> Nat
foo v = nat_one

{-
In a sentence, state precisely what this function does. In
a second sentence or two explain the purpose and the effect
of the curly braces around "a: Type." Then write code to test
the application of foo to the arguments nat_one and False,
respectively. What is the result in each case?
-}

--This function takes one argument of any defined Type and returns the natural number nat_one for all inputs.
--The purpose of the curly braces is to suggest that the variable 'a' can be of any type, making the function polymorphic.

fooTest: Nat
fooTest = foo nat_one

fooTest2: Nat
fooTest2 = foo False


-----------------------------
-- #4: POLYMORPHIC DATA TYPES
-----------------------------

{-
Write a type constructor called Foo with two type arguments,
a and b, and two constructors, Left a and Right b, and then
write code showing how you would assign values of this type
to variables, g and h, using the Left and Right constructors.
Pick different types for a and b to make it more interesting.
-}

data Foo a b = Left a | Right b

g: Foo Nat Bool
g = Left nat_one

h: Foo Nat Bool
h = Right True


---------------------------
-- #5: INDUCTIVE DATA TYPES
---------------------------

{-
A "Tree of values of type, a,"" is either Empty or it is a
Node with a value of type a and two smaller "Trees of values
of type a". The Tree type is thus defined inducively.

Write an Idris data type definition for a polymorphic type,
Tree a, accordingly.

Hint: It will have two constructors; you should call them Empty
and Node; and the Node constructor will take three arguments.
-}

data Tree a =
    Empty |
    Node a (Tree a) (Tree a)


{-
Next, write Idris code that binds an empty tree of natural
numbers to the variable, et, and a non-empty tree of natural
numbers to the variable, ft. Your non-empty tree may have
only one Node, though more would be ok as well.
-}

-- Fill in the holes in the following code with types and
-- implementation code to provide your answers.

et: Tree Nat
et = Empty

ft: Tree Nat
ft = Node nat_one (Empty) (Empty)


--------------------------
-- #6: RECURSIVE FUNCTIONS
--------------------------

{-
Write the implementation of a recursive function called
tree_size that takes a value of type (Tree a) and that
returns the number of Nodes in the tree. Hint the number
of Nodes in an Empty tree is zero, while the number of
Nodes is a non-empty tree is one (for the current Node)
plus the number of nodes in each of the two sub-trees.
-}

tree_size: (Tree a) -> Nat
tree_size Empty = nat_zero
tree_size (Node a b c) = nat_plus (nat_one) (nat_plus (tree_size b) (tree_size c))


------------------
-- #7: Termination
------------------

-- Part A. Why should you NOT evaluate badfun, below, in your REPL?

badfun: Nat -> Nat
badfun n = badfun n

--Running the function badfun would cause an infinite loop because there is no base case for the recursive function.
--A recursive function needs a base case in order to terminate, otherwise it will keep executing infinitely.

{-
Note: If you were curious and did actually try to evaluate, say,
badfun nat_zero, you have to restart your REPL or Atom editor.
Save this file before restarting Atom!
-}


{-
Part B. Is it true that a function that calls itself recursively
on the tail of a list argument will always terminate? Explain (as
text inside this comment).

Calling a function recursively on the tail of a list will only terminate if
the function has a base case for when a List is Nil. Calling a function recursively
on a tail should eventually result in a Nil in which the base case must be necessary
to determine the base value for what the function wants.
-}


-----------------------------
-- #8: HIGHER-ORDER FUNCTIONS
-----------------------------

{-
Write a function called applyfun that takes a function of type
Nat -> Nat and a value of type Nat and that returns the result of
applying the given function to the given value. Then write one
test case showing that it works. Document the expected answer and
use your REPL to confirm that that's the answer you're obtaining.
-}

applyFun: (Nat -> Nat) -> Nat -> Nat
applyFun func n = func n

applyFunTest: Nat
applyFunTest = nat_succ nat_one
--expect nat_two S (S Z)



-----------------------------------------
-- #9: THE MAP, FILTER AND FOLD FUNCTIONS
-----------------------------------------

{-
Part A. Define a value of type (List Nat) representing the list,
[1, 2, 3, 4]. Note: If you don't have a definition of nat_four,
you can always apply nat_succ to a smaller value, such as nat_three,
to construct the value you need.
-}

list_value: List Nat
list_value = Cons nat_one (Cons nat_two (Cons nat_three (Cons nat_four Nil)))

{-
Part B: Define s to be a variable of type Nat, and use the
list_fold_right to assign to s the value that is the sum of
the elements of the list you just defined.
-}

s: Nat
s = list_fold_right (nat_plus) (nat_zero) (list_value)

{-
Part C: Define q to be a variable of type (List Bool), and
use the list_map function to assign to q a list of Bool values,
one for each element of the list you defined, namely True if
the element is even and False otherwise. Hint: use nat_evenb
in your answer. Use the REPL to check the answer (False, True,
False, True).
-}

q: List Bool
q = list_map (nat_evenb) (list_value)


------------------------------
-- #10: EXTRA CREDIT FOR AN A+
------------------------------

{-
Write code to bind to a variable d a value of type
(Dictionary Nat Bool) containing the following three
key-value pairs, followed by two test cases where you
lookup the values for the keys zero and three respectively.
Check that you're getting the expected answers. Here are
the key-value pairs: 0:True, 1:False, 2:True. Note: You can
write the code that produces the required dictionary in one
big expression, or you can assign smaller dictionaries to
other variables to build up to the dictionary you assign to
the variable d. Final hint: dictionary_insert takes a
dictionary as an argument but also returns a dictionary as
a result. That returned dictionary can be an argument to
another dictionary_insert operation.
-}
