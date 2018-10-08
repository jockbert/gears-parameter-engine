Gears parameter engine
======================

Proof of concept project demonstrating consistent
handling of parameter relation models.


An example
----------
An example is the translation between mile
per imperial gallon (_mpg_) and liters per 100
kilometers (_l/100km_). By only defining a single
conversion function
```
 l/100km = 282.48 / mpg
```
you can convert from both liter per 100km to
miles per gallon and the other way around.

Further, if you specify that the valid interval
for _mpg_ is `[10, 200]`, you automatically get
valid value interval `[1.4124, 28.248]` for _l/100km_.

Also, knowing that aviation jet fuel density is
0.8 _kg/l_ and that flight fuel consumption can be
measured in _kg/km_ leads us to the following conversion
formula 
```
l/100km = 100/0.8 * kg/km = 125 * kg/km
```
for conversion between _l/100km_ and _kg/km_ aviation
jet fuel. With this relation we can build up a bigger
system of related parameters
```
kg/km <--> l/100km <--> mpg
```
Where a change in one of the parameters cascades to
all other related parameters.


Problems that the library solves
--------------------------------

* Bidirectional parameter relation by only defining a single translation
  function. This avoids numeric differences that can arise when defining
  both a function and its inverse for a bidirectional translation.

* Numeric precision and control in all parameter relationship,
  using something that in Gears is called _Backtrack Preference_.

* Automatic handling and propagation of changes in value and valid
  range of related parameters in large parameter relation models.

* Feedback on which relations a parameter has and which other
  related parameter that is currently restricting the range of valid
  parameter values.


Technical details to consider in use
------------------------------------
There are some technical details to be aware of when using the
Gears library.

### Backtracking
Assume that there are two properties `A` and `B`. Further let `f` be a
function from `A` to `B`. Propagating values from `A` to `B` is easily done
just using `f(A) = B`. In the other direction calculate `A`, given a
specific `B`, is not equally trivial. In that case a search algorithm
must be applied to find `A` and this use of a search algorithm is, in
the Gears library, referred to as _backtracking_.

The default backtracking algorithm used is the [bisection
method](https://en.wikipedia.org/wiki/Bisection_method).


### Valid function classes
For the backtracking mechanism used in Gears to work properly, all functions
must be [monotonic](https://en.wikipedia.org/wiki/Monotonic_function),
to not end up in a situation where it is impossible to backtrack a
unique function input, given a output.

Consider any periodic function. As an example, a _cosine_ function result
can not be backtracked to a unique input angle, if the input angle is not
scoped to a small subset of all possible _cosine_ input angles.

### Function direction

All functions have a [domain of valid input and a result
image](https://en.wikipedia.org/wiki/Domain_of_a_function). When
defining the relation between two parameters, use a function in the
direction from a large input domain to a smaller output image. This
will lead to a relation with better parameter value resolution than a
function from a small input domain to a larger image.

An example, consider the function from stock price to the boolean
decision to sell the stocks or not. Let say that the stocks should
be sold at the price $4.25 or higher.

```
doSell = stockPrice >= 4.25
```

In this case the function domain is the entire floating point set
of valid stock prices, but the image of the function is only the binary
set `{yes, no}`.

Using the inverse function instead, from the binary decision
{yes, no} to stock price, makes for a far less usable function
since the function result range only can consist of a maximum of
two different stock prices, e.g. the set `{$3, $5}`.


### Backtrack preference
An analogy to backtrack preference is to, by spring loading or by other
method, put tension on mechanical gears in order to reduce play or
backlash in the transfer mechanism.

Monotonic functions that for some reason has a plateau ("mechanical play")
in it's output range will have several input values that leads to the same
output value. Backtracking that output value to a function input value
raises the question which function input value should be preferred.

Using the earlier stock price example, backtracking the value `yes`
can lead to any of the values in the range `[4.25 , max_valid_stock_price]`.
Using a backtracking preference can guide the backtracking to
a specific value in the function domain:

| Backtrack preference | Backtracked value given `yes`      |
|----------------------|-----------------------------------:|
| minimum              | 4.25                               |
| minimum integer      | 5.00                               |
| mean                 | (4.25 + max_valid_stock_price) / 2 |
| maximum              | max_valid_stock_price              |


### A note on numeric limitations and the usefulness of backtrack preference

Consider the following function:

```
y = x + 40.0
```

Assume that floating point doubles are used for realizing the
function. Is the function in numeric sense to be considered
[_absolutely monotonic_ or just _monotonic_](
https://en.wikipedia.org/wiki/Monotonic_function#Monotonicity_in_calculus_and_analysis)?
In other words, does the function have plateaus or not?

As a matter of fact, since double floating point values has numeric
limitations, the function has plateaus and is hence only _monotonic_.

An example is using `x=10.0`, giving the unique function value
`y=50.0`. However, backtracking `y=50.0` can lead to any of the
following values, since all these `x` values leads to `y=50.0`:

| backtracked `x` given `y=50.0` |
|-------------------------------:|
| 9.999999999999996              |
| 9.999999999999998              |
| 10.0                           |
| 10.000000000000002             |
| 10.000000000000004             |

Using backtrack preference helps defining which backtracked `x` that
is preferable in this case.

Further, the reversed phenomena also exists. Using doubles, there
exists `y` values that can be backtracked to the same `x`
without being in the function image:

| `y` that can be uniquely backtracked to `x=-50.0` | Is `y` in the function image? |
|--------------------------------------------------:|:-----------------------------:|
| -10.0               | yes  |
| -10.000000000000002 | no   |
| -10.000000000000004 | no   |
| -10.000000000000005 | no   |




How to build
------------

The source contains a Gradle wrapper, so the only thing you need
to do after cloning the repository, in order to build and run the
library tests is to type the following in your terminal:

```
$ ./gradlew build
```

