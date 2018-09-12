Gears parameter engine
======================

Proof of concept project demonstrating consistent
handling of parameter relation models.


An example
--------------------
An example is the translanslation between mile
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
---------------------

* Bidirectional parameter relation by only defining one translation
  function. This avoids numeric differences that can arise when defining
  both a function and its inverse for a bidirectional translation. 

* Automatic handling and propagation of changes in value and valid
  range of related parameters in large relation models.

* Feedback on which relations a parameter has and which other
  parameter that is restricting the current range of valid values.


How to run
-----------------------

The source contains a Gradle wrapper, so the only thing you need
to do after cloning the repository, in order to build and run the
library tests is to type the following in your terminal:

```
$ ./gradlew build
```

