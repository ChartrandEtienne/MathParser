MathParser
==========

Algebra expression in, pretty-printed algebra expression out. 

(new branch here)

Well, for now. In the near future, I'd like this thing to out say the solutions, the derivative, the integration, the fourrier transform, I don't know. Things. 

It's now a proper SBT project. Just jump into the "project" folder and run your favourite sbt command, that is if your sbt all-time favourite is "run".

For now it just runs what I'm working on, which is flattening left-leaning trees of +/- and * into sums and products, which are so much more intuitive to think about but mean that I needed a tree with arbitrary numbers of branches, which seems to be an artifact that is not really a point of focus in litterature, despite the fact that they're pretty dang important, when you think about it. 

The next step is to make a negate function, which runs a tree to find the easiest way of turning it into it's reciprocal over the "+" operation. It will come handy when we'l have stuff like "1 + 2 - 3". From then on, the flattens are solved. 

After that, I'l need to implement the basic algebra over expressions, which is gonna be something. Then will come simplification and factorisation, then expansion, which are going to be titanically hard, but what's the point of automating algebra besides that amirite?

Roadmap for the algebraic primitives
------------------------------------

* free_of(expr): Boolean => (obvious)(probably not that easy)
* substitute(variable, expr) => planned to be part of the all-encompassing eval. 
* plynomial_gpe(expr, variables): Bool => is it a power series over variables? 
* degree_gpe(expr, variable) => degree of expr with respect to variable i.e. biggest exponent that can be found for variable.
* coefficient_gpe(expr, variable, exponent) => uh... Returns the sum of the coefficent of the terms that can be divided by variable^exponent
* leading_coefficient(expr, var) => ... wh ... what?
* variables(expr) => all of the variables. Seems to include the transcendental with variables as parameters. Like, the transcendental themselves. 

* expand: ??
* derivative(expr, variable): expr => obviously returns the derivative with respect to variable; no need to specify variable if the expression is over one variable. I think it won't be necessary to simplify the result of this guy in the context of internal calculations. 
* Partial Fraction Expansion: 186... Ouch fuck no

* long_division(expr, div): (quotient, rest) => self-explanatory. Will be heavily used to check for divisors with no rest in the facto algorithm. P. 126.
* gcd(expr1, expr2): gcd => greatest common divisor of two expressions, via the Euclidean algorithm. Will obviously use the division method. 
* extended_gcd(expr1, expr2): (gcd, x, y) => not sure if it will really be useful, but yeah, this is the Extended Euclidean algorithm. 


Roadmap for the simplyfying function
------------------------------------

There's this book called Computer Algebra and Symbolic Computation: Mathematical Methods by Joel S. Cohen, and it's a total bible. For example right now my goal is to implement most of the expression simplification algo described in in chapter 3. 

Here it goes. 

* the basic distributive transformation: this one is dubious in the context of simplification, but will probably pretty simple to implement and will be useful in many contexts anyways, so let's just do it anyways, as a separate entity, separate of the main simplify algo. 

* DONE the basic association transformation: a freshly parsed expression is a binary tree of binary ops, but some operations really map well to n-ary nodes, and those are usually called in algebra parlance to be associative. toFlatten runs the tree and turns every tree where one branch is the same op into an n-ary node, such that (2 + (3 + 4)) becomes (2 + 3 + 4), and especially (2 + (3 + (4 + 5))) becomes (2 + 3 + 4 + 5); this last one was hell to implement, and I don't even really remember how it works exactly. It also completely messed my data structures. But yeah. For all intents and purposes, this one is implemented under toFlatten. 

* the basic commutative transformation: this one probably would be better solved by having a function reorder operands by lexicographic ordering of variables; that's trivial in the trivial case of a power series, but it's weirder in arbitrary expressions. An interesting sub-function would test equality between two expressions: simple matter of reducing both to their lexicographic odering and checking for equivalence of that. Another approach would be to implement set operations on the data structures; set equivalence would mean algebraic equivalence for a given commutative operator. 

* the basic power transformation: a^n * b^m = a^m+n. This is a simple matter of proving that a = b and adding the exponents (aka creating a new node to add n and m). The tricky part is that this is going to happen a lot in the aformentionned associative suites, and this is going to be a lot of d*cking around. I really ought to create an operator which assembles terms of an n-ary operation according some condition that doesn't occur to me right now. Perhaps just a passed boolean closure. 

* DONE the basic difference transformation: part of the toFlatten function, yeah. Not for binops, but this is trivial to do anyways, especially now that I have a funky negate function to find the quick 'n easiest way to turn a - into a +

* the basic quotient transformation: yeah, this one might be *really* useful, especially just before the power one. Definitely a todo. 

* the basic identity transformation: todo. This will be simple but kind of tedious. Also how will I even deal with all those Undefined?

* the basic unary transformation: ought to be part of the toFlatten function, which is the only one which *can* produce unary ops. 

* the basic terms gathering transformation: where 2x^2 + 3x + 3x^2 becomes 5x^2 + 3x. This one will mostly pose problems when dealing with n-aries, and I *really* ought to find a way to deal with this in an easier way. Perhaps with a like a dictionnary or something. 

* the basic numerical transformation: the question here is "how far do we go". We'l see. 

