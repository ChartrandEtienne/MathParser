MathParser

Algebra expression in, pretty-printed algebra expression out. 

Well, for now. In the near future, I'd like this thing to out say the solutions, the derivative, the integration, the fourrier transform, I don't know. Things. 

It's now a proper SBT project. Just jump into the "project" folder and run your favourite sbt command, that is if your sbt all-time favourite is "run".

For now it just runs what I'm working on, which is flattening left-leaning trees of +/- and * into sums and products, which are so much more intuitive to think about but mean that I needed a tree with arbitrary numbers of branches, which seems to be an artifact that is not really a point of focus in litterature, despite the fact that they're pretty dang important, when you think about it. 

The next step is to make a negate function, which runs a tree to find the easiest way of turning it into it's reciprocal over the "+" operation. It will come handy when we'l have stuff like "1 + 2 - 3". From then on, the flattens are solved. 

After that, I'l need to implement the basic algebra over expressions, which is gonna be something. Then will come simplification and factorisation, then expansion, which are going to be titanically hard, but what's the point of automating algebra besides that amirite?
