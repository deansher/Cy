Staq
=======
The "low haskell" software platform,
where the functions are pure, the statements have effects, and the monads live in "for" statements.

Compiles to JavaScript, the winner in the write-once-run-everywhere war.

Created by Dean Thompson.

Goals
-----
The basic building blocks (pure functions and actors) are chosen to be easily and accurately composable.
They are also highly testable.

The language attempts to preserve the beauty of Haskell, but in a simpler and more approachable way.
Essentially, every function implicitly runs in an ST monad (Haskell's "state transformer"), with 
language support for vars (variables) in that monad.  Instead of the IO monad, real-world effects
are implemented as actors called "components".  Primitive IO operations are implemented underneath
the Staq platform (in JavaScript) and wrapped in components.

Control-flow monads (such as Maybe or List) can be implemented for use in "for" statements.  So
can any other kind of monad other than IO, but the built-in support for stateful effects through
vars and actors makes this less commonly necessary.

Everything is an object.  Objects have methods, to provide function namespacing and polymorphism 
(a simpler counterpart to Haskell typeclasses), but objects are immutable.  (This stretches 
the term "object".)

Social, code management, and debugging facilities are inherent in the platform instead of 
being after-thoughts: 

- API contracts and testing.
- Publishing, versioning, and dependency management.
- Open-source license management.
- Searching for components and functions that work in a particular context.
- Human-readable data (all types have readable JSON representations).
- Development-mode capture and playback of component and function inputs and outputs.
- Suitability for long-term IDE and debugger development.

This is secretly an effort to develop hard AI, but that's a long story that isn't worth telling yet.
It isn't too difficult to see the basic outline: adaptive computation needs a way to efficiently search
a space of algorithms that is dense in useful behaviors.

License
-------
All files in this project (whether so marked or not) are Copyright (c) Dean Thompson and
subject to the Apache 2 license: http://www.apache.org/licenses/LICENSE-2.0.html

Documentation
-------------
See the notes directory for a rough pile of info.  Also see the staq directory for examples.