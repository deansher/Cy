Staq
=======
The "low haskell" software platform,
where the functions are pure, the statements have effects, and the monads live in "for" statements.

Compiles to JavaScript, the winner in the write-once-run-everywhere war.

Created by Dean Thompson.

Goals
-----
Staq is a platform for building "stackable abstractions" -- abstractions that can be composed and
layered while maintaining a clear, concise behavioral contract.  The basic building blocks of Staq
(pure functions and actors) are chosen to be easily and accurately composable and highly testable.

The language attempts to preserve the beauty of Haskell, but in a simpler and more approachable way.
Essentially, every function implicitly runs in an ST monad (Haskell's "state transformer"), with
language support for vars (variables) in that monad.  Instead of the IO monad, real-world effects
are implemented as actors called "components".  Primitive IO operations are implemented underneath
the Staq platform (in JavaScript) and wrapped in components.

Control-flow monads (such as Maybe or List) can be implemented for use in "for" statements.  So
can any other kind of monad other than IO, but the built-in support for stateful effects through
vars and actors makes this less commonly necessary.

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
It isn't too difficult to see the basic outline: adaptive computation needs a way to efficiently
search a space of algorithms that is dense in useful behaviors.


Object Model
------------
Unlike Haskell, Staq does have a notion of objects, which borrows lightly from the OO world.
However, Staq's notion of an object is extremely simple.  It is just light syntactic sugar
over a Haskell-like functional model.

There is no subclassing or inheritance. This is to avoid the complexity of covariance, type
inference in the face of subclassing, etc. We aren't just being lazy; that complexity would spill
over into the programmer's mental model and into error messages.

Objects are immutable.

Method names do not live in a "class-local namespace".  They must be unique. The reason for this
is that we prefer using type inference (method and function names imply type) instead of
explicitly declaring types and using that to disambiguate names.  Also, this keeps type inference
very direct and simple, which makes error messages relatively easy to understand.

Methods, however, do have a one special privilege, plus some notational convenience.

The special privilege is that methods can access private fields.

The notational convenience is that a method

- takes a copy of the object on which it was invoked as an implicit first lvalue parameter;

- implicitly modifies that lvalue when it updates fields of the object;

- and implicitly returns that lvalue if it is declared to return the object's own type,
  and if it ends with a statement instead of an expression.
  
- Plus, this implicit behavior extends to a method that invokes another method: the implicit
  `this` lvalue is passed to the invoked method, and if that method returns a value of the same
  type, the `this` lvalue is updated in the caller.
     
When a method is used from outside the declaring object, or when applying a function declared at the top
level, method invocation syntax (a.foo b) or application syntax (foo a b) may be used
interchangeably. However, when invoking one method from inside another in the same object declaration,
method invocation syntax (foo b, with an implicit `this` as a) must be used.

  
Stackable Abstractions
----------------------
The easiest way to explain stackable abstractions is to give a typical example of non-stackable
abstractions.  This example uses Java's syntax, not because Java is any worse than most other
platforms, but because it is widely familiar.  Suppose we have the following classes Foo and Bar:

    /**
     * Data access layer for the "foo" db.  This class obtains the path of its configuration file
     * from the system property "foo.conf" (defaulting to "./foo.conf") and reads that file to get
     * its JDBC connection string.
     */
    class Foo {
      /**
       * Lists the F's in the "foo" db.  This method uses a connection pool thread to connect to the "foo" db
       * and temporarily obtains some locks on that database.
       */
      static List<String> listFs(String key) throws DbException
      { ... }
    }

    /**
     * Client proxy for the bar service.
     */
    class Bar {
      Bar(URL serviceUrl, ConnectionParams params) { ... }

      /**
       * Fetches the B values corresponding to a list of fs by making an RPC
       * to the bar service, which will internally obtain some database locks.
       */
      List<Integer> fetchBValues(List<String> fs) throws RpcException
      { ... }
    }

Now suppose we want to create a Bar method that combines these capabilities:

    List<String> listBValues(String fooKey) throws DbException, RpcException

To explain the actual, complete behavioral contract of the listBValues method, we would have to
write a short essay!  If we scale this up to the complexity of a real system, the contract becomes
so complex that it is never fully understood, much less written down.

Some of this complexity is unavoidable.  Systems need configuration values, which change their
behavior.  Operations take time, and sometimes they fail.  And so on.

Staq attempts to tame this complexity by providing a simple set of stackable primitives, and by
supporting a style of development that keeps the abstractions stackable. Here is how Staq deals with
the major issues:

- Functions are pure, meaning that they have no side effects. This includes not blocking and not
  observing any state outside their parameters.  So a function's contract can be specified entirely in
  terms of its parameters and return value.  When provided with the same parameter values, a given
  function always returns the same result.

- Components are actors: they have internal state, and they send and receive messages.  From the
  standpoint of a component's code, the component always starts out with the same constant internal
  state.  However, the first message that a component processes is always the "init" input of the
  "admin" port.  Like any input, "init" takes a statically typed message, which is expected to
  provide the component's configuration if any.  (If a component doesn't declare admin.init, the
  compiler provides one that takes a unit input and does nothing.  A component can be reinitialized
  by explicitly sending it the "init" message. The runtime always returns a component to its
  internal initial state before invoking its "init" message handler.)

- The actions (event handler executions) within a component are serialized with respect to each
  other.  For a component implemented in Staq, this is done by treating the component's vars as STM
  (Software Transactional Memory) variables. For a component implemented partly in JavaScript, this
  is required by convention.  Component actions are triggered by input events, replies from other
  components, and by these same occurrences when a component communicates with itself via internal
  actions.

- A component's state can only be observed externally through the component's outbound messages,
  including the init message and replies.  Because of this, and given that actions within a
  component are serialized, a component's contract can be specified entirely as invariants across
  its sequence of inbound and outbound messages.

- Execution never blocks within a function or an action. The flow of messages between actions
  (within and across components) is modeled as asynchronous events.  Staq's philosophy is that,
  although a synchronous model (where threads block on IO, RPCs, etc.) is superficially more
  convenient, it is such an unsupportable lie that it causes more trouble than it is worth.
  Instead, Staq focuses on making asynchrony as convenient and natural as possible.

- Staq provides built-in support for specifying and validating contracts at both the function
  level and the component level.

License
-------
All files in this project (whether so marked or not) are Copyright (c) Dean Thompson and
subject to the Apache 2 license: http://www.apache.org/licenses/LICENSE-2.0.html

Documentation
-------------
See the notes directory for a rough pile of info.  Also see the staq directory for examples.