Cy
==

Cy is an effort to reinvent the usability of programming just as radically as the iPhone reinvented
the usability of consumer devices.  It is a language and platform designed from the ground up for
"code mining" -- a planet-wide collaborative search through the space of useful software
components.

Linguistically, Cy aspires to preserve the beauty of Haskell, but in a simpler and more approachable
way. Cy is a "low Haskell" world where the functions are pure, the statements have effects, and the
monads live in "for" statements.

Operationally, Cy compiles to JavaScript, the winner in the write-once-run-everywhere war.

Cy was created by Dean Thompson.


Am I Crazy? (Dean)
------------------

Yes. This is a serious effort to introduce a new programming language and a new platform. I want to
reinvent the programming experience almost from the ground up with a different set of priorities.
Objectively speaking, the odds of something like this succeeding have to be 10,000 to 1 against, or
worse.

However, as a programming community, we do need to reinvent our craft from the ground up. We have
learned a lot in our first 50 years. We can do better now.

I have squinted off in the direction that many others are pointing -- Bret Victor in particular --
and I believe I see the new mountain in the distance. I also believe I see a rough path from here in
that direction. So I am starting down that path. If I starve along the way, I will have learned a
lot, and hopefully helped others find a better path. But my hope is that some of you will join me,
we will beat the odds, and we will be the first to reach that new mountain.

Wouldn't it make more sense to improve our odds by embracing some existing language and platform and
applying these principles to them?  I see the appeal in that idea, but I don't believe it will help
us get to the new mountain. An existing language and platform would be a huge gravitational force
trying constantly to pull us back to the status quo. Every time we "saved time" by reusing what
exists, it would pull us off our path. It would change this from an effort to find several 10x gains
into an effort to find several 10% gains.

Nothing is going to make this easy. Nothing will change the fact that we begin with the odds
overwhelmingly against us. But to have any chance at all, I feel strongly that we have to start with
nothing but our experience and a few key principles, and then assemble a new programming world
thoughtfully piece by piece. If this is indeed the right direction, our progress will be
exponential. For months or even years it will seem as though we have accomplished very little, until
we gradually start to realize that we are flying along.


Code Mining
-----------

Code mining is a collaborative search through the space of useful software components by a
planet-wide network of people and machines who take on the following five roles:

- *Principal*: the primary author of a particular set of software components.

- *Peer*: creates software and information for his own purposes, but that may be useful to the
   Principal. (A Peer acts as a Principal from the perspective of his own projects.)

- *Freelancer*: assists the Principal in a "work for hire" capacity, such as by coding to a spec or
   generating test cases. (A Freelancer acts as a Principal from the perspective of the tasks that
   he takes on.)

- *Random*: a member of the crowd, which takes on tasks such as certain kinds of testing that do not
   require the attention of a Principal.

- *Machine Assistant* (MA): a software component that takes on tasks such as coding to a spec or
   generating test cases.

The Cy platform facilitates each of these five roles and the flow among them. The goal is a
seamless interplay across creation, discovery, and validation, and among highly skilled
Principals, relatively unskilled Randoms, and sophisticated but mechanical MAs.

For example, a Principal needs a UI component for creating a to-do item. She begins writing some
high-level documentation for this component. An MA integrated into her IDE suggests an open source
component (written by a Peer) with documentation textually similar to what the Principal has written
so far, and with quality scores and open-source license that meet the Principal's filters. The
Principal examines the open source component and decides that although it doesn't perfectly fit her
needs, it is a good starting point. She clones the component, edits its documentation to describe a
variation that she wants, flags it as needing assistance with 3 assistance value points, and moves
on to something else. (She doesn't change the 12-hour deadline that she uses as her default for
assistance.)

A Freelancer who has worked with the original component before and who meets the Principal's
freelance reputation filter (and whose principal reputation filter was met by the Principal), is
alerted to this request. He checks the fee for the work (which was computed automatically from the
Principal's monthly budget, her assistance-value-point velocity, and the value points tht she
assigned to this task) and decides it is worth his while. He claims the assistance flag. (This
Principal pays cash and this Freelancer prefers cash. Most work-for-hire deals are done in-kind on
Cy credits.)

The Freelancer creates a new version of the component that he believes matches its new
documentation, adds some entries to the new version's human-testing plan, and releases the
assistance flag. An MA notices that both the component and the human-testing plan have been
modified since the component was last tested, that the component has an unclaimed assistance flag,
and that the Principal has a cash budget. It creates a set of crowd-sourcing tasks to test the
modified component against the modified human-testing plan. These tasks are carried out by Randoms,
with multiple Randoms being assigned the same testing tasks so that the results can be
cross-checked. The level of redundancy depends on the reputations of the Randoms who do the work.

The Principal is alerted that the modified component is ready for her review. She examines the
diffs from the original, the updates to the human-testing plan, and the summarized test reports from
the Randoms. She accepts the work, which is then incorporated into her project. This triggers
payments to the Freelancer and the Randoms.

The Peer who created the original version of the component is alerted that a new version has been
implemented and tested. He sees that the Principal's open-source license allows him to incorporate
the changes, and he likes them, so he incorporates them into his own version.


Cy Qualities for Code Mining
----------------------------

Cy's design is driven by the main platform qualities required for Code Mining:

- [Learnable Programming](http://worrydream.com/LearnableProgramming/), as envisioned by Bret
  Victor. 

- "Stackable abstractions" -- abstractions that can be composed and layered while maintaining a
  clear, concise behavioral contract. The basic building blocks of Cy (pure functions and actors)
  are chosen to be easily and accurately composable and highly testable.

- Elegant support for the code-mining flow, which is the primary driver of the platform design
  and the language design.

One reasonable way of looking at Cy is to see it as a language and platform that are driven from the
ground up by the goals of first realizing Bret Victor's vision of Learnable Programming and then
leveraging the resulting benefits. Cy is also inspired partly by Chris Granger's [Light
Table](http://www.chris-granger.com/2012/04/12/light-table---a-new-ide-concept/) project, but with a
belief that a new language and platform are necessary to realize the full potential of that
direction.

Cy's IDE will apply Bret's ideas about "creating by reacting" and "recomposition" by automatically
pulling in open-source code examples that meet the developer's needs, much as chess software pulls
in examples of public games that proceeded from the current position. Machine Assistants (MAs)
integrated into the IDE will discover or generate code to meet the developer's needs, much as chess
software shows potential lines of play and their likely outcomes. For example, when the developer
starts writing a signature and contract for a function, an MA will suggest existing functions in
open-source code that have similar signatures and contracts. It will decorate each suggested
function with its quality score and the developer's reputation and achievement scores. The intended
feel is much like how chess software shows potential lines of play with their statistics.

An MA will even suggest implementations of the function that combine small numbers of existing
functions to provide the specified signature and meet the contract. The developer can navigate to
examples in public source code that have combined those functions in that way. The intended feel is
much like how chess software shows references to published games that have proceeded from the
current board position or from a potential line of play.


Platform Design
---------------

From a linguistic perspective, Cy aspires to preserve the beauty of Haskell, but in a simpler and
more approachable way. In Haskell terms, every Cy function can be regarded as running in an ST
monad (Haskell's "state transformer"), with language support for vars (variables) in that monad.
Instead of Haskell's IO monad, real-world effects are implemented in Cy as actors called
"components", which Cy encourages to be RESTful where feasible. Primitive IO operations are
implemented underneath the Cy platform (in JavaScript) and wrapped in components. This approach is
motivated by the following reasoning:

- There is great power in pure functions in a typed language. They are highly composable, have
  a natural rough specification in the function signature, are naturally specified further
  by contracts, and define a highly searchable space of behaviors (ala Hoogle).

- Programs must also have effects. Since pure functions can't have effects, this forces a
  two-layer approach. Haskell has proven that a two-layer approach can be practical and 
  beautiful.

- However, Haskell's choice of upper layer -- monads -- forces programmers into intense mathematical
  abstraction that only suits a small minority of them.

- Since we are required to have at least two layers, we'd like to do as much as possible with
  just two. Cy takes the position that the most natural and capable upper layer is actors.

- RESTful components have proven to be an especially approachable and widely applicable subset of
  the more general space of actors.

Cy supports control-flow monads (such as Maybe or List) in "for" statements. In fact, Cy's "for"
statement supports any type of monad except IO, but the built-in support for stateful effects
through vars and actors greatly reduces the importance of monads for purposes other than control
flow.

The Cy language is designed for smooth interplay with the other Cy platform features needed to
support effective code mining:

- Reputation management and quality scoring.
- Freelancing and crowd-sourcing.
- Micropayments.
- API contracts and testing.
- Publishing, versioning, and dependency management.
- Open-source license management.
- Searching for components and functions that work in a particular context.
- Human-readable data (all types have readable JSON representations).
- Development-mode capture and playback of component and function inputs and outputs.
- Built-in tracing and debugging integrated with API contracts.
- Production deployment.

Incidentally, Cy's code-mining flow supports all types of information artifacts, so it can be
directly applied to projects that don't use the Cy language at all. Perhaps it should be called
a truth-mining flow, but we would be getting ahead of ourselves.


Stackable Abstractions
----------------------
The easiest way to explain stackable abstractions is to give a typical example of non-stackable
abstractions. This example uses Java's syntax, not because Java is any worse than most other
platforms, but as a typical example. Suppose we have the following classes Foo and Bar:

    /**
     * Data access layer for the "foo" db. This class obtains the path of its configuration file
     * from the system property "foo.conf" (defaulting to "./foo.conf") and reads that file to get
     * its JDBC connection string.
     */
    class Foo {
      /**
       * Lists the F's in the "foo" db. This method uses a connection pool thread to connect to the "foo" db
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
       * Fetches the B values corresponding to a list of F keys by making an RPC
       * to the bar service, which will internally obtain some database locks.
       */
      List<Integer> fetchBValues(List<String> fs) throws RpcException
      { ... }
    }

Now suppose we want to create a Bar method that combines these capabilities:

    List<Integer> listBValues(String fooKey) throws DbException, RpcException

To explain the actual, complete behavioral contract of the listBValues method, we would have to
write a short essay!  If we scale this up to the complexity of a real system, the contract becomes
so complex that it is never fully understood, much less written down.

Some of this complexity is unavoidable. Systems need configuration values, which change their
behavior. Operations take time, and sometimes they fail. And so on.

Cy attempts to tame this complexity by providing a simple set of stackable primitives, and by
supporting a style of development that keeps the abstractions stackable. Here is how Cy deals with
the major issues:

- Functions are pure, meaning that they have no side effects. This includes not blocking and not
  observing any state outside their parameters. So a function's contract can be specified entirely in
  terms of its parameters and return value. When provided with the same parameter values, a given
  function always returns the same result.

- Components are actors: they have internal state, and they send and receive messages. From the
  standpoint of a component's code, the component always starts out with the same constant internal
  state. However, the first message that a component processes is always the "init" input of the
  "admin" port. Like any input, "init" takes a statically typed message, which is expected to
  provide the component's configuration if any. (If a component doesn't declare admin.init, the
  compiler provides one that takes a unit input and does nothing. A component can be reinitialized
  by explicitly sending it the "init" message. The runtime always returns a component to its
  internal initial state before invoking its "init" message handler.)

- The actions (event handler executions) within a component are serialized with respect to each
  other. For a component implemented in Cy, this is done by treating the component's vars as STM
  (Software Transactional Memory) variables. For a component implemented partly in JavaScript, this
  is required by convention. Component actions are triggered by input events, replies from other
  components, and by these same occurrences when a component communicates with itself via internal
  actions.

- A component's state can only be observed externally through the component's outbound messages,
  including the init message and replies. Because of this, and given that actions within a
  component are serialized, a component's contract can be specified entirely as invariants across
  its sequence of inbound and outbound messages.

- Execution never blocks within a function or an action. The flow of messages between actions
  (within and across components) is modeled as asynchronous events. Cy's philosophy is that,
  although a synchronous model (where threads block on IO, RPCs, etc.) is superficially more
  convenient, it is such an unsupportable lie that it causes more trouble than it is worth.
  Instead, Cy focuses on making asynchrony as convenient and natural as possible.

- Components are RESTful by default, with built-in support for the Verbs GET, POST, PUT, and DELETE.
  There are additional Verbs DO and EVENT for non-RESTful actions with and without responses.

- Cy provides built-in support for specifying and validating contracts at both the function
  level and the component level.


Object Model
------------
Unlike Haskell, Cy does have a notion of objects, which borrows lightly from the OO world.
However, Cy's notion of an object is extremely simple. It is mostly light syntactic sugar
over a Haskell-like functional model.

There is no subclassing or inheritance. This is to avoid the complexity of covariance, type
inference in the face of subclassing, etc. We aren't just being lazy; that complexity would spill
over into the programmer's mental model and into error messages.

Objects are immutable.

Methods have the following special characteristics compared to regular functions:

- A method is invoked with the usual special syntax: `x.m y z`

- An object class can have private fields and methods, which a method can access.

- An object class serves as a namespace for its methods, so the same method name
  can be used in multiple object classes.

- Methods provide the following notational convenience:

  + A method takes a copy of the object on which it was invoked as an implicit first lvalue parameter;

  + implicitly modifies that lvalue when it updates fields of the object;

  + and implicitly returns that lvalue if it is declared to return the object's own type,
    and if it ends with a statement instead of an expression.

  + Plus, this implicit behavior extends to a method that invokes another method: the implicit
    `this` lvalue is passed to the invoked method, and if that method returns a value of the same
    type, the `this` lvalue is updated in the caller.
     
  
License
-------

All files in this project (whether so marked or not) are Copyright (c) Dean Thompson and
subject to the Apache 2 license: http://www.apache.org/licenses/LICENSE-2.0.html


Documentation
-------------

See the doc and notes directories for documentation work-in-progress. Also see the Cy directory
for examples.