Cy Configuration
================

From the standpoint of a component's code, the component always starts out with the same constant
internal state.  However, the first message that a component processes is always the "init" input of
the "admin" port.  Like any input, "init" takes a statically typed message (the "init object"),
which is expected to provide the component's configuration if any.  If a component doesn't declare
admin.init, the compiler provides one that takes a unit input and does nothing.  A component can be
reinitialized by explicitly sending it the "init" message. The runtime always returns a component to
its internal initial state before invoking its "init" message handler.

Cy has a built-in class called Blueprint that is used to specify a component configuration.  A
Blueprint specifies the component type, its init object, and (recursively) Blueprints for the
component's subcomponents.  A Blueprint also specifies wiring among the component and its
subcomponents.

When a component instance is designed (typically in an IDE), the design is captured as a Blueprint.
For example, the Blueprint for a GUI specifies settings for the UI widgets and layouts and
relationships among them.  Component Blueprints are Cy source code just like component
declarations and function definitions -- they are included in ".stq" source files in the source
repository, so they are versioned, managed, edited, etc. like any other Cy source.  A Blueprint
may be in the same package as the declaration of the component type it configures, or in a different
package.

The final Blueprint that is used to deploy a component for execution is typically defined in layers:

* The author of the declaration for a component with non-trivial configuration typically provides
  one or more Blueprints for useful instances of that component.

* The author of a system that uses the component typically copies and refines an existing Blueprint
  for that component.  They may start with a Blueprint provided by the component's author or with
  someone else's Blueprint for the component.  Each Blueprint has an "origin", which refers to the
  original Blueprint (by module, module version, and exported name) from which the new Blueprint was
  cloned.  This provides a sort of prototype inheritance at the source code level -- an IDE can show
  the trail from a given Blueprint to its origin, highlighting the changes along the way.  It also 
  allows an IDE to assist with upgrading a Blueprint when a new version of its origin Blueprint is
  published.

* Cy has an additional built-in class called BlueprintDelta, which contains a series of
  transformations (BlueprintSettings) that can be applied to a Blueprint.  A BlueprintDelta is
  conceptually similar to a CSS stylesheet, in the sense that each BlueprintSetting specifies a
  pattern that can match multiple subcomponents of the target Blueprint (a path in the component
  tree, optionally with wildcards, and optionally with properties to be matched) plus a
  transformation that will be applied to the Blueprint for every matching subcomponent.  Each
  transformation is a Cy assignment statement, such as 
  
  `initObject.serverUrl := "http://foo.bar.com/baz"`

  or

  `wiring .= wire "rawIO.input" "processedIO.input"`

The final runtime Blueprint for a component is typically by a reference to a starting Blueprint
(by module id, version, and exported name) plus a list of references (in the same manner)
to BlueprintDeltas.


  