Cy Design Decisions
===================


Why Compile Directly to JavaScript instead of to LLVM?
------------------------------------------------------

In some ways, it is extremely attractive to use LLVM as the Cy compiler's back end.  That would give
us strong backend optimizations and allow us to target many native code environments and even
JavaScript!  So how could it possibly make sense to hand-code the Cy compiler in Haskell to go 
directly to JavaScript?

Having thought about this at some length, I (Dean) conclude that Cy needs to whole-heartedly embrace
some existing platform, and the only real decision is which one.  The libraries, infrastructure, and
knowledge that surround a healthy platform are extraordinarily valuable.  Although the LLVM platform
has much to commend it, I believe the JavaScript platform is a better host for Cy.  I end up
concluding that the fact that LLVM can be targeted to JavaScript is a red herring -- Cy with an
LLVM backend would still be a member of the LLVM platform, not the JavaScript platform.
