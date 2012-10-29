Coding Synergy
==============

Developers "create" software.  Traditionally, this means we "build" software.  It is much like
building something with traditional Legos, or like writing a novel: we imagine what we want, and
then build it brick by brick (or word by word).  We keep the big picture mainly or entirely
in our minds.  We start with a blank sheet of paper, and fill it in by selecting from a fixed set of
primitives.

Software had to start there.  But it is time for us to instead "discover" software.  Cy aspires to
let us sketch in some initial suggestive details of what we want to create (I want to create a
command-line tool; I want to create a web site).  It presents us with existing software that might
best serve as a template, and then guides us through the necessary changes to reshape that software
to fit our mental image.

I'd like to create a command-line tool.  Here are some existing command-line tools, mapped by
quality and complexity.  Mine will be simple, and will process a text file.  I'll paste in an
example of the format of text I need to parse.  Now I see simple existing text-file-processing
command-line tools, with one highlighted that has sample input similar to my own.  That looks like a
good starting point -- I'll head over there.  Here's a good visual overview of it.  These subsystems
suit my needs; I'll keep them.  I'll delete those.  Here's what the remainder does for me -- not a
lot yet.

I'm going to need a data structure that looks kind of like this.  Now I see existing data structures
with characteristics similar to those I sketched.  Hey, that one looks like a good starting point.
I'll change this field, and add this new one.

I deleted this subsystem, so I need to implement my own.  It will process the data structure I just
tweaked.  The Cy environment sees that my tweaked data structure has strong similarities to the one
I started with, so it picks up some of the sample/test data from that one, and highlights the areas
where it is wrong or incomplete.  I edit those to create my own sample/test data.

Hmm -- I need to implement a function which, given this sample object, would yield the following
value.  Here's what I might call that function.  Cy creates several candidates to suggest, mimicking
existing functions that operate on similar data structures, especially those with similar names.  I
pick one and tweak it a bit.

etc.



Implementation Details
----------------------

A Cy account is typically tied to a Github account and a LinkedIn account.

Each Cy developer has a reputation score and an achievement score.  The reputation score is
based on a combination of LinkedIn activity (such as endorsements), Github activity, and Cy
activity.  The achievement score is based on the combined quality score of packages the developer
has written.

Each Cy package has a quality score.  This is based on thoroughness of contracts, test coverage,
amount of use, adherence to contracts in use, and social activity.

Each Cy package is surrounded with social activity such as tags, questions and answers, third-party
tests, third-party contracts, bug reports, etc.

When searching (explicitly or -- more typically -- implicitly in the IDE) for a Cy package, the
package API is used similarly to Hoogle and the social activity improves keyword matching.  Ranking
of search results is based on a combination of relevance, package quality, and developer scores.

The Cy social environment makes it easy to request proposals for freelance coding by specifying a
contract, a test coverage level, optionally a dollar amount, and optionally minimum developer
reputation and achievement scores.