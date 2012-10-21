Social Coding in Staq
=====================

A Staq account is typically tied to a Github account and a LinkedIn account.

Each Staq developer has a reputation score and an achievement score.  The reputation score is based
on a combination of LinkedIn activity (such as endorsements), Github activity, and Staq activity.
The achievement score is based on the combined quality score of packages the developer has written.

Each Staq package has a quality score.  This is based on thoroughness of contracts, test coverage,
amount of use, adherence to contracts in use, and social activity.

Each Staq package is surrounded with social activity such as tags, questions and answers, third-party
tests, third-party contracts, bug reports, etc.

When searching (explicitly or -- more typically -- implicitly in the IDE) for a Staq package, the
package API is used similarly to Hoogle and the social activity improves keyword matching.  Ranking
of search results is based on a combination of relevance, package quality, and developer scores.

The Staq social environment makes it easy to request proposals for freelance coding by specifying a
contract, a test coverage level, optionally a dollar amount, and optionally minimum developer
reputation and achievement scores.