The Glasgow Haskell Compiler - Stable Haskell Fork
==================================================

[![pipeline status](https://gitlab.haskell.org/ghc/ghc/badges/master/pipeline.svg?style=flat)](https://gitlab.haskell.org/ghc/ghc/commits/master)

**This is the Stable Haskell fork of GHC**, not the upstream GHC codebase.

This is the source tree for [GHC][1], a compiler and interactive
environment for the Haskell functional programming language.

**Important**: All issues and bug reports for this fork should be reported at:
<https://github.com/stable-haskell/ghc/issues>

For more information about upstream GHC, visit [GHC's web site][1].

Information for developers of upstream GHC can be found on the [GHC issue tracker][2], and you can also view [proposals for new GHC features][13].


Getting the Source
==================

There are two ways to get a source tree:

 1. *Download source tarballs*

    Download the GHC source distribution:

        ghc-<version>-src.tar.xz

    which contains GHC itself and the "boot" libraries.

 2. *Check out the source code from git*

        $ git clone --recurse-submodules https://github.com/stable-haskell/ghc.git

  *See the GHC team's working conventions regarding [how to contribute a patch to GHC](https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/fixing-bugs).* First time contributors are encouraged to get started by just sending a Merge Request.


Building & Installing
=====================

For full information on building GHC, see the [GHC Building Guide][3].
Here follows a summary - if you get into trouble, the Building Guide
has all the answers.

To build GHC, you need:
- A working version of [GHC][1] (>= 9.8.4), as the compiler is written in Haskell
- [cabal-install][9]

Both the bootstrap compiler and cabal-install can be easily installed with
[GHCup](https://www.haskell.org/ghcup/):

    $ ghcup install ghc 9.8.4
    $ ghcup install cabal
    $ ghcup set ghc 9.8.4

For additional system dependencies and libraries, see [Setting up your system for building GHC][8].
For instructions on how to port GHC to a new platform, see the [GHC Building Guide][3].

For building library documentation, you'll need [Haddock][6].  To build
the compiler documentation, you need [Sphinx](http://www.sphinx-doc.org/)
and Xelatex (only for PDF output).

**Quick start**: The following gives you a default build:

    $ make CABAL=$PWD/_build/stage0/bin/cabal

On Windows, you should run the build command from an appropriate
environment (e.g., MSYS2).

This gives you the default build, which includes everything
optimised and built in various ways (eg. profiling libs are built).
It can take a long time.

To run the test suite:

    $ make test CABAL=$PWD/_build/stage0/bin/cabal

Filing bugs and feature requests
================================

If you've encountered what you believe is a bug in this fork, or you'd like
to propose a feature request, please let us know! Submit an issue at
<https://github.com/stable-haskell/ghc/issues> and we'll be sure to look into it. Remember:
**Filing a bug is the best way to make sure your issue isn't lost over
time**, so please feel free.

If you're an active user of GHC, you may also be interested in joining
the [glasgow-haskell-users][11] mailing list, where developers and
GHC users discuss various topics and hang out.

Contributing to GHC
===================

Once you've filed a bug, maybe you'd like to fix it yourself? That
would be great, and we'd surely love your company! This section will
help you get started with contributing to GHC.

Getting Started with Development
---------------------------------

Make sure your system has the necessary tools to compile GHC. You can
find an overview of how to prepare your system here:

<https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation>

After building GHC (see "Building & Installing" above), you can start
making your commits. When you're done, you can submit a merge request
to [GitLab](https://gitlab.haskell.org/ghc/ghc/merge_requests) for
code review.

Changes to the `base` library require a proposal to the
[core libraries committee](https://github.com/haskell/core-libraries-committee/issues).

The GHC Wiki has a good summary for the
[overall process](https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions/fixing-bugs).
One or several reviewers will review your PR, and when they are ok with
your changes, they will assign the PR to
[Marge Bot](https://gitlab.haskell.org/marge-bot) which will automatically
rebase, batch and then merge your PR (assuming the build passes).

Useful Resources
----------------

The home for GHC hackers is our GitLab instance:

<https://gitlab.haskell.org/ghc/ghc>

From here, you can file bugs (or look them up), use the wiki, view the
git history, among other things.

An overview of things like using Git, the release process, filing bugs
and more can be located here:

<https://gitlab.haskell.org/ghc/ghc/wikis/contributing>

You can find our coding conventions for the compiler and RTS here:

<https://gitlab.haskell.org/ghc/ghc/wikis/commentary/coding-style>
<https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/conventions>

If you're going to contribute regularly, **learning how to use the
build system is important** and will save you lots of time. You should
read over this page carefully:

<https://gitlab.haskell.org/ghc/ghc/wikis/building/using>

If you want to watch issues and code review activities, the following page
is a good start:

<https://gitlab.haskell.org/ghc/ghc/activity>

How to Communicate with Us
--------------------------

GHC is a big project, so you'll surely need help. Luckily, we can
provide plenty through a variety of means!

### IRC

If you're an IRC user, be sure to drop by the official `#ghc` channel
on [Libera.Chat](https://libera.chat). Many (but not all) of the
developers and committers are actively there during a variety of
hours.

### Mailing lists

In the event IRC does not work or if you'd like a bigger audience, GHC
has several mailing lists for this purpose. The most important one is
[ghc-devs](http://www.haskell.org/pipermail/ghc-devs/), which is where
the developers actively hang out and discuss incoming changes and
problems.

There is no strict standard about where you post patches - either in
`ghc-devs` or in the bug tracker. Ideally, please put it in the bug
tracker with test cases or relevant information in a ticket, and set
the ticket status to `patch`. By doing this, we'll see the patch
quickly and be able to review. This will also ensure it doesn't get
lost. But if the change is small and self contained, feel free to
attach it to your email, and send it to `ghc-devs`.

Furthermore, if you're a developer (or want to become one!) you're
undoubtedly also interested in the other mailing lists:

 * [glasgow-haskell-users](http://www.haskell.org/mailman/listinfo/glasgow-haskell-users)
   is where developers/users meet.
 * [ghc-commits](http://www.haskell.org/mailman/listinfo/ghc-commits)
   for commit messages when someone pushes to the repository.

Governance and Acknowledgements
===============================

GHC is a community project developed by a team of highly-talented
 researchers, individual contributors, and full-time developers. We are indebted to the
[many people](https://gitlab.haskell.org/ghc/ghc-hq/-/blob/main/team.mkd?plain=0#user-content-the-ghc-team)
whose work has brought GHC to its current state.

Some larger decisions are made by a smaller group of core contributors,
as described in our [governance documentation](https://gitlab.haskell.org/ghc/ghc-hq#ghc-governance).


[1]:  http://www.haskell.org/ghc/            "www.haskell.org/ghc/"
[2]:  https://gitlab.haskell.org/ghc/ghc/issues
        "gitlab.haskell.org/ghc/ghc/issues"
[3]:  https://gitlab.haskell.org/ghc/ghc/wikis/building
        "https://gitlab.haskell.org/ghc/ghc/wikis/building"
[6]:  http://www.haskell.org/haddock/        "www.haskell.org/haddock/"
[8]:  https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation
        "https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation"
[9]:  https://github.com/haskell/cabal          "https://github.com/haskell/cabal"
[11]: http://www.haskell.org/pipermail/glasgow-haskell-users/
        "http://www.haskell.org/pipermail/glasgow-haskell-users/"
[13]: https://github.com/ghc-proposals/ghc-proposals
        "https://github.com/ghc-proposals/ghc-proposals"
