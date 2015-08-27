snake
=====

Attempt to make a Snake with my kids.

# Workaround

I use actionkid but the one from cabal does not compile for my box.
(There is a PR but i'm waiting for some kind of feedback https://github.com/egonSchiele/actionkid/pull/2)

In the mean time, I use cabal's sandbox like this:

```sh
cabal sandbox init
mkdir deps
git clone git@github.com:ardumont/actionkid.git deps/actionkid
cd deps/actionkid
git checkout fix-deps
cd -
cabal sandbox add-source deps/actionkid
cabal install
```
