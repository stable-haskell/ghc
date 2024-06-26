.. _type-synonyms:

Liberalised type synonyms
-------------------------

.. extension:: LiberalTypeSynonyms
    :shortdesc: Relax many of Haskell 98's rules on type synonym definitions.

    :implies: :extension:`ExplicitForAll`
    :since: 6.8.1

    Relax many of the Haskell 98 rules on type synonym definitions.

Type synonyms are like macros at the type level, but Haskell 98 imposes
many rules on individual synonym declarations. With the
:extension:`LiberalTypeSynonyms` extension, GHC does validity checking on types
*only after expanding type synonyms*. That means that GHC can be very
much more liberal about type synonyms than Haskell 98.

-  You can apply a type synonym to a forall type: ::

         type Foo a = a -> a -> Bool

         f :: Foo (forall b. b->b)

   After expanding the synonym, ``f`` has the legal (in GHC) type: ::

         f :: (forall b. b->b) -> (forall b. b->b) -> Bool

-  You can apply a type synonym to a partially applied type synonym: ::

         type Generic i o = forall x. i x -> o x
         type Id x = x

         foo :: Generic Id []

   After expanding the synonym, ``foo`` has the legal (in GHC) type: ::

         foo :: forall x. x -> [x]

GHC does kind checking before expanding synonyms.

After expanding type synonyms, GHC does validity checking on types,
looking for the following malformedness which isn't detected simply by
kind checking:

-  Type constructor applied to a type involving for-alls (if
   :extension:`ImpredicativeTypes` is off)

-  Partially-applied type synonym.

So, for example, this will be rejected: ::

      type Pr = forall a. a

      h :: [Pr]
      h = ...

because GHC does not allow type constructors applied to for-all types.


