## <a name="searching"></a>Searching

When you use Pursuit's search feature, there are three types of results which
may come up: declarations, modules, and packages.

### Declarations

A *declaration* is anything exported from a module which exists in a package
which has been uploaded to Pursuit. This is the most common type of search
result. It includes types, values (note: functions are values), data
constructors, type synonyms, type classes, kinds, and so on. For example:

* the query "const" will return the [`const`][] function in the results,
* the query "Unit" will return the [`Unit`][] type in the results, and
* the query "applicative" will return the [`Applicative`][] type class in the
  results.

Searching using prefixes of the desired result's name also works; for example,
the query "sequen" returns the [`sequence`][] function.

You can also search for declarations based on their type. For example, the type
of [`const`][] is `forall a b. a -> b -> a`; if you search for "a -> b -> a",
then `const` will appear in the search results.

When searching by type, Pursuit knows that the names you give to the type
variables do not matter. For example, the query "x -> y -> x" also returns
`const` in the results.

### Modules

A *module* is the basic unit of packaging PureScript code up for reuse. For more
information on modules, see the [Modules page in the documentation
repository][].

To search for a module, enter either the whole module name or a prefix of it.
For example:

- the query "Control.Plus" returns the module `Control.Plus`,
- the query "Data.String" returns the module `Data.String`, as well as
  `Data.String.Base64`, `Data.String.CaseInsensitive`, and so on, and
- the query "DOM" returns the module `DOM`, as well as all of the other modules
  which begin with `DOM`.

### Packages

A *package* contains a number of modules, which are usually related and
intended to be used together. Packages also have metadata attached to them such
as their author, version, publish date, and so on.

To search for a package, simply enter its name or a prefix of it. It is not
necessary to include the "purescript-" prefix. For example:

* the query "lazy" returns the package `purescript-lazy`,
* the query "strings" returns the packages `purescript-strings` and
  `purescript-strings-extra`, and
* the query "argonaut" returns the packages `purescript-argonaut`,
  `purescript-argonaut-codecs`, `purescript-argonaut-core`, and so on.

### Types of searches

There are two types of searches that can be performed: searching by name or by
type. Pursuit will infer which type of search is intended based on the query.
Only declarations can have types (modules and packages cannot), so when you
search by type, you will only get declaration results.

When searching by name, an entry in Pursuit's database is considered to be a
match if the query is a prefix of its name. For example, "con" matches "const"
but "cosnt" does not. (This may change in the future.)

Currently, searching for **visible type applications** (e.g. `forall @a. a -> a`)
is not currently supported.

Currently, documentation comments are not included in the search index. This
means that, for example, if you search for "Kleisli", there are no results,
even though the documentation for [`Star`][] mentions that this type is also
called "Kleisli" in some contexts. This may also change in the future; see
[Pursuit issue #318](https://github.com/purescript/pursuit/issues/318).

[`const`]: https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude#v:const
[`Applicative`]: https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude#t:Applicative
[`Unit`]: https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude#t:Unit
[`sequence`]: https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Traversable#t:Traversable
[Modules page in the documentation repository]: https://github.com/purescript/documentation/blob/master/language/Modules.md
[`Star`]: https://pursuit.purescript.org/packages/purescript-profunctor/docs/Data.Profunctor.Star#t:Star

## Kind Signatures

### Explicit and Inferred

Data, newtype, type synonym, and type class declarations all have kind
signatures. These signatures are either explicit (i.e. developer-defined)
or implicit (i.e. compiler-inferred). For example

```purescript
-- Explicit kind signature
data ExplicitFoo :: forall k. k -> Type
data ExplicitFoo a = ExplicitFoo

{- Kind signature inferred by the compiler for the below type:
data ImplicitFoo :: forall k. k -> (Type -> Type) -> Type         -}
data ImplicitFoo a f = ImplicitFoo (f Int)
```

### Merging Documentation Comments

Since both the kind signature declaration and the data/newtype/type/class
declaration can have documentation comments, both will be merged together
with a newline separating them. For example, the below code's doc comments...
```purescript
-- | Kind signature declaration documentation comment
data ExplicitFoo :: forall k. k -> Type
-- | Data declaration documentation comment
data ExplicitFoo a = ExplicitFoo
```
... will be rendered as
```
Kind signature declaration documentation comment
Data declaration documentation comment
```

### Interesting kinds are displayed; Uninteresting kinds are not

The following design choice should make it easier for new learners
to learn the language by limiting their exposure to these more
advanced language features.

By default, all kind signatures, whether explicitly-declared by the developer
or inferred by the compiler, will only be displayed if the kind signatures
are considered "interesting." Put differently, "uninteresting" kind signatures will not be displayed.

An "uninteresting" kind signature is one that follows this form:
- `Type`
- `Constraint`
- `Type -> K` where `K` refers to an "uninteresting" kind signature

Here's another way to think about it: kind signatures are considered
"uninteresting" if all of their type parameters' kinds have kind `Type`.

#### Examples of "uninteresting" kind signatures

```purescript
data TypeOnly :: Type
data TypeOnly

class IntentionallyEmpty :: Constraint
class IntentionallyEmpty

class Bar :: Type -> Type -> Constraint
class Bar a b where
  convert :: a -> b
```

#### Examples of "interesting" kind signatures

Each kind signature below is "interesting" because it has at least one
type parameter whose kind is something other than kind `Type`:
```purescript
-- the "k" part makes this kind signature "interesting"
data PolyProxy :: forall k. k -> Type
data PolyProxy a = PolyProxy

-- the `(Type -> Type)` part makes this kind signature "interesting"
data FunctorLike :: (Type -> Type) -> Type -> Type
data FunctorLike f a = FunctorLike (f Int) a

-- every type parameter makes this kind signature "interesting"
class TypeLevelProgrammingFunction :: Symbol -> Row Type -> Row Type -> Constraint
class TypeLevelProgrammingFunction sym row1 row2 | sym row1 -> row2
```
