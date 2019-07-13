
#[macro_export]
macro_rules! smtlib_theory {
    (theory $name:ident
      // TODO: can be random order
      : smt - lib - version $version:tt
      : smt - lib - release $release:tt
      : written - by $author:tt
      : date $date:tt
      : last - updated $last_updated:tt
      : update - history $update_history:tt

      // TODO: sorts and functions

      : definition $definition:tt
      : values $values:tt
     ) => {
        pub mod $name {
            pub const VERSION: &'static str = stringify!($version);
            pub const RELEASE: &'static str = $release;
            pub const AUTHOR: &'static str = $author;
            pub const DATE: &'static str = $date;
            pub const LAST_UPDATED: &'static str = $last_updated;
            pub const UPDATED_HISTORY: &'static str = $update_history;

            pub const DEFINITION: &'static str = $definition;
            pub const VALUES: &'static str = $values;
        }
    };
}

smtlib_theory! // test theory from http://smtlib.cs.uiowa.edu/theories-Core.shtml
(theory Core
  :smt-lib-version 2.6
  :smt-lib-release "2017-11-24"
  :written-by "Cesare Tinelli"
  :date "2010-04-17"
  :last-updated "2015-04-25"
  :update-history
 "Note: history only accounts for content changes, not release changes.
  2015-04-25 Updated to Version 2.5.
  2010-08-15 Minor fix.
 "

  :definition
 "For every expanded signature Sigma, the instance of Core with that signature
  is the theory consisting of all Sigma-models in which:

  - the sort Bool denotes the set {true, false} of Boolean values;

  - for all sorts s in Sigma,
    - (= s s Bool) denotes the function that
      returns true iff its two arguments are identical;
    - (distinct s s Bool) denotes the function that
      returns true iff its two arguments are not identical;
    - (ite Bool s s) denotes the function that
      returns its second argument or its third depending on whether
      its first argument is true or not;

  - the other function symbols of Core denote the standard Boolean operators
    as expected.
 "
  :values
 "The set of values for the sort Bool is {true, false}."
);

