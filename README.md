# lh-gsoc2015

## Short-Term Plan

 1. **Integrate into main LiquidHaskell pipeline, set up basic test cases**
 1. Handle type application edge cases
 1. Add type synonym structure and extract to it
 1. Swap out modInfoTyThings for traversal that pulls out non-exported
    top-level decls
 1. Hook in a traversal to re-enable local signature extraction
 1. Add expression parameter support: separate expression parameters from
    binders in parser/QQ
 1. Expression aliases

