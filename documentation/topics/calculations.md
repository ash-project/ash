

taken from `allow_async?`
      This is useful for calculations that are very expensive, especially when combined with complex filters/join
      scenarios. By adding this, we will rerun a trimmed down version of the main query, using the primary keys for
      fast access. This will be done asynchronously for each calculation that has `allow_async?: true`.

      Keep in mind that if the calculation is used in a filter or sort, it cannot be done asynchronously,
      and *must* be done in the main query.