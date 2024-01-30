// Redefine config values that have not previously been set

const
  {$if not Declared(CMM_DEBUG)}
  CMM_DEBUG = true;
  {$ifend}

  {$if not Declared(CMM_CONCURRENCY_POT)}
  CMM_CONCURRENCY_POT = 2;
  {$ifend}

  {$if not Declared(CMM_RELEASE_CLEAR)}
  CMM_RELEASE_CLEAR = false;
  {$ifend}

  {$if not Declared(CMM_RELEASE_TRASH)}
  CMM_RELEASE_TRASH = CMM_DEBUG and not CMM_RELEASE_CLEAR;
  {$ifend}

  {$if not Declared(CMM_ASYNC_PAGE_RELEASE)}
  CMM_ASYNC_PAGE_RELEASE = true;
  {$ifend}

  {$if not Declared(CMM_ASYNC_PAGE_RELEASE_INTERVAL)}
  CMM_ASYNC_PAGE_RELEASE_INTERVAL = 100;
  {$ifend}

// Set up environment according to config values

{$Optimization CONSTPROP}
{$Optimization DFA}

{$if CMM_DEBUG}
  {$Assertions on}
  {$Optimization LEVEL1}
{$else}
  {$Assertions off}
  {$Optimization LEVEL3}
  {$Optimization DEADSTORE}
  {$Optimization DEADVALUES}
  {$Optimization CSE}
  {$Optimization REGVAR}
{$ifend}

{$WARN 6018 off : unreachable code}

{$PackRecords 1}
{$ImplicitExceptions off}
{$TypedAddress on}



