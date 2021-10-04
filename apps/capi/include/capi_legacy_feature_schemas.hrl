-ifndef(__capi_feature_schemas__).
-define(__capi_feature_schemas__, 42).

% Marking some feature as `discriminator` will make featureset comparator consider two sets with different
% `discriminator` values as _different everywhere_ which usually helps with diff readability.
-define(discriminator, -1).
-define(difference, -1).

-endif.
