# For version 0.1.2

Overlay dropdowns:
x make remove button look nicer (actionLink instead?)
- automatic label of start/end date/time. Could use e.g. heading arg to
  overlayServer, with TRUE, FALSE, or some kind of function that would operate 
  on ov and i.
- automatic dropdown for label. Appears below heading. Perhaps argument would
  be `select` or similar to overlayServer. FALSE for none, TRUE for yes to 
  include a dropdown to select the overlay label from possible labels. A 
  question then is how to automatically include all the types. Anyway, when 
  this is changed, label is changed, etc.
- extra argument to overlayServer which would be a 'menu' argument. This would
  be a function of ov and i, like heading, snap, etc, which would return some
  ui components as a function of ov and i. This would be used in the package
  to inject the UI using insertUI rather than renderUI. I think that would 
  allow me to get rid of the invalidateLater stuff, which seems to be causing
  problems. (Test: click thrice rapidly on the up/down button for vaccination
  intervention in the epi example. It should only go up twice, and in some 
  cases get stuck in an infinite loop of toggling up/down.)
- overall styling of dropdown menus, for when this is all complete.
