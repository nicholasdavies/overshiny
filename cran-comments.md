## R CMD check results

This is a resubmission following 3 comments from CRAN for this new release.
Thank you for reviewing this package.

1. "Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'shiny'
Please note that package names are case sensitive."

Thank you. This has been fixed in DESCRIPTION.

2. "If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file [...]"

Thank you. There are no references describing the methods in my package.

3. "Please replace the \dontrun{}-wrapper with if(interactive()){}.
Since we cannot automatically check shiny interfaces, the best solution
would be to write tests for your not exported function (e.g. using
package testthat). Otherwise we wouldn't detect that your package does
not work any more because of changes in R or the packages you depend on."

Thank you. I have eliminated all dontrun wrappers, and have only added 
if (interactive()) where it is strictly needed. Thus the new examples now do
provide some testing of the parts of the package code that do not depend on 
a Shiny context. I have also added a 'testthat' test that checks one of
the key 'helper' functions of the package upon which the Shiny behaviour 
depends heavily (the panel_rects_ggplot() unexported function).

0 errors | 0 warnings | 1 note

* This is a new release.
