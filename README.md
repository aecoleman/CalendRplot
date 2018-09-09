# CalendRplot
Package for creating calendar heatmaps in R using ggplot2

This package is currently under development. Features may work incorrectly or not at all.

Presently, the objective is to create a package which exports a single function. This function's first argument is a data.frame, and subsequent arguments give the user some customization options. The function will export a ggplot object, which the user can then either further customize or add additional geoms to.

Planned Features:
- Create calendar heatmaps, with years faceted and months separated by lines
- Ability for user to supply their own groupings to be used in facets and in drawing the monthly separations
- Quarterly separations?
- Alternate separation styles? (lines, snake, alternating surrounding boundaries, etc)
