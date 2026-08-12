I'm analyzing subsistence harvest data from household surveys, and have created the attached example data frames to represent the data that I'm working with. Essentially, we rely on a csv of conversion factors to 1) convert the harvested amount of a resource into the pounds of usable weight each harvest yields, and 2) if need be, convert from pounds to a default unit of measure. Most species (resources, as they are called) have existing conversion factors that we pull from a database and enter into the conversion factor csv by hand. However, there are instances of 'unspecified' resources, where the respondent did not know exactly which species was harvested. In the case of unspecified resources, I would like to be able to calculate a conversion factor based on the harvested amounts and conversion factors for similar species. The harvest of any species may be given in various units, but for each resource there is only one default unit. Unit codes are:



unit	| unitDesc

\------------------------

1	| individual

2	| pound

4	| gallon

5	| quart

6	| cord

7	| dozen

8	| 6-gallon bucket

9	| plastic shopping bag

11	| pint

12	| cup



The default units for a given resource are included in a column of the conversion factor data frame.



I need a function that will calculate a weighted conversion factor for unspecified resources (resources where convFact is NA) given the following possible cases:



Standard case:

At least one source harvest is > 0 \& units == units of unspecified target (default units for both)

Approach: weighted average of source harvests to calculate conversion factor



Edge case 1:

At least one source harvest is > 0 \& units != units of unspecified target (default units for source, non-default for target)

Approach: use source conversion factors for calculating weighted average in default units



Edge case 2:

At least one source harvest is > 0 \& units != units of unspecified target (non-default units for source, default units for target)

Approach: apply temporary standardization; multiply harvest amounts of source by sUnits \[convFact \* lbsToDefault] then compute weighted average and apply default unit conversion factor to unspecified target



Edge case 3:

No source harvests for unspecified target

Approach: Return message "Conversion factor cannot be calculated using weighted average; please manually add a conversion factor for XX resource(s) to the conversion factor file (convFact\_final.csv)."



I would also like the function to:

* validate whether or not the correct columns are present in the data frame, and return an error if they aren't.
* take as arguments the harvest data frame, the conversion factor data frame, the unspecified lookup table, and a vector of column names that will be used to derived the weighted average harvest of source species.
* report the resource names and given units of unspecified resources for which calculated conversion factors were created.
* return an updated version of the conversion factor data frame, leaving the other inputs unmodified.
* use tidyverse verbs wherever possible, while keeping the code human readable and easy to follow (comments are appreciated).



Attached are a sample data frame of harvest data, a sample conversion factor data frame, and the unspecified lookup table. The vector of column names is always an object called 'harvAmtList' and in this example it has values 'amtSetGillNet', 'amtDriftGillNet', 'amtSeine', and 'amtOtherGear'. These will provide the data structure necessary for calculating weighted conversion factors.





\-----------------



Something I still can't get unstuck from my craw: If source was given in default units, but I need a conversion factor for non-default units, can I back-calculate the non-default CF from the default unit weighted average harvest? I guess this can be generalized to: if given units for target/source are mismatched, can I still calculate a CF for the target?



WE NEED TO UNSTANDARDIZE UNITS! Basically, there should be a step (could be invisible in the analysis, because it is only used for a few cases) where

If target is in unit x and not all or no sources are in unit x, convert



For each unspecified resource, identify which units it was reported in

Convert all harvest amounts for source resources into those units using CF\_unit\_y / CF\_unit\_x, CF\_unit\_z / CF\_unit\_x, etc. \[group by source unit]

Calculate weighted average harvest as wtd\_CF\_unit\_x

