# What have we learned?

1.  Using grouping for institutes, and even for first and last authors seems to cause troubles, since not all of them are measured repeatedly in the data (some countries might only have first authors, or only one observation for a particular institute). Estimating within institute variance is therefore not possible, without using more data.
2.  Stronger priors have helped, but not alleviated this problem. They are also scientifically meaningful, since we do not expect APCs of above 10k.
3.  Priors for lognormal model need to be set very small, especially since the exposure is also logged and centered.
4.  Using a hurdle model might be complicated

# What have we found?

1.  In the current model (m6.2), the average effect of PP_top10 on the APC is zero, after accounting for country and field -\> all effects are found in the country coeffients
2.  

# Next steps

1.  We could do a general model with n = 10k-30k. Based on those findings, can then choose particular sample of countries, and get more data for them.

# Interesting additions

1.  Having more information, both at country and at institution side might be interesting. In particular, GDP or income region or similar might help explain certain trends.
2.  We could do a model nesting field / country. This should show interaction effects, so pptop10 might have a different effect in different fields across different countries. But this is maybe far-fetched, since the definitions of field zugehörigkeit and country zugehörigkeit are wobbly.
