# TO DO

[Change function names to use underscores.](http://adv-r.had.co.nz/Style.html) This will be a major change and should have the version number incremented appropriately. This is in response to this warning:

```
> checking S3 generic/method consistency ... WARNING
  sum:
    function(..., na.rm)
  sum.resp.mats:
    function(mat.list, items)
  
  See section 'Generic functions and methods' in the 'Writing R
  Extensions' manual.
```

Some options are:

* ~~sum.resp.mats -> sum_resp_mats~~
* grid2nine -> grid_to_nine
* grid.cell.counts -> grid_cell_counts
* grid.item.info.ls -> grid_item_info
* rawgrid2uni -> create_grid_score
* within1diag -> within_diag
* grid.tr -> grid_trace
* grid.tri.summary -> grid_summary_tri
* delete.empty.mat -> delete_empty_grid
* fixLimeSurveyLikert -> fix_limesurvey_likert
* trinomial.test -> trinomial_test
* gen.probs.obj -> generate_trinomial_info
* calculate.ns -> calculate_ns
* find.critical.values -> find_critical_values
* find.rejection.region -> find_rejection_region
* find.cutpoints -> find_cutpoints
* ttpow -> trinomial_test_power

There are more things to update, especially internal functions and variable names. 
