

## Test environments

* Local Ubuntu 26.04 LTS, R 4.6.1

* R Under development (unstable) x86_64-w64-mingw32 R 4.6.0


## R CMD check results

* Local Ubuntu 26.04 LTS: 

```
Duration: 6m 32.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

* R Under development (unstable) x86_64-w64-mingw32:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Adrián Cidre González <adrian.cidre@gmail.com>'

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

## Other comments

This is a patch to debug the errors in some flavors: <https://cran.r-project.org/web/checks/check_results_duckspatial.html>. The error message is ambiguous, so this patch captures the complete error message to make it easier to check what's actually failing.
