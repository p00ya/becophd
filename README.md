# becophd R package

This is an R package for processing Bec's PhD data.

## License

Copyright (C) 2025 Dean Scarff

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 3.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

## Installation

Install R 4.1.0 or later.  Then from R:

```R
install.packages("remotes")
remotes::install_github("p00ya/becophd", auth_token = NULL)
```

## Example Usage

From R:

```R
library(becophd)
config <- Config("/path/to/data")
index <- ReadIndex(config)
dfs <- ReadAllConfiguredSummaries(config, index)
PlotAllConfiguredSummaries(config, index, dfs)
```
