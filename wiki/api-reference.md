# API Reference

<!-- cg:auto:functions -->
## Core Functions

This section documents the public API surface of the pipapi package. Functions are organized by domain:

### Poverty Statistics
Functions for computing poverty headcount, poverty gap, and related FGT measures.

### Welfare Distribution
Functions for analyzing and transforming household welfare distributions.

### Global Aggregates
Functions for accessing global and regional poverty aggregates.

*API documentation will be populated as functions are implemented and stabilized.*
<!-- cg:auto:end -->

<!-- cg:auto:parameters -->
## Parameters

### Common Parameters
- `country`: ISO3 country code (character)
- `year`: Reference year (numeric)
- `povline`: Poverty line in 2017 PPP USD per day (numeric)
- `welfare_type`: Type of welfare measure - "income" or "consumption" (character)
- `ppp`: PPP conversion factor (numeric, optional)

### Advanced Parameters
- `fill_gaps`: Whether to interpolate missing years (logical, default FALSE)
- `aggregation`: Aggregation level - "country", "region", "global" (character)

*Full parameter documentation will expand with API development.*
<!-- cg:auto:end -->

<!-- cg:auto:return-values -->
## Return Values

### Data Table Format
Most functions return data.table objects with consistent column naming conventions:
- `country_code`: ISO3 country identifier
- `year`: Reference year
- `headcount`: Poverty headcount ratio (0-1)
- `poverty_gap`: Poverty gap index (0-1)
- `poverty_severity`: Squared poverty gap (0-1)

### REST API Responses
API endpoints return JSON with standard structure:
```json
{
  "success": true,
  "data": [...],
  "metadata": {
    "request_time": "2026-08-27T21:10:55Z",
    "rows": 100
  }
}
```

*Return value specifications will be detailed as the API matures.*
<!-- cg:auto:end -->

← [Home](README.md)
