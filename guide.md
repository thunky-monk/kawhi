# Guide

## Gathering requirements

Ideally, the people responsible for [stats.nba.com](stats.nba.com) would explain exactly how to access all the data in an easy to use website. Unfortunately, that is not the current reality. For now, we have to do some detective work. Luckily, it's not too hard.

- Go to the page you want to get data from on [stats.nba.com](stats.nba.com). For example, go to [Kawhi Leonard's "advanced statistics" page](http://stats.nba.com/player/#!/202695/stats/advanced/?Season=2015-16&SeasonType=Regular%20Season).
- In the menu bar click `View -> Developer -> Developer Tools`.
- In the Developer Tools view, click the `Network` tab.
- Click `XHR` from the list of filters.
- Click the row with name that starts with `playerdashboardbygeneralsplits`.
- Click the `Preview` tab.

All the data you'll need is presented in this tab.

You'll need the `resource`, which is shown as `playerdashboardbygeneralsplits`. This is the unique identifier for this collection of data. In this case, it's the general splits for a specific player. **Record this value.**

Additionally, you'll need data in `parameters`. Click the arrow to open `parameters` to reveal all the data. This collection of pairs, like `MeasureType` and `Advanced`, are parameters that control the data returned in the `playerdashboardbygeneralsplits` resource. The key `MeasureType` with value `Advanced` specifies that the data should contain "advanced" statistics instead of basic ones. All the pairs shown are *required* for valid data. **Record all of the keys and values.**

Finally, you'll need some data from `resultSets`. Click the arrow to reveal all the data. The rows in `resultSets` are the splits. Find the split with the `name` `OverallPlayerDashboard`. This contains the data for the 2015-16 regular season. It corresponds to the `Overall` section on the regular webpage. The other splits correspond to the other sections on the webpage, like `Location`. Click the arrow to open the `OverallPlayerDashboard` split. Look for the `headers`. Click to reveal all of its data. Look at the list of values, like `OFF_RATING`. These values correspond to the columns in the regular webpage. For example, `OFF_RATING` corresponds to the `OffRtg` column. **Record all the values corresponding to columns you are interested in collecting data for.**
