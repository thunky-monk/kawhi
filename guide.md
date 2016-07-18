# Guide

## Gather requirements

Ideally, the people responsible for [stats.nba.com](stats.nba.com) would explain exactly how to access all the data in an easy to use website. Unfortunately, that is not the current reality. For now, we have to do some detective work. Luckily, it's not too hard.

- Go to the page you want to get data from on [stats.nba.com](stats.nba.com). For example, go to the [San Antonio Spurs "advanced statistics" page](http://stats.nba.com/team/#!/1610612759/stats/advanced/).
- In the menu bar click `View -> Developer -> Developer Tools`.
- In the Developer Tools view, click the `Network` tab.
- Click `XHR` from the list of filters.
- Click the row with name that starts with `teamdashboardbygeneralsplits`. You may need to refresh.
- Click the `Preview` tab.

All the data you'll need is presented in this tab.

You'll need the `resource`, which is shown as `teamdashboardbygeneralsplits`. This is the unique identifier for this collection of data. In this case, it's the general splits for a specific player. **Record this value.**

```
teamdashboardbygeneralsplits
```

Additionally, you'll need data in `parameters`. Click the arrow to open `parameters` to reveal all the data. This collection of pairs, like `MeasureType` and `Advanced`, are parameters that control the data returned in the `teamdashboardbygeneralsplits` resource. The key `MeasureType` with value `Advanced` specifies that the data should contain "advanced" statistics instead of basic ones. All the pairs shown are *required* for valid data. **Record all of the keys and values.**

```
"MeasureType": "Advanced"
"PerMode": "PerGame"
"PlusMinus": "N"
"PaceAdjust": "N"
"Rank": "N"
"LeagueID": "00"
"Season": "2015-16"
"SeasonType": "Regular Season"
"PORound": 0
"TeamID": 1610612759
"Outcome": null
"Location": null
"Month": 0
"SeasonSegment": null
"DateFrom": null
"DateTo": null
"OpponentTeamID": 0
"VsConference": null
"VsDivision": null
"GameSegment": null
"Period": 0
"ShotClockRange": null
"LastNGames": 0
```

Finally, you'll need some data from `resultSets`. Click the arrow to reveal all the data. The rows in `resultSets` are the splits. Find the split with the `name` `MonthTeamDashboard`. **Record this value.**

```
MonthTeamDashboard
```

This contains the data for the 2015-16 regular season, split by month. It corresponds to the `Month` section on the regular webpage. Click the arrow to open the `MonthTeamDashboard` split. Look for the `headers`. Click to reveal all of its data. Look at the list of values, like `OFF_RATING`. These values correspond to the columns in the regular webpage. For example, `OFF_RATING` corresponds to the `OffRtg` column. **Record all the values corresponding to columns you are interested in collecting data for.**

```
SEASON_MONTH_NAME
OFF_RATING
DEF_RATING
```

## Make a Haskell project
Look at the [example project](example). It uses the data from this guide. The output looks like:

```
AdvancedStats {month = "October", offensiveRating = 102.7, defensiveRating = 93.4}
AdvancedStats {month = "November", offensiveRating = 102.5, defensiveRating = 93.4}
AdvancedStats {month = "December", offensiveRating = 111.8, defensiveRating = 91.5}
AdvancedStats {month = "January", offensiveRating = 114.0, defensiveRating = 100.7}
AdvancedStats {month = "February", offensiveRating = 110.7, defensiveRating = 99.1}
AdvancedStats {month = "March", offensiveRating = 107.8, defensiveRating = 97.2}
AdvancedStats {month = "April", offensiveRating = 102.3, defensiveRating = 103.5}
```
