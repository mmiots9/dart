<h1 align="center">mdarts</h1>

This repo contains my package called <i><b>mdarts</b></i>, which contains functions to play darts matches, train yourself (next release) and analyze your stats (future releases).

<h3>Installation</h3>
To install this package, you should run this code in R:

```
devtools::install_github("mmiots9/mdarts")
```

<h3>Play a match</h3>
To play a match, you have to run this code:

```
match_501(player1, player2, sets2win = n, legs2win = k)
```

I strongly reccomend to store the result of the function in a variable, so that it is possible to get other features.
To train yourself in a single 501 match, a new function will be available soon.

<h3><i>Get</i> functions</h3>
There are a set of <i>get</i> functions that allow you to retrieve useful information about a match, a set or even a single leg.
These functions are:
<ul>
<li>getID: get the ID of the match/leg/set</li>
<li>getPlayers: get the name of the player/s of the match/set/leg</li>
<li>getWinner: get the name of the match winner</li>
<li>getStats: get the statistics for a single player or for both of them. It can be used on leg, set or match.</li>
<li>getDate: get the date of play</i>
</ul>

<h3>Classes</h3>
In this packages, 6 new S4 classes are used, each with its own methods for the get functions and for generic <i>show</i> and <i>summary</i> functions:
<ul>
<li>leg1p: single-player leg</li>
<li>leg2p: 2-players leg</li>
<li>set1: single-player set</li>
<li>set2p: 2-players set</li>
<li>match1p: single-player match</li>
<li>match2p: two-players match</li>
</ul>

<h3>Save and Load match functions</h3>
The functions <i>saveMatch</i> and <i>loadMatch</i> allow you to save and load a match into/from a text file.