-module(blurbs).
-export([gimme_text/1]).

gimme_text(Profile) ->

	Texts = 
	[{titan,"The first Twitter titan, like @@, to join Snoopon.me will ascend to demi-godhood as the Deity of Openness"},
	 {journey,"A journey to million followers starts with the first small screenshot posted to Snoopon.me ~ Anonymous cc: @@"},
	 {startup,"A 2010 'anti-privacy' startup to follow - Snoopon.me //tip: @@"},
	 {percent,"15% - Facebook, Google - 3%, Farmville - 2%... discovering what people are doing: Snoopon.me //tip: @@"},
	 {tagline,"Snoopon.me is how you answer truthfully to the Twitter tagline question: What are you doing now, @@?"},
	 {monitor,"A mild way to monitor your employees, @@: ask them to join Snoopon.me; and reciprocate"},
	 {farmville,"@@ Ask your boss to join Snoopon.me; know his current Farmville score"},
	 {love,"I would love to follow you on Snoopon.me one day, @@"}],

	Selected = lists:filter(fun({Tag,_Text}) ->
		sel(Tag, Profile)
	end, Texts),

	if Selected == [] ->
		none;
	true ->
		hd(tm_util:shuffle([Text || {_,Text} <- Selected]))
	end.

sel(farmville, _) ->
	false;
sel(_, {any,_,_,_}) ->
	true;
sel(_, {multiple,_,_,_}) ->
	true;
sel(titan, {_,_,3,_}) ->
	true;
sel(journey, {_,_,_,true}) ->
	true;
sel(startup, {biz,_,_,_}) ->
	true;
sel(percent, {author,_,_,_}) ->
	true;
sel(tagline, {author,_,_,_}) ->
	true;
sel(monitor, {clevel,_,_}) ->
	true;
sel(farmville, {manager,_,_}) ->
	true;
sel(love, {_,_,2,_}) ->
	true;
sel(love, {_,_,3,_}) ->
	true;
sel(_, _) ->
	false.

%%EOF
