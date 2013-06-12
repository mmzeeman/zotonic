%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012-2013 Maas-Maarten Zeeman
%% @doc Statistics used in Zotonic core

%% Copyright 2012-2013 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Collection of statistic operations.

-record(stats_from, 
    {host=zotonic,
     system=core,
     rsc_id=undefined
    }).

%% @doc Counters provide increment and decrement capabilities for a single value.
-record(counter, {name, op=incr, value=1}).

%% @doc Meter counters are increment only counters with mean rates and moving averages applied. 
-record(meter, {name, op=incr, value=1}).

%% @doc Histograms are collection of values that have statistical analysis dont to them such
%% as mean, min, max and percentile.
-record(histogram, {name, value=undefined}).

