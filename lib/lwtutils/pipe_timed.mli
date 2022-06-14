open Pipe
open Wv
    
(*** Time-related pipes. ***)

(* Uses the Lwt library.
 *  => It should be used inside a Lwt main loop. *)

(* Each time an event is received, it is re-emitted, but then the forwarding is disabled for the given delay. 
 * The delay depends on the event. *)
val inhibit: ?wv:wv -> ?target:'a ws -> delay:('a -> float) -> 'a rs -> 'a rs
    

(* Sample a value every period. (Considering the last received event.)
 * If only_change is true, no event is emitted if no event is received (init is not used, then). 
 *
 * If prio is given, it associates a priority to each event. The emitted event is the one with highest priority 
 * received since the last sampling. In case of equality, keep the latest event. *)
val sample: ?wv:wv -> ?target:'a ws -> ?prio:('a -> int) -> ?only_change:bool -> period:float -> init:'a -> 'a rs -> 'a rs


(* In order to limit the rate of events, you have to provide two functions.
 * A function that maps events to a category (of type 'c).
 * A function that maps categories to delays. *)
type ('a, 'c) rate_limit =
  { (* Returns the category or None if this event should not be rate-limited. *)
    category: 'a -> 'c option ;

    (* Returns the minimal delay, in s, for this category. *)
    delay: 'c -> float }

val cst_cat: float -> ('a, unit) rate_limit

(* A rate limit ensures the following invariants.
 * (To ease the explanation, we only consider one event 'ev', one category, whose delay is 'd'.
 *  In practice, events in the same categoy can be considered equal).
 *
 * We distinguish : ev-in(x), events signaled to the pool by the function 'send',
 * and ev-out(x), events sent to callbacks by the pool.
 * When an event does not have a category, all ev_in are immediately mapped to ev_out.
 *
 * INVARIANTS :
 *   A - Two occurrences of ev-out(_) are always separated in time by at least d.
 *   (That is, two occurrences of ev-out of the same category c are separated by at least delay(c) ).
 *
 *   We say that ev is clear when the latest occurrence of ev-out(_) was more than d seconds ago.
 *
 *   B - If ev-in(x) occurs and ev is clear, then ev-out(x) immediately occurs.
 *       If ev-in(x) occurs and ev is not clear, then some ev-out(y) will occur within at most d seconds.
 *
 *   C - When ev-out(y) is emitted, it equals the latest available value of ev-in(x).
 *)

val limit: ?wv:wv -> ?target:'a ws -> ('a, 'c) rate_limit -> 'a rs -> 'a rs
(* Note that a rate-limited pipe becomes asynchronous.
 * The output events it produces may happen later than its input events. *)

(* Another time-dependant pipe: sends only 'stabilized' events.
 * An event is stable when it is the last event of its category for a given delay.
 *
 * INVARIANT: ev-out(x) is emitted if and only if ev-in(x) was received exactly d seconds ago
 *            and no other ev-in(y) event was received since then.
 *
 * Hence, there is always (at least) a d-seconds lag between ev-in and ev-out.
 * If ev-in is not stabilized (e.g. reception every d/2 seconds), then ev-out does not occur.
 *
 * (Like 'limit' above, events are grouped by categories. See above)
 *)
val stabilized: ?wv:wv -> ?target:'a ws -> ('a, 'c) rate_limit -> 'a rs -> 'a rs
