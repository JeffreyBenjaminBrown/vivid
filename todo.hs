features to add, in cumulative order
  distribute messages across synths
  keeo a loop
    then multiple loops of varying durations
  make functions for manipulating loops
    spend a long time on this
  make a language

constraints
  want to change tempo
  want to start without delay
  user might redefine loop halfway through
  loops will have different lengths
  want sequences to be vectors
    they're easier to work with than functions

maybe do this
  compute each second in advance
  maintain a "time zero"
    When the user says to start, record time0 = the current time.
      Refer to that start for each upcoming second of planning.
    When the user says to change tempo, redefine time0 = the current time.
  Types
    A synth message is:
      A map from parameter names (strings) to values.
      To run a synth message, loop through each param name
        that exists (for that synth),
        even if it's not part of the message.
    A synth plan can be represented multiple ways:
      Each has a total duration
      An ordered sequence of (duration, synth message) pairs.
      Synths are never really off; time between notes = amplitude zero
    A collection of concurrent notes has to be distributed across synths
      automatically, rather than requiring the user to say which go where.
    An SCPlan is a map from synth plans (not oneSecondSynthPlans) to synth names.
