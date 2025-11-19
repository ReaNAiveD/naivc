use std::{collections::BTreeSet, ops::RangeInclusive};

use crate::regex::RangeSet;

/// Represents a nondeterministic finite automaton (NFA) for pattern matching.
///
/// An NFA consists of states connected by transitions that can match character ranges.
/// It has a single start state and a single accept state.
#[derive(Debug, Clone)]
pub struct NondeterministicFiniteAutomaton {
    start: StateHandle,
    accept: StateHandle,
    states: Vec<State>,
}

impl Default for NondeterministicFiniteAutomaton {
    fn default() -> Self {
        Self::empty()
    }
}

impl NondeterministicFiniteAutomaton {
    /// Creates a new NFA with initial start and accept states.
    fn new() -> Self {
        let states = vec![
            State::new(), // Start state
            State::new(), // Accept state
        ];

        Self {
            start: StateHandle::new(0),
            accept: StateHandle::new(1),
            states,
        }
    }

    pub fn empty() -> Self {
        let mut nfa = Self::new();
        // Add epsilon transition from start to accept
        nfa.add_epsilon_transition(nfa.start, nfa.accept);
        nfa
    }

    /// Creates an NFA that matches a single character.
    pub fn from_char(c: char) -> Self {
        Self::from_range_set(RangeSet::from_char(c))
    }

    /// Creates an NFA that matches a character range.
    pub fn from_range(start: char, end: char) -> Self {
        Self::from_range_set(RangeSet::from_range(start, end))
    }

    /// Creates an NFA that matches any character in the given range set.
    pub fn from_range_set(range_set: RangeSet) -> Self {
        let mut nfa = Self::new();
        nfa.add_range_set_transition(nfa.start, nfa.accept, range_set);
        nfa
    }

    /// Returns the start state handle.
    pub fn start(&self) -> StateHandle {
        self.start
    }

    /// Returns the accept state handle.
    pub fn accept(&self) -> StateHandle {
        self.accept
    }

    /// Adds a new intermediate state to the NFA.
    pub fn add_intermediate_state(&mut self) -> StateHandle {
        self.add_state(State::new())
    }

    /// Adds a state to the NFA and returns its handle.
    pub fn add_state(&mut self, state: State) -> StateHandle {
        let index = self.states.len();
        self.states.push(state);
        StateHandle::new(index)
    }

    /// Adds a transition between two states.
    pub fn add_range_set_transition(
        &mut self,
        from: StateHandle,
        to: StateHandle,
        range_set: RangeSet,
    ) {
        self.states[from.index()].add_range_set_transition(range_set, to);
    }

    /// Adds an epsilon transition (empty transition) between two states.
    pub fn add_epsilon_transition(&mut self, from: StateHandle, to: StateHandle) {
        self.states[from.index()].add_epsilon_transition(to);
    }

    /// Gets a reference to a state by its handle.
    pub fn state(&self, handle: StateHandle) -> &State {
        &self.states[handle.index()]
    }

    /// Gets a mutable reference to a state by its handle.
    pub fn state_mut(&mut self, handle: StateHandle) -> &mut State {
        &mut self.states[handle.index()]
    }

    /// Offsets all state indices by the given amount.
    /// Used internally when combining NFAs.
    fn offset_states(&mut self, index_offset: usize) {
        for state in &mut self.states {
            state.offset_transitions(index_offset);
        }
        self.start = self.start.offset(index_offset);
        self.accept = self.accept.offset(index_offset);
    }

    /// Applies the Kleene closure (*) operator to this NFA.
    /// The resulting NFA matches zero or more repetitions of the original pattern.
    pub fn closure(mut self) -> Self {
        let new_start = self.add_intermediate_state();
        let new_accept = self.add_intermediate_state();

        // Epsilon from new start to old start
        self.add_epsilon_transition(new_start, self.start);
        // Epsilon from new start to new accept (for zero matches)
        self.add_epsilon_transition(new_start, new_accept);
        // Epsilon from old accept to new accept
        self.add_epsilon_transition(self.accept, new_accept);
        // Epsilon from old accept back to old start (for repetition)
        self.add_epsilon_transition(self.accept, self.start);

        self.start = new_start;
        self.accept = new_accept;
        self
    }

    /// Applies the positive closure (+) operator to this NFA.
    /// The resulting NFA matches one or more repetitions of the original pattern.
    pub fn positive_closure(mut self) -> Self {
        let new_accept = self.add_intermediate_state();

        // Epsilon from old accept to new accept
        self.add_epsilon_transition(self.accept, new_accept);
        // Epsilon from old accept back to old start (for repetition)
        self.add_epsilon_transition(self.accept, self.start);

        self.accept = new_accept;
        self
    }

    /// Creates the union of multiple NFAs.
    /// The resulting NFA matches any of the input patterns.
    pub fn union_many(automata: impl IntoIterator<Item = Self>) -> Self {
        let mut result = Self::new();
        let new_start = result.start;
        let new_accept = result.accept;
        for mut nfa in automata {
            // Offset the NFA's states
            let index_offset = result.states.len();
            nfa.offset_states(index_offset);
            // Merge the states
            result.states.extend(nfa.states);
            // Epsilon from new start to nfa's start
            result.add_epsilon_transition(new_start, nfa.start);
            // Epsilon from nfa's accept to new accept
            result.add_epsilon_transition(nfa.accept, new_accept);
        }
        result
    }

    /// Concatenates this NFA with another.
    /// The resulting NFA matches the first pattern followed by the second.
    pub fn concat(mut self, mut other: Self) -> Self {
        // Offset the other NFA's states
        let index_offset = self.states.len();
        other.offset_states(index_offset);

        // Connect old accept to other's start with epsilon
        self.add_epsilon_transition(self.accept, other.start);

        // Merge the states
        self.states.extend(other.states);

        // Update accept state
        self.accept = other.accept;
        self
    }

    /// Makes this NFA optional (? operator).
    /// The resulting NFA matches zero or one occurrence of the pattern.
    pub fn optional(mut self) -> Self {
        let new_start = self.add_intermediate_state();
        let new_accept = self.add_intermediate_state();

        // Epsilon from new start to old start (for matching)
        self.add_epsilon_transition(new_start, self.start);
        // Epsilon from new start to new accept (for skipping)
        self.add_epsilon_transition(new_start, new_accept);
        // Epsilon from old accept to new accept
        self.add_epsilon_transition(self.accept, new_accept);

        self.start = new_start;
        self.accept = new_accept;
        self
    }

    /// Applies a quantifier {min,max} to this NFA.
    pub fn quantify(self, min: Option<usize>, max: Option<usize>) -> Self {
        match (min, max) {
            (Some(0), Some(1)) => self.optional(),
            (Some(0), None) => self.closure(),
            (Some(1), None) => self.positive_closure(),
            (Some(n), Some(m)) if n == m => self.repeat_exact(n),
            (Some(min), Some(max)) => self.repeat_range(min, max),
            (Some(min), None) => self.repeat_at_least(min),
            _ => panic!("Invalid quantifier bounds"),
        }
    }

    /// Repeats this NFA exactly n times.
    fn repeat_exact(self, n: usize) -> Self {
        if n == 0 {
            return Self::empty();
        }
        let mut result = self.clone();
        for _ in 1..n {
            result = result.concat(self.clone());
        }
        result
    }

    /// Repeats this NFA between min and max times (inclusive).
    fn repeat_range(self, min: usize, max: usize) -> Self {
        if min > max {
            panic!("Invalid range: min ({}) > max ({})", min, max);
        }

        // Build the minimum required repetitions
        let mut result = self.clone().repeat_exact(min);

        // Add optional repetitions up to max
        for _ in min..max {
            let optional_part = self.clone().optional();
            result = result.concat(optional_part);
        }

        result
    }

    /// Repeats this NFA at least min times.
    fn repeat_at_least(self, min: usize) -> Self {
        if min == 0 {
            return self.closure();
        }

        // min repetitions followed by closure
        let required = self.clone().repeat_exact(min);
        let optional = self.closure();
        required.concat(optional)
    }

    /// Returns the number of states in the NFA.
    pub fn state_count(&self) -> usize {
        self.states.len()
    }

    /// Computes the epsilon closure of a given state.
    /// Returns all states reachable from the given state through any number of epsilon transitions,
    /// including the state itself.
    pub fn epsilon_closure(&self, state: StateHandle) -> impl Iterator<Item = StateHandle> {
        let mut closure = BTreeSet::new();
        let mut stack = vec![state];
        while let Some(current) = stack.pop() {
            if closure.insert(current) {
                for next_state in self.state(current).epsilon_targets() {
                    stack.push(next_state);
                }
            }
        }
        closure.into_iter()
    }

    pub fn reverse(&self) -> Self {
        let mut reversed_nfa = Self {
            start: self.accept,
            accept: self.start,
            states: self.states.iter().map(|_| State::new()).collect(),
        };
        for (idx, state) in self.states.iter().enumerate() {
            for transition in &state.char_transitions {
                let from = StateHandle::new(transition.target.index());
                let to = StateHandle::new(idx);
                reversed_nfa
                    .state_mut(from)
                    .add_char_transition(transition.range.clone(), to);
            }
            for epsilon_target in &state.epsilon_transitions {
                let from = *epsilon_target;
                let to = StateHandle::new(idx);
                reversed_nfa.state_mut(from).add_epsilon_transition(to);
            }
        }
        reversed_nfa
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct StateHandle(usize);

impl StateHandle {
    /// Creates a new state handle with the given index.
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    /// Returns the index of this state.
    pub fn index(&self) -> usize {
        self.0
    }

    pub fn offset(&self, offset: usize) -> Self {
        Self(self.0 + offset)
    }
}

#[derive(Debug, Clone)]
pub struct State {
    // pub state_type: StateType,
    pub char_transitions: BTreeSet<LabelledTransition>,
    pub epsilon_transitions: BTreeSet<StateHandle>,
}

impl State {
    /// Creates a new empty state.
    pub fn new() -> Self {
        Self {
            char_transitions: BTreeSet::new(),
            epsilon_transitions: BTreeSet::new(),
        }
    }

    /// Adds a character transition to this state.
    /// Assumes ranges don't overlap.
    pub fn add_char_transition(&mut self, range: RangeInclusive<char>, target: StateHandle) {
        self.char_transitions
            .insert(LabelledTransition { range, target });
    }

    /// Adds transitions for a RangeSet (which may contain multiple ranges).
    pub fn add_range_set_transition(&mut self, range_set: RangeSet, target: StateHandle) {
        for range in range_set.iter() {
            self.add_char_transition(range.clone(), target);
        }
    }

    /// Adds an epsilon transition to this state.
    pub fn add_epsilon_transition(&mut self, target: StateHandle) {
        self.epsilon_transitions.insert(target);
    }

    /// Returns true if this state has no outgoing transitions.
    pub fn is_dead_end(&self) -> bool {
        self.char_transitions.is_empty() && self.epsilon_transitions.is_empty()
    }

    /// Returns true if this state has epsilon transitions.
    pub fn has_epsilon_transitions(&self) -> bool {
        !self.epsilon_transitions.is_empty()
    }

    /// Finds the next state for a given character.
    /// Could use binary search for better performance with many transitions.
    pub fn next_by(&self, c: char) -> Option<StateHandle> {
        // Binary search could be implemented here if needed
        for transition in &self.char_transitions {
            if transition.range.contains(&c) {
                return Some(transition.target);
            }
            if transition.range.start() > &c {
                break; // Ranges are sorted, no need to check further
            }
        }
        None
    }

    /// Gets all epsilon transition targets.
    pub fn epsilon_targets(&self) -> impl Iterator<Item = StateHandle> + '_ {
        self.epsilon_transitions.iter().copied()
    }

    pub fn offset_transitions(&mut self, offset: usize) {
        self.char_transitions = self
            .char_transitions
            .iter()
            .map(|t| LabelledTransition {
                range: t.range.clone(),
                target: StateHandle(t.target.index() + offset),
            })
            .collect();
        self.epsilon_transitions = self
            .epsilon_transitions
            .iter()
            .map(|h| StateHandle(h.index() + offset))
            .collect();
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LabelledTransition {
    pub range: RangeInclusive<char>,
    pub target: StateHandle,
}

impl Ord for LabelledTransition {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.range.start().cmp(other.range.start()) {
            std::cmp::Ordering::Equal => match self.range.end().cmp(other.range.end()) {
                std::cmp::Ordering::Equal => self.target.cmp(&other.target),
                ord => ord,
            },
            ord => ord,
        }
    }
}

impl PartialOrd for LabelledTransition {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
    pub range_set: Option<RangeSet>,
    pub from: StateHandle,
    pub to: StateHandle,
}

impl Transition {
    /// Creates a new transition.
    pub fn new(from: StateHandle, to: StateHandle, range_set: RangeSet) -> Self {
        Self {
            range_set: Some(range_set),
            from,
            to,
        }
    }

    /// Creates an epsilon transition (matches empty string).
    pub fn epsilon(from: StateHandle, to: StateHandle) -> Self {
        Self {
            range_set: None,
            from,
            to,
        }
    }

    /// Returns true if this is an epsilon transition.
    pub fn is_epsilon(&self) -> bool {
        self.range_set == None
    }

    pub fn offset(&mut self, offset: usize) {
        self.from.offset(offset);
        self.to.offset(offset);
    }
}
