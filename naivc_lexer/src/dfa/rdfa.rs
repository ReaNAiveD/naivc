use std::{
    collections::{BTreeSet, HashMap},
    ops::RangeInclusive,
    rc::Rc,
};

use crate::{
    dfa::{
        LabelledTransition, PriorizedTokenType, RangePartitioner,
        rrdfa::RRDeterministicFiniteAutomaton,
    },
    nfa::{NondeterministicFiniteAutomaton, StateHandle},
    regex::RegexCompiler,
};

#[derive(Debug, Clone)]
struct TokenPattern {
    accept: PriorizedTokenType,
    reversed_nfa: NondeterministicFiniteAutomaton,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct PatternHandle(usize);

impl PatternHandle {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NFAStateHandle {
    pattern: PatternHandle,
    state: StateHandle,
}

impl PartialOrd for NFAStateHandle {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.pattern.index().cmp(&other.pattern.index()) {
            std::cmp::Ordering::Equal => self.state.index().partial_cmp(&other.state.index()),
            ord => Some(ord),
        }
    }
}

impl Ord for NFAStateHandle {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.pattern.index().cmp(&other.pattern.index()) {
            std::cmp::Ordering::Equal => self.state.index().cmp(&other.state.index()),
            ord => ord,
        }
    }
}

#[derive(Debug, Clone)]
struct TokenPatternRegistry {
    patterns: Vec<TokenPattern>,
}

impl TokenPatternRegistry {
    pub fn new(patterns: Vec<TokenPattern>) -> Self {
        Self { patterns }
    }

    pub fn pattern(&self, handle: &PatternHandle) -> &TokenPattern {
        &self.patterns[handle.index()]
    }

    pub fn iter_patterns(&self) -> impl Iterator<Item = (PatternHandle, &TokenPattern)> {
        self.patterns
            .iter()
            .enumerate()
            .map(|(i, p)| (PatternHandle::new(i), p))
    }

    pub fn is_accept_state(&self, handle: &NFAStateHandle) -> bool {
        let pattern = self.pattern(&handle.pattern);
        pattern.reversed_nfa.accept() == handle.state
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct RDFAStateHandle(usize);

impl RDFAStateHandle {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NFAStateSet {
    states: BTreeSet<NFAStateHandle>,
}

#[derive(Debug, Clone)]
pub struct RDFAState {
    nfa_states: Rc<NFAStateSet>,
    pub transitions: BTreeSet<LabelledTransition<RDFAStateHandle>>,
    pub binding_patterns: BTreeSet<PriorizedTokenType>,
    pub accept_patterns: BTreeSet<PriorizedTokenType>,
}

impl RDFAState {
    pub fn new(subset: Rc<NFAStateSet>) -> Self {
        Self {
            nfa_states: subset,
            transitions: BTreeSet::new(),
            binding_patterns: BTreeSet::new(),
            accept_patterns: BTreeSet::new(),
        }
    }

    pub fn subset(&self) -> &Rc<NFAStateSet> {
        &self.nfa_states
    }
    pub fn add_transition(&mut self, range: RangeInclusive<char>, target: RDFAStateHandle) {
        self.transitions
            .insert(LabelledTransition { range, target });
    }
}

#[derive(Debug, Clone)]
pub struct ReversedRDFAState {
    /// The transitions are reversed compared to RDFAState,
    /// i.e., they could be overlapping and multiple transitions for the same character are allowed.
    pub transitions: BTreeSet<LabelledTransition<RDFAStateHandle>>,
    pub binding_patterns: BTreeSet<PriorizedTokenType>,
    pub accept_patterns: BTreeSet<PriorizedTokenType>,
}

impl ReversedRDFAState {
    pub fn add_transition(&mut self, range: RangeInclusive<char>, target: RDFAStateHandle) {
        self.transitions
            .insert(LabelledTransition { range, target });
    }

    pub fn next_by(&self, c: char) -> Vec<RDFAStateHandle> {
        let mut targets = Vec::new();
        for transition in &self.transitions {
            if transition.range.contains(&c) {
                targets.push(transition.target);
            }
        }
        targets
    }
}

#[derive(Debug, Clone)]
struct PatternSet {
    bind: BTreeSet<PriorizedTokenType>,
    accept: BTreeSet<PriorizedTokenType>,
}

impl PatternSet {
    fn from_subset(spec: &TokenPatternRegistry, subset: &Rc<NFAStateSet>) -> Self {
        let mut bind = BTreeSet::new();
        let mut accept = BTreeSet::new();
        for nfa_state_handle in &subset.states {
            let pattern = spec.pattern(&nfa_state_handle.pattern);
            bind.insert(pattern.accept.clone());
            if spec.is_accept_state(nfa_state_handle) {
                accept.insert(pattern.accept.clone());
            }
        }
        Self { bind, accept }
    }
}

#[derive(Debug)]
pub struct ReversedDeterministicFiniteAutomaton {
    dfa_states: Vec<RDFAState>,
    accept_states: BTreeSet<RDFAStateHandle>,
    start_state: RDFAStateHandle,
}

impl ReversedDeterministicFiniteAutomaton {
    pub fn from_prioritized_token_types(
        token_types: impl Iterator<Item = PriorizedTokenType>,
    ) -> Self {
        let patterns: Vec<TokenPattern> = token_types
            .map(|priorized_token_type| {
                let regex_lexer = RegexCompiler::from_regex(&priorized_token_type.token_type.regex);
                let nfa = regex_lexer.compile();
                let reversed_nfa = nfa.reverse();
                TokenPattern {
                    accept: priorized_token_type,
                    reversed_nfa,
                }
            })
            .collect();
        let registry = TokenPatternRegistry::new(patterns);
        Self::from_nfa(registry)
    }

    fn from_nfa(spec: TokenPatternRegistry) -> Self {
        ReversedDFABuilder::build(spec)
    }

    pub fn state(&self, handle: RDFAStateHandle) -> &RDFAState {
        &self.dfa_states[handle.index()]
    }

    pub fn reachable(&self) -> BTreeSet<RDFAStateHandle> {
        let mut visited = BTreeSet::new();
        let mut stack = vec![self.start_state];
        while let Some(current) = stack.pop() {
            if visited.insert(current) {
                let state = self.state(current);
                for transition in &state.transitions {
                    stack.push(transition.target);
                }
            }
        }
        visited
    }

    pub fn reverse(self) -> RRDeterministicFiniteAutomaton {
        let reachable_states = self.reachable();
        let mut reversed_states: HashMap<_, _> = self
            .dfa_states
            .iter()
            .enumerate()
            .filter_map(|(idx, state)| {
                let handle = RDFAStateHandle(idx);
                if reachable_states.contains(&handle) {
                    Some((
                        RDFAStateHandle(idx),
                        ReversedRDFAState {
                            transitions: BTreeSet::new(),
                            binding_patterns: state.binding_patterns.clone(),
                            accept_patterns: state.accept_patterns.clone(),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect();
        for (idx, state) in self.dfa_states.iter().enumerate() {
            let from_handle = RDFAStateHandle(idx);
            if let Some(_) = reversed_states.get(&from_handle) {
                for transition in &state.transitions {
                    let to_handle = transition.target;
                    if let Some(to_state) = reversed_states.get_mut(&to_handle) {
                        to_state.add_transition(transition.range.clone(), from_handle);
                    }
                }
            }
        }
        let start_states: BTreeSet<RDFAStateHandle> = self
            .accept_states
            .iter()
            .filter(|handle| reachable_states.contains(handle))
            .cloned()
            .collect();
        RRDeterministicFiniteAutomaton::new(reversed_states, start_states, self.start_state)
    }
}

struct ReversedDFABuilder {
    spec: TokenPatternRegistry,
    dfa_states_lut: HashMap<Rc<NFAStateSet>, RDFAStateHandle>,
    dfa_states: Vec<RDFAState>,
    accept_states: BTreeSet<RDFAStateHandle>,
    worklist: Vec<RDFAStateHandle>,
}

impl ReversedDFABuilder {
    fn build(spec: TokenPatternRegistry) -> ReversedDeterministicFiniteAutomaton {
        let mut builder = Self {
            spec,
            dfa_states_lut: HashMap::new(),
            dfa_states: Vec::new(),
            accept_states: BTreeSet::new(),
            worklist: Vec::new(),
        };
        let start_state = builder.initialize_start_state();
        builder.process_worklist();
        ReversedDeterministicFiniteAutomaton {
            dfa_states: builder.dfa_states,
            accept_states: builder.accept_states,
            start_state,
        }
    }

    fn initialize_start_state(&mut self) -> RDFAStateHandle {
        let subset_rc = self.build_initial_subset();
        let start_handle = RDFAStateHandle::new(0);
        self.dfa_states_lut
            .insert(subset_rc.clone(), start_handle);
        self.dfa_states.push(RDFAState::new(subset_rc.clone()));
        let pattern_set = PatternSet::from_subset(&self.spec, &subset_rc);
        self.apply_pattern_set(start_handle, pattern_set);
        self.worklist.push(start_handle);
        start_handle
    }

    fn build_initial_subset(&self) -> Rc<NFAStateSet> {
        let mut subset = BTreeSet::new();
        for (pattern_handle, pattern) in self.spec.iter_patterns() {
            let start_state = pattern.reversed_nfa.start();
            let closure = pattern.reversed_nfa.epsilon_closure(start_state);
            subset.extend(closure.map(|state| NFAStateHandle {
                pattern: pattern_handle,
                state,
            }));
        }
        Rc::new(NFAStateSet { states: subset })
    }

    fn apply_pattern_set(&mut self, handle: RDFAStateHandle, pattern_set: PatternSet) {
        let state = &mut self.dfa_states[handle.index()];
        state.binding_patterns.extend(pattern_set.bind);
        state.accept_patterns.extend(pattern_set.accept);
        if !state.accept_patterns.is_empty() {
            self.accept_states.insert(handle);
        }
    }

    fn process_worklist(&mut self) {
        while let Some(current_handle) = self.worklist.pop() {
            self.expand_state(current_handle);
        }
    }

    fn expand_state(&mut self, current_handle: RDFAStateHandle) {
        let current_subset_rc = {
            let state = &self.dfa_states[current_handle.index()];
            state.subset().clone()
        };
        let transition_set = current_subset_rc.states.iter().flat_map(|handle| {
            let pattern = self.spec.pattern(&handle.pattern);
            pattern
                .reversed_nfa
                .state(handle.state)
                .char_transitions
                .iter()
                .map(|transition| &transition.range)
        });
        let partitioned_ranges = RangePartitioner::new(transition_set);
        for range in partitioned_ranges {
            let mut next_subset = BTreeSet::new();
            for nfa_state_handle in &current_subset_rc.states {
                let pattern = self.spec.pattern(&nfa_state_handle.pattern);
                let nfa_state = pattern.reversed_nfa.state(nfa_state_handle.state);
                if let Some(next_state) = nfa_state.next_by(*range.start()) {
                    let closure = pattern.reversed_nfa.epsilon_closure(next_state);
                    next_subset.extend(closure.map(|state| NFAStateHandle {
                        pattern: nfa_state_handle.pattern,
                        state,
                    }));
                }
            }
            if next_subset.is_empty() {
                continue;
            }
            let next_subset_rc = Rc::new(NFAStateSet {
                states: next_subset,
            });
            let pattern_set = PatternSet::from_subset(&self.spec, &next_subset_rc);
            let (next_handle, is_new_state) = self.get_or_create_state(next_subset_rc.clone());
            self.apply_pattern_set(next_handle, pattern_set);
            if is_new_state {
                self.worklist.push(next_handle);
            }
            self.dfa_states[current_handle.index()]
                .add_transition(range.clone(), next_handle);
        }
    }

    fn get_or_create_state(
        &mut self,
        subset: Rc<NFAStateSet>,
    ) -> (RDFAStateHandle, bool) {
        if let Some(handle) = self.dfa_states_lut.get(&subset) {
            (*handle, false)
        } else {
            let new_handle = RDFAStateHandle::new(self.dfa_states.len());
            self.dfa_states.push(RDFAState::new(subset.clone()));
            self.dfa_states_lut.insert(subset, new_handle);
            (new_handle, true)
        }
    }
}
